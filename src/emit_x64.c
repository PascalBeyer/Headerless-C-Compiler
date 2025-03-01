
//////////////////////////////////////////////////////////////////////////
//                               Assembler                              //
//////////////////////////////////////////////////////////////////////////

// Main entry point is 'emit_code_for_function'.
// Does a recursive tree walk in 'emit_code_for_ast' and writes out assembly
// into a linear buffer. Any step returns an 'emit_location'.
// Registers get allocated linearly and can be accessed via
//     'context->register_to_emit_location_map'.
// When to many registers are allocated they get spilled to the stack, and
// the emit location gets replaced by a 'register_relative' one.
// There are currently two registers maps, one for XMM registers and one
// for General Purpose Registers.
// All emit_locations are 'deleted' after each statement.
//                                                                -12.09.2020

enum rex_encoding{
    REXB = 0x41, // makes the modrm 'regm' field into an exteded register (r8 - r15) and some other stuff
    REXX = 0x42, // makes the sib 'index' field into an exteded register (r8 - r15)
    REXR = 0x44, // makes the modrm 'reg' field into an exteded register (r8 - r15)
    REXW = 0x48,
};

// calling convention x64(windows)
// "The first four integer or pointer parameters are passed in the rcx, rdx, r8, and r9 registers."
// "The first four floating-point parameters are passed in the first four SSE registers, xmm0-xmm3."
// "Any additional arguments are passed on the stack."
// "An integer or pointer return value is returned in the rax register, while a floating-point return value is returned in xmm0."
// rbx, rbp, rdi, rsi, r12-r15 are to be saved.

enum x64_OPCODE{
    ADD_REGM8_REG8                     = 0x0,
    ADD_REGM_REG                       = 0x1,
    ADD_REG8_REGM8                     = 0x2,
    ADD_REG_REGM                       = 0x3, // size bigger than 8, determined by rex and mod/rm
    ADD_AL_IMMEDIATE8                  = 0x4,
    ADD_RAX_IMMIDIATE                  = 0x5,
    
    INVALID_INSTRUCTION_0x6            = 0x6,
    INVALID_INSTRUCTION_0x7            = 0x7,
    
    OR_REGM8_REG8                      = 0x8,
    OR_REGM_REG                        = 0x9,
    OR_REG8_REGM8                      = 0xA,
    OR_REG_REGM                        = 0xB, // size bigger than 8, determined by rex and mod/rm
    OR_AL_IMMEDIATE8                   = 0xC,
    OR_RAX_IMMIDIATE                   = 0xD, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0xE            = 0xE,
    TWO_BYTE_INSTRUCTION_PREFIX        = 0xF,
    
    // Add with Carry                  = dest <- dest + src + CF
    ADD_WITH_CARRY_REGM8_REG8          = 0x10,
    ADD_WITH_CARRY_REGM_REG            = 0x11,
    ADD_WITH_CARRY_REG8_REGM8          = 0x12,
    ADD_WITH_CARRY_REG_REGM            = 0x13, // size bigger than 8, determined by rex and mod/rm
    ADD_WITH_CARRY_AL_IMMEDIATE8       = 0x14,
    ADD_WITH_CARRY_RAX_IMMIDIATE       = 0x15, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0x16           = 0x16,
    INVALID_INSTRUCTION_0x17           = 0x17,
    
    // Integer subtraction with borrow = dest <- dest - (src + CF)
    SBB_REGM8_REG8                     = 0x18,
    SBB_REGM_REG                       = 0x19,
    SBB_REG8_REGM8                     = 0x1A,
    SBB_REG_REGM                       = 0x1B, // size bigger than 8, determined by rex and mod/rm
    SBB_AL_IMMEDIATE8                  = 0x1C,
    SBB_RAX_IMMIDIATE                  = 0x1D, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0x1E           = 0x1E,
    INVALID_INSTRUCTION_0x1F           = 0x1F,
    
    AND_REGM8_REG8                     = 0x20,
    AND_REGM_REG                       = 0x21,
    AND_REG8_REGM8                     = 0x22,
    AND_REG_REGM                       = 0x23, // size bigger than 8, determined by rex and mod/rm
    AND_AL_IMMEDIATE8                  = 0x24,
    AND_RAX_IMMIDIATE                  = 0x25, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0x26           = 0x26, // null Prefix
    INVALID_INSTRUCTION_0x27           = 0x27,
    
    SUB_REGM8_REG8                     = 0x28,
    SUB_REGM_REG                       = 0x29,
    SUB_REG8_REGM8                     = 0x2A,
    SUB_REG_REGM                       = 0x2B, // size bigger than 8, determined by rex and mod/rm
    SUB_AL_IMMEDIATE8                  = 0x2C,
    SUB_RAX_IMMIDIATE                  = 0x2D, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0x2E           = 0x2E, // prefix
    INVALID_INSTRUCTION_0x2F           = 0x2F,
    
    XOR_REGM8_REG8                     = 0x30,
    XOR_REGM_REG                       = 0x31,
    XOR_REG8_REGM8                     = 0x32,
    XOR_REG_REGM                       = 0x33, // size bigger than 8, determined by rex and mod/rm
    XOR_AL_IMMEDIATE8                  = 0x34,
    XOR_RAX_IMMIDIATE                  = 0x35, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0x36           = 0x36, // null Prefix
    INVALID_INSTRUCTION_0x37           = 0x37,
    
    // performs a sub and sets the flags accordingly
    // @note: rhs gets sign-extended
    CMP_REGM8_REG8                     = 0x38,
    CMP_REGM_REG                       = 0x39,
    CMP_REG8_REGM8                     = 0x3A,
    CMP_REG_REGM                       = 0x3B, // size bigger than 8, determined by rex and mod/rm
    CMP_AL_8BIT                        = 0x3C,
    CMP_RAX_IMMIDIATE                  = 0x3D, // can be extended by the rex prefix
    
    INVALID_INSTRUCTION_0x3E           = 0x3E, // prefix
    INVALID_INSTRUCTION_0x3F           = 0x3F,
    
    REX_PREFIX_0x40                    = 0x40,
    REX_PREFIX_0x41                    = 0x41,
    REX_PREFIX_0x42                    = 0x42,
    REX_PREFIX_0x43                    = 0x43,
    REX_PREFIX_0x44                    = 0x44,
    REX_PREFIX_0x45                    = 0x45,
    REX_PREFIX_0x46                    = 0x46,
    REX_PREFIX_0x47                    = 0x47,
    REX_PREFIX_0x48                    = 0x48,
    REX_PREFIX_0x49                    = 0x49,
    REX_PREFIX_0x4A                    = 0x4A,
    REX_PREFIX_0x4B                    = 0x4B,
    REX_PREFIX_0x4C                    = 0x4C,
    REX_PREFIX_0x4D                    = 0x4D,
    REX_PREFIX_0x4E                    = 0x4E,
    REX_PREFIX_0x4F                    = 0x4F,
    
    PUSH_REGISTER                      = 0x50, // + register
    PUSH_REGISTER_A                    = 0x50,
    PUSH_REGISTER_C                    = 0x51,
    PUSH_REGISTER_D                    = 0x52,
    PUSH_REGISTER_B                    = 0x53,
    PUSH_REGISTER_SP                   = 0x54,
    PUSH_REGISTER_BP                   = 0x55,
    PUSH_REGISTER_SI                   = 0x56,
    PUSH_REGISTER_DI                   = 0x57,
    
    POP_REGISTER                       = 0x58, // + register
    POP_REGISTER_A                     = 0x58,
    POP_REGISTER_C                     = 0x59,
    POP_REGISTER_D                     = 0x5A,
    POP_REGISTER_B                     = 0x5B,
    POP_REGISTER_SP                    = 0x5C,
    POP_REGISTER_BP                    = 0x5D,
    POP_REGISTER_SI                    = 0x5E,
    POP_REGISTER_DI                    = 0x5F,
    
    INVALID_INSTRUCTION_0x60           = 0x60,
    INVALID_INSTRUCTION_0x61           = 0x61,
    INVALID_INSTRUCTION_0x62           = 0x62,
    
    MOVE_WITH_SIGN_EXTENSION_REG_REGM     = 0x63,
    
    LEGACY_FS_SEGMENT_OVERRIDE_PREFIX     = 0x64,
    LEGACY_GS_SEGMENT_OVERRIDE_PREFIX     = 0x65,
    LEGACY_OPERAND_SIZE_OVERRIDE_PREFIX   = 0x66, // this one is used to switch to 16 bit stuff
    LEGACY_PERCISION_SIZE_OVERRIDE_PREFIX = 0x66,
    LEGACY_ADDRESS_SIZE_OVERRIDE_PREFIX   = 0x67,
    
    // 63 - 68 are legacy prefixes
    
    PUSH_IMMEDIATE           = 0x68, // size is either 16 or 32
    IMUL_REG_REGM_IMMIDIATE  = 0x69,
    PUSH_IMMEDIATE8          = 0x6A,
    IMUL_REG_REGM_SIGN_EXTENDED_IMMIDIATE8 = 0x6B,
    
    // 6C - 6F are port stuff, todo figure out what this actually is and what to do with them
    
    
    // FLAGS: 8 bit
    // CF - carry flag
    // PF - parity flag (1 even, 0 odd)
    // AF - adjust flag
    // ZF - zero flag
    // SF - sign flag
    // TF - trap flag
    // IF - interrupt flag
    // DF - direction flag
    // OF - overflow flag
    
    
    JUMP_REL8_IF_OVERFLOW                    = 0x70, // OF = 1
    JUMP_REL8_IF_NO_OVERFLOW                 = 0x71, // OF = 0
    JUMP_REL8_IF_CARRY                       = 0x72, // CF = 1 seems to also jump if (a < b)
    JUMP_REL8_IF_NOT_CARRY                   = 0x73, // CF = 0
    JUMP_REL8_IF_ZERO                        = 0x74, // ZF = 1
    JUMP_REL8_IF_NOT_ZERO                    = 0x75, // ZF = 0
    JUMP_REL8_IF_CARRY_OR_ZERO               = 0x76, // CF = 1 or  ZF = 1
    JUMP_REL8_IF_NOT_CARRY_AND_NOT_ZERO      = 0x77, // CF = 0 and ZF = 0
    JUMP_REL8_IF_SIGN                        = 0x78, // SF = 1
    JUMP_REL8_IF_NOT_SIGN                    = 0x79, // SF = 0
    JUMP_REL8_IF_EVEN                        = 0x7A, // PF = 1
    JUMP_REL8_IF_ODD                         = 0x7B, // PF = 0
    JUMP_REL8_IF_LESS                        = 0x7C, // SF != OF
    JUMP_REL8_IF_NOT_LESS                    = 0x7D, // SF == OF
    JUMP_REL8_IF_LESS_OR_ZERO                = 0x7E, // SF != OF or  ZF = 1
    JUMP_REL8_IF_NOT_LESS_AND_NOT_ZERO       = 0x7F, // SF == OF and ZF = 0
    
    REG_EXTENDED_OPCODE_REGM8_IMMIDIATE8 = 0x80,
    REG_EXTENDED_OPCODE_REGM_IMMIDIATE   = 0x81,
    
    INVALID_INSTRUCTION_0x82 = 0x82,
    
    REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8 = 0x83,
    
    REG_OPCODE_ADD = 0x0,
    REG_OPCODE_OR  = 0x1,
    REG_OPCODE_ADC = 0x2,
    REG_OPCODE_SBB = 0x3,
    REG_OPCODE_AND = 0x4,
    REG_OPCODE_SUB = 0x5,
    REG_OPCODE_XOR = 0x6,
    REG_OPCODE_CMP = 0x7, // rhs gets sign-extended
    
    // performs a logical and and sets the flags accordingly
    TEST_REGM8_REG8     = 0x84,
    TEST_REGM_REG       = 0x85,
    
    EXCHANGE_REG8_REGM8 = 0x86,
    EXCHANGE_REG_REGM   = 0x87,
    
    MOVE_REGM8_REG8     = 0x88,
    MOVE_REGM_REG       = 0x89,
    MOVE_REG8_REGM8     = 0x8A,
    MOVE_REG_REGM       = 0x8B,
    
    MOVE_REG_SREG       = 0x8C,  // help me i dunno what this does. todo
    
    LOAD_ADDRESS_REG_MEMORY_LOCATION = 0x8D,
    MOVE_SREG_REG                    = 0x8E,
    POP_REGM                         = 0x8F,
    
    ONE_BYTE_NOP                     = 0x90, // exchange RAX RAX, PAUSE if F3???? todo
    
    EXCHANGE_RCX_RAX                 = 0x91, // REX.R allows to access R8-R15
    EXCHANGE_RDX_RAX                 = 0x92, // todo
    EXCHANGE_RBX_RAX                 = 0x93,
    EXCHANGE_RSP_RAX                 = 0x94,
    EXCHANGE_RBP_RAX                 = 0x95,
    EXCHANGE_RSI_RAX                 = 0x96,
    EXCHANGE_RDI_RAX                 = 0x97,
    
    CONVERT_RAX_EAX_SIGN_EXTENDED    = 0x98, // sizes depend on REX.W and 16/32 bit operation mode
    SIGN_EXTEND_A_INTO_D             = 0x99,
    
    INVALID_INSTRUCTION_0x9A         = 0x9A,
    
    TEST_FOR_UNMASKED_FLOATING_POINT_EXEPTIONS = 0x9B,
    
    //WAIT_PREFIX                       = 0x9C, ????
    
    PUSH_FLAGS                        = 0x9C,
    POP_FLAGS                         = 0x9D,
    STORE_AH_INTO_FLAGS               = 0x9E,
    LOAD_AH_FROM_FLAGS                = 0x9F,
    
    MOVE_BYTE                         = 0xA4, // these always go
    MOVE_MULTIBYTE                    = 0xA5,
    
    STORE_BYTE                        = 0xAA,
    
    MOVE_REG8_IMMEDIATE8              = 0xB0, // + REGISTER
    MOVE_REG_IMMEDIATE                = 0xB8, // + REGISTER
    
    SHIFT_OR_ROTATE_REGM8_IMMEDIATE8  = 0xC0,
    SHIFT_OR_ROTATE_REGM_IMMEDIATE8   = 0xC1,
    
    REG_OPCODE_ROTATE_LEFT = 0,
    REG_OPCODE_ROTATE_RIGHT = 1,
    REG_OPCODE_ROTATE_WITH_CARRY_LEFT = 2,
    REG_OPCODE_ROTATE_WITH_CARRY_RIGHT = 3,
    
    REG_OPCODE_SHIFT_LEFT  = 4, // @note: this is the same as shift arith left as we assume the top bits are 1
    REG_OPCODE_SHIFT_RIGHT = 5,
    REG_OPCODE_SHIFT_ARITHMETIC_LEFT  = 6,
    REG_OPCODE_SHIFT_ARITHMETIC_RIGHT = 7,
    
    NEAR_RET_INSTRUCTION              = 0xC3,
    
    MOVE_REGM8_IMMEDIATE8             = 0xC6,
    MOVE_REGM_IMMEDIATE               = 0xC7,
    
    FAR_RET_INSTRUCTION               = 0xCB,
    
    SHIFT_OR_ROTATE_REGM8_1           = 0xD0,
    SHIFT_OR_ROTATE_REGM_1            = 0xD1,
    SHIFT_OR_ROTATE_REGM8_CL          = 0xD2,
    SHIFT_OR_ROTATE_REGM_CL           = 0xD3, // see subtable for 0xC0
    
    CALL_RELATIVE         = 0xE8, // this is a near call
    JUMP_REL32            = 0xE9,
    JUMP_REL8             = 0xEB,
    // some mov instructions do not need a rex.w to be 64 bit adress size.
    
    // F0 is lock prefix see 'enum prefix' below
    // F1 is call to interruppt or something
    
    HALT_THE_CPU          = 0xF4,
    COMPLEMENT_CARRY_FLAG = 0xF5,
    
    REG_EXTENDED_UNARY_REGM8 = 0xF6,
    REG_EXTENDED_UNARY_REGM  = 0xF7,
    
    REG_OPCODE_NOT_REGM      = 0x2,
    REG_OPCODE_NEGATE_REGM   = 0x3,
    // these muliply or divide into rdx:rax
    REG_OPCODE_MUL_REGM_RAX  = 0x4,
    REG_OPCODE_IMUL_REGM_RAX = 0x5,
    REG_OPCODE_DIV_REGM_RAX  = 0x6,
    REG_OPCODE_IDIV_REGM_RAX = 0x7,
    
    REG_EXTENDED_OPCODE_FE    = 0xFE,
    REG_EXTENDED_OPCODE_FF    = 0xFF,
    FF_INCREMENT_REGM         = 0,
    FF_DECREMENT_REGM         = 1,
    FF_CALL_REGM              = 2,
    FF_CALL_REGM_RIP_RELATIVE = 3,
};

enum TWO_BYTE_OPCODES{ // these are of the form 0x0F opcode
    
    MOVE_UNALIGNED_XMM_REGM = 0x10, // movups (00), movupd(66), movss(f3), movsd(f2)
    MOVE_UNALIGNED_REGM_XMM = 0x11, // movups (00), movupd(66), movss(f3), movsd(f2)
    
    MOVE_ALIGNED_XMM_XMMM = 0x28, // movaps (00), movapd (66),
    MOVE_ALIGNED_XMMM_XMM = 0x29, // movaps (00), movapd (66),
    
    READ_TIME_STAMP_COUNTER = 0x31,
    
    // @cleanup: where to use which?
    UNORDERED_COMPARE_XMM  = 0x2e,
    COMPARE_XMM  = 0x2f,
    
    AND_XMM = 0x54,
    AND_NOT_XMM = 0x55,
    OR_XMM  = 0x56,
    XOR_XMM = 0x57,
    ADD_XMM = 0x58,
    MUL_XMM = 0x59,
    CONVERT_DOUBLE_AND_SINGLE = 0x5A,
    
    SUB_XMM = 0x5c,
    MIN_XMM = 0x5d,
    DIV_XMM = 0x5e,
    MAX_XMM = 0x5f,
    
    
    MOVQ_REGM_XMM = 0x7e,
    
    // relative jumps
    JUMP_REL32_IF_OVERFLOW              = 0x80, // jo
    JUMP_REL32_IF_NOT_OVERFLOW          = 0x81, // jno
    JUMP_REL32_IF_SMALLER               = 0x82, // jb,   jnae, jc
    JUMP_REL32_IF_BIGGER_EQUALS         = 0x83, // jnb,  jae,  jnc
    JUMP_REL32_IF_EQUALS                = 0x84, // je,   jz
    JUMP_REL32_IF_UNEQUALS              = 0x85, // jne,  jnz
    JUMP_REL32_IF_SMALLER_EQUALS        = 0x86, // jbe,  jna
    JUMP_REL32_IF_BIGGER                = 0x87, // jnbe, ja
    JUMP_REL32_IF_NEGATIVE              = 0x88, // js
    JUMP_REL32_IF_POSITIVE              = 0x89, // jns
    JUMP_REL32_IF_EVEN                  = 0x8A, // jp,   jpe
    JUMP_REL32_IF_ODD                   = 0x8B, // jnp,  jpo
    JUMP_REL32_IF_SMALLER_SIGNED        = 0x8C, // jl,   jnge
    JUMP_REL32_IF_BIGGER_EQUALS_SIGNED  = 0x8D, // jnl,  jge
    JUMP_REL32_IF_SMALLER_EQUALS_SIGNED = 0x8E, // jle,  jng
    JUMP_REL32_IF_BIGGER_SIGNED         = 0x8F, // jnle, jg
    
    
    SET_REGM8_IF_OVERFLOW              = 0x90,
    SET_REGM8_IF_NO_OVERFLOW           = 0x91,
    SET_REGM8_IF_SMALLER               = 0x92,
    SET_REGM8_IF_BIGGER_EQUALS         = 0x93,
    SET_REGM8_IF_EQUALS                = 0x94,
    SET_REGM8_IF_UNEQUALS              = 0x95,
    SET_REGM8_IF_SMALLER_EQUALS        = 0x96,
    SET_REGM8_IF_BIGGER                = 0x97,
    SET_REGM8_IF_NEGATIVE              = 0x98,
    SET_REGM8_IF_POSITIVE              = 0x99,
    SET_REGM8_IF_EVEN                  = 0x9A,
    SET_REGM8_IF_ODD                   = 0x9B,
    SET_REGM8_IF_SMALLER_SIGNED        = 0x9C,
    SET_REGM8_IF_BIGGER_EQUALS_SIGNED  = 0x9D,
    SET_REGM8_IF_SMALLER_EQUALS_SIGNED = 0x9E,
    SET_REGM8_IF_BIGGER_SIGNED         = 0x9F,
    
    IMUL_REG_REGM = 0xAF,
    
    COMPARE_EXCHANGE_REG8_REGM8 = 0xB0,
    COMPARE_EXCHANGE_REG_REGM   = 0xB1,
    
    MOVE_WITH_ZERO_EXTENSION_REG_REGM8  = 0xB6,
    MOVE_WITH_ZERO_EXTENSION_REG_REGM16 = 0xB7,
    
    MOVE_WITH_SIGN_EXTENSION_REG_REGM8  = 0xBE,
    MOVE_WITH_SIGN_EXTENSION_REG_REGM16 = 0xBF,
    
    EXCHANGE_ADD_REGM8_REG8 = 0xC0,
    EXCHANGE_ADD_REGM_REG   = 0xC1,
    
    REG_EXTENDED_OPCODE_C7 = 0xC7,
    C7_COMPARE_EXCHANGE_REGM128 = 1,
};

#define register_is_extended(reg) ((reg) & 0xF8)
enum register_encoding{
    INVALID_REGISTER = -1, // used to say that there is no index register (remove me once we are relative to emit_locations)
    REGISTER_A  = 0, // accumulatior
    REGISTER_C  = 1, // count
    REGISTER_D  = 2, // data
    
    REGISTER_B  = 3, // base pointer to data
    REGISTER_SP = 4, // stack pointer
    REGISTER_BP = 5, // stack base pointer
    REGISTER_SI = 6, // source index or pointer to data
    REGISTER_DI = 7, // destination index or pointer to data
    
    REGISTER_R8  = 8 + 0,
    REGISTER_R9  = 8 + 1,
    REGISTER_R10 = 8 + 2,
    REGISTER_R11 = 8 + 3,
    REGISTER_R12 = 8 + 4,
    REGISTER_R13 = 8 + 5,
    REGISTER_R14 = 8 + 6,
    REGISTER_R15 = 8 + 7,
    
    REGISTER_XMM0 = 0,
    REGISTER_XMM1 = 1,
    REGISTER_XMM2 = 2,
    REGISTER_XMM3 = 3,
    REGISTER_XMM4 = 4,
    REGISTER_XMM5 = 5,
    REGISTER_XMM6 = 6,
    REGISTER_XMM7 = 7,
    
    REGISTER_SIB_EXTENSION = REGISTER_SP,
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////

#define emit(val)     emit_bytes(context, 1, val)
#define emit_u16(val) emit_bytes(context, 2, val)
#define emit_u32(val) emit_bytes(context, 4, val)
#define emit_u64(val) emit_bytes(context, 8, val)

func smm emit_bytes_unchecked(struct context *context, smm size, u64 data){
    struct emit_pool *emit_pool = &context->emit_pool;
    
    memcpy(emit_pool->current, &data, size);
    smm offset = emit_pool->current - context->current_emit_base;
    assert(offset >= 0);
    emit_pool->current += size;
    return offset;
}

func smm emit_bytes(struct context *context, smm size, u64 data){
    assert(size <= 8);
    struct emit_pool *emit_pool = &context->emit_pool;
    
    if(emit_pool->current + size < emit_pool->end){
        return emit_bytes_unchecked(context, size, data);
    }else if(emit_pool->capacity < emit_pool->reserved){
        struct os_virtual_buffer buf = os_commit_memory(emit_pool->end, mega_bytes(100));
        if(!buf.memory){
            print("Memory error!\n");
            os_panic(1);
        }
        emit_pool->capacity += mega_bytes(100);
        emit_pool->end = emit_pool->base + emit_pool->capacity;
        return emit_bytes_unchecked(context, size, data);
    }else{
        report_error(context, context->current_function->base.token, "Too many bytes of code. Maximally %lld allowed.", emit_pool->capacity);
        return emit_pool->capacity; // @cleanup: test this path
    }
}

func smm get_bytes_emitted(struct context *context){
    return context->emit_pool.current - context->current_emit_base;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// 'sse_prefix' -  0x00 = packed float (or no prefix because not sse)
//                 0x66 = packed double
//                 0xf2 = double
//                 0xf3 = float


func struct prefixes get_sse_prefix_for_scalar(smm size){
    enum legacy_prefixes sse_prefix;
    if(size == 4){
        sse_prefix = ASM_PREFIX_SSE_float;
    }else if(size == 8){
        sse_prefix = ASM_PREFIX_SSE_double;
    }else{
        assert(size == 16); // @cleanup: do we need double here ever?
        sse_prefix = 0;
    }
    return create_prefixes(sse_prefix);
}


///////////////////////////////////////////////////////////////////////////////////////////////////////
struct opcode{
    u32 amount_of_bytes;
    u8 bytes[4];
};

func struct opcode one_byte_opcode(u8 op){
    struct opcode ret;
    ret.amount_of_bytes = 1;
    ret.bytes[0] = op;
    return ret;
}

func struct opcode two_byte_opcode(u8 op){
    struct opcode ret;
    ret.amount_of_bytes = 2;
    ret.bytes[0] = TWO_BYTE_INSTRUCTION_PREFIX;
    ret.bytes[1] = op;
    return ret;
}

func struct opcode three_byte_opcode(u8 op1, u8 op2){
    struct opcode ret;
    ret.amount_of_bytes = 3;
    ret.bytes[0] = TWO_BYTE_INSTRUCTION_PREFIX;
    ret.bytes[1] = op1;
    ret.bytes[2] = op2;
    return ret;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////
// The VEX-Prefix:
//     [Prefixes] [Vex] [OPCODE] [MODR/M] [SIB] [DISP] [IMM]
// 2-Byte version
//     C5 RvvvvLpp, R = ~REX.R, vvvv = ~(middle-register), L = 0-128, 1-256, pp = SIMD-prefix (0 = none, 1 = 66, 2 = F3, 11 = F2)
// 3-Byte version
//     C4 RXBm-mmmm WvvvvLpp, RXBW = ~REX.RXBW, m-mmmm = 1 = 0F, 2 = 0F 38, 3 0F 3A
//
// Examples:
// c5 fd | 7f 41 20 vmovdqa YMMWORD PTR [rcx+0x20], ymm0
// c5 = two byte vex
// fd = 0b11111101 
//        RvvvvLpp
//        ||   ||__ 66 simd prefix
//        ||   |___ 256-bit
//        ||_______ no additional register
//        |________ no REX.R
//
// c4 c1 7d 7f 40 20 vmovdqa YMMWORD PTR [r8+0x20],ymm0
// c4 = three byte vex
// c1 = 0b11000001
//        RXBmmmmm
//        |  |_____ 0F-prefix
//        |________ REX.B
// 7d = 0b01111101
//        WvvvvLpp
//        ||   ||__ 66 simd prefix
//        ||   |___ 256-bits
//        ||_______ no additional register
//        |________ REX.W
//
// c5 f0 58 01 vaddps xmm0,xmm1,XMMWORD PTR [rcx]
// c5 = two byte vex
// f0 = 0b11110000
//        ||   ||__ no SIMD prefix (ps)
//        ||   |___ 128-bit
//        ||_______ ~(1110) = 1 = xmm1
//        |________ no REX.R


// @WARNING:@WARNING:@WARNING:@WARNING:@WARNING:@WARNING: 
// dont use these with the 'emit_register_register' and such api. They expect legacy prefixes, not actual an actual prefix
// @cleanup: maybe strongly type legacy_prefixes!
enum prefix{
    LEGACY_PREFIX_LOCK                  = 0xF0,
    LEGACY_PREFIX_REPEAT_WHILE_NOT_ZERO = 0xF2,
    LEGACY_PREFIX_REPEAT_WHILE_ZERO     = 0xF3,
    LEGACY_PREFIX_REPEAT                = 0xF3,
    
    SSE_PREFIX_none          = 0x00,
    SSE_PREFIX_packed_float  = 0x00,
    SSE_PREFIX_packed_double = 0x66,
    SSE_PREFIX_double        = 0xf2,
    SSE_PREFIX_float         = 0xf3,
    
    
    // note that this is not true if there are no 'packed' versions,
    // i.e. for COMIS (COMPARE_SCALAR) 0x66 means 'double' nothing means 'float'
    SSE_PREFIX_NON_PACKED_OP_double = 0x66,
    SSE_PREFIX_NON_PACKED_OP_float  = 0x00,
};

func void emit_prefixes_and_opcode(struct context *context, struct prefixes _prefixes, enum register_encoding reg, enum register_encoding regm, enum register_encoding index, smm size, struct opcode opcode){
    enum legacy_prefixes prefixes = _prefixes.legacy_prefixes;
    
    if(prefixes & ASM_PREFIX_VEX){
        b32 is_three_byte = false;
        if(register_is_extended(regm) || (index >= 0  && register_is_extended(index))) is_three_byte = true;
        
        if(opcode.amount_of_bytes == 3) is_three_byte = true;
        
        assert((prefixes & (ASM_PREFIX_VEX | ASM_PREFIX_66 | ASM_PREFIX_F2 | ASM_PREFIX_F3)) == prefixes);
        assert(__popcnt((u32)prefixes) <= 2);
        
        u8 simd_prefix = 0;
        if(prefixes & ASM_PREFIX_66) simd_prefix = 1;
        if(prefixes & ASM_PREFIX_F3) simd_prefix = 2;
        if(prefixes & ASM_PREFIX_F2) simd_prefix = 3;
        
        u8 last_byte = (((~_prefixes.vex_register) & 0b1111) << 3) | ((size == 32) ? (1 << 2) : 0) | simd_prefix;
        if(is_three_byte){
            emit(0xc4); // three byte vex 
            
            // @note: inverted!
            u8 rex = (register_is_extended(reg) ? 0 : (1 << 2)) | (register_is_extended(regm) ? 0 : (1 << 0));
            if(index >= 0){
                rex |= register_is_extended(index) ? 0 : (1 << 1);
            }else{
                rex |= (1 << 1);
            }
            
            u8 mmmmm = 1; // 0F
            if(opcode.amount_of_bytes == 3){
                assert(opcode.bytes[1] == 0x38 || opcode.bytes[1] == 0x3A);
                mmmmm = opcode.bytes[1] == 0x38 ? 2 : 3;
            }
            
            u8 byte = (rex << 5) | mmmmm;
            emit(byte);
        }else{
            emit(0xc5); // two byte vex
            last_byte |= register_is_extended(reg) ? 0 : 0x80; // note: inverted!
        }
        
        // @cleanup: what does rexW do?
        emit(last_byte);
        emit(opcode.bytes[opcode.amount_of_bytes - 1]);
        
    }else{
        if(size == 2) prefixes |= ASM_PREFIX_66;
        if(prefixes & ASM_PREFIX_F0) emit(0xf0);
        if(prefixes & ASM_PREFIX_F2) emit(0xf2);
        if(prefixes & ASM_PREFIX_F3) emit(0xf3);
        if(prefixes & ASM_PREFIX_64) emit(0x64);
        if(prefixes & ASM_PREFIX_65) emit(0x65);
        if(prefixes & ASM_PREFIX_66) emit(0x66);
        if(prefixes & ASM_PREFIX_67) emit(0x67);
        
        u8 rex = 0;
        if(size == 8) rex |= REXW;
        if(register_is_extended(reg))  rex |= REXR;
        if(register_is_extended(regm)) rex |= REXB;
        if(index >= 0 && register_is_extended(index)) rex |= REXX;
        if(rex) emit(rex);
        
        // emit the opcode
        for(u32 i = 0; i < opcode.amount_of_bytes; i++){
            emit(opcode.bytes[i]);
        }
    }
    
}

// MODRM has 3 fields the mod, a register operand (R) and a memory operand (M)
// MODRM_MOD:
// mod 0: inst R, [M]
// mod 1: inst R, [M + disp8]
// mod 2: inst R, [M + disp(addressing mode)] // I think addressing mode should always be 32???
// mod 3: inst R, M
// note that "inst R, [M]" vs "inst [M], R" depends only on the opcode(inst). (i.e which one the memory operand is).

// if the memory operand M = REGISTER_BP, and MODRM_MOD = MODRM_REGM(0) then we get [rip + disp32]

// if the memory operand M = REGISTER_SP (and MOD != MODRM_REG(3)), then this means that a SIB-byte follows the MODRM-byte.
// SIB has 3 fields scale (2 bytes) index (3 bytes) base (3 bytes)
// so we get [base + scale * index + disp(MODRM)]
// index == REGISTER_SP means no index.

enum MODRM_MOD{
    MODRM_REGM   = 0,
    MODRM_REGM8  = 1,
    MODRM_REGM32 = 2,
    MODRM_REG    = 3,
};

func u8 make_modrm(u8 mod, u8 reg, u8 rm){
    assert(mod < 4);
    assert(reg < 8);
    assert(rm  < 8);
    return ((mod << 6) | (reg << 3) | (rm << 0));
}

// follows modrm if rm == REGISTER_SIB_EXTENSION (4)
// scale index base
func s8 make_sib(u8 shift, s8 index, s8 base){
    assert(shift < 4);
    assert(index < 8);
    assert(base  < 8);
    return ((shift << 6) | (index << 3) | (base << 0));
}

// this is now only used for reg extended and some internal stuff
func void emit_reg_reg__(struct context *context, u8 rex, u8 opcode, u8 reg, u8 regm){
    if(rex) emit(rex);
    emit(opcode);
    emit(make_modrm(MODRM_REG, reg, regm));
}

///////////////////////////////////////
// volitile registers:
// RAX - return register
// RCX - First integer argument
// RDX - Second integer argument
// R8  - Third integer argument
// R9  - Fourth integer argument

enum comp_condition{
    COMP_none,                     // jmp
    COMP_equals,                   // je, jz
    COMP_unequals,                 // jne, jnz
    COMP_not_zero = COMP_unequals, // jne, jnz
    COMP_smaller,                  // jb, jnae, jc
    COMP_smaller_equals,           // jbe, jna
    COMP_bigger,                   // jnbe, ja
    COMP_bigger_equals,            // jbe, jae, jnc
    COMP_smaller_signed,           // jl, jnge
    COMP_smaller_equals_signed,    // jle, jng
    COMP_bigger_signed,            // jnle, jg
    COMP_bigger_equals_signed,     // jnl, jge
    COMP_positive,                 // jns
    COMP_negative,                 // js
};

enum emit_location_state{
    EMIT_LOCATION_invalid,
    
    EMIT_LOCATION_loaded,
    EMIT_LOCATION_immediate,
    EMIT_LOCATION_register_relative,
    EMIT_LOCATION_conditional,
    EMIT_LOCATION_freed,
    
    EMIT_LOCATION_count,
};


struct emit_location{
    enum emit_location_state state;
    smm size;
    
    //
    // freeing is also prevented when spilling is prevented
    //
    u32 prevent_spilling;
    u32 prevent_freeing;
    
    enum register_kind register_kind_when_loaded;
    union{
        struct{
            enum register_encoding loaded_register;
            
            //:inline_asm_user_referenced_registers
            b32 inline_asm__was_used_by_user;
        }; // loaded & xmm
        struct{
            enum comp_condition condition;
        }; // conditional
        struct{
            smm value;
        }; // immediate
        struct{
            struct emit_location *base;
            struct emit_location *index; // null means not present
            
            smm log_index_scale;
            smm offset;
            struct ast *ast; // rip_relative iff (ast != null)
        }; // register_relative & rip_relative
    };
};

// @cleanup: I think we actually rely on this begin null now, so this abstraction is really
//           useless not... get rid of it!
#define emit_location_invalid(context) ((struct emit_location *)null)

func struct emit_location *emit_location_loaded(struct context *context, enum register_kind kind, enum register_encoding reg, smm size){
    assert(reg != INVALID_REGISTER);
    assert(!context->register_allocators[kind].emit_location_map[reg]);
    
    struct emit_location *ret = push_struct(&context->scratch, struct emit_location);
    ret->register_kind_when_loaded = kind;
    ret->state = EMIT_LOCATION_loaded;
    ret->loaded_register = reg;
    ret->size = size;
    
    // :asm_block_use_allocate_specific_register_but_disable_the_register_allocator
    //
    // Do not _map_ this register, as this would cause it to get spilled on the next
    // 'allocate_specific_register'. In an __asm__-block only the registers which were
    // mapped before entering the __asm__-block should get spilled!
    context->register_allocators[kind].emit_location_map[reg] = ret;
    
    return ret;
}

func struct emit_location *emit_location_conditional(struct context *context, enum comp_condition condition){
    struct emit_location *ret = push_struct(&context->scratch, struct emit_location);
    ret->register_kind_when_loaded = REGISTER_KIND_gpr;
    ret->state = EMIT_LOCATION_conditional;
    ret->condition = condition;
    ret->size = 4;
    return ret;
}

func struct emit_location *emit_location_register_relative__internal(struct context *context, enum register_kind kind, struct emit_location *base, struct emit_location *index_register, smm offset, smm size, struct ast *ast){
    struct emit_location *ret = push_struct(&context->scratch, struct emit_location);
    ret->register_kind_when_loaded = kind;
    ret->state = EMIT_LOCATION_register_relative;
    ret->base = base;
    ret->index = index_register;
    ret->log_index_scale = 0; // @note: this is almost never used, thus we set it specifically if we need it...
    ret->offset = offset;
    ret->size = size;
    ret->ast = ast;
    return ret;
}


func struct emit_location *emit_location_register_relative(struct context *context, enum register_kind kind, struct emit_location *base, struct emit_location *index_register, smm offset, smm size){
    return emit_location_register_relative__internal(context, kind, base, index_register, offset, size, null);
}

func struct emit_location *emit_location_rip_relative(struct context *context, struct ast *ast, enum register_kind kind, smm size){
    return emit_location_register_relative__internal(context, kind, context->register_bp, null, 0, size, ast);
}

func struct emit_location *emit_location_stack_relative(struct context *context, enum register_kind kind, smm offset, smm size){
    // @note: note the inversion in the offset @hmm.
    return emit_location_register_relative__internal(context, kind, context->register_bp, null, -offset, size, null);
}

func struct emit_location *emit_location_immediate(struct context *context, u64 value, smm size){
    struct emit_location *ret = push_struct(&context->scratch, struct emit_location);
    ret->state = EMIT_LOCATION_immediate;
    ret->value = value;
    ret->size  = size;
    return ret;
}

//_____________________________________________________________________________________________________________________


func struct emit_location *emit_allocate_temporary_stack_location(struct context *context, enum register_kind kind, smm size, smm alignment){
    
    context->temporary_stack_allocator  = align_up(context->temporary_stack_allocator, alignment);
    context->temporary_stack_allocator += size;
    context->temporary_stack_high_water_mark = max_of(context->temporary_stack_high_water_mark, context->temporary_stack_allocator);
    // :stack_space_needed
    smm stack_location_to_allocate = context->current_function->stack_space_needed + context->temporary_stack_allocator;
    return emit_location_stack_relative(context, kind, stack_location_to_allocate, size);
}

func void spill_register(struct context *context, enum register_kind allocator, enum register_encoding register_to_spill){
    
    // assert(!context->in_inline_asm_function);
    assert(allocator == REGISTER_KIND_gpr || allocator == REGISTER_KIND_xmm);
    
    struct register_allocator *alloc = context->register_allocators + allocator;
    
    assert(alloc->emit_location_map[register_to_spill]);
    assert(alloc->emit_location_map[register_to_spill]->prevent_spilling == 0);
    
    struct emit_location *location_to_spill = alloc->emit_location_map[register_to_spill];
    u32 prevent_freeing = location_to_spill->prevent_freeing;
    
    assert(is_power_of_two(location_to_spill->size));
    
    // @note: for basic types we have alignment == size
    *location_to_spill = *emit_allocate_temporary_stack_location(context, allocator, location_to_spill->size, location_to_spill->size);
    location_to_spill->prevent_freeing = prevent_freeing;
    
    alloc->emit_location_map[register_to_spill] = null;
    
    smm offset = location_to_spill->offset;
    smm size   = location_to_spill->size;
    
    //  Legacy prefix / sse prefix
    struct prefixes prefixes = no_prefix();
    if(allocator == REGISTER_KIND_xmm){
        enum legacy_prefixes sse_prefix = ASM_PREFIX_none;
        if(size == 4)  sse_prefix = ASM_PREFIX_SSE_float;
        if(size == 8)  sse_prefix = ASM_PREFIX_SSE_double;
        if(size == 16) sse_prefix = ASM_PREFIX_SSE_packed_float;
        if(size == 32) sse_prefix = ASM_PREFIX_VEX; // vmovups
        
        prefixes = create_prefixes(sse_prefix);
    }
    
    struct opcode opcode;
    
    if(allocator == REGISTER_KIND_gpr){
        u8 inst = MOVE_REGM_REG;
        if(size == 1) inst = MOVE_REGM8_REG8;
        opcode = one_byte_opcode(inst);
    }else{
        opcode = two_byte_opcode(MOVE_UNALIGNED_REGM_XMM);
    }
    
    emit_prefixes_and_opcode(context, prefixes, register_to_spill, REGISTER_BP, -1, size, opcode);
    
    // modrm
    u8 mod = MODRM_REGM8;
    if(offset > s8_max || offset < s8_min){
        mod = MODRM_REGM32;
    }
    emit(make_modrm(mod, register_to_spill & 0x7, REGISTER_BP));
    
    // immediate
    if(mod == MODRM_REGM32){
        emit_u32(offset);
    }else{
        emit(offset);
    }
}

func enum register_encoding allocate_specific_register(struct context *context, enum register_kind alloc, enum register_encoding reg){
    assert(alloc < REGISTER_KIND_count && reg < 16);
    
    if(context->register_allocators[alloc].emit_location_map[reg]){
        // We should never attempt to spill a register that the user explicitly referenced!
        assert(!context->register_allocators[alloc].emit_location_map[reg]->inline_asm__was_used_by_user);
        
        spill_register(context, alloc, reg);
    }
    
    return reg;
}


static enum register_encoding volatile_xmm_registers[] = {
    REGISTER_XMM0,
    REGISTER_XMM1,
    REGISTER_XMM2,
    REGISTER_XMM3,
    REGISTER_XMM4,
    REGISTER_XMM5,
};

static enum register_encoding volatile_general_purpose_registers[] = {
    REGISTER_A,
    REGISTER_C,
    REGISTER_D,
    REGISTER_R8,
    REGISTER_R9,
    REGISTER_R10,
    REGISTER_R11,
};

// this tries to find a 'register_encoding' such that context->register_to_emit_location_map[register] = null
// otherwise it spills a register that is not locked.
func enum register_encoding allocate_register(struct context *context, enum register_kind allocator){
    
    enum register_encoding *volatile_registers = null;
    smm amount_of_volatile_registers = 0;
    
    if(allocator == REGISTER_KIND_gpr){
        volatile_registers = volatile_general_purpose_registers;
        amount_of_volatile_registers = array_count(volatile_general_purpose_registers);
    }else{
        assert(allocator == REGISTER_KIND_xmm);
        volatile_registers = volatile_xmm_registers;
        amount_of_volatile_registers = array_count(volatile_xmm_registers);
    }
    
    struct register_allocator *alloc = context->register_allocators + allocator;
    u32 at = alloc->rolling_index++;
    if(alloc->rolling_index >= amount_of_volatile_registers){
        alloc->rolling_index= 0;
    }
    
    //
    // Attempt to find a free register
    //
    for(u32 i = at; i < amount_of_volatile_registers; i++){
        enum register_encoding reg = volatile_registers[i];
        if(!alloc->emit_location_map[reg]) return reg;
    }
    
    for(u32 i = 0; i < at; i++){
        enum register_encoding reg = volatile_registers[i];
        if(!alloc->emit_location_map[reg]) return reg;
    }
    
    // if we are here we need to _spill_ a register and give that back.
    // if is_locked is set we currently need that register, so do not spill it
    enum register_encoding first_non_locked_register = INVALID_REGISTER;
    
    for(u32 i = at; i < amount_of_volatile_registers; i++){
        enum register_encoding reg = volatile_registers[i];
        if(alloc->emit_location_map[reg]->prevent_spilling) continue;
        first_non_locked_register = reg;
        goto found_a_non_locked_register;
    }
    
    for(u32 i = 0; i < at; i++){
        enum register_encoding reg = volatile_registers[i];
        if(alloc->emit_location_map[reg]->prevent_spilling) continue;
        first_non_locked_register = reg;
        goto found_a_non_locked_register;
    }
    
    if(context->inline_asm_mode){
        // @hmm: can we detect this in the parser? probably not
        report_error(context, context->inline_asm_mode, "Not enough unallocated registers to emit this expression. You can use 'free <register>' to mark a register for reuse. @cleanup: unimplemented.");
        return REGISTER_A; // who cares at this point
    }else{
        os_debug_break();
        report_error(context, 0, "Internal compiler error: All registers are locked, this should be impossible.");
    }
    
    found_a_non_locked_register:;
    
    spill_register(context, allocator, first_non_locked_register);
    return first_non_locked_register;
}

func void spill_all_allocated_volatile_registers(struct context *context){
    for(u32 i = 0; i < array_count(volatile_general_purpose_registers); i++){
        enum register_encoding reg = volatile_general_purpose_registers[i];
        if(context->gpr_allocator.emit_location_map[reg]){
            spill_register(context, REGISTER_KIND_gpr, reg);
        }
    }
    
    for(u32 i = 0; i < array_count(volatile_xmm_registers); i++){
        enum register_encoding reg = volatile_xmm_registers[i];
        if(context->xmm_allocator.emit_location_map[reg]){
            spill_register(context, REGISTER_KIND_xmm, reg);
        }
    }
}

func void free_emit_location(struct context *context, struct emit_location *loc){
    if(loc == context->register_bp) return;
    assert(loc->state != EMIT_LOCATION_freed); // no double frees
    if(loc->prevent_freeing || loc->prevent_spilling) return;
    
    switch(loc->state){
        case EMIT_LOCATION_loaded:{
            
            if(loc->inline_asm__was_used_by_user){
                assert(context->inline_asm_mode);
                // if we call something like 'emit_binary_op__internal', this implicitly free the source register.
                // but in an asm-block we dont want to free these, if they are marked 'inline__asm_was_used_by_user'.
                return;
            }
            
            enum register_encoding reg = loc->loaded_register;
            struct register_allocator *alloc = &context->register_allocators[loc->register_kind_when_loaded];
            
            assert(reg < array_count(alloc->emit_location_map));
            alloc->emit_location_map[reg] = null;
            
            // @note: we do not reset context->rolling_register_allocator, so we wont allocate registers in a stack like fashion, but instead just roll through them -7.1.2019
            
        }break;
        case EMIT_LOCATION_register_relative:{
            if(!loc->ast){
                free_emit_location(context, loc->base);
                if(loc->index) free_emit_location(context, loc->index);
            }
        }break;
        case EMIT_LOCATION_immediate:
        case EMIT_LOCATION_conditional:{
        }break;
        invalid_default_case();
    }
    
    loc->state = EMIT_LOCATION_freed;
}

func struct emit_location *emit_load(struct context *context, struct emit_location *loc);


//
// @note: freeing is always prevented when spilling is prevented
//
func void emit_location_prevent_freeing(struct context *context, struct emit_location *loc){
    (void)context;
    assert(loc->state == EMIT_LOCATION_loaded || loc->state == EMIT_LOCATION_immediate);
    loc->prevent_freeing += 1;
}


func void emit_location_allow_freeing(struct context *context, struct emit_location *loc){
    (void)context;
    assert(loc->prevent_freeing > 0);
    loc->prevent_freeing -= 1;
}


func void emit_location_prevent_spilling(struct context *context, struct emit_location *loc){
    loc->prevent_spilling += 1;
    if(loc->state == EMIT_LOCATION_register_relative){
        loc->base = emit_load(context, loc->base);
        loc->base->prevent_spilling += 1;
        if(loc->index){
            loc->index = emit_load(context, loc->index);
            loc->index->prevent_spilling += 1;
        }
    }
}

func void emit_location_allow_spilling(struct context *context, struct emit_location *loc){
    (void)context;

    assert(loc->prevent_spilling > 0);
    loc->prevent_spilling -= 1;
    if(loc->state == EMIT_LOCATION_register_relative){
        assert(loc->base->prevent_spilling > 0);
        loc->base->prevent_spilling -= 1;
        if(loc->index){
            assert(loc->index->prevent_spilling > 0);
            loc->index->prevent_spilling -= 1;
        }
    }
}


/////////////////////////////////////////////////

func void emit_register_op__internal(struct context *context, struct prefixes prefixes, struct opcode opcode, enum register_encoding reg, enum register_encoding regm, smm size){
    emit_prefixes_and_opcode(context, prefixes, reg, regm, -1, size, opcode);
    
    emit(make_modrm(MODRM_REG, reg & 7, regm & 7));
}

func void emit_reg_extended_op(struct context *context, struct prefixes prefixes, struct opcode opcode, u8 extension, struct emit_location *loaded){
    assert(loaded->state == EMIT_LOCATION_loaded);
    
    emit_register_op__internal(context, prefixes, opcode, extension, loaded->loaded_register, loaded->size);
}

func void emit_register_register(struct context *context, struct prefixes prefixes, struct opcode opcode, struct emit_location *_reg, struct emit_location *_regm){
    assert(_reg->state == EMIT_LOCATION_loaded);
    assert(_regm->state == EMIT_LOCATION_loaded);
    assert(_reg->register_kind_when_loaded == _regm->register_kind_when_loaded);
    
    // @note: allow mismatches for xmm, as it gets weird in inline_asm blocks.
    // @cleanup: is this still true?
    assert(_reg->register_kind_when_loaded == REGISTER_KIND_xmm || _reg->size == _regm->size);
    
    enum register_encoding reg  = _reg->loaded_register;
    enum register_encoding regm = _regm->loaded_register;
    
    emit_register_op__internal(context, prefixes, opcode, reg, regm, _reg->size);
}

func void emit_register_relative__internal(struct context *context, struct prefixes prefixes, struct opcode opcode, u8 other_reg, struct emit_location *loc, struct emit_location *immediate){
    assert(loc->state == EMIT_LOCATION_register_relative);
    assert(is_power_of_two(loc->size));
    
    //
    // Quick dance to ensure that both 'loc->base' and 'loc->index' are loaded.
    //
    emit_location_prevent_spilling(context, loc);
    emit_location_allow_spilling(context, loc);
    
    assert(loc->base->state == EMIT_LOCATION_loaded);
    assert(!loc->index || loc->index->state == EMIT_LOCATION_loaded);
    
    enum register_encoding base =  loc->base->loaded_register;
    enum register_encoding index = loc->index ? loc->index->loaded_register : INVALID_REGISTER;
    
    emit_prefixes_and_opcode(context, prefixes, other_reg, base, index, loc->size, opcode);
    
    u8 mod = MODRM_REGM;
    if(loc->ast){
        // rip relative
        // mod = MODRM_REGM;
        assert(index == -1 && base == REGISTER_BP);
    }else{
        assert(loc->offset <= s32_max && loc->offset >= s32_min);
        if(loc->offset > s8_max || loc->offset < s8_min){
            mod = MODRM_REGM32;
        }else if(loc->offset != 0){
            mod = MODRM_REGM8;
        }else if(base == REGISTER_BP){
            // We have to handle [rbp] as [rbp + 0] as 'base = RBP' and 'mod = MOD_REGM' means [rip + disp32].
            // @cleanup: is this even supposed to happen?
            mod = MODRM_REGM8;
        }
    }
    
    if(index >= 0){
        u8 log_size = 0;
        
        if(index != REGISTER_SP){
            log_size = (u8)loc->log_index_scale;
        }
        
        emit(make_modrm(mod, other_reg & 0x7, REGISTER_SIB_EXTENSION));
        emit(make_sib(log_size, index & 0x7, base & 0x7));
    }else{
        if(base == REGISTER_SIB_EXTENSION){
            emit(make_modrm(mod, other_reg & 0x7, REGISTER_SIB_EXTENSION));
            emit(make_sib(mod, REGISTER_SP, base & 0x7));
        }else{
            emit(make_modrm(mod, other_reg & 0x7, base & 0x7));
        }
    }
    
    smm byte_offset = -1;
    if(loc->ast){
        byte_offset = emit_u32(0);
    }else{
        if(mod == MODRM_REGM32){
            emit_u32(loc->offset);
        }else if(mod == MODRM_REGM8){
            emit(loc->offset);
        }
    }
    
    if(immediate){
        assert(immediate->state == EMIT_LOCATION_immediate);
        assert(loc->size >= immediate->size);
        smm value = immediate->value;
        switch(immediate->size){
            case 1: emit(value);     break;
            case 2: emit_u16(value); break;
            case 4: emit_u32(value); break;
            case 8: emit_u64(value); break;
            invalid_default_case();
        }
    }
    
    if(loc->ast){
        assert(byte_offset >= 0);
        smm rip_at = get_bytes_emitted(context);
        emit_patch(context, PATCH_rip_relative, loc->ast, loc->offset, &context->current_function->as_decl, byte_offset, rip_at);
    }
    
}

// Emits instructions of the form:
//    1) 'op [base + scale * index + offset], register'
//    2) 'op register, [base + scale * index + offset]'
func void emit_register_relative_register(struct context *context, struct prefixes prefixes, struct opcode opcode, enum register_encoding other_reg, struct emit_location *loc){
    emit_register_relative__internal(context, prefixes, opcode, other_reg, loc, null);
}

// Emits instructions of the form:
//   'op [base + scale * index + offset], immediate'
// caller has to make sure that the 'opcode' matches the 'immediate->size'
func void emit_register_relative_immediate(struct context *context, struct prefixes prefixes, struct opcode opcode, u8 reg_extension, struct emit_location *register_relative, struct emit_location *immediate){
    assert(register_relative->register_kind_when_loaded == REGISTER_KIND_gpr);
    emit_register_relative__internal(context, prefixes, opcode, reg_extension, register_relative, immediate);
}

func void emit_register_relative_extended(struct context *context, struct prefixes prefixes, struct opcode opcode, 
        u8 extension, struct emit_location *register_relative){
    emit_register_relative__internal(context, prefixes, opcode, extension, register_relative, null);
}

/////////////////////////////////////////////////

// @note: this always uses the reg_regm versions, so this can be used to load either reg or regm
func struct opcode get_opcode_for_move_instruction_and_adjust_size(struct emit_location *source, smm dest_size, b32 is_signed){
    struct opcode opcode;
    if(source->size < dest_size){
        if(is_signed){
            if(source->size == 1){
                opcode = two_byte_opcode(MOVE_WITH_SIGN_EXTENSION_REG_REGM8);
            }else if(source->size == 2){
                opcode = two_byte_opcode(MOVE_WITH_SIGN_EXTENSION_REG_REGM16);
            }else{
                // "MOVXD without REXW is discouraged"
                opcode = one_byte_opcode(MOVE_WITH_SIGN_EXTENSION_REG_REGM);
                assert(source->size == 4);
            }
            source->size = dest_size;
        }else{
            if(source->size == 1){
                opcode = two_byte_opcode(MOVE_WITH_ZERO_EXTENSION_REG_REGM8);
                source->size = dest_size;
            }else if(source->size == 2){
                opcode = two_byte_opcode(MOVE_WITH_ZERO_EXTENSION_REG_REGM16);
                source->size = dest_size;
            }else{
                assert(source->size == 4);
                // only move, it will zero extend
                opcode = one_byte_opcode(MOVE_REG_REGM);
            }
        }
    }else{
        if(dest_size == 1){
            opcode = one_byte_opcode(MOVE_REG8_REGM8);
        }else{
            opcode = one_byte_opcode(MOVE_REG_REGM);
        }
    }
    return opcode;
}

func struct emit_location *emit_load_into_specific_gpr(struct context *context, struct emit_location *source, enum register_encoding register_to_load_into){
    if(source->size == 0){
        // This can happen for zero sized structs, for example 'function(zero_sized_struct)'.
        // Just load the register, but leave its contents uninitialized.
        free_emit_location(context, source);
        return emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
    }
    
    assert(source->size == 1 || source->size == 2 || source->size == 4 || source->size == 8);
    
    assert(source->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    switch(source->state){
        case EMIT_LOCATION_loaded:{
            // If we have the desired register there is nothing to do.
            if(source->loaded_register == register_to_load_into) return source;
            
            struct opcode opcode;
            if(source->size == 1){
                opcode = one_byte_opcode(MOVE_REG8_REGM8); // @cleanup: movzx?
            }else{
                opcode = one_byte_opcode(MOVE_REG_REGM);
            }
            
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            
            struct emit_location *load_into =  emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
            
            emit_register_register(context, create_prefixes(0), opcode, load_into, source);
            free_emit_location(context, source);
            return load_into;
        }break;
        
        // I think this pretty much only has to be called for function calls.
        case EMIT_LOCATION_immediate:{
            
            if(source->value == 0){
                allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
                struct emit_location *ret = emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
                emit_register_register(context, create_prefixes(0), one_byte_opcode(XOR_REG_REGM), ret, ret);
                return ret;
            }
            
            // @cleanup: Can we load smaller here if 'source->size == 8' but |source->value| small?
            
            // @note: This does not have a u8 or u16 version, as the registers wont get cleared for those.
            u8 rex = 0;
            if(source->size == 8) rex |= REXW;
            if(register_is_extended(register_to_load_into)) rex |= REXB;
            if(rex) emit(rex); // this decides wheter 32 or 64 bits in this case
            
            emit(MOVE_REG_IMMEDIATE + (register_to_load_into & 7));
            if(source->size == 8){
                emit_u64(source->value);
            }else{
                emit_u32(source->value);
            }
            
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            return emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
        }break;
        case EMIT_LOCATION_register_relative:{
            struct opcode opcode;
            if(source->size == 1){
                opcode = one_byte_opcode(MOVE_REG8_REGM8);
            }else{
                opcode = one_byte_opcode(MOVE_REG_REGM);
            }
            
            emit_register_relative_register(context, create_prefixes(0), opcode, register_to_load_into, source);
            free_emit_location(context, source);
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            return emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
        }break;
        case EMIT_LOCATION_conditional:{
            // Usually conditionals are of size 4 (int) by cspec, but for cast to _Bool we load them as size 1.
            assert(source->size == 4 || source->size == 1);
            
            u8 inst;
            switch(source->condition){
                case COMP_equals:                inst = SET_REGM8_IF_EQUALS;                break;
                case COMP_unequals:              inst = SET_REGM8_IF_UNEQUALS;              break;
                case COMP_smaller_equals:        inst = SET_REGM8_IF_SMALLER_EQUALS;        break;
                case COMP_smaller:               inst = SET_REGM8_IF_SMALLER;               break;
                case COMP_bigger_equals:         inst = SET_REGM8_IF_BIGGER_EQUALS;         break;
                case COMP_bigger:                inst = SET_REGM8_IF_BIGGER;                break;
                case COMP_smaller_signed:        inst = SET_REGM8_IF_SMALLER_SIGNED;        break;
                case COMP_smaller_equals_signed: inst = SET_REGM8_IF_SMALLER_EQUALS_SIGNED; break;
                case COMP_bigger_signed:         inst = SET_REGM8_IF_BIGGER_SIGNED;         break;
                case COMP_bigger_equals_signed:  inst = SET_REGM8_IF_BIGGER_EQUALS_SIGNED;  break;
                case COMP_positive:              inst = SET_REGM8_IF_POSITIVE;              break;
                case COMP_negative:              inst = SET_REGM8_IF_NEGATIVE;              break;
                invalid_default_case(inst = SET_REGM8_IF_EQUALS);
            }
            
            // "The reg field in the ModR/M byte is unused"
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            
            struct emit_location *load_into = emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
            emit_reg_extended_op(context, create_prefixes(0), two_byte_opcode(inst), 0, load_into);
            if(source->size != 1){
                emit_register_register(context, create_prefixes(0), two_byte_opcode(MOVE_WITH_ZERO_EXTENSION_REG_REGM8), load_into, load_into);
            }
            
            return load_into;
        }break;
        invalid_default_case(return null);
    }
}

func struct emit_location *emit_load_float_into_specific_register(struct context *context, struct emit_location *loc, enum register_encoding reg){
    
    assert(loc->register_kind_when_loaded == REGISTER_KIND_xmm);
    if(loc->state == EMIT_LOCATION_loaded && loc->loaded_register == reg) return loc;
    
    struct prefixes sse_prefix;
    if(loc->size == 32){
        sse_prefix = create_prefixes(ASM_PREFIX_SSE_packed_float | ASM_PREFIX_VEX);
    }else{
        sse_prefix = get_sse_prefix_for_scalar(loc->size);
    }
    
    allocate_specific_register(context, REGISTER_KIND_xmm, reg);
    struct emit_location *ret = emit_location_loaded(context, REGISTER_KIND_xmm, reg, loc->size);
    
    
    
    switch(loc->state){
        case EMIT_LOCATION_loaded:{
            emit_register_register(context, sse_prefix, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), ret, loc);
        }break;
        case EMIT_LOCATION_register_relative:{
            emit_register_relative_register(context, sse_prefix, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), reg, loc);
        }break;
        invalid_default_case();
    }
    free_emit_location(context, loc);
    return ret;
}

func struct emit_location *emit_load_float(struct context *context, struct emit_location *loc){
    if(loc->state == EMIT_LOCATION_loaded) return loc;
    return emit_load_float_into_specific_register(context, loc, allocate_register(context, REGISTER_KIND_xmm));
}

func struct emit_location *emit_load(struct context *context, struct emit_location *loc){
    // @cleanup: this really should set the register_to_load_into to loc->loaded_register
    if(loc->state == EMIT_LOCATION_loaded) return loc;
    
    if(loc->register_kind_when_loaded == REGISTER_KIND_gpr){
        enum register_encoding register_to_load_into = allocate_register(context, REGISTER_KIND_gpr);
        struct emit_location *ret = emit_load_into_specific_gpr(context, loc, register_to_load_into);
        return ret;
    }else{
        return emit_load_float(context, loc);
    }
}

func struct emit_location *emit_load_without_freeing(struct context *context, struct emit_location *loc){
    loc->prevent_freeing += 1;
    struct emit_location *ret = emit_load(context, loc);
    loc->prevent_freeing -= 1;
    return ret;
}

func struct emit_location *emit_load_address(struct context *context, struct emit_location *loc, enum register_encoding load_into){
    assert(loc->state == EMIT_LOCATION_register_relative);
    
    // @hack: we expect to only get register sized things in emit_register_relative, but this is fine
    smm saved_size = loc->size;
    loc->size = 8;
    struct emit_location *loaded = emit_location_loaded(context, REGISTER_KIND_gpr, load_into, 8);
    emit_location_prevent_spilling(context, loaded);
    emit_register_relative_register(context, create_prefixes(0), one_byte_opcode(LOAD_ADDRESS_REG_MEMORY_LOCATION), load_into, loc);
    emit_location_allow_spilling(context, loaded);
    
    loc->size = saved_size;
    
    return loaded;
}

func void emit_memcpy(struct context *context, struct emit_location *dest, struct emit_location *source){
    assert(dest->state == EMIT_LOCATION_register_relative);
    assert(source->state == EMIT_LOCATION_register_relative);
    assert(dest->size >= source->size); // bigger or equal for 'char asd[5] = "asd";'
    
    // on a zero sized memcpy we dont have to do anything!
    if(dest->size == 0) return;
    
    // @cleanup: we may only have to load 4 bytes or so...
    struct emit_location *count = emit_location_immediate(context, source->size, 8);
    
    enum register_encoding rdi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_DI);
    struct emit_location *dest_address   = emit_load_address(context, dest, rdi);
    emit_location_prevent_spilling(context, dest_address);
    
    enum register_encoding rsi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_SI);
    struct emit_location *source_address = emit_load_address(context, source, rsi);
    emit_location_prevent_spilling(context, source_address);
    
    enum register_encoding rcx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_C);
    struct emit_location *count_register = emit_load_into_specific_gpr(context, count, rcx);
    
    emit(LEGACY_PREFIX_REPEAT);
    emit(MOVE_BYTE);
    
    emit_location_allow_spilling(context, dest_address);
    emit_location_allow_spilling(context, source_address);
    
    free_emit_location(context, dest_address);
    free_emit_location(context, source_address);
    free_emit_location(context, count_register);
}

func void emit_memset(struct context *context, struct emit_location *dest, u8 _value){
    assert(dest->state == EMIT_LOCATION_register_relative);
    
    // @cleanup: we may only have to load 4 bytes or so...
    struct emit_location *count = emit_location_immediate(context, dest->size, 8);
    struct emit_location *value = emit_location_immediate(context, _value, 4);
    
    enum register_encoding rdi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_DI);
    enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
    enum register_encoding rcx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_C);
    
    struct emit_location *dest_address    = emit_load_address(context, dest, rdi);
    struct emit_location *source_register = emit_load_into_specific_gpr(context, value, rax);
    struct emit_location *count_register  = emit_load_into_specific_gpr(context, count, rcx);
    
    emit(LEGACY_PREFIX_REPEAT);
    emit(STORE_BYTE);
    
    free_emit_location(context, dest_address);
    free_emit_location(context, source_register);
    free_emit_location(context, count_register);
}

func u64 integer_literal_to_bytes(struct ast *ast){
    assert(ast->kind == AST_integer_literal);
    return integer_literal_as_u64(ast);
}

func void emit_store(struct context *context, struct emit_location *dest, struct emit_location *source){
    assert(dest->state == EMIT_LOCATION_register_relative);
    assert(dest->size == source->size);
    
    if(source->size == 0){
        goto end; // nothing to do here!
    }
    
    // @hack: but it's fine
    enum register_encoding source_reg = REGISTER_A;
    
    switch(source->state){
        case EMIT_LOCATION_register_relative:{
            
            if(size_is_big_or_oddly_sized(source->size)){
                // @cleanup: maybe handle small copies manually
                emit_memcpy(context, dest, source);
                goto end;
            }
            
            // load 'source' for it's natural size, the store will then happen at 'dest->size'
            source = emit_load(context, source);
            goto loaded;
        }break;
        case EMIT_LOCATION_conditional:{
            // @cleanup: we could emit SET_REGM8 here
            source = emit_load(context, source);
            goto loaded;
        }break;
        case EMIT_LOCATION_loaded:{
            loaded:;
            if(source->register_kind_when_loaded == REGISTER_KIND_xmm){
                assert(source->size == dest->size);
                
                struct prefixes sse_prefix;
                if(source->size == 32){
                    sse_prefix = create_prefixes(ASM_PREFIX_SSE_packed_float | ASM_PREFIX_VEX);
                }else{
                    sse_prefix = get_sse_prefix_for_scalar(source->size);
                }
                
                emit_register_relative_register(context, sse_prefix, two_byte_opcode(MOVE_UNALIGNED_REGM_XMM), source->loaded_register, dest);
                break;
            }
            
            u8 inst = (dest->size == 1) ? MOVE_REGM8_REG8 : MOVE_REGM_REG;
            source_reg = source->loaded_register;
            emit_register_relative_register(context, no_prefix(), one_byte_opcode(inst), source_reg, dest);
        }break;
        case EMIT_LOCATION_immediate:{
            if(source->size == 8) {
                source = emit_load(context, source);
                goto loaded;
            }
            u8 inst = (dest->size == 1) ? MOVE_REGM8_IMMEDIATE8 : MOVE_REGM_IMMEDIATE;
            
            source->size = dest->size;
            // @cleanup: the 'reg' field get ignored I guess?
            emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(inst), 0, dest, source);
        }break;
        invalid_default_case();
    }
    
    end:;
    free_emit_location(context, source);
}

///////////////////////////////////////////////////////////////////////////////////////////////


func struct emit_location *emit_code_for_ast(struct context *context, struct ast *ast);


func struct emit_location *emit_binary_op__internal(struct context *context, struct prefixes prefixes, struct emit_location *lhs,
        struct emit_location *rhs, smm size, b32 is_signed, u8 reg_extended, u8 u8_code, u8 opcode){
    
    struct emit_location *dest   = emit_load(context, lhs);
    struct emit_location *source = rhs;
    
    emit_location_prevent_spilling(context, dest);
    
    switch(source->state){
        case EMIT_LOCATION_immediate:{
            // if REXW is present, the immediate is of size 4 and gets sign-extended.
            // so we have to make sure, that if we are not signed we sign_extend
            if(source->size == 8 || ((source->size == 4) && !is_signed && source->value > s32_max)){
                source = emit_load(context, source);
                source->size = dest->size;
                goto loaded;
            }
            
            u8 inst = REG_EXTENDED_OPCODE_REGM_IMMIDIATE;
            if(source->size == 1){
                if(is_signed || source->value < s8_max){
                    inst = REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                }else{
                    source->size = dest->size;
                }
            }
            
            if(size == 1) inst = REG_EXTENDED_OPCODE_REGM8_IMMIDIATE8;
            emit_reg_extended_op(context, prefixes, one_byte_opcode(inst), reg_extended, dest);
            if(source->size == 1){
                emit(source->value);
            }else{
                switch(size){
                    case 1: emit(source->value);     break;
                    case 2: emit_u16(source->value); break;
                    case 4: emit_u32(source->value); break;
                    case 8: emit_u32(source->value); break;
                    invalid_default_case();
                }
            }
        }break;
        case EMIT_LOCATION_loaded:{
            loaded:;
            assert(dest->size == source->size);
            u8 inst = opcode;
            if(source->size == 1) inst = u8_code;
            emit_register_register(context, prefixes, one_byte_opcode(inst), dest, source);
        }break;
        case EMIT_LOCATION_register_relative:{
            assert(dest->size == source->size);
            u8 inst = opcode;
            if(source->size == 1) inst = u8_code;
            emit_register_relative_register(context, prefixes, one_byte_opcode(inst), dest->loaded_register, source);
        }break;
        invalid_default_case();
    }
    
    emit_location_allow_spilling(context, dest);
    free_emit_location(context, source);
    return dest;
}


func struct emit_location *emit_binary_op(struct context *context, struct ast *ast, u8 reg_extended, u8 u8_code, u8 opcode){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    // @note: @quality: for commutative operations, we could switch dest and source, if rhs is loaded
    // and lhs is not.
    
    // @note: these are not the same, because of pointers
    assert(op->lhs->resolved_type->size == op->rhs->resolved_type->size);
    smm size = op->lhs->resolved_type->size;
    
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    // @note: we have to load here as we want to evaluate things left to right. E.g.
    //            (var = 12) + (var = 13) 
    //        should evaluate to 25, but if we did not load here 'lhs' would be register relative to var.
    //        Now 'var' gets overwritten with 13 and the expression evaluates to 26, and not the intended 25.
    //        C does not care about this, but I think I want to be consistent!
    //                                                                                           -19.09.2021
    lhs = emit_load(context, lhs);
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    if(rhs->state == EMIT_LOCATION_conditional) rhs = emit_load(context, rhs);
    
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_gpr);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    assert(lhs->size == rhs->size && rhs->size == size);
    b32 is_signed = type_is_signed(op->rhs->resolved_type);
    return emit_binary_op__internal(context, no_prefix(), lhs, rhs, size, is_signed, reg_extended, u8_code, opcode);
}

func struct emit_location *emit_divide_or_mod_or_multiply(struct context *context, struct ast *ast, u8 REG_OPCODE_signed, u8 REG_OPCODE_unsigned, b32 is_assignment){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    
    smm size = op->lhs->resolved_type->size;
    b32 is_signed = type_is_signed(ast->resolved_type);
    
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    rhs = emit_load(context, rhs);
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_gpr);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    assert(op->lhs->resolved_type->size == op->rhs->resolved_type->size);
    //
    // thses instructions perform an operation like MUL rdx:rax (rax * REGM)
    // i.e multiply rax with REGM then store the 'upper part' in rdx and the 'lower part' in rax.
    
    // @cleanup: immediate versions
    
    // right now the only thing we do is if(size == 8) emit(REXW);
    // correct lhs are: size lhs
    //                   1   AX
    //                   2   DX:AX
    //                   4   EDX:EAX
    //                   8   RDX:RAX
    
    enum register_encoding rdx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_D);
    struct emit_location *upper_part = emit_location_loaded(context, REGISTER_KIND_gpr, rdx, size);
    emit_location_prevent_spilling(context, upper_part);
    
    // @quality: we don't have to spill rax just to load it right after...
    enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
    struct emit_location *lower_part = emit_location_loaded(context, REGISTER_KIND_gpr, rax, size);
    emit_location_prevent_spilling(context, lower_part);
    
    // lock lhs *after* we allocated rax and rdx (so we don't allocate them)
    if(is_assignment) emit_location_prevent_spilling(context, lhs);
    emit_location_allow_spilling(context, lower_part);
    
    lower_part = emit_load_into_specific_gpr(context, lhs, rax);
    emit_location_prevent_spilling(context, lower_part);
    
    
    if(is_signed){
        if(size == 1){
            emit_register_register(context, no_prefix(), two_byte_opcode(MOVE_WITH_SIGN_EXTENSION_REG_REGM8), lower_part, lower_part);
        }else{
            if(size == 2) emit(LEGACY_OPERAND_SIZE_OVERRIDE_PREFIX);
            if(size == 8) emit(REXW);
            emit(SIGN_EXTEND_A_INTO_D);
        }
    }else{
        if(size == 1){
            // @note: we need to zero into AH to not get an exeption
            emit_register_register(context, no_prefix(), two_byte_opcode(MOVE_WITH_ZERO_EXTENSION_REG_REGM8), lower_part, lower_part);
        }else{
            emit_register_register(context, no_prefix(), one_byte_opcode(XOR_REG_REGM), upper_part, upper_part);
        }
    }
    
    u8 inst = REG_EXTENDED_UNARY_REGM;
    if(size == 1) inst = REG_EXTENDED_UNARY_REGM8;
    u8 extension = REG_OPCODE_unsigned;
    if(is_signed) extension = REG_OPCODE_signed;
    
    // @cleanup: do we commit to this? this is here because if rhs is of size < 4 then the operations are weird
    if((rhs->size >= 4) && (rhs->state == EMIT_LOCATION_register_relative)){
        emit_register_relative_extended(context, no_prefix(), one_byte_opcode(inst), extension, rhs);
        free_emit_location(context, rhs);
    }else{
        struct emit_location *loaded_rhs = emit_load(context, rhs);
        emit_reg_extended_op(context, no_prefix(), one_byte_opcode(inst), extension, loaded_rhs);
        free_emit_location(context, loaded_rhs);
    }
    
    emit_location_allow_spilling(context, lower_part);
    emit_location_allow_spilling(context, upper_part);
    
    // @note: truncate the size again (we did the operantion in 4 byte for speed)
    upper_part->size = size;
    lower_part->size = size;
    
    if(is_assignment){
        if(ast->kind  == AST_modulo_assignment){
            if(size == 1){
                free_emit_location(context, upper_part);
                // the result is in AX
                lower_part->size = 2;
                emit_reg_extended_op(context, no_prefix(), one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_RIGHT, lower_part);
                emit(8);
                lower_part->size = 1;
                emit_store(context, lhs, lower_part);
            }else{
                free_emit_location(context, lower_part);
                emit_store(context, lhs, upper_part);
            }
        }else{
            free_emit_location(context, upper_part);
            emit_store(context, lhs, lower_part);
        }
        emit_location_allow_spilling(context, lhs);
        return lhs;
    }
    
    if(ast->kind == AST_binary_mod){
        // if size == 1 then the upper_part is 'AH' and not 'DL'...
        if(size == 1){
            free_emit_location(context, upper_part);
            // the result is in AX
            lower_part->size = 2;
            emit_reg_extended_op(context, no_prefix(), one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_RIGHT, lower_part);
            emit(8);
            return lower_part;
        }
        
        free_emit_location(context, lower_part);
        // the modulus is saved in the upper part if we perform an idiv or div instruction
        
        return upper_part;
    }else{
        // for mul, imul, div and idiv we only care about the 'lower_part'
        free_emit_location(context, upper_part);
        return lower_part;
    }
}

func struct emit_location *emit_compound_assignment__internal(struct context *context, struct prefixes prefixes, struct emit_location *lhs,
        struct emit_location *rhs, b32 is_signed, u8 reg_extension, u8 u8_code, u8 opcode){
    
    assert(lhs->state == EMIT_LOCATION_register_relative);
    
    switch(rhs->state){
        case EMIT_LOCATION_loaded:{
            loaded:;
            u8 inst = opcode;
            if(lhs->size == 1) inst = u8_code;
            emit_location_prevent_spilling(context, rhs);
            emit_register_relative_register(context, prefixes, one_byte_opcode(inst), rhs->loaded_register, lhs);
            emit_location_allow_spilling(context, rhs);
        }break;
        case EMIT_LOCATION_conditional:
        case EMIT_LOCATION_register_relative:{
            // @note: if lhs is bigger then upconvert, otherwise load and truncate afterwards
            rhs = emit_load(context, rhs);
            goto loaded;
        }break;
        case EMIT_LOCATION_immediate:{
            u8 inst = REG_EXTENDED_OPCODE_REGM_IMMIDIATE;
            if(lhs->size == 1){
                inst = REG_EXTENDED_OPCODE_REGM8_IMMIDIATE8;
            }else if(rhs->size == 1){
                if(is_signed || rhs->value <= s8_max){
                    inst = REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                }else{
                    rhs->size = lhs->size;
                }
            }else if(rhs->size == 8){
                rhs = emit_load(context, rhs);
                goto loaded;
            }else if(rhs->size == 4 && is_signed && rhs->value > s32_max){
                rhs = emit_load(context, rhs);
                goto loaded;
            }else{
                rhs->size = lhs->size;
            }
            
            emit_register_relative_immediate(context, prefixes, one_byte_opcode(inst), reg_extension, lhs, rhs);
        }break;
        invalid_default_case();
    }
    free_emit_location(context, rhs);
    return lhs;
}

// '+=', '-=', ...
func struct emit_location *emit_compound_assignment(struct context *context, struct ast *ast, u8 reg_extension, u8 u8_code, u8 opcode){
    struct ast_binary_op *assign = cast(struct ast_binary_op *)ast;
    
    assert(assign->lhs->resolved_type->kind == AST_integer_type || assign->lhs->resolved_type->kind == AST_pointer_type || assign->lhs->resolved_type->kind == AST_atomic_integer_type);
    assert(assign->rhs->resolved_type->kind == AST_integer_type);
    assert(assign->lhs->resolved_type->size == assign->rhs->resolved_type->size);
    assert(assign->base.resolved_type == assign->lhs->resolved_type);
    
    struct emit_location *rhs = emit_code_for_ast(context, assign->rhs);
    if(rhs->state == EMIT_LOCATION_conditional) rhs = emit_load(context, rhs);
    struct emit_location *lhs = emit_code_for_ast(context, assign->lhs);
    
    b32 is_signed = type_is_signed(assign->base.resolved_type);
    
    // @incomplete: This only works for some of the instructions and is technically incorrect, if the the resulting values is read.
    //              Because I think it will re-fetch the value of the atomic.
    struct prefixes prefix = no_prefix();
    if(assign->lhs->resolved_type->kind == AST_atomic_integer_type) prefix.legacy_prefixes |= ASM_PREFIX_lock;
    
    return emit_compound_assignment__internal(context, prefix, lhs, rhs, is_signed, reg_extension, u8_code, opcode);
}

////////////////////////////////////////////////////////////////////////////////////////

func struct emit_location *emit_binary_op_xmm(struct context *context, struct ast *ast, u8 inst){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    
    assert(lhs->size == rhs->size);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    lhs = emit_load(context, lhs);
    
    struct prefixes sse_prefix = get_sse_prefix_for_scalar(lhs->size);
    
    if(rhs->state == EMIT_LOCATION_loaded){
        emit_register_register(context, sse_prefix, two_byte_opcode(inst), lhs, rhs);
    }else{
        assert(rhs->state == EMIT_LOCATION_register_relative);
        emit_register_relative_register(context, sse_prefix, two_byte_opcode(inst), lhs->loaded_register, rhs);
    }
    
    free_emit_location(context, rhs);
    return lhs;
}

func struct emit_location *emit_compound_assignment_xmm(struct context *context, struct ast *ast, u8 inst){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    assert(lhs->state == EMIT_LOCATION_register_relative);
    emit_location_prevent_spilling(context, lhs);
    
    rhs = emit_load(context, rhs);
    emit_location_prevent_spilling(context, rhs);
    struct emit_location *lhs_loaded = emit_load(context, lhs);
    
    assert(lhs->size == rhs->size);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    
    // emit 'op lhs, rhs'
    emit_register_register(context, get_sse_prefix_for_scalar(lhs->size), two_byte_opcode(inst), lhs_loaded, rhs);
    
    emit_location_allow_spilling(context, rhs);
    free_emit_location(context, rhs);
    
    emit_location_prevent_spilling(context, lhs_loaded);
    emit_store(context, lhs, lhs_loaded);
    
    emit_location_allow_spilling(context, lhs);
    free_emit_location(context, lhs);
    
    emit_location_allow_spilling(context, lhs_loaded);
    return lhs_loaded;
}

////////////////////////////////////////////////////////////////////////////////////////

struct jump_node{
    struct jump_node *next;
    u8 *patch_location;
    smm jump_from;
};

enum jump_context_condition{
    JUMP_CONTEXT_jump_on_true,
    JUMP_CONTEXT_jump_on_false,
};

struct jump_context{
    struct{
        struct jump_node *first;
        struct jump_node *last;
    } jump_list;
    
    struct context *context;
    enum jump_context_condition condition;
};

func struct jump_context emit_begin_jumps(struct context *context, enum jump_context_condition condition){
    struct jump_context jump_context = zero_struct;
    jump_context.context = context;
    jump_context.condition = condition;
    return jump_context;
}

func u8 instruction_from_comp_condition(enum comp_condition cond, enum jump_context_condition jump_if){
    
    u8 inst;
    if(cond == COMP_none){
        inst = JUMP_REL32;
    }else if(jump_if == JUMP_CONTEXT_jump_on_false){
        switch(cond){
            case COMP_unequals:              inst = JUMP_REL32_IF_EQUALS;                break;
            case COMP_equals:                inst = JUMP_REL32_IF_UNEQUALS;              break;
            case COMP_smaller:               inst = JUMP_REL32_IF_BIGGER_EQUALS;         break;
            case COMP_smaller_equals:        inst = JUMP_REL32_IF_BIGGER;                break;
            case COMP_bigger:                inst = JUMP_REL32_IF_SMALLER_EQUALS;        break;
            case COMP_bigger_equals:         inst = JUMP_REL32_IF_SMALLER;               break;
            case COMP_smaller_signed:        inst = JUMP_REL32_IF_BIGGER_EQUALS_SIGNED;  break;
            case COMP_smaller_equals_signed: inst = JUMP_REL32_IF_BIGGER_SIGNED;         break;
            case COMP_bigger_signed:         inst = JUMP_REL32_IF_SMALLER_EQUALS_SIGNED; break;
            case COMP_bigger_equals_signed:  inst = JUMP_REL32_IF_SMALLER_SIGNED;        break;
            case COMP_positive:              inst = JUMP_REL32_IF_NEGATIVE;              break;
            case COMP_negative:              inst = JUMP_REL32_IF_POSITIVE;              break;
            
            invalid_default_case(inst = JUMP_REL32_IF_UNEQUALS);
        };
    }else if(jump_if == JUMP_CONTEXT_jump_on_true){
        switch(cond){
            case COMP_unequals:              inst = JUMP_REL32_IF_UNEQUALS;              break;
            case COMP_equals:                inst = JUMP_REL32_IF_EQUALS;                break;
            case COMP_smaller:               inst = JUMP_REL32_IF_SMALLER;               break;
            case COMP_smaller_equals:        inst = JUMP_REL32_IF_SMALLER_EQUALS;        break;
            case COMP_bigger:                inst = JUMP_REL32_IF_BIGGER;                break;
            case COMP_bigger_equals:         inst = JUMP_REL32_IF_BIGGER_EQUALS;         break;
            case COMP_smaller_signed:        inst = JUMP_REL32_IF_SMALLER_SIGNED;        break;
            case COMP_smaller_equals_signed: inst = JUMP_REL32_IF_SMALLER_EQUALS_SIGNED; break;
            case COMP_bigger_signed:         inst = JUMP_REL32_IF_BIGGER_SIGNED;         break;
            case COMP_bigger_equals_signed:  inst = JUMP_REL32_IF_BIGGER_EQUALS_SIGNED;  break;
            case COMP_positive:              inst = JUMP_REL32_IF_POSITIVE;              break;
            case COMP_negative:              inst = JUMP_REL32_IF_NEGATIVE;              break;
            invalid_default_case(inst = JUMP_REL32_IF_EQUALS);
        };
    }else{
        inst = JUMP_REL32;
        assert(false);
    }
    
    return inst;
}

func void jump_context_emit(struct jump_context *jump_context, enum comp_condition cond){
    struct context *context = jump_context->context;
    struct jump_node *node = push_struct(context->arena, struct jump_node);
    
    u8 inst = instruction_from_comp_condition(cond, jump_context->condition);
    
    if(cond != COMP_none)emit(TWO_BYTE_INSTRUCTION_PREFIX);
    emit(inst);
    node->patch_location = context->emit_pool.current;
    node->jump_from = emit_bytes(context, 4, 0) + 4;
    
    sll_push_back(jump_context->jump_list, node);
}

func void jump_node_end_jump(struct jump_node *node, smm jump_location){
    s32 patch = to_s32(jump_location - node->jump_from);
    memcpy(node->patch_location, &patch, sizeof(s32));
}

func void emit_end_jumps_location(struct jump_context context, smm jump_location){
    for(struct jump_node *node = context.jump_list.first; node; node = node->next){
        jump_node_end_jump(node, jump_location);
    }
}

func void emit_end_jumps(struct jump_context jump_context){
    emit_end_jumps_location(jump_context, get_bytes_emitted(jump_context.context));
}

func void emit_jump(struct context *context, smm location, enum comp_condition cond, enum jump_context_condition jump_on){
    u8 inst = instruction_from_comp_condition(cond, jump_on);
    
    if(cond != COMP_none) emit(TWO_BYTE_INSTRUCTION_PREFIX);
    emit(inst);
    // @note: plus 4 because we have to factor in the 'emit_u32' afterwards
    u32 rel_location = cast(u32)(location - (get_bytes_emitted(context) + 4));
    emit_u32(rel_location);
}

//_____________________________________________________________________________________________________________________
// Bitfields

func struct emit_location *emit_load_bitfield(struct context *context, struct emit_location *loc, struct ast_bitfield_type *bitfield){
    
    loc = emit_load(context, loc);
    emit_location_prevent_spilling(context, loc);
    
    // Adjust the size of 'loc' to be at least 4.
    // We do this because the resulting type is 'int'.
    loc->size = loc->size <= 4 ? 4 : 8;
    
    u32 bit_size = loc->size <= 4 ? 32 : 64;
    
    if(bitfield->bit_index + bitfield->width < bit_size){
        // 
        // Shift up by the so that the value is at the top of the register,
        // then shift down (either arithmetically or logically based on whether 
        // or not the base type is signed) such that the value is in its place.
        // 
        emit_reg_extended_op(context, no_prefix(), one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_LEFT, loc);
        emit(bit_size - (bitfield->bit_index + bitfield->width));
    }
        
    if(bitfield->width != bit_size){
        u8 shift_type = type_is_signed(bitfield->base_type) ? REG_OPCODE_SHIFT_ARITHMETIC_RIGHT : REG_OPCODE_SHIFT_RIGHT;
        emit_reg_extended_op(context, no_prefix(), one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), shift_type, loc);
        emit(bit_size - bitfield->width);
    }
    
    emit_location_allow_spilling(context, loc);
    
    // Manually adjust the size to the size we expect.
    loc->size = (bitfield->width <= 32) ? 4 : 8;
    
    return loc;
}

struct emit_location *emit_store_bitfield(struct context *context, struct ast_bitfield_type *bitfield, struct emit_location *lhs, struct emit_location *rhs){
    
    emit_location_prevent_spilling(context, lhs);
    
    struct emit_location *rhs_loaded = emit_load(context, rhs);
    emit_location_prevent_spilling(context, rhs_loaded);
    
    // Truncate the right hand side.
    struct emit_location *mask = emit_location_immediate(context, (1ull << bitfield->width) - 1, lhs->size);
    struct emit_location *mask_loaded = emit_load(context, mask);
    emit_register_register(context, no_prefix(), one_byte_opcode(AND_REG_REGM), rhs_loaded, mask_loaded);
    free_emit_location(context, mask_loaded);
    
    if(bitfield->bit_index){
        // Shift the right hand side into the right spot.
        emit_reg_extended_op(context, no_prefix(), one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_LEFT, rhs_loaded);
        emit(bitfield->bit_index);
    }
    
    // Put the rest of the result in its place.
    struct emit_location *lhs_loaded = emit_load(context, lhs);
    emit_location_prevent_spilling(context, lhs_loaded);
    
    mask = emit_location_immediate(context, ~(((1ull << bitfield->width) - 1) << bitfield->bit_index), lhs->size);
    mask_loaded = emit_load(context, mask);
    emit_register_register(context, no_prefix(), one_byte_opcode(AND_REG_REGM), lhs_loaded, mask_loaded);
    free_emit_location(context, mask_loaded);
    
    // Or the two results together.
    emit_register_register(context, no_prefix(), one_byte_opcode(OR_REG_REGM), lhs_loaded, rhs_loaded);
    
    emit_location_allow_spilling(context, rhs_loaded);
    emit_location_allow_spilling(context, lhs_loaded);
    free_emit_location(context, rhs_loaded);
    
    // Store the result.
    emit_store(context, lhs, lhs_loaded);
    emit_location_allow_spilling(context, lhs);
    
    return lhs; // @cleanup: should this not return something else? lhs should be register-relative.
}

//_____________________________________________________________________________________________________________________
// Atomic integers

struct emit_location *emit_store_atomic_integer(struct context *context, struct emit_location *lhs, struct emit_location *rhs){
    
    // A plain move is not enough to store with memory_order_seq_cst, we have to use an xchg instruction instead.
    
    emit_location_prevent_spilling(context, lhs);
    struct emit_location *rhs_loaded = emit_load(context, rhs);
    
    // xchg lhs, rhs_loaded
    u8 inst = (lhs->size == 1) ? EXCHANGE_REG8_REGM8 : EXCHANGE_REG_REGM;
    emit_register_relative_register(context, no_prefix(), one_byte_opcode(inst), rhs_loaded->loaded_register, lhs);
    
    free_emit_location(context, rhs_loaded);
    emit_location_allow_spilling(context, lhs);
    return lhs;
}

//_____________________________________________________________________________________________________________________
// Conditions

func struct emit_location *emit_compare_to_zero(struct context *context, struct emit_location *loaded){
    assert(loaded->state == EMIT_LOCATION_loaded);
    
    if(loaded->register_kind_when_loaded == REGISTER_KIND_gpr){
        u8 inst = TEST_REGM_REG;
        if(loaded->size == 1) inst = TEST_REGM8_REG8;
        
        emit_register_register(context, no_prefix(), one_byte_opcode(inst), loaded, loaded);
        free_emit_location(context, loaded);
    }else{
        enum register_encoding reg = allocate_register(context, REGISTER_KIND_xmm);
        struct emit_location *zero = emit_location_loaded(context, REGISTER_KIND_xmm, reg, loaded->size);
        emit_register_register(context, no_prefix(), two_byte_opcode(XOR_XMM), zero, zero);
        
        enum legacy_prefixes prefix = loaded->size == 8 ? ASM_PREFIX_NON_PACKED_OP_double : ASM_PREFIX_NON_PACKED_OP_float;
        emit_register_register(context, create_prefixes(prefix), two_byte_opcode(COMPARE_XMM), loaded, zero);
        free_emit_location(context, loaded);
        free_emit_location(context, zero);
    }
    
    return emit_location_conditional(context, COMP_not_zero);
}

func struct emit_location *emit_code_for_plain_condition(struct context *context, struct ast *ast){
    struct emit_location *cond = emit_code_for_ast(context, ast);
    enum comp_condition condition;
    if(cond->state == EMIT_LOCATION_conditional){
        condition = cond->condition;
        cond = emit_location_conditional(context, condition);
    }else{
        
        struct emit_location *loaded;
        if(ast->resolved_type->kind == AST_bitfield_type){
            loaded = emit_load_bitfield(context, cond, (struct ast_bitfield_type *)ast->resolved_type);
        }else{
            assert(ast->resolved_type->kind == AST_integer_type || ast->resolved_type->kind == AST_atomic_integer_type || ast->resolved_type->kind == AST_pointer_type || ast->resolved_type->kind == AST_float_type);
            // @cleanup: atomic integers might need a special load instruction on other architectures?
            loaded = emit_load(context, cond);
        }
        
        cond = emit_compare_to_zero(context, loaded);
    }
    
    return cond;
}

func struct jump_context emit_code_for_if_condition(struct context *context, struct ast *ast){
    struct jump_context or_jump_context = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
    
    ast->byte_offset_in_function = to_s32(get_bytes_emitted(context));
    
    struct ast *or_it = ast;
    while(true){
        b32 should_loop = true;
        struct ast *and_it = or_it;
        if(or_it->kind == AST_logical_or){
            struct ast_binary_op *logical_or = cast(struct ast_binary_op *)or_it;
            and_it = logical_or->lhs;
            
            or_it = logical_or->rhs;
        }else{
            should_loop = false;
        }
        
        struct jump_context and_jump_context = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_false);
        while(and_it->kind == AST_logical_and){
            struct ast_binary_op *logical_and = cast(struct ast_binary_op *)and_it;
            
            // if this fails we want to jump to the next 'or' block
            struct emit_location *loc = emit_code_for_plain_condition(context, logical_and->lhs);
            jump_context_emit(&and_jump_context, loc->condition);
            
            and_it = logical_and->rhs;
        }
        
        // if this succeeds we want to jump to the inside of the 'if'
        // the last one automatically 'jumps' into the if block
        struct emit_location *loc = emit_code_for_plain_condition(context, and_it);
        
        if(!should_loop){
            jump_context_emit(&and_jump_context, loc->condition);
            // this is inside the 'if'
            emit_end_jumps(or_jump_context);
            return and_jump_context;
        }else{
            jump_context_emit(&or_jump_context, loc->condition);
            // after this is the next 'or' block
            emit_end_jumps(and_jump_context);
        }
    }
    
    invalid_code_path;
}

func struct emit_location *emit_shift_or_rotate__internal(struct context *context, struct prefixes prefixes, struct emit_location *lhs_loc,
        struct emit_location *rhs_loc, u8 reg_extension, b32 compound){
    
    smm size = lhs_loc->size;
    
    assert(rhs_loc->register_kind_when_loaded == REGISTER_KIND_gpr);
    assert(lhs_loc->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    if(compound){
        assert(lhs_loc->state == EMIT_LOCATION_register_relative);
    }
    
    struct emit_location *ret = null;
    if(rhs_loc->state == EMIT_LOCATION_immediate){
        assert(rhs_loc->size == 1);
        
        u8 inst = SHIFT_OR_ROTATE_REGM_IMMEDIATE8;
        if(lhs_loc->size == 1){
            inst = SHIFT_OR_ROTATE_REGM8_IMMEDIATE8;
        }
        
        if(compound){
            emit_register_relative_extended(context, prefixes, one_byte_opcode(inst), reg_extension, lhs_loc);
            ret = lhs_loc;
        }else{
            struct emit_location *lhs = emit_load(context, lhs_loc);
            emit_reg_extended_op(context, prefixes, one_byte_opcode(inst), reg_extension, lhs);
            ret = lhs;
        }
        emit(rhs_loc->value);
    }else{
        struct emit_location *rhs;
        if(rhs_loc->state == EMIT_LOCATION_loaded && rhs_loc->loaded_register == REGISTER_C){
            rhs = rhs_loc;
        }else{
            enum register_encoding cl = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_C);
            rhs = emit_load_into_specific_gpr(context, rhs_loc, cl);
        }
        
        emit_location_prevent_spilling(context, rhs);
        if(compound){
            u8 inst = SHIFT_OR_ROTATE_REGM_CL;
            if(lhs_loc->size == 1) inst = SHIFT_OR_ROTATE_REGM8_CL;
            emit_register_relative_extended(context, prefixes, one_byte_opcode(inst), reg_extension, lhs_loc);
            ret = lhs_loc;
        }else{
            struct emit_location *lhs = emit_load(context, lhs_loc);
            emit_reg_extended_op(context, prefixes, one_byte_opcode(SHIFT_OR_ROTATE_REGM_CL), reg_extension, lhs);
            lhs->size = size; // truncate it
            ret = lhs;
        }
        emit_location_allow_spilling(context, rhs);
        free_emit_location(context, rhs);
    }
    return ret;
}

func struct emit_location *emit_shift_or_rotate(struct context *context, struct ast *ast, u8 reg_extension, b32 compound){
    struct ast_binary_op *shift = cast(struct ast_binary_op *)ast;
    
    struct emit_location *rhs_loc = emit_code_for_ast(context, shift->rhs);
    rhs_loc = emit_load(context, rhs_loc);
    struct emit_location *lhs_loc = emit_code_for_ast(context, shift->lhs);
    
    assert(lhs_loc->size == shift->base.resolved_type->size);
    
    return emit_shift_or_rotate__internal(context, no_prefix(), lhs_loc, rhs_loc, reg_extension, compound);
}

///////////////////////////////////////////////////////////////////////////////////////////////

func void assert_that_no_registers_are_allocated(struct context *context){
    for(enum register_kind a = 0; a < REGISTER_KIND_count; a++){
        for(u32 i = 0; i < array_count(context->register_allocators[a].emit_location_map); i++){
            assert(context->register_allocators[a].emit_location_map[i] == 0);
        }
    }
}

func void emit_inline_asm_binary_op(struct context *context, struct prefixes prefixes, struct emit_location *lhs, struct emit_location *rhs,
        b32 is_signed, u8 reg_extended, u8 u8_code, u8 opcode, u8 u8_compound, u8 opcode_compound){
    assert(lhs->size == rhs->size);
    if(lhs->state == EMIT_LOCATION_register_relative){
        emit_compound_assignment__internal(context, prefixes, lhs, rhs, is_signed, reg_extended,
                u8_compound, opcode_compound);
    }else{
        emit_binary_op__internal(context, prefixes, lhs, rhs, lhs->size, is_signed, reg_extended, u8_code, opcode);
    }
}

#include "emit_inline_asm_block.c"

struct alloca_patch_node{
    struct alloca_patch_node *next;
    u8 *patch_location;
};

func struct emit_location *emit_intrinsic(struct context *context, struct ast_function_call *call){
    assert(call->identifier_expression->kind == AST_identifier);
    struct ast_identifier *ident = cast(struct ast_identifier *)call->identifier_expression;
    
    if(string_match(ident->base.token->string, string("_alloca"))){
        // 
        // Alloca stack setup:
        //     
        //     arguments
        //     return_pointer
        //     pushed_registers
        //     old_rbp
        //                                   < This is where RBP is pointing to.
        //     declarations
        //     temporary_stack               < This we collect in `temporary_stack_high_water_mark`.
        //     <alloca-space>
        //     function_arguments            < This we collect in `context->max_amount_of_function_call_arguments`.
        //                                   < This is where RSP is pointing to.
        //     stack of the next function
        // 
        // So `_alloca` should increment the stack by it's argument aligned to a 16-byte boundary, 
        // and return rsp + `context->max_amount_of_function_call_arguments * 8`, aligned to a 16-byte boundary.
        // Because `context->max_amount_of_function_call_arguments * 8` is not yet known, we need to emit a "patch".
        // 
        
        struct emit_location *size = emit_load(context, emit_code_for_ast(context, call->call_arguments.first->value));
        
        // Align-up the size to a 16-byte boundary.
        //    add size, 15
        //    and size, ~15
        emit_register_op__internal(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_REGM_IMMIDIATE), REG_OPCODE_ADD, size->loaded_register, /*size*/8);
        emit_u32(15);
        emit_register_op__internal(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_REGM_IMMIDIATE), REG_OPCODE_AND, size->loaded_register, /*size*/8);
        emit_u32((u32)~15);
        
        // Subtract the size from rsp
        //     sub rsp, size
        emit_register_register(context, no_prefix(), one_byte_opcode(SUB_REG_REGM), context->register_sp, size);
        
        // Load the resulting rsp plus the size needed for `function_arguments`
        //      lea return_register, [rsp + `context->max_amount_of_function_call_arguments * 8`]
        enum register_encoding return_register = size->loaded_register;
        enum rex_encoding rex = REXW;
        if(register_is_extended(return_register)) rex |= REXR;
        emit(rex);
        emit(LOAD_ADDRESS_REG_MEMORY_LOCATION);
        emit(make_modrm(MODRM_REGM32, (return_register & 7), REGISTER_SP));
        emit(make_sib(0, REGISTER_SP, REGISTER_SP));
        u8 *patch_location = context->emit_pool.current;
        emit_u32(0x13371337);
        
        struct alloca_patch_node *patch_node = push_struct(&context->scratch, struct alloca_patch_node);
        patch_node->patch_location = patch_location;
        
        sll_push_back(context->alloca_patch_nodes, patch_node);
        
        return size; // This is now the address.
    }else{
        invalid_code_path;
    }
    
#if 0
    
    struct intrinsic_info *info = lookup_intrinsic(ident->decl->identifier->atom);
    // smm sse_prefix = -1;
    
    switch(info->kind){
        
        case INTRINSIC_KIND_va_start:{
            // preprocesses to '((void)(__va_start(&_ArgList,   _Format)))'
            assert(call->call_arguments.count == 2);
            struct ast *arglist = call->call_arguments.first->value;
            assert(arglist->resolved_type->kind == AST_pointer_type);
            
            struct emit_location *lhs = emit_code_for_ast(context, arglist);
            
            struct ast *format = call->call_arguments.last->value;
            assert(format->kind == AST_identifier);
            struct emit_location *rhs = emit_code_for_ast(context, format);
            assert(rhs->state == EMIT_LOCATION_register_relative);
            rhs->offset += 8;
            enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
            struct emit_location *addr = emit_load_address(context, rhs, reg);
            
            struct emit_location *store_in = emit_location_register_relative(context, REGISTER_KIND_gpr, lhs, null, 0, 8);
            emit_store(context, store_in, addr);
            
            return store_in;
        }break;
        
        invalid_default_case(return emit_location_invalid(context));
    }
#endif
}

// @cleanup: get rid of me once we actually do special stuff in the 'array_subscript' case
func struct emit_location *emit_code_for_pointer_subscript(struct context *context, struct ast_type *deref_type, struct emit_location *pointer, struct emit_location *index){
    
    // @cleanup: what about 'index' integer literal
    
    //
    // We want to return a register relative emit_location which as a base has 'pointer' and
    // as 'index' has either a premultiplied 'index' or uses a 'scale'
    //
    enum register_kind register_kind = get_register_kind_for_type(deref_type);
    
    #if 0
    int log_scale = -1;
    switch(deref_type->size){
        case 1: log_scale = 0; break;
        case 2: log_scale = 1; break;
        case 4: log_scale = 2; break;
        case 8: log_scale = 3; break;
    }
    
    if(log_scale != -1){
        struct emit_location *loc = emit_location_register_relative(context, register_kind, pointer, index, 0, deref_type->size);
        loc->log_index_scale = log_scale;
        return loc;
    }
    #endif
    index = emit_load(context, index);
    
    // @incomplete:
    assert((s32)deref_type->size == deref_type->size);
    
    // @cleanup: small immediate
    emit_register_register(context, no_prefix(), one_byte_opcode(IMUL_REG_REGM_IMMIDIATE), index, index);
    emit_u32(deref_type->size);
    
    return emit_location_register_relative(context, register_kind, pointer, index, 0, deref_type->size);
}

//_____________________________________________________________________________________________________________________
// :emit_code_for_ast

func struct emit_location *emit_code_for_ast(struct context *context, struct ast *ast){
    assert(ast->resolved_type);
    
    ast->byte_offset_in_function = to_s32(get_bytes_emitted(context));
    
    switch(ast->kind){
        case AST_typedef:  return emit_location_invalid(context);
        case AST_function: return emit_location_invalid(context);
        case AST_declaration:{
            struct ast_declaration *decl = cast(struct ast_declaration *)ast;
            
            if(decl->flags & DECLARATION_FLAGS_is_local_persist){
                // nothing to do here local_persists don't need to be initialized or allocated at local scope
                return emit_location_invalid(context);
            }
            
            if(decl->flags & DECLARATION_FLAGS_is_global){
                // This was an extern variable, nothing to do here.
                return emit_location_invalid(context);
            }
            
            // @cleanup: constant?
            if(decl->assign_expr){
                emit_code_for_ast(context, decl->assign_expr);
            }
            
            return emit_location_invalid(context);
        }break;
        case AST_declaration_list:{
            struct ast_declaration_list *list = cast(struct ast_declaration_list *)ast;
            for(struct declaration_node *node = list->list.first; node; node = node->next){
                emit_code_for_ast(context, cast(struct ast *)node->decl); // @speed, we don't have to recurse here...
            }
            return emit_location_invalid(context);
        }break;
        case AST_identifier:{
            struct ast_identifier *ident = cast(struct ast_identifier *)ast;
            struct ast_declaration *decl = ident->decl;
            
            // There are some cases:
            //    1) Enum Member  -> immediate
            //    2) Global       -> rip relative
            //    3) Local        -> stack relative
            //    4) big argument -> register relative to stack location
            //    5) dllimport    -> register relative to rip relative location
            
            if(decl->flags & DECLARATION_FLAGS_is_enum_member){
                // @cleanup: can we even get in here? I thought we resolved them while parsing
                assert(decl->assign_expr);
                assert(decl->assign_expr->kind == AST_integer_literal);
                return emit_location_immediate(context, integer_literal_to_bytes(decl->assign_expr), decl->assign_expr->resolved_type->size);
            }
            
            enum register_kind register_kind = get_register_kind_for_type(decl->type);
            smm decl_size = decl->type->size;
            
            struct emit_location *ret = null;
            if(decl->flags & DECLARATION_FLAGS_is_thread_local){
                // 
                // Loading thread locals:
                // 
                //     mov eax, offset(<var>)
                //     mov ecx, [_tls_index]
                //     mov rdx, gs:[58h]
                //     mov rcx, [rdx + rcx*8]
                //     return [rcx + rax]
                // 
                
                // mov <offset_reg>, offset_in_tls(<decl>)
                struct emit_location *offset_reg = emit_location_loaded(context, REGISTER_KIND_gpr, allocate_register(context, REGISTER_KIND_gpr), 4);
                if(register_is_extended(offset_reg->loaded_register)) emit(REXB);
                emit(MOVE_REG_IMMEDIATE + (offset_reg->loaded_register & 7));
                smm byte_offset = emit_u32(0);
                smm rip_at = get_bytes_emitted(context);
                emit_patch(context, PATCH_section_offset, &decl->base, 0, &context->current_function->as_decl, byte_offset, rip_at);
                
                struct emit_location *tls_index = emit_location_rip_relative(context, &globals.tls_index_declaration->base, REGISTER_KIND_gpr, 4);
                
                // mov <tls_slots>, gs:[58h]
                struct emit_location *tls_slots = emit_location_loaded(context, REGISTER_KIND_gpr, allocate_register(context, REGISTER_KIND_gpr), 8);
                emit(LEGACY_GS_SEGMENT_OVERRIDE_PREFIX);
                if(register_is_extended(tls_slots->loaded_register)) emit(REXW | REXR); else emit(REXW);
                emit(MOVE_REG_REGM);
                emit(make_modrm(MODRM_REGM, (/*reg*/tls_slots->loaded_register & 7), REGISTER_SIB_EXTENSION));
                emit(make_sib(0, /*no-index*/REGISTER_SP, /*no-base*/REGISTER_BP));
                emit_u32(0x58);
                
                // mov tls_slot, [rdx + rcx*8]
                struct emit_location *tls_slot_register_relative = emit_location_register_relative(context, REGISTER_KIND_gpr, tls_slots, tls_index, /*offset*/0, /*size*/8);
                tls_slot_register_relative->log_index_scale = 3;
                struct emit_location *tls_slot = emit_load(context, tls_slot_register_relative);
                
                ret = emit_location_register_relative(context, register_kind, tls_slot, offset_reg, 0, decl_size);
            }else if(decl->flags & (DECLARATION_FLAGS_is_global | DECLARATION_FLAGS_is_local_persist)){
                    
                if(decl->flags & DECLARATION_FLAGS_is_dllimport){
                    // :dllimport_loading
                    // 
                    // for an dllimport the address out of the dllimport table. The 'memory_location' and 
                    // 'relative_virtual_address' describe the dllimport table address, thus we patch to load this 
                    // address.
                    // Once we have this address, we have a register relative location.
                    // Note that for functions calls similar code special cases dllimports as well, but we dont end
                    // up in here, because if we call an identifier we never recurse into emit_code_for_ast.
                    //                                                                              -08.08.2021
                    
                    struct emit_location *address = emit_location_rip_relative(context, &decl->base, register_kind, 8);
                    ret = emit_location_register_relative(context, register_kind, address, 0, 0, decl_size);
                }else{
                    ret = emit_location_rip_relative(context, &decl->base, register_kind, decl_size);
                }
            }else{
                
                // :MemoryLocations 
                // 
                // stack memory locations might be negative if the declaration is an argument, but they can never be -1.
                assert(decl->offset_on_stack != -1);
                
                if(decl->flags & DECLARATION_FLAGS_is_big_function_argument){
                    // :PassingStructArguments
                    // 
                    // it is actually a pointer to the declaration, not the declaration itself.
                    // we emit '[[stack_location]]' here. (register relative to register relative
                    
                    struct emit_location *stack_location = emit_location_stack_relative(context, REGISTER_KIND_gpr, decl->offset_on_stack, 8);
                    ret = emit_location_register_relative(context, register_kind, stack_location, 0, 0, decl_size);
                }else{
                    ret = emit_location_stack_relative(context, register_kind, decl->offset_on_stack, decl_size);
                }
            }
            
            return ret;
        }break;
        case AST_compound_literal:{
            struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)ast;
            struct ast_declaration *decl = compound_literal->decl;
            
            struct emit_location *decl_location = emit_location_stack_relative(context, REGISTER_KIND_gpr, decl->offset_on_stack, decl->type->size);
            if(decl->type->kind == AST_struct || decl->type->kind == AST_union || decl->type->kind == AST_array_type){
                emit_memset(context, decl_location, 0);
            }
            
            for_ast_list(compound_literal->assignment_list){
                struct emit_location *loc = emit_code_for_ast(context, it->value);
                if(loc) free_emit_location(context, loc);
            }
            
            return decl_location;
        }break;
        case AST_member:{
            struct ast_dot_or_arrow *dot = cast(struct ast_dot_or_arrow *)ast;
            assert(dot->lhs->resolved_type->kind == AST_struct || dot->lhs->resolved_type->kind == AST_union);
            
            struct emit_location *loc = emit_code_for_ast(context, dot->lhs);
            assert(loc->state == EMIT_LOCATION_register_relative);
            
            loc->register_kind_when_loaded = get_register_kind_for_type(dot->base.resolved_type);
            
            loc->size    = dot->base.resolved_type->size;
            loc->offset += dot->member->offset_in_type;
            
            return loc;
        }break;
        case AST_member_deref:{
            struct ast_dot_or_arrow *arrow = cast(struct ast_dot_or_arrow *)ast;
            assert(arrow->lhs->resolved_type->kind == AST_pointer_type);
            
            struct emit_location *loc = emit_code_for_ast(context, arrow->lhs);
            enum register_kind register_kind = get_register_kind_for_type(arrow->base.resolved_type);
            
            return emit_location_register_relative(context, register_kind, loc, null, arrow->member->offset_in_type, arrow->base.resolved_type->size);
        }break;
        case AST_pointer_subscript:{
            struct ast_subscript *subscript = (struct ast_subscript *)ast;
            struct emit_location *pointer = emit_code_for_ast(context, subscript->lhs);
            struct emit_location *index   = emit_code_for_ast(context, subscript->index);
            
            struct ast_pointer_type *pointer_type = (struct ast_pointer_type *)subscript->lhs->resolved_type;
            
            return emit_code_for_pointer_subscript(context, pointer_type->pointer_to, pointer, index);
        }break;
        case AST_array_subscript:{
            struct ast_subscript *subscript = (struct ast_subscript *)ast;
            struct emit_location *array = emit_code_for_ast(context, subscript->lhs);
            
            assert(array->state == EMIT_LOCATION_register_relative);
            
            u64 size  = subscript->base.resolved_type->size;
            enum register_kind register_kind = get_register_kind_for_type(subscript->base.resolved_type);
            
            array->register_kind_when_loaded = register_kind;
            array->size = size;
            
            if(subscript->index->kind == AST_integer_literal){
                
                u64 index = integer_literal_to_bytes(subscript->index);
                
                // @cleanup: overflow?
                if(array->offset + size * index > s32_max){
                    
                    struct emit_location *index_register = emit_location_immediate(context, size * index, 8);
                    
                    if(array->ast || array->index){
                        // 'loc' is rip-relative, we cannot have [rip + rax + 0x1337], hence we need to load rip + 0x1337 first.
                        // Similarly, if 'loc' already has an index register, we need to load it so we can add _another_ index register.
                        struct emit_location *pointer = emit_load_address(context, array, allocate_register(context, REGISTER_KIND_gpr));
                        free_emit_location(context, array);
                        array = emit_location_register_relative(context, register_kind, pointer, index_register, 0, size);
                    }else{
                        array->index = index_register;
                    }
                }else{
                    array->offset += size * index;
                }
                
                return array;
            }else if(size == 1 || size == 2 || size == 4 || size == 8){
                
                struct emit_location *index_register = emit_code_for_ast(context, subscript->index);
                
                if(array->ast || array->index){
                    // If 'loc' is rip-relative, we cannot have [rip + rax + 0x1337], hence we need to load rip + 0x1337 first.
                    // Similarly, if 'loc' already has an index register, we need to load it so we can add _another_ index register.
                    struct emit_location *pointer = emit_load_address(context, array, allocate_register(context, REGISTER_KIND_gpr));
                    free_emit_location(context, array);
                    array = emit_location_register_relative(context, register_kind, pointer, index_register, 0, size);
                }else{
                    array->index = index_register;
                }
                
                switch(size){
                    case 1: array->log_index_scale = 0; break;
                    case 2: array->log_index_scale = 1; break;
                    case 4: array->log_index_scale = 2; break;
                    case 8: array->log_index_scale = 3; break;
                }
                
                return array;
            }else{
                struct emit_location *index = emit_code_for_ast(context, subscript->index);
                
                //
                // @cleanup: hack for now: first load the address then call into the 'pointer' case.
                //
                
                struct emit_location *pointer = emit_load_address(context, array, allocate_register(context, REGISTER_KIND_gpr));
                free_emit_location(context, array);
                
                struct ast_array_type *array_type = (struct ast_array_type *)subscript->lhs->resolved_type;
                
                return emit_code_for_pointer_subscript(context, array_type->element_type, pointer, index);
            }
        }break;
        
        case AST_integer_literal:{
            return emit_location_immediate(context, integer_literal_to_bytes(ast), ast->resolved_type->size);
        }break;
        case AST_pointer_literal:{
            struct ast_pointer_literal *pointer = cast(struct ast_pointer_literal *)ast;
            return emit_location_immediate(context, (u64)pointer->pointer, ast->resolved_type->size);
        }break;
        case AST_float_literal:{
            struct ast_float_literal *f = cast(struct ast_float_literal *)ast;
            sll_push_back(context->float_literals, f);
            context->float_literals.amount_of_float_literals += 1;
            return emit_location_rip_relative(context, &f->base, REGISTER_KIND_xmm, f->base.resolved_type->size);
        }break;
        case AST_string_literal:{
            struct ast_string_literal *lit = cast(struct ast_string_literal *)ast;
            sll_push_back(context->string_literals, lit);
            context->string_literals.amount_of_strings += 1;
            return emit_location_rip_relative(context, &lit->base, REGISTER_KIND_gpr, 8);
        }break;
        case AST_implicit_address_conversion:
        case AST_unary_address:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            
            struct emit_location *loaded = emit_load_address(context, loc, allocate_register(context, REGISTER_KIND_gpr));
            free_emit_location(context, loc);
            
            return loaded;
        }break;
        
        case AST_pointer_literal_deref:{
            struct ast_pointer_literal *pointer_literal_deref = (struct ast_pointer_literal *)ast;
            enum register_kind register_kind = get_register_kind_for_type(ast->resolved_type);
            
            struct emit_location *loc = emit_location_immediate(context, (u64)pointer_literal_deref->pointer, 8);
            return emit_location_register_relative(context, register_kind, loc, null, 0, ast->resolved_type->size);
        }break;
        
        case AST_unary_deref:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            enum register_kind register_kind = get_register_kind_for_type(op->base.resolved_type);
            
            return emit_location_register_relative(context, register_kind, loc, null, 0, op->base.resolved_type->size);
        }break;
        case AST_unary_plus:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            return emit_code_for_ast(context, op->operand);
        }break;
        case AST_unary_bitwise_not:
        case AST_unary_minus:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            struct emit_location *loaded = emit_load(context, loc);
            
            if(loaded->register_kind_when_loaded == REGISTER_KIND_xmm){
                emit_location_prevent_spilling(context, loaded);
                
                // 
                // @note: To allow for -(0) == -0, instead of 0, we have to use an xor.
                // 
                
                enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
                struct emit_location *as_gpr = emit_location_loaded(context, REGISTER_KIND_gpr, reg, loaded->size);
                emit_location_prevent_spilling(context, as_gpr);
                
                // movd / movq as_gpr, loaded
                emit_register_op__internal(context, create_prefixes(ASM_PREFIX_66), two_byte_opcode(0x7E), loaded->loaded_register, as_gpr->loaded_register, loaded->size); // @sigh: internall because of register_kind mismatch
                
                if(loaded->size == 4){
                    // xor as_gpr, 0x80000000
                    emit_register_op__internal(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_REGM_IMMIDIATE), REG_OPCODE_XOR, as_gpr->loaded_register, 4);
                    emit_u32(0x80000000);
                }else{
                    assert(loaded->size == 8);
                    struct emit_location *loaded_immediate = emit_load(context, emit_location_immediate(context, 0x8000000000000000, 8));
                    
                    emit_register_register(context, no_prefix(), one_byte_opcode(XOR_REG_REGM), as_gpr, loaded_immediate);
                    free_emit_location(context, loaded_immediate);
                }
                
                // movd / movq loaded, as_gpr
                emit_register_op__internal(context, create_prefixes(ASM_PREFIX_66), two_byte_opcode(0x6E), loaded->loaded_register, as_gpr->loaded_register, loaded->size); // @sigh: internall because of register_kind mismatch
                
                emit_location_allow_spilling(context, as_gpr);
                free_emit_location(context, as_gpr);
                
                emit_location_allow_spilling(context, loaded);
                return loaded;
            }else{
                // @cleanup: this needs a one byte case right?
                
                u8 extension = (ast->kind == AST_unary_minus) ?  REG_OPCODE_NEGATE_REGM : REG_OPCODE_NOT_REGM;
                emit_reg_extended_op(context, no_prefix(), one_byte_opcode(REG_EXTENDED_UNARY_REGM), extension, loaded);
                return loaded;
            }
        }break;
        case AST_unary_predec:
        case AST_unary_preinc:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            assert(loc->state == EMIT_LOCATION_register_relative);
            
            if(op->base.resolved_type->kind == AST_pointer_type){
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)op->base.resolved_type;
                u8 reg_inst = (ast->kind == AST_unary_preinc) ? REG_OPCODE_ADD : REG_OPCODE_SUB;
                smm size = pointer->pointer_to->size;
                b32 is_big = size > max_s8;
                
                assert(size >= 0); // @note: Allow empty structs.
                assert(size <= 0xffffffff);
                u8 inst = is_big ? REG_EXTENDED_OPCODE_REGM_IMMIDIATE : REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                
                assert(loc->size == 8);
                struct emit_location *immediate = emit_location_immediate(context, size, is_big ? 4 : 1);
                emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(inst), reg_inst, loc, immediate);
            }else if(op->base.resolved_type == &globals.typedef_Bool){
                
                // 
                // For _Bool we want the following:
                //  ++arst: mov [arst], 1
                //  --arst: xor [arst], 1
                
                struct emit_location *immediate = emit_location_immediate(context, /*value*/1, /*size*/1);
                
                if(ast->kind == AST_unary_predec){
                    emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_REGM8_IMMIDIATE8), REG_OPCODE_XOR, loc, immediate);
                }else{
                    emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(MOVE_REGM8_IMMEDIATE8), 0, loc, immediate);
                }
            }else if(op->base.resolved_type->kind == AST_integer_type){
                u8 inst = (ast->kind == AST_unary_preinc) ? FF_INCREMENT_REGM : FF_DECREMENT_REGM;
                u8 opcode = loc->size == 1 ? REG_EXTENDED_OPCODE_FE : REG_EXTENDED_OPCODE_FF;
                emit_register_relative_extended(context, no_prefix(), one_byte_opcode(opcode), inst, loc);
            }else if(op->base.resolved_type->kind == AST_float_type){
                struct ast_float_literal *one = parser_ast_push(context, op->base.token, float_literal);
                one->value = 1.0;
                set_resolved_type(&one->base, op->base.resolved_type, null);
                
                struct emit_location *rhs = emit_code_for_ast(context, &one->base);
                assert(rhs->state == EMIT_LOCATION_register_relative);
                
                emit_location_prevent_spilling(context, loc);
                struct emit_location *lhs = emit_load(context, loc);
                
                // emit 'op lhs, [float_literal]'
                emit_register_relative_register(context, get_sse_prefix_for_scalar(lhs->size), two_byte_opcode((ast->kind == AST_unary_preinc) ? ADD_XMM : SUB_XMM), lhs->loaded_register, rhs);
                
                emit_store(context, loc, lhs);
                
                emit_location_allow_spilling(context, loc);
            }else{
                assert(op->base.resolved_type->kind == AST_bitfield_type);
                struct ast_bitfield_type *bitfield = (struct ast_bitfield_type *)op->base.resolved_type;
                
                emit_location_prevent_spilling(context, loc);
                
                // 
                // Load the bitfield value.
                // Because usually we also up-convert the value to an int,
                // 'loaded' is at least of size 4.
                // 
                struct emit_location *loaded = emit_load_bitfield(context, loc, bitfield);
                
                // Increment/Decrement 'loaded'.
                u8 inst = (ast->kind == AST_unary_preinc) ? FF_INCREMENT_REGM : FF_DECREMENT_REGM;
                emit_reg_extended_op(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_FF), inst, loaded);
                
                // Truncate the value of loaded again to the size of 'loc', 
                // as that is what 'emit_store_bitfield' expects.
                loaded->size = loc->size;
                emit_store_bitfield(context, bitfield, loc, loaded);
                
                emit_location_allow_spilling(context, loc);
            }
            
            return emit_load(context, loc);
        }break;
        
        // @cleanup: This should be unified with the pre{*} case.
        case AST_unary_postdec:
        case AST_unary_postinc:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            assert(loc->state == EMIT_LOCATION_register_relative);
            
            emit_location_prevent_spilling(context, loc);
            struct emit_location *loaded = emit_load(context, loc);
            
            if(op->base.resolved_type->kind == AST_pointer_type){
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)op->base.resolved_type;
                u8 reg_inst = (ast->kind == AST_unary_postinc) ? REG_OPCODE_ADD : REG_OPCODE_SUB;
                smm size = pointer->pointer_to->size;
                b32 is_big = size > max_s8;
                
                assert(size >= 0); // @note: allow empty structs to be incremented.
                assert(size <= 0xffffffff);
                u8 inst = is_big ? REG_EXTENDED_OPCODE_REGM_IMMIDIATE : REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                
                assert(loc->size == 8);
                struct emit_location *immediate = emit_location_immediate(context, size, is_big ? 4 : 1);
                emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(inst), reg_inst, loc, immediate);
            }else if(op->base.resolved_type == &globals.typedef_Bool){
                
                // 
                // For _Bool we want the following:
                //  ++arst: mov [arst], 1
                //  --arst: xor [arst], 1
                
                struct emit_location *immediate = emit_location_immediate(context, /*value*/1, /*size*/1);
                
                if(ast->kind == AST_unary_postdec){
                    emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_REGM8_IMMIDIATE8), REG_OPCODE_XOR, loc, immediate);
                }else{
                    emit_register_relative_immediate(context, no_prefix(), one_byte_opcode(MOVE_REGM8_IMMEDIATE8), 0, loc, immediate);
                }
            }else if(op->base.resolved_type->kind == AST_integer_type){
                u8 inst = (ast->kind == AST_unary_postinc) ? FF_INCREMENT_REGM : FF_DECREMENT_REGM;
                u8 opcode = loc->size == 1 ? REG_EXTENDED_OPCODE_FE : REG_EXTENDED_OPCODE_FF;
                emit_register_relative_extended(context, no_prefix(), one_byte_opcode(opcode), inst, loc);
            }else if(op->base.resolved_type->kind == AST_float_type){
                struct ast_float_literal *one = parser_ast_push(context, op->base.token, float_literal);
                one->value = 1.0;
                set_resolved_type(&one->base, op->base.resolved_type, null);
                
                struct emit_location *rhs = emit_code_for_ast(context, &one->base);
                assert(rhs->state == EMIT_LOCATION_register_relative);
                
                struct emit_location *lhs = emit_load(context, loc); // @cleanup: This should be a register-to-register move instead.
                
                // emit 'op lhs, [float_literal]'
                emit_register_relative_register(context, get_sse_prefix_for_scalar(lhs->size), two_byte_opcode((ast->kind == AST_unary_postinc) ? ADD_XMM : SUB_XMM), lhs->loaded_register, rhs);
                emit_store(context, loc, lhs);
            }else{
                assert(op->base.resolved_type->kind == AST_bitfield_type);
                struct ast_bitfield_type *bitfield = (struct ast_bitfield_type *)op->base.resolved_type;
                
                // @note: We return 'loaded' in the end, which is still a bitfield.
                emit_location_prevent_spilling(context, loaded);
                
                // Copy 'loaded' so we retain the original value.
                enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
                struct emit_location *copied = emit_load_into_specific_gpr(context, loaded, reg);
                
                // Load the bitfield.
                copied = emit_load_bitfield(context, copied, bitfield);
                
                // Increment/Decrement 'copied'.
                u8 inst = (ast->kind == AST_unary_postinc) ? FF_INCREMENT_REGM : FF_DECREMENT_REGM;
                u8 opcode = loc->size == 1 ? REG_EXTENDED_OPCODE_FE : REG_EXTENDED_OPCODE_FF;
                emit_reg_extended_op(context, no_prefix(), one_byte_opcode(opcode), inst, copied);
                
                // Truncate the value of loaded again to the size of 'loc', 
                // as that is what 'emit_store_bitfield' expects.
                copied->size = loc->size;
                
                // Store copied.
                emit_store_bitfield(context, bitfield, loc, copied);
                
                // "Return" the original "loaded" value.
                emit_location_allow_spilling(context, loaded);
            }
            
            emit_location_allow_spilling(context, loc);
            free_emit_location(context, loc);
            return loaded;
        }break;
        case AST_binary_or:{
            return emit_binary_op(context, ast, REG_OPCODE_OR, OR_REG8_REGM8, OR_REG_REGM);
        }break;
        case AST_binary_xor:{
            return emit_binary_op(context, ast, REG_OPCODE_XOR, XOR_REG8_REGM8, XOR_REG_REGM);
        }break;
        case AST_binary_and:{
            return emit_binary_op(context, ast, REG_OPCODE_AND, AND_REG8_REGM8, AND_REG_REGM);
        }break;
        case AST_binary_plus:{
            if(ast->resolved_type == &globals.typedef_f32 || ast->resolved_type == &globals.typedef_f64){
                return emit_binary_op_xmm(context, ast, ADD_XMM);
            }
            return emit_binary_op(context, ast, REG_OPCODE_ADD, ADD_REG8_REGM8, ADD_REG_REGM);
        }break;
        case AST_binary_minus:{
            if(ast->resolved_type == &globals.typedef_f32 || ast->resolved_type == &globals.typedef_f64){
                return emit_binary_op_xmm(context, ast, SUB_XMM);
            }
            return emit_binary_op(context, ast, REG_OPCODE_SUB, SUB_REG8_REGM8, SUB_REG_REGM);
        }break;
        case AST_binary_times:{
            if(ast->resolved_type == &globals.typedef_f32 || ast->resolved_type == &globals.typedef_f64){
                return emit_binary_op_xmm(context, ast, MUL_XMM);
            }
            
            return emit_divide_or_mod_or_multiply(context, ast, REG_OPCODE_IMUL_REGM_RAX, REG_OPCODE_MUL_REGM_RAX, false);
        }break;
        
        case AST_binary_divide:
        case AST_binary_mod:{
            if(ast->resolved_type == &globals.typedef_f32 || ast->resolved_type == &globals.typedef_f64){
                assert(ast->kind == AST_binary_divide);
                return emit_binary_op_xmm(context, ast, DIV_XMM);
            }
            return emit_divide_or_mod_or_multiply(context, ast, REG_OPCODE_IDIV_REGM_RAX, REG_OPCODE_DIV_REGM_RAX, false);
        }break;
        case AST_binary_left_shift:{
            smm is_signed = type_is_signed(ast->resolved_type);
            u8 inst = is_signed ? REG_OPCODE_SHIFT_ARITHMETIC_LEFT : REG_OPCODE_SHIFT_LEFT;
            return emit_shift_or_rotate(context, ast, inst, false);
        }break;
        case AST_binary_right_shift:{
            smm is_signed = type_is_signed(ast->resolved_type);
            u8 inst = is_signed ? REG_OPCODE_SHIFT_ARITHMETIC_RIGHT : REG_OPCODE_SHIFT_RIGHT;
            return emit_shift_or_rotate(context, ast, inst, false);
        }break;
        case AST_unary_logical_not:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_plain_condition(context, op->operand);
            switch(loc->condition){
                case COMP_equals:                loc->condition = COMP_unequals;              break;
                case COMP_unequals:              loc->condition = COMP_equals;                break;
                case COMP_smaller:               loc->condition = COMP_bigger_equals;         break;
                case COMP_bigger:                loc->condition = COMP_smaller_equals;        break;
                case COMP_smaller_equals:        loc->condition = COMP_bigger;                break;
                case COMP_bigger_equals:         loc->condition = COMP_smaller;               break;
                case COMP_bigger_equals_signed:  loc->condition = COMP_smaller_signed;        break;
                case COMP_smaller_equals_signed: loc->condition = COMP_bigger_signed;         break;
                case COMP_bigger_signed:         loc->condition = COMP_smaller_equals_signed; break;
                case COMP_smaller_signed:        loc->condition = COMP_bigger_equals_signed;  break;
                invalid_default_case();
            }
            return loc;
        }break;
        case AST_logical_or:{
            //        for 'or' switch the branches, i.e:
            //            if(lhs){
            //                result = true;
            //            }else{
            //                result = rhs;
            //            }
            
            // :spill_all_registers_on_every_branch
            // Because this has branches, we need to spill all volatile registers, otherwise an expression like
            //    a + (b || c * d);
            // might spill a on the right hand side, but not the lhs and thus would be uninitialized if the 
            // lhs is taken.
            spill_all_allocated_volatile_registers(context);
            
            struct ast_binary_op *op = cast(struct ast_binary_op*)ast;
            struct jump_context jump_context = emit_code_for_if_condition(context, op->lhs);
            
            enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
            struct emit_location *immediate = emit_location_immediate(context, 1, 4);
            struct emit_location *loaded = emit_load_into_specific_gpr(context, immediate, reg);
            free_emit_location(context, loaded); // we load it later again.
            
            struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            jump_context_emit(&jump_over_else, COMP_none);
            emit_end_jumps(jump_context);
            
            struct emit_location *rhs_loc = emit_code_for_plain_condition(context, op->rhs);
            loaded = emit_load_into_specific_gpr(context, rhs_loc, reg);
            
            emit_end_jumps(jump_over_else);
            return loaded;
        }break;
        case AST_logical_and:{
            // @note: this is only the expression case, i.e int c = (a && b); and not the 'if' case.
            //        in the 'and' case this becomes the same as
            //            if(lhs){
            //                result = rhs;
            //            }else{
            //                result = false;
            //            }
            
            // :spill_all_registers_on_every_branch
            // Because this has branches, we need to spill all volatile registers, otherwise an expression like
            //    a + (b && c * d);
            // might spill a on the right hand side, but not the lhs and thus would be uninitialized if the 
            // lhs is taken.
            spill_all_allocated_volatile_registers(context);
            
            struct ast_binary_op *op = cast(struct ast_binary_op*)ast;
            struct jump_context jump_context = emit_code_for_if_condition(context, op->lhs);
            
            struct emit_location *rhs_loc = emit_code_for_plain_condition(context, op->rhs);
            struct emit_location *rhs = emit_load(context, rhs_loc);
            
            struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            jump_context_emit(&jump_over_else, COMP_none);
            emit_end_jumps(jump_context);
            
            emit_register_register(context, no_prefix(), one_byte_opcode(XOR_REG_REGM), rhs, rhs);
            emit_end_jumps(jump_over_else);
            
            return rhs;
        }break;
        case AST_binary_bigger:
        case AST_binary_bigger_equals:
        case AST_binary_smaller:
        case AST_binary_smaller_equals:
        case AST_binary_logical_equals:
        case AST_binary_logical_unequals:{
            // @quality: in the case of "cmp [rax], 1" we do not have to load [rax] as cmp does not write
            struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
            
            struct emit_location *result;
            if(op->lhs->resolved_type->kind == AST_float_type){
                
                struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
                struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
                
                assert(lhs->register_kind_when_loaded == REGISTER_KIND_xmm);
                assert(rhs->register_kind_when_loaded == REGISTER_KIND_xmm);
                lhs = emit_load(context, lhs);
                
                struct prefixes prefix;
                if(lhs->size == 8){
                    prefix = create_prefixes(ASM_PREFIX_NON_PACKED_OP_double);
                }else{
                    assert(lhs->size == 4);
                    prefix = create_prefixes(ASM_PREFIX_NON_PACKED_OP_float);
                }
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, prefix, two_byte_opcode(COMPARE_XMM), lhs, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, prefix, two_byte_opcode(COMPARE_XMM), lhs->loaded_register, rhs);
                }
                free_emit_location(context, rhs);
                result = lhs;
            }else{
                result = emit_binary_op(context, ast, REG_OPCODE_CMP, CMP_REG8_REGM8, CMP_REG_REGM);
            }
            // @paranoid, technically these should always agree
            b32 is_signed = type_is_signed(op->lhs->resolved_type) || type_is_signed(op->rhs->resolved_type);
            free_emit_location(context, result); // we only care about the flags
            
            enum comp_condition cond;
            switch(ast->kind){
                case AST_binary_logical_equals:{
                    cond = COMP_equals;
                }break;
                case AST_binary_logical_unequals:{
                    cond = COMP_unequals;
                }break;
                case AST_binary_smaller:{
                    cond = is_signed ? COMP_smaller_signed : COMP_smaller;
                }break;
                case AST_binary_smaller_equals:{
                    cond = is_signed ? COMP_smaller_equals_signed : COMP_smaller_equals;
                }break;
                case AST_binary_bigger:{
                    cond = is_signed ? COMP_bigger_signed : COMP_bigger;
                }break;
                case AST_binary_bigger_equals:{
                    cond = is_signed ? COMP_bigger_equals_signed : COMP_bigger_equals;
                }break;
                
                invalid_default_case(cond = COMP_equals);
            }
            
            return emit_location_conditional(context, cond);
        }break;
        case AST_conditional_expression:{
            
            // :spill_all_registers_on_every_branch
            // If there is an expression like 
            //     a + (b ? c : d * e)
            // and 'a' gets loaded into 'edx' then we might spill it in the 'd*e' branch, but not in the 
            // 'c' branch. This would be bad, as a would get reloaded in either case and thus might be unintialized.
            // To prevent this, we spill all registers prior to the instruction.
            spill_all_allocated_volatile_registers(context);
            
            struct ast_conditional_expression *conditional = cast(struct ast_conditional_expression *)ast;
            struct ast_type *type = ast->resolved_type;
            // @note: conditional expressions are not l-values
            
            if(type == &globals.typedef_void){
                //
                // Special case for void, as we don't have to do any dancing to keep the 'return_value' around.
                //
                
                struct jump_context jump_context = emit_code_for_if_condition(context, conditional->condition);
                
                struct emit_location *if_true = emit_code_for_ast(context, conditional->if_true);
                assert(if_true == emit_location_invalid(context));
                
                struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
                jump_context_emit(&jump_over_else, COMP_none);
                
                emit_end_jumps(jump_context);
                
                struct emit_location *if_false = emit_code_for_ast(context, conditional->if_false);
                assert(if_false == emit_location_invalid(context));
                
                emit_end_jumps(jump_over_else);
                
                return emit_location_invalid(context);
            }
            
            
            // used if 'should_memcpy'
            b32 should_memcpy = size_is_big_or_oddly_sized(type->size);
            struct emit_location *temporary = emit_location_invalid(context);
            
            // used if the type is register sized
            enum register_kind register_kind = get_register_kind_for_type(conditional->base.resolved_type);
            enum register_encoding reg = INVALID_REGISTER;
            if(should_memcpy){
                temporary = emit_allocate_temporary_stack_location(context, REGISTER_KIND_gpr, type->size, type->alignment);
            }else{
                reg = allocate_register(context, register_kind);
            }
            
            struct jump_context jump_context = emit_code_for_if_condition(context, conditional->condition);
            struct emit_location *if_true = emit_code_for_ast(context, conditional->if_true);
            
            b32 is_float = (if_true->register_kind_when_loaded == REGISTER_KIND_xmm);
            
            struct emit_location *dumb = null;
            if(should_memcpy){
                emit_store(context, temporary, if_true);
            }else if(is_float){
                dumb = emit_load_float_into_specific_register(context, if_true, reg);
            }else{
                dumb = emit_load_into_specific_gpr(context, if_true, reg);
            }
            
            // we have to free it here because we re assign it in the second call, this is kinda dumb
            if(dumb) free_emit_location(context, dumb);
            
            struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            jump_context_emit(&jump_over_else, COMP_none);
            
            emit_end_jumps(jump_context);
            struct emit_location *if_false = emit_code_for_ast(context, conditional->if_false);
            
            struct emit_location *loaded = null;
            if(should_memcpy){
                emit_store(context, temporary, if_false);
            }else if(is_float){
                loaded = emit_load_float_into_specific_register(context, if_false, reg);
            }else{
                loaded = emit_load_into_specific_gpr(context, if_false, reg);
            }
            
            if(loaded) free_emit_location(context, loaded);
            emit_end_jumps(jump_over_else);
            
            if(should_memcpy) return temporary;
            
            loaded = emit_location_loaded(context, register_kind, reg, type->size);
            
            if(type->kind == AST_struct || type->kind == AST_union){
                //
                // if its a register sized struct we need have a register relative reference to it.
                //
                
                struct emit_location *ret = emit_allocate_temporary_stack_location(context, REGISTER_KIND_gpr, type->size, type->alignment);
                emit_store(context, ret, loaded);
                return ret;
            }
            
            return loaded;
        }break;
        case AST_comma_expression:{
            struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
            struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
            if(lhs) free_emit_location(context, lhs);
            return emit_code_for_ast(context, op->rhs);
        }break;
        case AST_empty_statement:{
            return emit_location_invalid(context);
        }break;
        case AST_assignment:{
            struct ast_binary_op *assign = cast(struct ast_binary_op *)ast;
            
            if(assign->rhs->kind == AST_string_literal && assign->lhs->resolved_type->kind == AST_array_type){
                // 
                // for 'char asd[] = "asd";' emit a memcpy
                // There are three cases:
                //     char asd[10] = "asd"; // copy the string literal zero the upper part of the buffer.
                //     char asd[4]  = "asd"; // copy the string literal including the null-terminator
                //     char asd[3]  = "asd"; // copy the string literal excluding the null-terminator
                //     
                
                struct ast_array_type   *array = (struct ast_array_type *)assign->lhs->resolved_type;
                struct ast_string_literal *lit = (struct ast_string_literal *)assign->rhs;
                
                // Mark the string literal as being used.
                sll_push_back(context->string_literals, lit);
                context->string_literals.amount_of_strings += 1;
                
                smm array_size = array->amount_of_elements * array->element_type->size;
                if(array->is_of_unknown_size){
                    // We are in an initializer like:
                    // 
                    // struct s{
                    //     char array[];
                    // } arst = {"hello :)"};
                    // 
                    // We have made sure to allocate enough space to hold the initializer.
                    array_size = lit->value.size + array->element_type->size;
                }
                
                
                smm extra = (array_size == lit->value.size) ? 0 : array->element_type->size;
                
                struct emit_location *lhs = emit_code_for_ast(context, assign->lhs);
                if(array->is_of_unknown_size) lhs->size = array_size;
                
                if(array_size > lit->value.size + extra){
                    // @cleanup: We only would have to zero the upper part of the 'lhs'.
                    emit_memset(context, lhs, 0);
                }
                
                struct emit_location *rhs = emit_location_rip_relative(context, &lit->base, REGISTER_KIND_gpr, lit->value.size + extra);
                emit_memcpy(context, lhs, rhs);
                
                return lhs;
            }else if(assign->lhs->kind == AST_array_range){
                // 
                // GNU extension array range initializer:
                //     
                //     [1 ... 5] = expr,
                // 
                struct ast_array_range *array_range = (struct ast_array_range *)assign->lhs;
                
                // Get the rhs:
                struct emit_location *rhs = emit_code_for_ast(context, assign->rhs);
                if(rhs->state == EMIT_LOCATION_conditional) rhs = emit_load(context, rhs);
                
                emit_location_prevent_freeing(context, rhs);
                
                struct emit_location *lhs_base = emit_code_for_ast(context, array_range->lhs);
                assert(lhs_base->state == EMIT_LOCATION_register_relative);
                
                u64 element_size = array_range->base.resolved_type->size;
                lhs_base->offset += array_range->start_index * element_size;
                lhs_base->size = element_size;
                
                for(u64 index = array_range->start_index; index <= array_range->end_index; index++, lhs_base->offset += element_size){
                    emit_store(context, lhs_base, rhs);
                }
                
                emit_location_allow_freeing(context, rhs);
                free_emit_location(context, rhs);
                free_emit_location(context, lhs_base);
                
                return emit_location_invalid(context);
            }
            
            // 
            // @note: For assignments its more intuitive to first evaluate the right hand side, and then the left.
            // 
            struct emit_location *rhs = emit_code_for_ast(context, assign->rhs);
            if(rhs->state == EMIT_LOCATION_conditional) rhs = emit_load(context, rhs);
            struct emit_location *lhs = emit_code_for_ast(context, assign->lhs);
            
            if(assign->lhs->resolved_type->kind == AST_bitfield_type){
                struct ast_bitfield_type *bitfield = (struct ast_bitfield_type *)assign->lhs->resolved_type;
                return emit_store_bitfield(context, bitfield, lhs, rhs);
            }else if(assign->lhs->resolved_type->flags & TYPE_FLAG_is_atomic){
                return emit_store_atomic_integer(context, lhs, rhs);
            }
            
            emit_store(context, lhs, rhs);
            return lhs;
        }break;
        case AST_and_assignment:{
            return emit_compound_assignment(context, ast, REG_OPCODE_AND, AND_REGM8_REG8, AND_REGM_REG);
        }break;
        case AST_plus_assignment:{
            if(ast->resolved_type->kind == AST_float_type){
                return emit_compound_assignment_xmm(context, ast, ADD_XMM);
            }
            return emit_compound_assignment(context, ast, REG_OPCODE_ADD, ADD_REGM8_REG8, ADD_REGM_REG);
        }break;
        case AST_minus_assignment:{
            if(ast->resolved_type->kind == AST_float_type){
                return emit_compound_assignment_xmm(context, ast, SUB_XMM);
            }
            return emit_compound_assignment(context, ast, REG_OPCODE_SUB, SUB_REGM8_REG8, SUB_REGM_REG);
        }break;
        case AST_or_assignment:{
            return emit_compound_assignment(context, ast, REG_OPCODE_OR, OR_REGM8_REG8, OR_REGM_REG);
        }break;
        case AST_xor_assignment:{
            return emit_compound_assignment(context, ast, REG_OPCODE_XOR, XOR_REGM8_REG8, XOR_REGM_REG);
        }break;
        case AST_left_shift_assignment:{
            smm is_signed = type_is_signed(ast->resolved_type);
            u8 inst = is_signed ? REG_OPCODE_SHIFT_ARITHMETIC_LEFT : REG_OPCODE_SHIFT_LEFT;
            return emit_shift_or_rotate(context, ast, inst, true);
        }break;
        case AST_right_shift_assignment:{
            smm is_signed = type_is_signed(ast->resolved_type);
            u8 inst = is_signed ? REG_OPCODE_SHIFT_ARITHMETIC_RIGHT : REG_OPCODE_SHIFT_RIGHT;
            return emit_shift_or_rotate(context, ast, inst, true);
        }break;
        case AST_times_assignment:{
            if(ast->resolved_type->kind == AST_float_type){
                return emit_compound_assignment_xmm(context, ast, MUL_XMM);
            }else{
                return emit_divide_or_mod_or_multiply(context, ast, REG_OPCODE_IMUL_REGM_RAX, REG_OPCODE_MUL_REGM_RAX, true);
            }
        }break;
        case AST_modulo_assignment:
        case AST_divide_assignment:{
            if(ast->resolved_type->kind == AST_float_type){
                return emit_compound_assignment_xmm(context, ast, DIV_XMM);
            }else{
                return emit_divide_or_mod_or_multiply(context, ast, REG_OPCODE_IDIV_REGM_RAX, REG_OPCODE_DIV_REGM_RAX, true);
            }
        }break;
        case AST_return:{
            struct ast_return *ret = cast(struct ast_return *)ast;
            
            context->gpr_allocator.rolling_index = REGISTER_A;
            
            // @note: this is the same as 'ret->expr->resolved_type' whenever we have 'ret->expr', but we would
            //        only be able to assert 'types_are_equal' as the same types do not necessary live at the same
            //        address (e.g. pointer types). Hence, no assert :(
            struct ast_type *return_type = context->current_function->type->return_type;
            
            if(ret->expr){
                struct emit_location *loc = emit_code_for_ast(context, ret->expr);
                
                if(return_type == &globals.typedef_void){
                    // @note: We might have 'ret->expr' even if the return type is void.
                    //        But in this case we don't need to load it.
                    assert(loc == emit_location_invalid(context)); // We should have a void return-value, i.e nothing.
                }else if(type_is_returned_by_address(return_type)){
                    // :returning_structs :function_epilog
                    // load the address of what we want to copy into rsi, the actual copy will then happen in
                    // the epilog
                    
                    enum register_encoding rsi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_SI);
                    struct emit_location *loaded = emit_load_address(context, loc, rsi);
                    free_emit_location(context, loc);
                    free_emit_location(context, loaded);
                }else{
                    if(return_type->kind == AST_float_type){
                        struct emit_location *loaded = emit_load_float_into_specific_register(context, loc, REGISTER_XMM0);
                        free_emit_location(context, loaded);
                    }else if(return_type->flags & TYPE_FLAG_is_intrin_type){
                        loc->register_kind_when_loaded = REGISTER_KIND_xmm;
                        struct emit_location *loaded = emit_load_float_into_specific_register(context, loc, REGISTER_XMM0);
                        free_emit_location(context, loaded);
                    }else{
                        struct emit_location *loaded = emit_load_into_specific_gpr(context, loc, REGISTER_A);
                        free_emit_location(context, loaded);
                    }
                }
            }
            
            if(!context->should_not_emit_ret_jump){
                // :function_epilog
                // jump to the function epilog, this does not have to happen, when we are already at the end of a function
                jump_context_emit(context->jump_to_function_epilog, COMP_none);
            }
            
            return emit_location_invalid(context);
        }break;
        case AST_scope:{
            struct ast_scope *scope = cast(struct ast_scope *)ast;
            context->current_scope = scope;
            
            for(struct ast_list_node *it = scope->statement_list.first; it; it = it->next){
                context->temporary_stack_allocator = 0;
                
                // if this is the last statement in a function and it is a return, 
                // then we should not emit the jump to the epilog, as we are already there
                context->should_not_emit_ret_jump = !it->next && (scope->flags & SCOPE_FLAG_is_function_scope);
                
                struct emit_location *loc = emit_code_for_ast(context, it->value);
                if(loc) free_emit_location(context, loc);
                
                assert_that_no_registers_are_allocated(context);
            }
            
            scope->scope_end_byte_offset_in_function = to_u32(get_bytes_emitted(context));
            
            return emit_location_invalid(context);
        }break;
        case AST_if:{
            struct ast_if *ast_if = cast(struct ast_if *)ast;
            
            struct jump_context jump_context = emit_code_for_if_condition(context, ast_if->condition);
            struct emit_location *loc = emit_code_for_ast(context, ast_if->statement);
            if(loc) free_emit_location(context, loc);
            
            if(ast_if->else_statement){
                struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
                jump_context_emit(&jump_over_else, COMP_none);
                emit_end_jumps(jump_context);
                struct emit_location *else_statement = emit_code_for_ast(context, ast_if->else_statement);
                if(else_statement) free_emit_location(context, else_statement);
                
                emit_end_jumps(jump_over_else);
            }else{
                emit_end_jumps(jump_context);
            }
            
            return emit_location_invalid(context);
        }break;
        case AST_switch:{
            struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
            
            struct emit_location *switch_on = emit_code_for_ast(context, ast_switch->switch_on);
            
            // do switches at least in 32-bit
            switch_on = emit_load(context, switch_on);
            
            // *** switch ***
            // if(a == 1) goto loc_1;
            // if(a == 2) goto loc_2;
            // if(a == 3) goto loc_3;
            // ...
            // {switch->statement}
            
            for_ast_list(ast_switch->case_list){
                struct ast_case *ast_case = (struct ast_case *)it->value;
                // @speed could do this manually, we know its an immediate, also we know the size should be
                //        switch_on->size
                struct emit_location *imm = emit_location_immediate(context, ast_case->value, switch_on->size);
                assert(imm->state == EMIT_LOCATION_immediate);
                
                b32 is_signed = type_is_signed(ast_switch->switch_on->resolved_type);
                if(imm->size == 8 || ((imm->size == 4) && is_signed && imm->value > s32_max)){
                    struct emit_location *loaded = emit_load(context, imm);
                    
                    emit_register_register(context, no_prefix(), one_byte_opcode(CMP_REG_REGM), switch_on, loaded);
                    free_emit_location(context, loaded);
                }else{
                    
                    u8 inst = REG_EXTENDED_OPCODE_REGM_IMMIDIATE;
                    if(imm->size == 1){
                        if(is_signed || imm->value < s8_max){
                            inst = REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                        }else{
                            imm->size = switch_on->size;
                        }
                    }
                    emit_reg_extended_op(context, no_prefix(), one_byte_opcode(inst), REG_OPCODE_CMP, switch_on);
                    
                    if(imm->size == 1){
                        emit(imm->value);
                    }else{
                        switch(switch_on->size){
                            case 1: emit(imm->value); break;
                            case 2: emit_u16(imm->value); break;
                            case 4: emit_u32(imm->value); break;
                            case 8: emit_u32(imm->value); break;
                            invalid_default_case();
                        }
                    }
                    
                }
                
                // @cleanup: is this the right arena?
                struct jump_context *jump = push_struct(context->arena, struct jump_context);
                *jump = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
                jump_context_emit(jump, COMP_equals);
                ast_case->jump = jump;
            }
            free_emit_location(context, switch_on);
            
            struct jump_context *old_break_jump_context = context->break_jump_context;
            struct jump_context break_jumps = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            context->break_jump_context = &break_jumps;
            
            if(ast_switch->default_case){
                // @cleanup: is this the right arena?
                struct jump_context *jump = push_struct(context->arena, struct jump_context);
                *jump = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
                jump_context_emit(jump, COMP_none);
                ast_switch->default_case->jump = jump;
            }else{
                jump_context_emit(&break_jumps, COMP_none);
            }
            
            struct emit_location *loc = emit_code_for_ast(context, ast_switch->statement);
            if(loc) free_emit_location(context, loc);
            
            emit_end_jumps(break_jumps);
            context->break_jump_context = old_break_jump_context;
            
            return emit_location_invalid(context);
        }break;
        case AST_case:{
            while(true){
                struct ast_case *ast_case = cast(struct ast_case *)ast;
                assert(ast_case->jump);
                emit_end_jumps(*ast_case->jump);
                
                if(ast_case->statement){ 
                    // Chain all case statements and do not recurse for them.
                    // This avoids stack overflows and is faster.
                    if(ast_case->statement->kind == AST_case){
                        ast = ast_case->statement;
                        ast->byte_offset_in_function = to_s32(get_bytes_emitted(context));
                        continue;
                    }
                    
                    struct emit_location *loc = emit_code_for_ast(context, ast_case->statement);
                    if(loc) free_emit_location(context, loc);
                }
                
                break;
            }
            return emit_location_invalid(context);
        }break;
        case AST_do_while:
        case AST_for:{
            // :AST_while
            // @note: we desugar while(cond) into for(;cond;)
            
            // :AST_do_while
            // @note: we use struct ast_for for do_while as well, but give it kind AST_do_while.
            //        We do this as the code is the same modulo one initial jump.
            
            struct ast_for *ast_for = cast(struct ast_for *)ast;
            
            ast_for->scope_for_decl->base.byte_offset_in_function = ast_for->base.byte_offset_in_function;
            
            // save old values, as sort of a stack
            struct jump_context *old_break_jump_context    = context->break_jump_context;
            struct jump_context *old_continue_jump_context = context->continue_jump_context;
            
            struct jump_context break_jumps    = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            struct jump_context continue_jumps = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            context->break_jump_context = &break_jumps;
            context->continue_jump_context = &continue_jumps;
            
            // *** This is what a for(decl; cond; inc) body compiles to ***
            // decl;
            // loop:
            //    if(cond){
            //        body;
            //        continue:
            //        inc;
            //        goto loop;
            //     }
            // break:
            
            // :AST_do_while, if ast->kind == AST_do_while we emit an initial jump to body.
            struct jump_context init_jump_for_do_while = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            if(ast->kind == AST_do_while){
                jump_context_emit(&init_jump_for_do_while, COMP_none);
            }
            
            if(ast_for->decl){
                struct emit_location *loc = emit_code_for_ast(context, ast_for->decl);
                if(loc) free_emit_location(context, loc);
            }
            
            // loop:
            smm jump_back_location = get_bytes_emitted(context);
            // if (!cond) goto break;
            // @cleanup: ast_for->condition can be null, but maybe we do that in parse
            struct jump_context end_loop_context = emit_code_for_if_condition(context, ast_for->condition);
            {
                if(ast->kind == AST_do_while) emit_end_jumps(init_jump_for_do_while);
                
                // body;
                struct emit_location *body = emit_code_for_ast(context, ast_for->body);
                if(body) free_emit_location(context, body);
                
                // continue:
                emit_end_jumps(continue_jumps);
                
                // inc;
                if(ast_for->increment){
                    struct emit_location *inc = emit_code_for_ast(context, ast_for->increment);
                    if(inc) free_emit_location(context, inc);
                }
                
                // goto loop:
                emit_jump(context, jump_back_location, COMP_none, JUMP_CONTEXT_jump_on_true);
            }
            
            // break:
            emit_end_jumps(end_loop_context);
            emit_end_jumps(break_jumps);
            
            // restore old values
            context->break_jump_context = old_break_jump_context;
            context->continue_jump_context = old_continue_jump_context;
            
            ast_for->scope_for_decl->scope_end_byte_offset_in_function = to_u32(get_bytes_emitted(context));
            
            return emit_location_invalid(context);
        }break;
        case AST_break:{
            //struct ast_break *ast_break = cast(struct ast_break *)ast;
            jump_context_emit(context->break_jump_context, COMP_none);
            return emit_location_invalid(context);
        }break;
        case AST_continue:{
            //struct ast_continue *ast_continue = cast(struct ast_continue *)ast;
            jump_context_emit(context->continue_jump_context, COMP_none);
            return emit_location_invalid(context);
        }break;
        case AST_goto:{
            struct ast_goto *ast_goto = cast(struct ast_goto *)ast;
            
            // @copy from jump context emit
            struct jump_node *node = push_struct(context->arena, struct jump_node);
            emit(JUMP_REL32);
            node->patch_location = context->emit_pool.current;
            node->jump_from = emit_bytes(context, 4, 0) + 4;
            
            ast_goto->jump_node = node;
            return emit_location_invalid(context);
        }break;
        case AST_label:{
            struct ast_label *label = (struct ast_label *)ast;
            label->byte_offset_in_function = to_u32(get_bytes_emitted(context));
            
            if(label->statement){ 
                struct emit_location *loc = emit_code_for_ast(context, label->statement);
                if(loc) free_emit_location(context, loc);
            }
            
            return emit_location_invalid(context);
        }break;
        case AST_function_call:{
            struct ast_function_call *call = cast(struct ast_function_call *)ast;
            
            // @note: This case handles both a call to a function as well as a call to a function-pointer.
            //        We detect this as whether or not the 'call->identifier_expression' is a function or a pointer.
            //        In the function case we don't have to emit any code here and just set 'identifier_to_call'.
            //        Otherwise we 'emit_code_for_ast' on the 'call->identifier_expression' and store it in
            //        'function_pointer_location'.                                        13.06.2021
            
            struct ast_function_type *function_type;
            
            struct emit_location  *function_pointer_location;
            struct ast_identifier *identifier_to_call;
            
            // This is the declaration which acts as the source for the call. 
            struct ast_function *patch_call_source_declaration = null; 
            
            if(call->identifier_expression->resolved_type->kind == AST_function_type){
                identifier_to_call = (struct ast_identifier *)call->identifier_expression;
                function_type = (struct ast_function_type *)identifier_to_call->base.resolved_type;
                
                while(identifier_to_call->base.kind == AST_comma_expression){
                    // @cleanup: This sort of sucks.
                    //           Maybe we should spitt the function pointer call case 
                    //           from the function call case.
                    struct ast_binary_op *comma = (struct ast_binary_op *)identifier_to_call;
                    struct emit_location *ignored = emit_code_for_ast(context, comma->lhs);
                    if(ignored) free_emit_location(context, ignored); 
                    
                    identifier_to_call = (struct ast_identifier *)comma->rhs;
                }
                
                assert(identifier_to_call->base.kind == AST_identifier);
                
                if(!patch_call_source_declaration) patch_call_source_declaration = (struct ast_function *)identifier_to_call->decl;
                
                function_pointer_location = null;
            }else{
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)call->identifier_expression->resolved_type;
                assert(pointer->pointer_to->kind == AST_function_type);
                
                function_type = cast(struct ast_function_type *)pointer->pointer_to;
                
                identifier_to_call = null;
                function_pointer_location = emit_code_for_ast(context, call->identifier_expression);
            }
            
            if(function_type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic){
                // @note: no need to spill registers for intrinsics
                return emit_intrinsic(context, call);
            }
            
            // spill all volitile registers, so they are saved
            spill_all_allocated_volatile_registers(context);
            
            smm function_argument_count = function_type->argument_list.count;
            smm arg_count = call->call_arguments.count;
            
            b32 function_is_inline_asm = patch_call_source_declaration && (patch_call_source_declaration->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm);
            
            // 
            // :returning_structs
            // 
            // If the function returns a big struct, there is an implicit first argument, which is the
            // address of the return value. We memcpy in 'case AST_return'.
            b32 returns_big_struct = type_is_returned_by_address(function_type->return_type);
            if(returns_big_struct){
                arg_count += 1;
                function_argument_count += 1;
            }
            
            context->max_amount_of_function_call_arguments = max_of(context->max_amount_of_function_call_arguments, arg_count);
            // @cleanup: draw a picture of the stack
            
            // :returning_structs
            struct emit_location *stack_return_location = null;
            
            enum register_encoding argument_registers[REGISTER_KIND_count][4] = {
                [REGISTER_KIND_gpr][0] = REGISTER_C,
                [REGISTER_KIND_gpr][1] = REGISTER_D,
                [REGISTER_KIND_gpr][2] = REGISTER_R8,
                [REGISTER_KIND_gpr][3] = REGISTER_R9,
                
                [REGISTER_KIND_xmm][0] = REGISTER_XMM0,
                [REGISTER_KIND_xmm][1] = REGISTER_XMM1,
                [REGISTER_KIND_xmm][2] = REGISTER_XMM2,
                [REGISTER_KIND_xmm][3] = REGISTER_XMM3,
            };
            
            
            struct emit_location **emit_locations = push_uninitialized_data(&context->scratch, struct emit_location *, arg_count);
            // @cleanup: we need this below... we could _re-iterate_ but dunno
            smm *argument_sizes = push_uninitialized_data(&context->scratch, smm, arg_count);
            
            // first 4 arguments are passed in rcx rdx r8 r9 in this order from left to right
            // all other arguments are passed on the stack
            {
                smm arg_at = 0;
                
                // :returning_structs
                if(returns_big_struct){
                    struct ast_type *return_type = function_type->return_type;
                    stack_return_location = emit_allocate_temporary_stack_location(context, REGISTER_KIND_gpr, return_type->size, return_type->alignment);
                    
                    emit_locations[arg_at] = emit_load_address(context, stack_return_location, allocate_register(context, REGISTER_KIND_gpr));
                    arg_at++;
                }
                
                struct ast_list_node *it = call->call_arguments.first;
                struct ast_list_node *type_it = function_type->argument_list.first;
                
                for( ;it; arg_at++, it = it->next){
                    if(arg_at < array_count(*argument_registers)){
                        context->gpr_allocator.rolling_index = argument_registers[REGISTER_KIND_gpr][arg_at];
                        context->xmm_allocator.rolling_index = argument_registers[REGISTER_KIND_xmm][arg_at];
                    }
                    
                    struct emit_location *loc = emit_code_for_ast(context, it->value);
                    if(loc->state == EMIT_LOCATION_conditional) loc = emit_load(context, loc);
                    
                    struct ast_type *type = null;
                    if(!type_it){
                        assert(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs);
                        assert(arg_at >= function_argument_count);
                        type = it->value->resolved_type;
                        
                        // @note: Only convert floats and doubles, not intrinsic arguments.
                        if(loc->size < 16 && loc->register_kind_when_loaded == REGISTER_KIND_xmm){
                            assert(loc->size == 4 || loc->size == 8);
                            
                            // If we are a floating point argument in a vararg function, 
                            // which is in the '...' arglist we need to transform the argument 
                            // into a gpr using a movq.
                            
                            struct emit_location *float_reg = emit_load_float(context, loc);
                            
                            enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
                            struct emit_location *gpr_reg = emit_location_loaded(context, REGISTER_KIND_gpr, reg, 8);
                            
                            // @cleanup: holy fuck, I really don't understand the prefix convention here...
                            // prefix to load the _lower_ half (mm|xmm) and we want xmm I guess..
                            emit_register_op__internal(context, create_prefixes(ASM_PREFIX_NON_PACKED_OP_double), two_byte_opcode(MOVQ_REGM_XMM), float_reg->loaded_register, gpr_reg->loaded_register, 8);
                            free_emit_location(context, float_reg);
                            
                            loc = gpr_reg;
                        }
                    }else{
                        assert(type_it->value->kind == AST_declaration);
                        type = (cast(struct ast_declaration*)type_it->value)->type;
                        type_it = type_it->next;
                    }
                    
                    smm size = it->value->resolved_type->size;
                    assert(size == type->size);
                    
                    if(size_is_big_or_oddly_sized(type->size)){ // :PassingStructArguments
                        
                        if(function_is_inline_asm && (type->flags & TYPE_FLAG_is_intrin_type)){
                            //
                            // If its a type like '__m128' and we are calling an 'inline_asm' function, 
                            // we want to load the argument into an *mm register.
                            //
                            loc->register_kind_when_loaded = REGISTER_KIND_xmm; // @cleanup: maybe this should be set when we find the declaration, we could check the TYPE_FLAG_is_intrin_type
                            loc = emit_load_float(context, loc);
                        }else{
                            //
                            // big struct arguments are copied by the caller and passed on the stack
                            //
                            assert(type->kind == AST_struct || type->kind == AST_union);
                            
                            struct emit_location *copy_into = emit_allocate_temporary_stack_location(context, REGISTER_KIND_gpr, type->size, type->alignment);
                            
                            if(loc->state == EMIT_LOCATION_loaded){
                                // this can happen, if we have a simd intrinsic as an argument, e.g
                                //     do_something(_mm_set_pd(1.0, 2.0));
                                // I am not 100% sure whats the calling convention is here but for now I am
                                // gonna just spill it and be done with it
                                assert(type->flags & TYPE_FLAG_is_intrin_type);
                                assert(loc->register_kind_when_loaded == REGISTER_KIND_xmm);
                                
                                copy_into->register_kind_when_loaded = REGISTER_KIND_xmm;
                                emit_store(context, copy_into, loc);
                            }else{
                                emit_memcpy(context, copy_into, loc);
                                free_emit_location(context, loc); // @hmm: emit_store frees, emit_memcpy does not
                            }
                            
                            loc = emit_load_address(context, copy_into, allocate_register(context, REGISTER_KIND_gpr));
                            size = 8;
                        }
                    }
                    
                    assert(loc->size == size);
                    emit_locations[arg_at] = loc;
                    argument_sizes[arg_at] = size;
                }
            }
            
            
            if(function_is_inline_asm){
                //
                // Here we have emit all emit locations into 'emit_locations' and did all the promotions.
                // But, importantly we have not loaded all the emit locations!
                // If we are in an '__declspec(inline_asm)' function, we want to just use these locations 
                // as the arguments. 
                // In this way they are the integer literals stay integer literals while all types are
                // as you would expect. 
                //                                                                   28.11.2021
                
                //
                // :inline_asm_argument_substitution
                //
                {
                    sll_clear(context->inline_asm_function_arguments);
                    
                    int argument_index = 0;
                    for_ast_list(patch_call_source_declaration->type->argument_list){
                        struct ast_declaration *decl = (struct ast_declaration *)it->value;
                        
                        struct inline_asm_function_argument *inline_asm_argument = push_struct(&context->scratch, struct inline_asm_function_argument);
                        inline_asm_argument->declaration = decl;
                        
                        if(emit_locations[argument_index]->state == EMIT_LOCATION_immediate){
                            inline_asm_argument->integer_location = emit_locations[argument_index];
                        }
                        emit_locations[argument_index] = emit_load(context, emit_locations[argument_index]);
                        inline_asm_argument->loaded_location = emit_locations[argument_index];
                        
                        sll_push_back(context->inline_asm_function_arguments, inline_asm_argument);
                        
                        emit_location_prevent_freeing(context, emit_locations[argument_index]);
                        
                        argument_index++;
                    }
                }
                
                //
                // Carefully get the asm_block
                //
                // @note: we use the 'patch_call_source_declaration' here, as the other one might not be defined.
                struct ast_function *function = (struct ast_function *)patch_call_source_declaration;
                assert(function->scope->kind == AST_scope);
                struct ast_scope *scope = (struct ast_scope *)function->scope;
                assert(scope->statement_list.count == 1);
                assert(scope->statement_list.first->value->kind == AST_asm_block);
                
                struct ast_asm_block *asm_block = (struct ast_asm_block *)scope->statement_list.first->value;
                context->in_inline_asm_function = call->identifier_expression->token;
                
                emit_inline_asm_block(context, asm_block);
                
                // free all 'argument_locations'
                for(u32 i = 0; i < arg_count; i++){
                    emit_location_allow_freeing(context, emit_locations[i]);
                    
                    if(emit_locations[i] != context->asm_block_return){
                        free_emit_location(context, emit_locations[i]);
                    }
                }
                
                context->in_inline_asm_function = null;
                context->inline_asm_mode = null;
                
                if(function_type->return_type != &globals.typedef_void){
                    assert(context->asm_block_return);
                    
                    struct emit_location *ret = context->asm_block_return;
                    context->asm_block_return = null;
                    assert(ret->state == EMIT_LOCATION_loaded);
                    
                    // :asm_block_the_same_register_with_different_sizes
                    //
                    // As we sometimes _over-allocate_ the registers which were
                    // 'inline_asm__was_used_by_user', we have to make sure
                    // that the return is actually _allocated_.
                    context->register_allocators[ret->register_kind_when_loaded].emit_location_map[ret->loaded_register] = ret;
                    ret->inline_asm__was_used_by_user = false;
                    
                    struct ast_type *return_type = function_type->return_type;
                    if(return_type->kind == AST_float_type){
                        // we allow 'return xmm0' for floats, therefor we have to fix up the size here.
                        assert(ret->register_kind_when_loaded == REGISTER_KIND_xmm);
                        ret->size = return_type->size;
                    }
                    return ret;
                }else{
                    return emit_location_invalid(context);
                }
            }
            
            {   // store all 'emit_locations' that are not passed in registers onto the stack
                smm stack_pass_location = 0x20;
                for(u32 arg_at = array_count(*argument_registers); arg_at < arg_count; arg_at++){
                    // @note: we cannot _just_ store these here, as we would overwrite these
                    //        if e.g. the last argument is a function with a lot of arguments
                    struct emit_location *loc = emit_locations[arg_at];
                    smm size = argument_sizes[arg_at];
                    
                    struct emit_location *store_in = emit_location_register_relative(context, loc->register_kind_when_loaded, context->register_sp, context->register_sp, stack_pass_location, size);
                    emit_store(context, store_in, loc);
                    // :MSVC_function_call_stack_increase
                    stack_pass_location += 8;
                }
            }
            
            // load all the 'register_locations' into the 'argument_registers'
            // and lock them so they do not get spilled by the later 'argument_registers'
            for(u32 i = 0; i < array_count(*argument_registers) && i < arg_count; i++){
                
                enum register_kind register_kind = emit_locations[i]->register_kind_when_loaded;
                
                if(emit_locations[i]->state == EMIT_LOCATION_loaded){
                    if(emit_locations[i]->loaded_register == argument_registers[register_kind][i]){
                        emit_location_prevent_spilling(context, emit_locations[i]);
                        continue;
                    }
                }
                
                enum register_encoding arg_reg = allocate_specific_register(context, register_kind, argument_registers[register_kind][i]);
                
                // @cleanup: maybe do an emit_load_into_specific_register, that takes a register kind?
                if(register_kind == REGISTER_KIND_gpr){
                    emit_locations[i] = emit_load_into_specific_gpr(context, emit_locations[i], arg_reg);
                }else{
                    assert(emit_locations[i]->register_kind_when_loaded == REGISTER_KIND_xmm);
                    emit_locations[i] = emit_load_float_into_specific_register(context, emit_locations[i], arg_reg);
                }
                
                emit_location_prevent_spilling(context, emit_locations[i]);
            }
            
            // :patches_are_32_bit
            // @note: if we are here that means that this procedure is not yet emitted. Therefore it has to be a procedure we emit for. now we will only allow .text sections to be at most ((2 << 31) - 1) big, so that we can use relative calls everywhere. Thus we know that this is a realive call. -15.19.19
            // @note: if we want an executable, all patches calls to known locations are in fact 32 bit.
            //        and we have to patch them all as we later copy the code into the .text section -16.10.19
            // @note now that we are emiting into a list now, so we have to move the memory in the end so we have to patch in every case -2.12.19
            
            if(identifier_to_call){
                
                struct ast_function *function = (struct ast_function *)identifier_to_call->decl;
                assert(function->base.kind == AST_function);
                
                if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
                    // :dllimport_loading
                    struct emit_location *function_location = emit_location_rip_relative(context, &identifier_to_call->decl->base, REGISTER_KIND_gpr, 8);
                    emit_register_relative_extended(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_FF), FF_CALL_REGM, function_location);
                }else{
                    emit(CALL_RELATIVE);
                    smm patch_offset = emit_bytes(context, sizeof(s32), 0);
                    
                    // @note: We have to lookup the identifier either here or in patching, 
                    //        I think if makes more sense to do it here, than in patching,
                    //        because string literals and static variables do not want that lookup. 
                    //                                                        -3.1.2020
                    
                    emit_patch(context, PATCH_rip_relative, &patch_call_source_declaration->base, 0, &context->current_function->as_decl, patch_offset, patch_offset + 4);
                }
            }else{
                assert(function_pointer_location);
                
                switch(function_pointer_location->state){
                    case EMIT_LOCATION_register_relative:{
                        emit_register_relative_extended(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_FF), FF_CALL_REGM, function_pointer_location);
                    }break;
                    case EMIT_LOCATION_loaded:{
                        emit_reg_extended_op(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_FF), FF_CALL_REGM, function_pointer_location);
                    }break;
                    invalid_default_case();
                }
                free_emit_location(context, function_pointer_location);
            }
            
            // free all 'argument_locations'
            for(u32 i = 0; i < array_count(*argument_registers) && i < arg_count; i++){
                emit_location_allow_spilling(context, emit_locations[i]);
                free_emit_location(context, emit_locations[i]);
            }
            
            if(function_type->return_type == &globals.typedef_void){
                return emit_location_invalid(context);
            }
            
            
            struct ast_type *return_type = function_type->return_type;
            
            
            // :returning_structs
            if(stack_return_location){
                assert(!(return_type->flags & TYPE_FLAG_is_intrin_type));
                assert(type_is_returned_by_address(return_type));
                return stack_return_location;
            }
            
            if(return_type->kind == AST_float_type || (return_type->flags & TYPE_FLAG_is_intrin_type)){
                return emit_location_loaded(context, REGISTER_KIND_xmm, REGISTER_XMM0, return_type->size);
            }
            
            // if we are a compound type or whatever we have return a 'register_relative' emit location, so we
            // spill rax to the stack.
            if(return_type->kind == AST_struct || return_type->kind == AST_union || return_type->kind == AST_array_type){
                // handles like 'struct{u64 a;}' which are returned in 'rax'
                
                assert(!type_is_returned_by_address(return_type)); // otherwise it should have allready been handled
                struct emit_location *ret = emit_allocate_temporary_stack_location(context, REGISTER_KIND_gpr, return_type->size, return_type->alignment);
                struct emit_location *rax = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_A, return_type->size);
                emit_store(context, ret, rax);
                return ret;
            }
            
            assert(return_type->kind == AST_integer_type || return_type->kind == AST_pointer_type);
            return emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_A, return_type->size);
        }break;
        case AST_cast:{
            struct ast_unary_op *cast = cast(struct ast_unary_op *)ast;
            
            struct ast *cast_what = cast->operand;
            struct emit_location *loc = emit_code_for_ast(context, cast_what);
            struct ast_type *cast_to = cast->base.resolved_type;
            
            if(cast_to->kind == AST_bitfield_type){
                // 'int a : 3 = 123;' we can only get here, if we inserted an implicit assignment cast.
                // just cast to the base type and let the 'AST_assignment'-case take care of the rest.
                struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)cast_to;
                cast_to = bitfield->base_type;
            }
            
            if(cast_what->resolved_type == cast_to){
                return loc;
            }
            
            if(cast_to == &globals.typedef_void){
                // dont free if (void)(void) a // @cleanup: is this still necessary after the equality check above?
                if(loc) free_emit_location(context, loc);
                return emit_location_invalid(context);
            }
            
            if(cast_what->resolved_type->kind == AST_bitfield_type){
                // this is where we load bitfields
                return emit_load_bitfield(context, loc, (struct ast_bitfield_type *)cast_what->resolved_type);
            }
            
            if(cast_what->resolved_type->kind == AST_atomic_integer_type){
                // @note: It seems to me, that on x64 atomic-loads can simply be implemented as a mov.
                return emit_load(context, loc);
            }
            
            if(cast_to == &globals.typedef_Bool){
                loc = emit_load(context, loc);
                loc = emit_compare_to_zero(context, loc);
                loc->size = 1; // load the thing as size 1
                return emit_load(context, loc);
            }
            
            assert(type_is_arithmetic(cast_to) || cast_to->kind == AST_pointer_type);
            assert(type_is_arithmetic(cast_what->resolved_type) || cast_what->resolved_type->kind == AST_pointer_type);
            
            if(loc->state != EMIT_LOCATION_register_relative){
                loc = emit_load(context, loc);
            }
            
            if(loc->register_kind_when_loaded == REGISTER_KIND_xmm && cast_to->kind == AST_float_type){
                assert(cast_what->resolved_type->kind == AST_float_type);
                //
                // Cast f32 -> f64 or f64 -> f32
                //
                
                // @cleanup: is this the wrong way around, which argument is this prefix for?
                struct prefixes sse_prefix = get_sse_prefix_for_scalar(loc->size);
                
                // cvtsd2ss - convert_scalar_double_to_scalar_single
                // cvtss2sd - convert_scalar_single_to_scalar_double
                if(loc->state == EMIT_LOCATION_register_relative){
                    struct emit_location *loaded = emit_location_loaded(context, REGISTER_KIND_xmm, allocate_register(context, REGISTER_KIND_xmm), cast_to->size);
                    emit_register_relative_register(context, sse_prefix, two_byte_opcode(0x5A), loaded->loaded_register, loc);
                    free_emit_location(context, loc);
                    loc = loaded;
                }else{
                    // The last argument of this (4) only decides whether or not we emit a REXW prefix. This instruction does not need REXW.
                    // So we pass 4, which does not emit any prefixes!
                    emit_register_op__internal(context, sse_prefix, two_byte_opcode(0x5A), loc->loaded_register, loc->loaded_register, 4);
                }
                loc->size = cast_to->size;
                return loc;
            }
            
            if(loc->register_kind_when_loaded == REGISTER_KIND_xmm){
                assert(loc->size == 4 || loc->size == 8);
                
                // 
                // We want to cast some float-type to some integer-type.
                // 
                // We use the instructions 'cvttsd2si' or 'cvttss2si' based on the 
                // the floating point type. These instructions convert to *signed* integer type.
                // Hence, we have to do something special here for unsigned types.
                // 
                // If the destination is a 'u8' or 'u16',  we can simply use the 32-bit version and then truncate.
                // If the destination is a 'u32', we can simply use the 64-bit version and then truncate.
                // If the destination is a 'u64', we have to check first if the value in the source is large.
                // 
                // gcc and msvc compare emit something like this:
                //     movsd  xmm0, doubled              ; Load the initial value into xmm0.
                //     comisd xmm0, qword ptr[rip + big] ; Compare against a big number 9.22337e+19 (0x8000000000000000).
                //     jb simple_conversion              ; We are less then the first value which overflows a s64.
                //     
                //     sub xmm0, qword ptr[rip + big]    ; We exceed the big value, subtract it.
                //     cvttsd2si rax, xmm0               ; Then convert, now the result should be (unless overflow in the correct range).
                //     mov rcx, 0x8000000000000000       ; Then finally, add the 0x8000000000000000 back into rax.
                //     add rax, rcx
                //     jmp end
                //     
                // simple_conversion:
                //     cvttsd2si rax, xmm0               ; We know it is in range, simply convert.
                //     
                // end:
                //     
                
                smm size = (cast_to->size < 4) ? 4 : cast_to->size;
                if(cast_to == &globals.typedef_u32) size = 8;
                
                if(cast_to == &globals.typedef_u64){
                    //
                    // This is the hard case. @incomplete:
                    //
                }
                
                struct emit_location *loaded = emit_location_loaded(context, REGISTER_KIND_gpr, allocate_register(context, REGISTER_KIND_gpr), cast_to->size);
                
                struct prefixes sse_prefix = get_sse_prefix_for_scalar(loc->size);
                if(loc->state == EMIT_LOCATION_register_relative){
                    loc->size = size; // make sure we emit rexw if we should
                    emit_register_relative_register(context, sse_prefix, two_byte_opcode(0x2c), loaded->loaded_register, loc);
                }else{
                    emit_register_op__internal(context, sse_prefix, two_byte_opcode(0x2c), loaded->loaded_register, loc->loaded_register, size);
                }
                free_emit_location(context, loc);
                return loaded;
            }
            
            assert(loc->register_kind_when_loaded == REGISTER_KIND_gpr);
            b32 source_is_signed = type_is_signed(cast_what->resolved_type);
            
            if(cast_to->kind != AST_float_type && loc->size >= cast_to->size){
                // If it is an integer to integer cast and the cast_to->size fits just truncate and return
                // this works because of :little_endian
                loc->size = cast_to->size;
                return loc;
            }
            
            if(cast_to->kind != AST_float_type || loc->size < 4){
                // If its an integer to integer cast ints a zero / sign extension.
                // Thus load the thing into a bigger registers.
                // If its a float to integer conversion and the integer is small also load value into a bigger register,
                // because the float to int instructions only take s32 or s64.
                
                smm size_to_load_into = cast_to->size;
                if(loc->register_kind_when_loaded != REGISTER_KIND_gpr) size_to_load_into = 4;
                
                struct opcode opcode = get_opcode_for_move_instruction_and_adjust_size(loc, size_to_load_into, source_is_signed);
                
                if(loc->state == EMIT_LOCATION_register_relative){
                    enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
                    emit_register_relative_register(context, no_prefix(), opcode, reg, loc);
                    free_emit_location(context, loc);
                    loc = emit_location_loaded(context, REGISTER_KIND_gpr, reg, size_to_load_into);
                }else{
                    emit_register_register(context, no_prefix(), opcode, loc, loc);
                    loc->size = size_to_load_into;
                }
            }
            
            if(cast_to->kind == AST_float_type){
                assert(loc->register_kind_when_loaded == REGISTER_KIND_gpr);
                assert(loc->size == 4 || loc->size == 8);
                assert(cast_what->resolved_type->kind == AST_integer_type); // pointers are disallowed!
                
                // Casting from int to float:
                //
                // The conversion instructions 'cvtsi2ss' and 'cvtsi2sd' treat the source operand as signed.
                // This means signed -> float/double is easy, the unsigned int case is more complicated.
                // For 'u32' we can _extend_ the value into a 64-bit register and then use the 64 bit variant.
                // For 'u64' it gets complicated (see the code below).
                
                enum register_encoding reg = allocate_register(context, REGISTER_KIND_xmm);
                struct emit_location *ret = emit_location_loaded(context, REGISTER_KIND_xmm, reg, cast_to->size);
                
                enum legacy_prefixes prefix = (cast_to == &globals.typedef_f32) ? ASM_PREFIX_SSE_float : ASM_PREFIX_SSE_double;
                
                if(cast_what->resolved_type == &globals.typedef_u64){
                    assert(!source_is_signed && loc->size == 8);
                    // 
                    // This is the hard case. 
                    // We have to shift the u64 down by one and then add it to itself, while keeping rounding correct.
                    // 
                    
                    // The code msvc and gcc emit looks something like this:
                    //    mov      rax,  u64  ; load the initial value into rax
                    //    test     rax,  rax  ; check if the top bit is set and set SF <- MSB(rax)
                    //    js       msb_set    ; jump if 'SF', i.e MSB(rax) is set, to the slow part
                    //    
                    //    cvtsi2sd xmm1, rax  ; Convert the value if MSB(rax) is not set
                    //    jmp      end        ; jump to end as we are done
                    // 
                    // msb_set:
                    //    mov      rcx,  rax  ; save the u64 so we can fix up rounding
                    //    shr      rax,  1    ; shift the u64, such that the sign bit is not set
                    //    and      rcx,  1    ; get the parity from the original u64
                    //    or       rax,  rcx  ; fix up the last bit to be set if either of the last two bits of the original value are set
                    //    cvtsi2sd xmm1, rax  ; Convert the resulting value
                    //    addsd    xmm1, xmm1 ; Double it to get back to the desired value
                    //    
                    // end:
                    
                    // load the value and lock it, so we can copy it
                    loc = emit_load(context, loc);
                    emit_location_prevent_spilling(context, loc);
                    
                    // test the value
                    emit_register_register(context, no_prefix(), one_byte_opcode(TEST_REGM_REG), loc, loc);
                    
                    struct jump_context msb_set_jump = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
                    jump_context_emit(&msb_set_jump, COMP_negative);
                    
                    // we are good, we have passed the test, MSB is not set. just convert the value!
                    emit_register_op__internal(context, create_prefixes(prefix), two_byte_opcode(0x2A), ret->loaded_register, loc->loaded_register, 8);
                    
                    struct jump_context jump_to_end = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
                    jump_context_emit(&jump_to_end, COMP_none);
                    
                    // msb_set:
                    emit_end_jumps(msb_set_jump);
                    
                    // copy the value
                    enum register_encoding saved_gpr = allocate_register(context, REGISTER_KIND_gpr);
                    struct emit_location *saved = emit_load_into_specific_gpr(context, loc, saved_gpr);
                    
                    // shift the value to the right by 1.
                    emit_reg_extended_op(context, no_prefix(), one_byte_opcode(SHIFT_OR_ROTATE_REGM_1), REG_OPCODE_SHIFT_RIGHT, loc);
                    
                    // extract the last bit of the saved value by and'ing it with 1.
                    emit_reg_extended_op(context, no_prefix(), one_byte_opcode(REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8), REG_OPCODE_AND, saved);
                    emit(1);
                    
                    // or the the last bit into the shifted value
                    emit_register_register(context, no_prefix(), one_byte_opcode(OR_REG_REGM), loc, saved);
                    
                    // convert the value
                    emit_register_op__internal(context, create_prefixes(prefix), two_byte_opcode(0x2A), ret->loaded_register, loc->loaded_register, 8);
                    
                    // double the value
                    emit_register_register(context, create_prefixes(prefix), two_byte_opcode(ADD_XMM), ret, ret);
                    
                    // end:
                    emit_end_jumps(jump_to_end);
                    
                    free_emit_location(context, saved);
                    emit_location_allow_spilling(context, loc);
                    free_emit_location(context, loc);
                    return ret;
                }
                
                if(cast_what->resolved_type == &globals.typedef_u32){
                    assert(!source_is_signed && loc->size == 4);
                    // if the source type is 
                    // we have to extend 'loc' into a full register, just in case it was casted from 64 bit.
                    enum register_encoding loaded_gpr = allocate_register(context, REGISTER_KIND_gpr);
                    struct emit_location *loaded = emit_load_into_specific_gpr(context, loc, loaded_gpr);
                    loaded->size = 8;
                    loc = loaded;
                }
                
                if(loc->state == EMIT_LOCATION_loaded){
                    emit_register_op__internal(context, create_prefixes(prefix), two_byte_opcode(0x2A), ret->loaded_register, loc->loaded_register, loc->size);
                }else{
                    assert(loc->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, create_prefixes(prefix), two_byte_opcode(0x2A), ret->loaded_register, loc);
                }
                
                free_emit_location(context, loc);
                return ret;
            }
            
            // else 'cast_to' and 'operand' are of integer type, so we are just done as we extended it above
            assert(loc->size  == cast_to->size);
            assert(loc->state == EMIT_LOCATION_loaded);
            return loc;
        }break;
        case AST_asm_block:{
            emit_inline_asm_block(context, (struct ast_asm_block *)ast);
            
            // @cleanup: is this neccessary?
            for(enum register_kind a = 0; a < REGISTER_KIND_count; a++){
                for(u32 i = 0; i < array_count(context->register_allocators[a].emit_location_map); i++){
                    context->register_allocators[a].emit_location_map[i] = null;
                }
            }
            
            context->inline_asm_mode = null;
            return emit_location_invalid(context);
        }break;
        case AST_panic:{
            // __fastfail
            emit(0xcd);
            emit(0x29);
            return emit_location_immediate(context, 0, 4);
        }break;
        
        invalid_default_case(return null);
    }
    //invalid_code_path;
}

func void emit_code_for_function(struct context *context, struct ast_function *function){
    begin_counter(context, emit_code_for_function);
    
    context->current_function = function;
    context->max_amount_of_function_call_arguments = 4;
    context->temporary_stack_high_water_mark = 0;
    
    context->register_sp = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_SP, 8);
    context->gpr_allocator.emit_location_map[REGISTER_SP] = null;
    context->register_bp = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_BP, 8);
    context->gpr_allocator.emit_location_map[REGISTER_BP] = null;
    
    
    // :stack_space_needed. The amount of stack space needed needs to be aligned to 16.
    function->stack_space_needed = align_up(function->stack_space_needed, 0x10);
    
    // stack layout before we allocate stack memory
    // | arg n | ... |  arg 1  | arg 0 | ret ptr | memory for the function |
    //                [rsp+16]  [rsp+8]   [rsp]  ^rsp
    // see below how memory  for the function is layed out. -12.02.2020
    
    // "The first four integer or pointer parameters are passed in the rcx, rdx, r8, and r9 registers."
    // we have to save these in their slots

    enum register_encoding argument_registers[REGISTER_KIND_count][4] = {
        [REGISTER_KIND_gpr][0] = REGISTER_C,
        [REGISTER_KIND_gpr][1] = REGISTER_D,
        [REGISTER_KIND_gpr][2] = REGISTER_R8,
        [REGISTER_KIND_gpr][3] = REGISTER_R9,
        
        // "Floating point arguments are passed in XMM0L, XMM1L, XMM2L, and XMM3L."
        [REGISTER_KIND_xmm][0] = REGISTER_XMM0,
        [REGISTER_KIND_xmm][1] = REGISTER_XMM1,
        [REGISTER_KIND_xmm][2] = REGISTER_XMM2,
        [REGISTER_KIND_xmm][3] = REGISTER_XMM3,
    };
    
    context->gpr_allocator.rolling_index = REGISTER_C;
    
    struct ast_type *return_type = function->type->return_type;
    
    b32 do_first_loop_for_a_big_return = type_is_returned_by_address(return_type); // :returning_structs
    
    
    // set the current code section to the prolog
    u8 *prolog_start = context->emit_pool.current;
    context->current_emit_base = prolog_start;
    function->base_of_prolog = prolog_start;
    
    // @cleanup: only do this if we have a memcpy, this value is also needed to be known when returning a large struct
    //           maybe the large struct code should live in the epilog?
    // @incomplete: we do not honor the calling convention here...
    s32 amount_of_saved_registers = 2;
    emit(PUSH_REGISTER_DI);
    emit(PUSH_REGISTER_SI);
    
    function->pushed_register_mask = (1 << REGISTER_SI) | (1 << REGISTER_DI) | (1 << REGISTER_BP);
    
    emit(PUSH_REGISTER_BP);
    emit_reg_reg__(context, REXW, MOVE_REG_REGM, REGISTER_BP, REGISTER_SP);
    
    {
        // at this point we have pushed rbp so the memory layout is as follows:
        //    | memory for the function | old rbp | saved non-volitiles | ret ptr | arg0 | arg1 | arg2 | ...
        //                           rbp^                                         ^
        //                              ^rsp                                      ^rbp + 16 + 8 * amount_of_saved_registers = rbp - (-16 + 8 * amount_of_saved_registers)
        
        smm stack_at = -(16 + 8 * amount_of_saved_registers); // at offset zero is the return pointer.
        
        struct ast_list_node *it = function->type->argument_list.first;
        for(u32 i = 0; i < array_count(*argument_registers) || it; i++){
            if(!do_first_loop_for_a_big_return && !it && !(function->type->flags & FUNCTION_TYPE_FLAGS_is_varargs)) break;
            
            // @cleanup: there must be a better way to factor this...
            
            // save the first 4 register if we need them
            if(i < array_count(*argument_registers)){
                
                enum register_kind register_kind = REGISTER_KIND_gpr;
                if(!do_first_loop_for_a_big_return && it){ // if it is varargs all further arguments are in gprs
                    struct ast_declaration *decl = (struct ast_declaration *)it->value;
                    register_kind = get_register_kind_for_type(decl->type);
                    
                    if(decl->type->flags & TYPE_FLAG_is_intrin_type){
                        // intrinsic types are passed by reference.
                        register_kind = REGISTER_KIND_gpr;
                    }
                }
                
                enum register_encoding at = argument_registers[register_kind][i];
                
                struct emit_location *dest = emit_location_stack_relative(context, register_kind, stack_at, 8);
                
                struct emit_location *source = emit_location_loaded(context, register_kind,
                        allocate_specific_register(context, register_kind, at), 8);
                emit_store(context, dest, source);
            }
            
            if(do_first_loop_for_a_big_return){
                do_first_loop_for_a_big_return = false;
            }else if((function->type->flags & FUNCTION_TYPE_FLAGS_is_varargs) && !it){
                // this is fine, do nothing
            }else{
                assert(it->value->kind == AST_declaration);
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                decl->offset_on_stack = stack_at;
                it = it->next;
                
                // :PassingStructArguments
                if(size_is_big_or_oddly_sized(decl->type->size)){
                    decl->flags |= DECLARATION_FLAGS_is_big_function_argument;
                }
            }
            
            // :MSVC_function_call_stack_increase
            stack_at -= 8;
        }
    }
    
    emit_reg_reg__(context, REXW, REG_EXTENDED_OPCODE_REGM_IMMIDIATE, REG_OPCODE_SUB, REGISTER_SP);
    u32 *stack_space_subtract_address = cast(u32 *)context->emit_pool.current;
    emit_u32(0);
    
    function->rsp_subtract_offset = get_bytes_emitted(context);
    
    // stack layout:
    // high addresses ----------------------------------------------------------- low adresses
    // | return value | non-volatiles | declarations | temporary stack | stack of the next function |
    // rbp ---------------------------^
    // rsp ------------------------------------------------------------^
    //                                                                             -25.07.2021
    
    function->size_of_prolog = get_bytes_emitted(context);
    
    // main code section
    u8 *main_function_start = context->emit_pool.current;
    context->current_emit_base = main_function_start;
    function->base_of_main_function = main_function_start;
    
    struct jump_context jump_to_function_epilog = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
    context->jump_to_function_epilog = &jump_to_function_epilog;
    
    
    // all other arguments are already in their place
    
    // LET'S GO:
    ////////////////////////////////////////////////
    emit_code_for_ast(context, function->scope);
    ////////////////////////////////////////////////
    
    
    // :stack_space_needed
    // amount of stack needed for the declarations in the scope, is saved in 'function->stack_space_needed'
    // at the end of parsing. Here we patch it.
    smm stack_memory_needed = function->stack_space_needed;
    
    // "Space is allocated on the call stack as a shadow store for callees to save those registers."
    u32 shadow_space = 32;
    stack_memory_needed += shadow_space;
    
    // temporary stack memory
    stack_memory_needed += to_u32(context->temporary_stack_high_water_mark);
    stack_memory_needed += 8 * context->max_amount_of_function_call_arguments;
    
    // "Most structures are aligned to their natural alignment. The primary exceptions are the stack pointer and malloc or alloca memory, which are aligned to 16 bytes in order to aid performance"
    stack_memory_needed = align_up(stack_memory_needed, 16);
    if(amount_of_saved_registers & 1){
        // if we have an odd amount of saved registers, we have an even amount of saved registers + the saved rbp,
        // which means (stack_memory_needed + (amount_of_saved_registers + /* rbp */1) * 8) == 0 mod 16.
        // but this means the frame (which also contains the return value), would alternate alignment.
        stack_memory_needed += 8;
    }
    function->stack_space_needed = stack_memory_needed;
    
    // at this point 'stack_space_needed' is the amount of stack the function needs, before pushing and poping registers
    *stack_space_subtract_address = to_u32(stack_memory_needed);
    
    smm stack_frame_size = stack_memory_needed + amount_of_saved_registers * 8 + /* rbp */8 + /*return value*/8;
    assert((stack_frame_size & 15) == 0);
    
    // :function_epilog
    // we jump to here, instead of returning on the spot, this is so we can get canonical stack framing.
    emit_end_jumps(jump_to_function_epilog);
    
    if(type_is_returned_by_address(return_type)){
        // :returning_structs
        
        // Get the value of the implicit return value, which was passed as an implicit first operand.
        // And thus is contained in the first argument location.
        // We should return this again, thus load it into rax.
        struct emit_location *location_of_rax = emit_location_stack_relative(context, REGISTER_KIND_gpr, -(16 + 8 * amount_of_saved_registers), 8);
        enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
        struct emit_location *address_of_return_struct = emit_load_into_specific_gpr(context, location_of_rax, rax);
        struct emit_location *dest = emit_location_register_relative(context, REGISTER_KIND_gpr, address_of_return_struct, 0, 0, return_type->size);
        
        // In the AST_return, we have loaded the location of the return struct into rsi now we want to emit a memcpy for 
        // 'return_type->size' bytes to the saved rax.
        enum register_encoding rsi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_SI);
        struct emit_location *rsi_loc = emit_location_loaded(context, REGISTER_KIND_gpr, rsi, 8); 
        struct emit_location *source = emit_location_register_relative(context, REGISTER_KIND_gpr, rsi_loc, 0, 0, return_type->size);
        
        emit_memcpy(context, dest, source);
        
        // not sure if we need this @cleanup
        free_emit_location(context, source);
        free_emit_location(context, address_of_return_struct);
    }
    
    // deallocate stack memory
    emit_reg_reg__(context, REXW, MOVE_REG_REGM, REGISTER_SP, REGISTER_BP);
    emit(POP_REGISTER_BP);
    
    // @cleanup: only do this if we have a memcpy
    emit(POP_REGISTER_SI);
    emit(POP_REGISTER_DI);
    
    emit(NEAR_RET_INSTRUCTION);
    
    for_ast_list(function->goto_list){
        struct ast_goto *ast_goto = cast(struct ast_goto *)it->value;
        jump_node_end_jump(ast_goto->jump_node, ast_goto->label_to_goto->byte_offset_in_function);
    }
    
    if(context->alloca_patch_nodes.first){
        for(struct alloca_patch_node *alloca_patch_node = context->alloca_patch_nodes.first; alloca_patch_node; alloca_patch_node = alloca_patch_node->next){
            smm call_space_needed = 8 * (context->max_amount_of_function_call_arguments + (context->max_amount_of_function_call_arguments & 1));
            *(u32 *)alloca_patch_node->patch_location = (u32)call_space_needed;
        }
        context->alloca_patch_nodes.first = context->alloca_patch_nodes.last = null;
    }
    
    function->byte_size_without_prolog = get_bytes_emitted(context);
    
    function->byte_size = function->size_of_prolog + function->byte_size_without_prolog;
    
    end_counter(context, emit_code_for_function);
}


#undef emit
#undef emit_u16
#undef emit_u32
#undef emit_u64
