
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
// There are currently two registers stacks, one for XMM registers and one 
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
    LEGACY_OPERANT_SIZE_OVERRIDE_PREFIX   = 0x66, // this one is used to switch to 16 bit stuff
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
    MOVE_REG_SREG                = 0x8C,  // help me i dunno what this does. todo
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
    
    MOVE_REG_IMMEDIATE8               = 0xB0, // + REGISTER
    MOVE_REG_IMMEDIATE                = 0xB8, // + REGISTER
    
    SHIFT_OR_ROTATE_REGM8_IMMEDIATE8  = 0xC0,
    SHIFT_OR_ROTATE_REGM_IMMEDIATE8   = 0xC1,
    REG_OPCODE_SHIFT_LEFT  = 4, // @note: this is the same as shift arith left as we assume the top bits are 1
    REG_OPCODE_SHIFT_RIGHT = 5,
    REG_OPCODE_SHIFT_ARITHMETIC_LEFT  = 6, 
    REG_OPCODE_SHIFT_ARITHMETIC_RIGHT = 7, 
    
    NEAR_RET_INSTRUCTION              = 0xC3,
    
    MOVE_REGM8_IMMEDIATE8              = 0xC6,
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
    JUMP_REL32_IF_OVERFLOW              = 0x80,
    JUMP_REL32_IF_NOT_OVERFLOW          = 0x81,
    JUMP_REL32_IF_SMALLER               = 0x82,
    JUMP_REL32_IF_BIGGER_EQUALS         = 0x83,
    JUMP_REL32_IF_EQUALS                = 0x84,
    JUMP_REL32_IF_UNEQUALS              = 0x85,
    JUMP_REL32_IF_SMALLER_EQUALS        = 0x86,
    JUMP_REL32_IF_BIGGER                = 0x87,
    JUMP_REL32_IF_NEGATIVE              = 0x88, 
    JUMP_REL32_IF_POSITIVE              = 0x89,
    JUMP_REL32_IF_EVEN                  = 0x8A,
    JUMP_REL32_IF_ODD                   = 0x8B,
    JUMP_REL32_IF_SMALLER_SIGNED        = 0x8C,
    JUMP_REL32_IF_BIGGER_EQUALS_SIGNED  = 0x8D,
    JUMP_REL32_IF_SMALLER_EQUALS_SIGNED = 0x8E,
    JUMP_REL32_IF_BIGGER_SIGNED         = 0x8F,
    
    
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
    
    XMM0 = 0, 
    XMM1 = 1, 
    XMM2 = 2, 
    XMM3 = 3, 
    XMM4 = 4, 
    XMM5 = 5, 
    XMM6 = 6, 
    XMM7 = 7,
    
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
        report_error(context, context->current_function->base.token, "To many bytes of code. Maximally %llu allowed.", emit_pool->capacity);
        return emit_pool->capacity; // @cleanup: test this path
    }
}

func smm get_bytes_emited(struct context *context){
    return context->emit_pool.current - context->current_emit_base;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// 'sse_prefix' -  0x00 = packed float (or no prefix because not sse)
//                 0x66 = packed double
//                 0xf2 = double
//                 0xf3 = float

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


func enum prefix get_sse_prefix_for_scalar(smm size){
    enum prefix sse_prefix;
    if(size == 4){
        sse_prefix = SSE_PREFIX_float;
    }else if(size == 8){
        sse_prefix = SSE_PREFIX_double;                    
    }else{
        assert(size == 16); // @cleanup: do we need double here ever?
        sse_prefix = SSE_PREFIX_packed_float;
    }
    return sse_prefix;
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

func void emit_opcode(struct context *context, struct opcode opcode){
    for(u32 i = 0; i < opcode.amount_of_bytes; i++){
        emit(opcode.bytes[i]);
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

// follows modrm if rm == 8
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
    COMP_none,
    COMP_equals,
    COMP_unequals,
    COMP_not_zero = COMP_unequals,
    COMP_smaller,
    COMP_smaller_equals,
    COMP_bigger,
    COMP_bigger_equals,
    COMP_smaller_signed,
    COMP_smaller_equals_signed,
    COMP_bigger_signed,
    COMP_bigger_equals_signed,
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
    b32 is_locked; // this both blocks frees as well as being spilled (might change in the future)
    enum register_kind register_kind_when_loaded;
    union{
        struct{
            enum register_encoding loaded_register;
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
            
            smm index_scale;
            smm offset;
            struct ast *ast; // rip_relative iff (ast != null)
        }; // register_relative & rip_relative
    };
};

#define emit_location_invalid(context) ((struct emit_location *)null)

func struct emit_location *emit_location_loaded(struct context *context, enum register_kind kind, enum register_encoding reg, smm size){
    assert(!context->register_allocators[kind].emit_location_map[reg]);
    
    struct emit_location *ret = push_struct(&context->scratch, struct emit_location);
    ret->register_kind_when_loaded = kind;
    ret->state = EMIT_LOCATION_loaded;
    ret->loaded_register = reg;
    ret->is_locked = false;
    ret->size = size;
    
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
    ret->index_scale = size;
    ret->offset = offset;
    ret->size = size;
    ret->ast = ast;
    return ret;
}


func struct emit_location *emit_location_register_relative(struct context *context, enum register_kind kind, struct emit_location *base, struct emit_location *index_register, smm offset, smm size){
    return emit_location_register_relative__internal(context, kind, base, index_register, offset, size, null);
}

func enum register_kind get_register_kind_for_type(struct ast_type *type){
    enum register_kind kind = REGISTER_KIND_gpr;
    //assert(type != &globals.typedef_void); @cleanup: why does this fire?
    if(type == &globals.typedef_f32 || type == &globals.typedef_f64){
        kind = REGISTER_KIND_xmm;
    }
    return kind;
}

func struct emit_location *emit_location_rip_relative(struct context *context, struct ast *ast, struct ast_type *type, smm size){
    enum register_kind kind = get_register_kind_for_type(type);
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
    ret->size = size;
    return ret;
}

/////////////////////////////////////////////////

func struct emit_location *emit_allocate_tempoary_stack_location(struct context *context, enum register_kind kind, smm size, smm alignment){
    
    context->tempoary_stack_allocator  = align_up(context->tempoary_stack_allocator, alignment);
    context->tempoary_stack_allocator += size;
    context->tempoary_stack_high_water_mark = max_of(context->tempoary_stack_high_water_mark, context->tempoary_stack_allocator);
    smm stack_location_to_allocate = context->current_emit_offset_of_rsp + context->tempoary_stack_allocator;
    return emit_location_stack_relative(context, kind, stack_location_to_allocate, size);
}

func void spill_register(struct context *context, enum register_kind allocator, 
                         enum register_encoding register_to_spill){
    assert(allocator == REGISTER_KIND_gpr || allocator == REGISTER_KIND_xmm);
    
    struct register_allocator *alloc = context->register_allocators + allocator;
    
    assert(alloc->emit_location_map[register_to_spill]);
    assert(alloc->emit_location_map[register_to_spill]->is_locked == false);
    
    struct emit_location *location_to_spill = alloc->emit_location_map[register_to_spill];
    assert((location_to_spill->size == 1) || (location_to_spill->size == 2) || 
           (location_to_spill->size == 4) || (location_to_spill->size == 8));
    // @note: for basic types we have alignment == size
    *location_to_spill = *emit_allocate_tempoary_stack_location(context, allocator, location_to_spill->size, location_to_spill->size);
    
    alloc->emit_location_map[register_to_spill] = null;
    
    smm offset = location_to_spill->offset;
    smm size   = location_to_spill->size;
    
    //  Legacy prefix / sse prefix
    if(allocator == REGISTER_KIND_gpr){
        if(size == 2) emit(LEGACY_OPERANT_SIZE_OVERRIDE_PREFIX); // not possible for xmm
    }else{
        enum prefix sse_prefix = SSE_PREFIX_none;
        if(size == 4)   sse_prefix = SSE_PREFIX_float;
        if(size == 8)   sse_prefix = SSE_PREFIX_double;
        if(size == 16)  sse_prefix = SSE_PREFIX_packed_float;
        if(sse_prefix != SSE_PREFIX_none) emit(sse_prefix);
    }
    
    // REX prefix
    u8 rex = 0;
    if(size == 8) rex |= REXW;
    if(register_is_extended(register_to_spill)) rex |= REXR;
    if(rex) emit(rex);
    
    // opcode
    if(allocator == REGISTER_KIND_gpr){
        u8 inst = MOVE_REGM_REG;
        if(size == 1) inst = MOVE_REGM8_REG8;
        emit(inst);
    }else{
        emit(TWO_BYTE_INSTRUCTION_PREFIX);
        emit(MOVE_UNALIGNED_REGM_XMM);
    }
    
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
    if(context->register_allocators[alloc].emit_location_map[reg]){
        spill_register(context, alloc, reg);
    }
    return reg;
}

static const enum register_encoding volitile_registers[] = {
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
    struct register_allocator *alloc = context->register_allocators + allocator;
    u32 at = alloc->rolling_index++;
    if(alloc->rolling_index >= array_count(volitile_registers)){
        alloc->rolling_index= 0;
    }
    
    for(u32 i = at; i < array_count(volitile_registers); i++){
        enum register_encoding reg = volitile_registers[i];
        if(!alloc->emit_location_map[reg]){
            return reg;
        }
    }
    
    for(u32 i = 0; i < at; i++){
        enum register_encoding reg = volitile_registers[i];
        if(!alloc->emit_location_map[reg]){
            return reg;
        }
    }
    
    // if we are here we need to _spill_ a register and give that back.
    // if is_locked is set we currently need that register, so do not spill it
    enum register_encoding first_non_locked_register = INVALID_REGISTER;
    
    for(u32 i = at; i < array_count(volitile_registers); i++){
        enum register_encoding reg = volitile_registers[i];
        if(alloc->emit_location_map[reg]->is_locked) continue;
        first_non_locked_register = reg;
        goto found_a_non_locked_register;
    }
    
    for(u32 i = 0; i < at; i++){
        enum register_encoding reg = volitile_registers[i];
        if(alloc->emit_location_map[reg]->is_locked) continue;
        first_non_locked_register = reg;
        goto found_a_non_locked_register;
    }
    
    os_debug_break();
    report_error(context, 0, "Internal compiler error: Spilled all registers, this should be impossible.");
    os_panic(1);
    
    found_a_non_locked_register:;
    
    spill_register(context, allocator, first_non_locked_register);
    return first_non_locked_register;
}

func void free_emit_location(struct context *context, struct emit_location *loc){
    if(loc == context->register_bp) return;
    assert(loc->state != EMIT_LOCATION_freed); // no double frees
    if(loc->is_locked) return;
    
    switch(loc->state){
        case EMIT_LOCATION_loaded:{
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

func struct emit_location *emit_load(struct context *context, struct emit_location *loc, smm dest_size);

func void lock_emit_location(struct context *context, struct emit_location *loc){
    loc->is_locked = true;
    if(loc->state == EMIT_LOCATION_register_relative){
        loc->base = emit_load(context, loc->base, 8);
        loc->base->is_locked = true;
        if(loc->index){
            loc->index = emit_load(context, loc->index, 8);
            loc->index->is_locked = true;
        }
    }
}

func void unlock_emit_location(struct emit_location *loc){
    loc->is_locked = false;
    if(loc->state == EMIT_LOCATION_register_relative){
        loc->base->is_locked = false;
        if(loc->index) loc->index->is_locked = false;
    }
}


/////////////////////////////////////////////////


func void emit_register_op__internal(struct context *context, struct opcode opcode, enum register_encoding reg, enum register_encoding regm, smm size){
    
    u8 rex = 0;
    if(size == 8) rex |= REXW;
    if(size == 2) emit(LEGACY_OPERANT_SIZE_OVERRIDE_PREFIX);
    
    if(register_is_extended(reg))  rex |= REXR;
    if(register_is_extended(regm)) rex |= REXB;
    if(rex) emit(rex);
    emit_opcode(context, opcode);
    emit(make_modrm(MODRM_REG, reg & 7, regm & 7));
}

func void emit_reg_extended_op(struct context *context, struct opcode opcode, u8 extension, struct emit_location *loaded){
    assert(loaded->state == EMIT_LOCATION_loaded);
    
    emit_register_op__internal(context, opcode, extension, loaded->loaded_register, loaded->size);
}

func void emit_register_register(struct context *context, struct opcode opcode, struct emit_location *_reg, struct emit_location *_regm){
    assert(_reg->state == EMIT_LOCATION_loaded);
    assert(_regm->state == EMIT_LOCATION_loaded);
    
    assert(_reg->register_kind_when_loaded == _regm->register_kind_when_loaded);
    
    assert(_reg->size == _regm->size);
    
    enum register_encoding reg  = _reg->loaded_register;
    enum register_encoding regm = _regm->loaded_register;
    
    emit_register_op__internal(context, opcode, reg, regm, _reg->size);
}

// if you want register relative with immediate use 'emit_immediate_for_register_relative' immediatly after the 
// call to 'emit_register_relative'

func void emit_register_relative__internal(struct context *context, enum prefix prefix, struct opcode opcode, u8 other_reg, struct emit_location *loc, struct emit_location *immediate){
    assert(loc->state == EMIT_LOCATION_register_relative);
    assert(loc->size == 1 || loc->size == 2 || loc->size == 4 || loc->size == 8 || loc->size == 16);
    
    // @cleanup: should we lock 'other_reg' here? we could just look it up.
    
    // @cleanup: these loads should be specific we know that they are stack locative if they are spilled
    struct emit_location *base_reg = emit_load(context, loc->base, 8);
    loc->base = base_reg; // @note: we have to do this so we can free them
    
    enum register_encoding base =  base_reg->loaded_register;
    enum register_encoding index = INVALID_REGISTER; // -1
    if(loc->index){
        lock_emit_location(context, base_reg);
        struct emit_location *index_location = emit_load(context, loc->index, 8);
        loc->index = index_location; // @note: we have to do this so we can free them
        index = index_location->loaded_register;
        unlock_emit_location(base_reg);
    }
    
    // @note: right now only ever one prefix...
    if(prefix) emit(prefix);
    
    if(loc->size == 2) emit(LEGACY_OPERANT_SIZE_OVERRIDE_PREFIX);
    
    u8 rex = 0;
    if(loc->size == 8)                            rex |= REXW;
    if(register_is_extended(other_reg))           rex |= REXR;
    if(register_is_extended(base))                rex |= REXB;
    if(index >= 0 && register_is_extended(index)) rex |= REXX;
    
    if(rex) emit(rex);
    
    emit_opcode(context, opcode);
    
    u8 mod = MODRM_REGM;
    if(loc->ast){
        // rip relative
        //mod = MODRM_REGM;
        assert(index == -1 && base == REGISTER_BP);
    }else{
        assert(loc->offset <= s32_max && loc->offset >= s32_min);
        if(loc->offset > s8_max || loc->offset < s8_min){
            mod = MODRM_REGM32;
        }else if(loc->offset != 0){
            mod = MODRM_REGM8;
        }
    }
    
    if(index >= 0){
        u8 log_size = 0;
        
        if(index != REGISTER_SP){
            switch(loc->index_scale){
                case 1: log_size = 0; break;
                case 2: log_size = 1; break;
                case 4: log_size = 2; break;
                case 8: log_size = 3; break;
                invalid_default_case();
            }
        }
        
        emit(make_modrm(mod, other_reg & 0x7, REGISTER_SIB_EXTENSION));
        emit(make_sib(log_size, index & 0x7, base & 0x7));
    }else{
        emit(make_modrm(mod, other_reg & 0x7, base & 0x7));
    }
    
    smm byte_offset = -1;
    if(loc->ast){
        byte_offset = emit_u32(0);
    }else{
        if(loc->offset > s8_max || loc->offset < s8_min){
            emit_u32(loc->offset);
        }else if(loc->offset != 0){
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
            case 8: emit_u32(value); break;
            invalid_default_case();
        }
    }
    
    if(loc->ast){
        assert(byte_offset >= 0);
        smm rip_at = get_bytes_emited(context);
        emit_patch(context, PATCH_rip_relative, loc->ast, loc->offset, &context->current_function->as_decl, byte_offset, rip_at);
    }
    
}

// emits instructions of the form:
//    1) 'op [base + scale * index + offset], register'
//    2) 'op register, [base + scale * index + offset]'
func void emit_register_relative_register(struct context *context, enum prefix prefix, struct opcode opcode, enum register_encoding other_reg, struct emit_location *loc){
    emit_register_relative__internal(context, prefix, opcode, other_reg, loc, null);
}

// emits instructions of the form:
//   'op [base + scale * index + offset], immediate'
// caller has to make sure that the 'opcode' matches the 'immediate->size'
func void emit_register_relative_immediate(struct context *context, enum prefix prefix, struct opcode opcode, u8 reg_extension, struct emit_location *register_relative, struct emit_location *immediate){
    assert(register_relative->register_kind_when_loaded == REGISTER_KIND_gpr);
    emit_register_relative__internal(context, prefix, opcode, reg_extension, register_relative, immediate);
}

func void emit_register_relative_extended(struct context *context, enum prefix prefix, struct opcode opcode, u8 extension, struct emit_location *register_relative){
    emit_register_relative__internal(context, prefix, opcode, extension, register_relative, null);
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
                // "MOVXD without REWX is discouraged"
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


// @note: this is mostly so complicated because we want to be able to do stuff like emit_load(context, loc, maxof(size, 4)); @cleanup: is this still true?
func struct emit_location *emit_load_into_specific_gpr(struct context *context, struct emit_location *source,  enum register_encoding register_to_load_into){
    assert(source->size == 1 || source->size == 2 || source->size == 4 || source->size == 8);
    
    assert(source->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    switch(source->state){
        case EMIT_LOCATION_loaded:{
            // if we have the desired register there is nothing to do.
            if((source->loaded_register == register_to_load_into)) return source;
            
            struct opcode opcode;
            if(source->size == 1){
                opcode = one_byte_opcode(MOVE_REG8_REGM8);
            }else{
                opcode = one_byte_opcode(MOVE_REG_REGM);
            }
            
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            
            struct emit_location *load_into =  emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
            
            emit_register_register(context, opcode, load_into, source);
            free_emit_location(context, source);
            return load_into;
        }break;
        // I think this pretty much only has to be called for function calls
        case EMIT_LOCATION_immediate:{
            
            if(source->value == 0){
                allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
                struct emit_location *ret = emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
                emit_register_register(context, one_byte_opcode(XOR_REG_REGM), ret, ret);
                return ret;
            }
            
            // @cleanup: can we load smaller here if 'source->size == 8' but |source->value| small?
            
            // @note this does not have a u8 or u16 version, as the registers wont get cleared for those.
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
            
            emit_register_relative_register(context, 0, opcode, register_to_load_into, source);
            free_emit_location(context, source);
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            return emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, source->size);
        }break;
        case EMIT_LOCATION_conditional:{
            assert(source->size == 4);
            
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
                
                invalid_default_case(inst = SET_REGM8_IF_EQUALS);
            }
            
            // "The reg field in the ModR/M byte is unused"
            allocate_specific_register(context, REGISTER_KIND_gpr, register_to_load_into);
            
            struct emit_location *load_into = emit_location_loaded(context, REGISTER_KIND_gpr, register_to_load_into, 4);
            emit_reg_extended_op(context, two_byte_opcode(inst), 0, load_into);
            emit_register_register(context, two_byte_opcode(MOVE_WITH_ZERO_EXTENSION_REG_REGM8), load_into, load_into);
            
            return load_into;
        }break;
        invalid_default_case(return null);
    }
}

func struct emit_location *emit_load_float_into_specific_register(struct context *context, struct emit_location *loc, enum register_encoding reg){
    
    assert(loc->register_kind_when_loaded == REGISTER_KIND_xmm);
    if(loc->state == EMIT_LOCATION_loaded && loc->loaded_register == reg) return loc;
    
    enum prefix sse_prefix = get_sse_prefix_for_scalar(loc->size);
    allocate_specific_register(context, REGISTER_KIND_xmm, reg);
    struct emit_location *ret = emit_location_loaded(context, REGISTER_KIND_xmm, reg, loc->size);
    
    switch(loc->state){
        case EMIT_LOCATION_loaded:{
            emit(sse_prefix);
            emit_register_register(context, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), ret, loc);
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

func struct emit_location *emit_load(struct context *context, struct emit_location *loc, smm dest_size){
    // @cleanup: this really should set the register_to_load_into to loc->loaded_register
    if(loc->state == EMIT_LOCATION_loaded && loc->size == dest_size) return loc;
    if(loc->register_kind_when_loaded == REGISTER_KIND_gpr){
        enum register_encoding register_to_load_into = allocate_register(context, REGISTER_KIND_gpr);
        struct emit_location *ret = emit_load_into_specific_gpr(context, loc, register_to_load_into);
        assert(ret->size == dest_size);
        return ret;
    }else{
        return emit_load_float(context, loc);
    }
}

func struct emit_location *emit_load_address(struct context *context, struct emit_location *loc, enum register_encoding load_into){
    assert(loc->state == EMIT_LOCATION_register_relative);
    
    // @hack: we expect to only get register sized things in emit_register_relative, but this is fine
    smm saved_size = loc->size;
    loc->size = 8;
    struct emit_location *loaded = emit_location_loaded(context, REGISTER_KIND_gpr, load_into, 8);
    lock_emit_location(context, loaded);
    emit_register_relative_register(context, 0, one_byte_opcode(LOAD_ADDRESS_REG_MEMORY_LOCATION), load_into, loc);
    unlock_emit_location(loaded);
    
    loc->size = saved_size;
    
    return loaded;
}

func void emit_memcpy(struct context *context, struct emit_location *dest, struct emit_location *source){
    assert(dest->state == EMIT_LOCATION_register_relative);
    assert(source->state == EMIT_LOCATION_register_relative);
    assert(dest->size >= source->size); // bigger or equal for 'char asd[5] = "asd";'
    
    // @cleanup: we may only have to load 4 bytes or so...
    struct emit_location *count = emit_location_immediate(context, source->size, 8);
    
    enum register_encoding rdi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_DI);
    enum register_encoding rsi = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_SI);
    enum register_encoding rcx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_C);
    
    struct emit_location *dest_address   = emit_load_address(context, dest, rdi);
    struct emit_location *source_address = emit_load_address(context, source, rsi);
    struct emit_location *count_register = emit_load_into_specific_gpr(context, count, rcx);
    
    emit(LEGACY_PREFIX_REPEAT);
    emit(MOVE_BYTE);
    
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
    
    if(!(source->register_kind_when_loaded == REGISTER_KIND_xmm && source->size == 16)){
        // @hack: for an intrinsic (e.g. __m128d _mm_add_sd(__m128d _A, __m128d _B);), we don't know that the
        //        dest (__m128d) is XMM (because it kinda isn't), but we want to be able to write over it
        assert(dest->register_kind_when_loaded == source->register_kind_when_loaded);
    }
    
    // @hack: but it's fine
    enum register_encoding source_reg = REGISTER_A;
    u8 inst;
    switch(source->state){
        case EMIT_LOCATION_register_relative:{
            if(source->size > 8){
                emit_memcpy(context, dest, source);
                goto end;
            }
            
            // load 'source' for it's natural size, the store will then happen at 'dest->size'
            source = emit_load(context, source, source->size);
            goto loaded;
        }break;
        case EMIT_LOCATION_conditional:{
            // @cleanup: we could emit SET_REGM8 here
            source = emit_load(context, source, dest->size);
            goto loaded;
        }break;
        case EMIT_LOCATION_loaded:{
            loaded:;
            if(source->register_kind_when_loaded == REGISTER_KIND_xmm){
                assert(source->size == dest->size);
                
                enum prefix sse_prefix = get_sse_prefix_for_scalar(source->size);
                emit_register_relative_register(context, sse_prefix, two_byte_opcode(MOVE_UNALIGNED_REGM_XMM), source->loaded_register, dest);
                break;
            }
            
            
            inst = (dest->size == 1) ? MOVE_REGM8_REG8 : MOVE_REGM_REG;
            source_reg = source->loaded_register;
            emit_register_relative_register(context, 0, one_byte_opcode(inst), source_reg, dest);
        }break;
        case EMIT_LOCATION_immediate:{
            if(source->size == 8) {
                source = emit_load(context, source, source->size);
                goto loaded;
            }
            inst = (dest->size == 1) ? MOVE_REGM8_IMMEDIATE8 : MOVE_REGM_IMMEDIATE;
            
            source->size = dest->size;
            // @cleanup: the 'reg' field get ignored I guess?
            emit_register_relative_immediate(context, 0, one_byte_opcode(inst), 0, dest, source);
        }break;
        invalid_default_case(inst = MOVE_REGM_REG);
    }
    
    end:;
    free_emit_location(context, source);
}

///////////////////////////////////////////////////////////////////////////////////////////////

func struct emit_location *emit_code_for_ast(struct context *context, struct ast *ast);

func struct emit_location *emit_binary_op(struct context *context, struct ast *ast, u8 reg_extended, u8 u8_code, u8 opcode){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    // @note: @quality: for cummutative operations, we could switch dest and source, if rhs is loaded
    // and lhs is not.
    
    // @note: we dont have that op->lhs->resolved_type->size == op->base.resolved_type->size, as we handle
    //        '==', and stuff -10.03.2020
    
    // @note: these are not the same, as pointers
    assert(op->lhs->resolved_type->size == op->rhs->resolved_type->size);
    smm size = op->lhs->resolved_type->size;
    
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    lhs = emit_load(context, lhs, size);
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    // @clenup: why do we have to load here?
    if(rhs->state == EMIT_LOCATION_conditional) rhs = emit_load(context, rhs, size);
    
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_gpr);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    // :little_endian @cleanup: can we get rid of this?
    if(lhs->size > size) lhs->size = size;
    if(rhs->size > size) rhs->size = size;
    
    struct emit_location *dest   = emit_load(context, lhs, size);
    struct emit_location *source = rhs;
    
    lock_emit_location(context, dest);
    
    switch(source->state){
        case EMIT_LOCATION_immediate:{
            b32 is_signed = type_is_signed(op->rhs->resolved_type);
            // if REXW is present, the immediate is of size 4 and gets sign-extended.
            // so we have to make sure, that if we are not signed we sign_extend
            if(source->size == 8 || ((source->size == 4) && !is_signed && source->value > s32_max)){
                source = emit_load(context, source, source->size);
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
            emit_reg_extended_op(context, one_byte_opcode(inst), reg_extended, dest);
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
            emit_register_register(context, one_byte_opcode(inst), dest, source);
        }break;
        case EMIT_LOCATION_register_relative:{
            assert(dest->size == source->size);
            u8 inst = opcode;
            if(source->size == 1) inst = u8_code;
            emit_register_relative_register(context, 0, one_byte_opcode(inst), dest->loaded_register, source);
        }break;
        invalid_default_case();
    }
    
    unlock_emit_location(dest);
    free_emit_location(context, source);
    free_emit_location(context, dest);
    return emit_location_loaded(context, REGISTER_KIND_gpr, dest->loaded_register, size);
}

func struct emit_location *emit_divide_or_mod_or_multiply(struct context *context, struct ast *ast, u8 REG_OPCODE_signed, u8 REG_OPCODE_unsigned, b32 is_assignment){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    
    // @cleanup: I guess we are not doing this anymore
    smm actual_size = op->lhs->resolved_type->size; 
    //smm size = max_of(4, actual_size); // we work in at least 32 bit because faster, also (size != 1)
    smm size = actual_size;
    b32 is_signed = type_is_signed(ast->resolved_type);
    
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    rhs = emit_load(context, rhs, size);
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
    lock_emit_location(context, upper_part);
    
    // @quality: we don't have to spill rax just to load it right after...
    enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
    struct emit_location *lower_part = emit_location_loaded(context, REGISTER_KIND_gpr, rax, size);
    lock_emit_location(context, lower_part);
    
    // lock lhs *after* we allocated rax and rdx (so we don't allocate them)
    if(is_assignment) lock_emit_location(context, lhs);
    unlock_emit_location(lower_part);
    
    lower_part = emit_load_into_specific_gpr(context, lhs, rax);
    lock_emit_location(context, lower_part);

    
    if(is_signed){
        if(size == 1){
            emit_register_register(context, two_byte_opcode(MOVE_WITH_SIGN_EXTENSION_REG_REGM8), lower_part, lower_part);
        }else{
            if(size == 2) emit(LEGACY_OPERANT_SIZE_OVERRIDE_PREFIX);
            if(size == 8) emit(REXW);
            emit(SIGN_EXTEND_A_INTO_D);
        }
    }else{
        if(size == 1){
            // @note: we need to zero into AH to not get an exeption
            emit_register_register(context, two_byte_opcode(MOVE_WITH_ZERO_EXTENSION_REG_REGM8), lower_part, lower_part);
        }else{
            emit_register_register(context, one_byte_opcode(XOR_REG_REGM), upper_part, upper_part);
        }
    }
    
    u8 inst = REG_EXTENDED_UNARY_REGM;
    if(size == 1) inst = REG_EXTENDED_UNARY_REGM8;
    u8 extension = REG_OPCODE_unsigned;
    if(is_signed) extension = REG_OPCODE_signed;
    
    // @cleanup: do we commit to this? this is here because if rhs is of size < 4 then the operations are weird
    if((rhs->size >= 4) && (rhs->state == EMIT_LOCATION_register_relative)){
        emit_register_relative_extended(context, 0, one_byte_opcode(inst), extension, rhs);
        free_emit_location(context, rhs);
    }else{
        struct emit_location *loaded_rhs = emit_load(context, rhs, size);
        emit_reg_extended_op(context, one_byte_opcode(inst), extension, loaded_rhs);
        free_emit_location(context, loaded_rhs);
    }
    
    unlock_emit_location(lower_part);
    unlock_emit_location(upper_part);
    
    // @note: truncate the size again (we did the operantion in 4 byte for speed)
    upper_part->size = actual_size;
    lower_part->size = actual_size;
    
    if(is_assignment){
        if(ast->kind  == AST_modulo_assignment){
            if(size == 1){
                free_emit_location(context, upper_part);
                // the result is in AX
                lower_part->size = 2;
                emit_reg_extended_op(context, one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_RIGHT, lower_part);
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
        unlock_emit_location(lhs);
        return lhs;
    }
    
    if(ast->kind == AST_binary_mod){
        // if size == 1 then the upper_part is 'AH' and not 'DL'...
        if(size == 1){
            free_emit_location(context, upper_part);
            // the result is in AX
            lower_part->size = 2;
            emit_reg_extended_op(context, one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_RIGHT, lower_part);
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

// '+=', '-=', ... @cleanup:
func struct emit_location *emit_compound_assignment(struct context *context, struct ast *ast, u8 reg_extension, u8 u8_code, u8 opcode){
    struct ast_binary_op *assign = cast(struct ast_binary_op *)ast;
    
    assert(assign->lhs->resolved_type->size == assign->rhs->resolved_type->size);
    assert(assign->base.resolved_type == assign->lhs->resolved_type);
    
    struct emit_location *rhs = emit_code_for_ast(context, assign->rhs);
    rhs = emit_load(context, rhs, assign->lhs->resolved_type->size);
    struct emit_location *lhs = emit_code_for_ast(context, assign->lhs);
    
    assert(lhs->state == EMIT_LOCATION_register_relative);
    
    switch(rhs->state){
        case EMIT_LOCATION_loaded:{
            loaded:;
            u8 inst = opcode;
            if(lhs->size == 1) inst = u8_code;
            lock_emit_location(context, rhs);
            emit_register_relative_register(context, 0, one_byte_opcode(inst), rhs->loaded_register, lhs);
            unlock_emit_location(rhs);
        }break;
        case EMIT_LOCATION_conditional:
        case EMIT_LOCATION_register_relative:{
            // @note: if lhs is bigger then upconvert, otherwise load and truncate afterwards
            rhs = emit_load(context, rhs, max_of(lhs->size, rhs->size));
            goto loaded;
        }break;
        case EMIT_LOCATION_immediate:{
            b32 is_signed = type_is_signed(assign->base.resolved_type);
            
            u8 inst = REG_EXTENDED_OPCODE_REGM_IMMIDIATE;
            if(lhs->size == 1){
                inst = REG_EXTENDED_OPCODE_REGM8_IMMIDIATE8;
            }
            if(rhs->size == 1){
                if(is_signed || rhs->value <= s8_max){
                    inst = REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                }else{
                    rhs->size = lhs->size;
                }
            }else if(rhs->size == 8){
                rhs = emit_load(context, rhs, 8);
                goto loaded;
            }else if(rhs->size == 4 && is_signed && rhs->value > s32_max){
                rhs = emit_load(context, rhs, 4);
                goto loaded;
            }else{
                rhs->size = lhs->size;
            }
            
            emit_register_relative_immediate(context, 0, one_byte_opcode(inst), reg_extension, lhs, rhs);
        }break;
        invalid_default_case();
    }
    free_emit_location(context, rhs);
    return lhs;
}

////////////////////////////////////////////////////////////////////////////////////////

func struct emit_location *emit_binary_op_xmm(struct context *context, struct ast *ast, u8 inst){
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    struct emit_location *rhs = emit_code_for_ast(context, op->rhs);
    
    assert(lhs->size == rhs->size);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    lhs = emit_load(context, lhs, lhs->size);
    
    enum prefix sse_prefix = get_sse_prefix_for_scalar(lhs->size);
    
    if(rhs->state == EMIT_LOCATION_loaded){
        emit(sse_prefix);
        emit_register_register(context, two_byte_opcode(inst), lhs, rhs);
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
    rhs = emit_load(context, rhs, rhs->size);
    
    struct emit_location *lhs = emit_code_for_ast(context, op->lhs);
    assert(lhs->state == EMIT_LOCATION_register_relative);
    
    assert(lhs->size == rhs->size);
    assert(lhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    assert(rhs->register_kind_when_loaded == REGISTER_KIND_xmm);
    
    enum prefix sse_prefix = get_sse_prefix_for_scalar(lhs->size);
    emit_register_relative_register(context, sse_prefix, two_byte_opcode(inst), rhs->loaded_register, lhs);
    
    free_emit_location(context, rhs);
    return lhs;
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
    emit_end_jumps_location(jump_context, get_bytes_emited(jump_context.context));
}

func void emit_jump(struct context *context, smm location, enum comp_condition cond, enum jump_context_condition jump_on){
    u8 inst = instruction_from_comp_condition(cond, jump_on);
    
    if(cond != COMP_none) emit(TWO_BYTE_INSTRUCTION_PREFIX);
    emit(inst);
    // @note: plus 4 because we have to factor in the 'emit_u32' afterwards
    u32 rel_location = cast(u32)(location - (get_bytes_emited(context) + 4));
    emit_u32(rel_location);
}

////////////////////////////////////////////////////////////////////////////////////////////

func struct emit_location *emit_code_for_plain_condition(struct context *context, struct ast *ast){
    struct emit_location *cond = emit_code_for_ast(context, ast);
    enum comp_condition condition;
    if(cond->state == EMIT_LOCATION_conditional){
        condition = cond->condition;
    }else{
        condition = COMP_not_zero;
        
        // @note: used to be max_of(cond>size, 4);
        struct emit_location *loaded = emit_load(context, cond, cond->size);
        u8 inst = TEST_REGM_REG;
        if(cond->size == 1){
            inst = TEST_REGM8_REG8;
        }
        emit_register_register(context, one_byte_opcode(inst), loaded, loaded);
        free_emit_location(context, loaded);
    }
    
    return emit_location_conditional(context, condition);
}

func struct jump_context emit_code_for_if_condition(struct context *context, struct ast *ast){
    struct jump_context or_jump_context = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
    
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

func struct emit_location *emit_shift_or_rotate(struct context *context, struct ast *ast, u8 reg_extension, b32 compound){
    struct ast_binary_op *shift = cast(struct ast_binary_op *)ast;
    
    smm size = shift->base.resolved_type->size;
    
    struct emit_location *rhs_loc = emit_code_for_ast(context, shift->rhs);
    rhs_loc = emit_load(context, rhs_loc, shift->rhs->resolved_type->size);
    struct emit_location *lhs_loc = emit_code_for_ast(context, shift->lhs);
    
    assert(rhs_loc->register_kind_when_loaded == REGISTER_KIND_gpr);
    assert(lhs_loc->register_kind_when_loaded == REGISTER_KIND_gpr);
    
    assert(lhs_loc->size == shift->base.resolved_type->size);
    
    if(compound){
        assert(lhs_loc->state == EMIT_LOCATION_register_relative);
    }
    
    struct emit_location *ret = null;
    if(rhs_loc->state == EMIT_LOCATION_immediate){
        assert(rhs_loc->size == 1);
        if(compound){
            u8 inst = SHIFT_OR_ROTATE_REGM_IMMEDIATE8;
            if(lhs_loc->size == 1){
                inst = SHIFT_OR_ROTATE_REGM8_IMMEDIATE8;
            }
            
            emit_register_relative_extended(context, 0, one_byte_opcode(inst), reg_extension, lhs_loc);
            ret = lhs_loc;
        }else{
            struct emit_location *lhs = emit_load(context, lhs_loc, max_of(size, 4));
            emit_reg_extended_op(context, one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), reg_extension, lhs);
            lhs->size = size;
            ret = lhs;
        }
        emit(rhs_loc->value);
    }else{
        enum register_encoding cl = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_C);
        struct emit_location *rhs = emit_load_into_specific_gpr(context, rhs_loc, cl);
        lock_emit_location(context, rhs);
        if(compound){
            u8 inst = SHIFT_OR_ROTATE_REGM_CL;
            if(lhs_loc->size == 1) inst = SHIFT_OR_ROTATE_REGM8_CL;
            emit_register_relative_extended(context, 0, one_byte_opcode(inst), reg_extension, lhs_loc);
            ret = lhs_loc;
        }else{
            struct emit_location *lhs = emit_load(context, lhs_loc, max_of(size, 4));
            emit_reg_extended_op(context, one_byte_opcode(SHIFT_OR_ROTATE_REGM_CL), reg_extension, lhs);
            lhs->size = size; // truncate it
            ret = lhs;
        }
        unlock_emit_location(rhs);
        free_emit_location(context, rhs);
    }
    return ret;
}

///////////////////////////////////////////////////////////////////////////////////////////////

func struct emit_location *emit_intrinsic(struct context *context, struct ast_function_call *call){
    assert(call->identifier_expression->kind == AST_identifier);
    struct ast_identifier *ident = cast(struct ast_identifier *)call->identifier_expression;
    struct intrinsic_info *info = lookup_intrinsic(ident->decl->identifier);
    
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
        case INTRINSIC_KIND_rdtsc:{
            assert(call->call_arguments.count == 0);
            enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
            enum register_encoding rdx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_D);
            
            // "read time-stamp counter into EDX:EAX" zeroes the top part of both
            emit_opcode(context, two_byte_opcode(READ_TIME_STAMP_COUNTER));
            struct emit_location *low  = emit_location_loaded(context, REGISTER_KIND_gpr, rax, 8);
            struct emit_location *high = emit_location_loaded(context, REGISTER_KIND_gpr, rdx, 8);
            
            
            emit_reg_extended_op(context, one_byte_opcode(SHIFT_OR_ROTATE_REGM_IMMEDIATE8), REG_OPCODE_SHIFT_LEFT, high);
            emit(32);
            emit_register_register(context, one_byte_opcode(OR_REG_REGM), low, high);
            free_emit_location(context, high);
            
            return low;
        }break;
        case INTRINSIC_KIND_pause:{
            emit(0xf3);
            emit(0x90);
            return emit_location_invalid(context);
        }break;
        
        // @cleanup: I feel like we can just do the same here... varify?
        case INTRINSIC_KIND_packed_double_op:
        case INTRINSIC_KIND_scalar_double_op:{
            assert(call->call_arguments.count == 2);
            
            struct emit_location *lhs = emit_code_for_ast(context, call->call_arguments.first->value);
            lhs->register_kind_when_loaded = REGISTER_KIND_xmm;
            assert(lhs->size == 16);
            lhs = emit_load(context, lhs, 16);
            struct emit_location *rhs = emit_code_for_ast(context, call->call_arguments.last->value);
            rhs->register_kind_when_loaded = REGISTER_KIND_xmm;
            rhs = emit_load(context, rhs, 16);
            
            emit(SSE_PREFIX_packed_double);
            emit_register_register(context, two_byte_opcode(info->opcode), lhs, rhs);
            free_emit_location(context, rhs);
            return lhs;
        }break;
        case INTRINSIC_KIND_set_scalar_double:{
            assert(call->call_arguments.count == 1);
            
            // @note: might be spilled by the 'emit_code_for_ast' below
            struct emit_location *loaded = emit_location_loaded(context, REGISTER_KIND_xmm, allocate_register(context, REGISTER_KIND_xmm), 16);
            // clear the register
            emit_register_register(context, two_byte_opcode(XOR_XMM), loaded, loaded);
            
            struct emit_location *arg = emit_code_for_ast(context, call->call_arguments.first->value);
            
            loaded = emit_load(context, loaded, 16); // if it was spilled, reload it
            if(arg->state == EMIT_LOCATION_register_relative){
                // @note: this is usually not tested right now because float literals get loaded immediatly
                //        @cleanup: document, why the get loaded immediatly, because I dont remember.
                //        could still be exercised for identifiers
                
                lock_emit_location(context, loaded);
                emit_register_relative_register(context, SSE_PREFIX_double, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), loaded->loaded_register, arg);
                unlock_emit_location(loaded);
            }else{
                arg = emit_load(context, arg, arg->size);
                //emit_register_register(context, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), loaded, arg);
                // @note: we use the internal version here to curcumvent the size assert
                //        this is at this point usual practice for any 'load'-type instructions
                emit(SSE_PREFIX_double);
                emit_register_op__internal(context, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), loaded->loaded_register, arg->loaded_register, 16);
                free_emit_location(context, arg);
            }
            return loaded;
        }break;

        case INTRINSIC_KIND_InterlockedCompareExchange64:{
            assert(call->call_arguments.count == 3);
            
            struct ast *_dest      = call->call_arguments.first->value;
            struct ast *_value     = call->call_arguments.first->next->value;
            struct ast *_comperand = call->call_arguments.last->value;
            
            assert(_dest->resolved_type->kind == AST_pointer_type);
            assert(_value->resolved_type      == &globals.typedef_s64);
            assert(_comperand->resolved_type  == &globals.typedef_s64);
            
            struct emit_location *dest      = emit_code_for_ast(context, _dest);
            struct emit_location *value     = emit_code_for_ast(context, _value);
            struct emit_location *comperand = emit_code_for_ast(context, _comperand);
            
            // "Compare RAX with r/m64 if equal set ZF and r64 is loaded into r/m64 
            //  else clear ZF and load r/m64 into RAX"
            enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
            comperand = emit_load_into_specific_gpr(context, comperand, rax);
            lock_emit_location(context, comperand);
            
            value = emit_load(context, value, 8);
            lock_emit_location(context, value);
            
            dest = emit_load(context, dest, 8);
            
            struct emit_location *ref = emit_location_register_relative(context, REGISTER_KIND_gpr, dest, null, 0, 8);      
            emit_register_relative_register(context, LEGACY_PREFIX_LOCK, two_byte_opcode(COMPARE_EXCHANGE_REG_REGM), value->loaded_register, ref);
            
            unlock_emit_location(value);
            unlock_emit_location(comperand);
            
            free_emit_location(context, value);
            free_emit_location(context, dest);
            
            // @note: returns the original value
            return comperand;
        }break;
        case INTRINSIC_KIND_InterlockedCompareExchange128:{
            // @cleanup: we could check cpuid if we have the feature...
            
            //unsigned char _InterlockedCompareExchange128(__int64 volatile * _Destination, __int64 _ExchangeHigh, __int64 _ExchangeLow, __int64 * _ComparandResult);
            assert(call->call_arguments.count == 4);
            
            struct ast *_dest       = call->call_arguments.first->value;
            struct ast *_value_high = call->call_arguments.first->next->value;
            struct ast *_value_low  = call->call_arguments.first->next->next->value;
            struct ast *_comperand  = call->call_arguments.last->value;
            
            struct emit_location *dest       = emit_code_for_ast(context, _dest);
            struct emit_location *value_high = emit_code_for_ast(context, _value_high);
            struct emit_location *value_low  = emit_code_for_ast(context, _value_low);
            struct emit_location *comperand  = emit_code_for_ast(context, _comperand);
            
            // "Compare RDX:RAX with m128. If equal, set ZF and load RCX:RBX into m128. 
            // Else, clear ZF and load m128 into RDX:RAX."
            
            // load the 'value' into RCX:RBX, and lock it
            enum register_encoding rcx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_C);
            value_high = emit_load_into_specific_gpr(context, value_high, rcx);
            lock_emit_location(context, value_high);
            enum register_encoding rbx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_B);
            value_low = emit_load_into_specific_gpr(context, value_low, rbx);
            lock_emit_location(context, value_low);
            
            // we allocate specifically r8 to the comperand, because we referance it twice, therefore 
            // we have to lock it, but we if it would be one of the other used registers this would not work.
            // @quality: if it is already loaded in a 'save' register, we could just lock that
            enum register_encoding r8 = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_R8);
            comperand = emit_load_into_specific_gpr(context, comperand, r8);
            lock_emit_location(context, comperand); // we have to lock it, because we referance it twice
            
            struct emit_location *comperand_low_ref = emit_location_register_relative(context, REGISTER_KIND_gpr, comperand, null, 0, 8);
            lock_emit_location(context, comperand_low_ref);
            
            struct emit_location *comperand_high_ref = emit_location_register_relative(context, REGISTER_KIND_gpr, comperand, null, 8, 8);
            lock_emit_location(context, comperand_high_ref);
            
            // load the comperand into RDX:RAX, and lock it
            enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
            struct emit_location *comperand_low = emit_load_into_specific_gpr(context, comperand_low_ref, rax);
            lock_emit_location(context, comperand_low);
            
            enum register_encoding rdx = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_D);
            struct emit_location *comperand_high = emit_load_into_specific_gpr(context, comperand_high_ref, rdx);
            lock_emit_location(context, comperand_high);
            
            dest = emit_load(context, dest, 8);
            struct emit_location *dest_ref = emit_location_register_relative(context, REGISTER_KIND_gpr, dest, null, 0, 8);
            
            emit_register_relative_extended(context, LEGACY_PREFIX_LOCK, two_byte_opcode(REG_EXTENDED_OPCODE_C7), C7_COMPARE_EXCHANGE_REGM128, dest_ref);
            
            // store the old values back
            emit_store(context, comperand_low_ref, comperand_low);
            emit_store(context, comperand_high_ref, comperand_high);
            
            // unlock and free everything
            unlock_emit_location(comperand);
            unlock_emit_location(value_high);
            unlock_emit_location(value_low);
            unlock_emit_location(comperand_low);
            unlock_emit_location(comperand_high);
            unlock_emit_location(comperand_low_ref);  // @note: no need to free, is just [comperand]
            unlock_emit_location(comperand_high_ref); // @note: no need to free, is just [comperand + 8]

            free_emit_location(context, comperand);
            free_emit_location(context, value_high);
            free_emit_location(context, value_low);
            free_emit_location(context, comperand_low);
            free_emit_location(context, comperand_high);
            
            free_emit_location(context, dest);
            
            return emit_location_conditional(context, COMP_equals);
        }break;
        case INTRINSIC_KIND_interlocked_inc_dec_64:{
            assert(call->call_arguments.count == 1);
            assert(call->call_arguments.first->value->resolved_type->kind == AST_pointer_type);
            struct emit_location *arg = emit_code_for_ast(context, call->call_arguments.first->value);
            
            arg = emit_load(context, arg, 8);
            struct emit_location *ref = emit_location_register_relative(context, REGISTER_KIND_gpr, arg, null, 0, 8);
            // @note: returns the incremented value
            emit_register_relative_extended(context, LEGACY_PREFIX_LOCK, one_byte_opcode(REG_EXTENDED_OPCODE_FF), info->opcode, ref);
            return ref;
        }break;
        case INTRINSIC_KIND_interlocked_fetch_op_64:{
            assert(call->call_arguments.count == 2);
            struct emit_location *dest = emit_code_for_ast(context, call->call_arguments.first->value);
            struct emit_location *addend = emit_code_for_ast(context, call->call_arguments.last->value);
            
            dest = emit_load(context, dest, 8);
            lock_emit_location(context, dest);
            
            addend = emit_load(context, addend, 8);
            lock_emit_location(context, addend);
            
            struct emit_location *dest_ref = emit_location_register_relative(context, REGISTER_KIND_gpr, dest, null, 0, 8);
            
            // "Exchange r/m64 with r64 and load their sum into r/m64"
            emit_register_relative_register(context, LEGACY_PREFIX_LOCK, two_byte_opcode(EXCHANGE_ADD_REGM_REG), addend->loaded_register, dest_ref);
            
            unlock_emit_location(dest);
            unlock_emit_location(addend);
            free_emit_location(context, dest);
            
            
            return addend;
        }break;
        invalid_default_case(return emit_location_invalid(context));
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////


func struct emit_location *emit_code_for_ast(struct context *context, struct ast *ast){
    assert(ast->resolved_type);
    
    //if(ast->s == 4642) os_debug_break();
    //if(ast->token->line == 570) os_debug_break();
    
    ast->byte_offset_in_function = to_s32(get_bytes_emited(context));
    
    switch(ast->kind){
        case AST_typedef: return emit_location_invalid(context);
        case AST_function: return emit_location_invalid(context);
        case AST_declaration:{
            struct ast_declaration *decl = cast(struct ast_declaration *)ast;
            
            if(decl->flags & DECLARATION_FLAGS_is_local_persist){
                // nothing to do here local_persists don't need to be initialized or allocated at local scope
                return emit_location_invalid(context);
            }
            
            // we cannot have a global declaration at local scope
            assert(!(decl->flags & DECLARATION_FLAGS_is_global));
            
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
            
            if(decl->flags & DECLARATION_FLAGS_is_enum_member){
                // @cleanup: can we even get in here? I thought we resolved them while parsing
                assert(decl->assign_expr);
                assert(decl->assign_expr->kind == AST_integer_literal);
                return emit_location_immediate(context, integer_literal_to_bytes(decl->assign_expr), decl->assign_expr->resolved_type->size);
            }
            
            struct emit_location *ret = null;
            if(decl->flags & (DECLARATION_FLAGS_is_global | DECLARATION_FLAGS_is_local_persist)){
                ret = emit_location_rip_relative(context, &decl->base, decl->type, decl->type->size);
            }else{
                assert(decl->offset_on_stack != -1); 
                // :MemoryLocations stack memory locations might be negative if the declaration is an argument, but they can never be -1.
                
                enum register_kind register_kind = get_register_kind_for_type(decl->type);
                
                // :MemoryLocations
                struct emit_location *stack_location = emit_location_stack_relative(context, register_kind, decl->offset_on_stack, ident->base.resolved_type->size);
                
                // :PassingStructArguments
                if(decl->flags & DECLARATION_FLAGS_is_big_function_argument){
                    stack_location->size = 8;
                    struct emit_location *loaded = emit_load(context, stack_location, 8);
                    ret = emit_location_register_relative(context, REGISTER_KIND_gpr, loaded, 0, 0, decl->type->size);
                }else{
                    ret = stack_location;
                }
            }
            
            if(ident->base.resolved_type->kind == AST_function_type){
                struct emit_location *address = emit_load_address(context, ret, allocate_register(context, REGISTER_KIND_gpr));
                free_emit_location(context, ret);
                ret = address;
            }
            
            return ret;
        }break;
        case AST_struct_or_array_literal:{
            struct ast_struct_or_array_literal *lit = cast(struct ast_struct_or_array_literal *)ast;
            struct ast_declaration *decl = lit->decl;
            
            // :struct_literal_location
            struct emit_location *decl_location;
            if(decl){
                decl_location = emit_location_stack_relative(context, REGISTER_KIND_gpr, decl->offset_on_stack, decl->type->size);
                assert(decl->type->kind == AST_struct || decl->type->kind == AST_union || decl->type->kind == AST_array_type);
                
                emit_memset(context, decl_location, 0);
            }else{
                decl_location = emit_location_invalid(context);
            }
            
            for_ast_list(lit->assignment_list){
                struct emit_location *loc = emit_code_for_ast(context, it->value);
                if(loc) free_emit_location(context, loc);
            }
            return decl_location;
        }break;
        case AST_member:{
            struct ast_dot_or_arrow *dot = cast(struct ast_dot_or_arrow *)ast;
            struct emit_location *loc = emit_code_for_ast(context, dot->lhs);
            assert(loc->state == EMIT_LOCATION_register_relative);
            
            loc->register_kind_when_loaded = get_register_kind_for_type(dot->base.resolved_type);
            
            loc->size      = dot->base.resolved_type->size;
            loc->offset   += dot->member_decl->offset_in_type;
            
            return loc;
        }break;
        case AST_member_deref:{
            struct ast_dot_or_arrow *arrow = cast(struct ast_dot_or_arrow *)ast;
            struct emit_location *loc = emit_code_for_ast(context, arrow->lhs);
            enum register_kind register_kind = get_register_kind_for_type(arrow->base.resolved_type);
            loc = emit_location_register_relative(context, register_kind, loc, null, arrow->member_decl->offset_in_type, arrow->base.resolved_type->size);
            
            return loc;
        }break;
        case AST_integer_literal:{
            return emit_location_immediate(context, integer_literal_to_bytes(ast), ast->resolved_type->size);
        }break;
        case AST_float_literal:{
            struct ast_float_literal *f = cast(struct ast_float_literal *)ast;
            struct emit_location *loc;
            if(globals.want_executable){
                if(context->current_function){
                    sll_push_back(context->current_function->float_literals, f);
                }
                loc = emit_location_rip_relative(context, &f->base, f->base.resolved_type, f->base.resolved_type->size);
            }else{
                // @cleanup: copied from string literal
                struct emit_location *immediate = emit_location_immediate(context, cast(u64)&f->base.token->_f64, 8);
                struct emit_location *loaded = emit_load(context, immediate, 8);
                loc = emit_location_register_relative(context, REGISTER_KIND_xmm, loaded, 0, 0, f->base.resolved_type->size);
            }
            
            return emit_load_float(context, loc);
        }break;
        case AST_string_literal:{
            struct ast_string_literal *lit = cast(struct ast_string_literal *)ast;
            
            struct emit_location *loc;
            if(globals.want_executable){
                // @note: the size (8) does not matter so we load a constant
                loc = emit_location_rip_relative(context, &lit->base, lit->base.resolved_type, 8);
            }else{
                struct emit_location *immediate = emit_location_immediate(context, cast(u64)lit->value->data, 8);
                struct emit_location *loaded = emit_load(context, immediate, 8);
                loc = emit_location_register_relative(context, REGISTER_KIND_gpr, loaded, 0, 0, 8);
            }
            
            return loc;
        }break;
        case AST_unary_address:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            
            // if it is a function or an array we have already loaded it.
            if((op->operand->resolved_type->kind != AST_function_type)){
                struct emit_location *loaded = emit_load_address(context, loc, allocate_register(context, REGISTER_KIND_gpr));
                free_emit_location(context, loc);
                loc = loaded;
            }
            
            return loc;
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
            struct emit_location *loaded = emit_load(context, loc, loc->size);
            
            u8 extension = (ast->kind == AST_unary_minus) ?  REG_OPCODE_NEGATE_REGM : REG_OPCODE_NOT_REGM;
            emit_reg_extended_op(context, one_byte_opcode(REG_EXTENDED_UNARY_REGM), extension, loaded);
            return loaded;
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
                
                assert(size > 0);
                assert(size <= 0xffffffff);
                u8 inst = is_big ? REG_EXTENDED_OPCODE_REGM_IMMIDIATE : REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                
                assert(loc->size == 8);
                struct emit_location *immediate = emit_location_immediate(context, size, is_big ? 4 : 1);
                emit_register_relative_immediate(context, 0, one_byte_opcode(inst), reg_inst, loc, immediate);
            }else{
                u8 inst = (ast->kind == AST_unary_preinc) ? FF_INCREMENT_REGM : FF_DECREMENT_REGM;
                emit_register_relative_extended(context, 0, one_byte_opcode(REG_EXTENDED_OPCODE_FF), inst, loc);
            }
            return emit_load(context, loc, loc->size);
        }break;
        case AST_unary_postdec:
        case AST_unary_postinc:{
            struct ast_unary_op *op = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, op->operand);
            assert(loc->state == EMIT_LOCATION_register_relative);
            
            lock_emit_location(context, loc);
            struct emit_location *loaded = emit_load(context, loc, loc->size);
            
            if(op->base.resolved_type->kind == AST_pointer_type){
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)op->base.resolved_type;
                u8 reg_inst = (ast->kind == AST_unary_postinc) ? REG_OPCODE_ADD : REG_OPCODE_SUB;
                smm size = pointer->pointer_to->size;
                b32 is_big = size > max_s8;
                
                assert(size > 0);
                assert(size <= 0xffffffff);
                u8 inst = is_big ? REG_EXTENDED_OPCODE_REGM_IMMIDIATE : REG_EXTENDED_OPCODE_REGM_SIGN_EXTENDED_IMMIDIATE8;
                
                assert(loc->size == 8);
                struct emit_location *immediate = emit_location_immediate(context, size, is_big ? 4 : 1);
                emit_register_relative_immediate(context, 0, one_byte_opcode(inst), reg_inst, loc, immediate);
            }else{
                u8 inst = (ast->kind == AST_unary_postinc) ? FF_INCREMENT_REGM : FF_DECREMENT_REGM;
                emit_register_relative_extended(context, 0, one_byte_opcode(REG_EXTENDED_OPCODE_FF), inst, loc);
            }
            unlock_emit_location(loc);
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
        if(ast->resolved_type == &globals.typedef_f32 || ast->resolved_type == &globals.typedef_f64){
            return emit_binary_op_xmm(context, ast, DIV_XMM);
        } /* fall through */
        case AST_binary_mod:{
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
        case AST_logical_not:{
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
            struct ast_binary_op *op = cast(struct ast_binary_op*)ast;
            struct jump_context jump_context = emit_code_for_if_condition(context, op->lhs);
            
            enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
            struct emit_location *immediate = emit_location_immediate(context, 1, 4);
            struct emit_location *loaded = emit_load_into_specific_gpr(context, immediate, reg);
            free_emit_location(context, loaded); // we 
            
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
            
            struct ast_binary_op *op = cast(struct ast_binary_op*)ast;
            struct jump_context jump_context = emit_code_for_if_condition(context, op->lhs);
            
            struct emit_location *rhs_loc = emit_code_for_plain_condition(context, op->rhs);
            struct emit_location *rhs = emit_load(context, rhs_loc, 4);
            
            struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            jump_context_emit(&jump_over_else, COMP_none);
            emit_end_jumps(jump_context);
            
            emit_register_register(context, one_byte_opcode(XOR_REG_REGM), rhs, rhs);
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
                lhs = emit_load(context, lhs, lhs->size);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    if(lhs->size == 8) emit(SSE_PREFIX_NON_PACKED_OP_double);
                    emit_register_register(context, two_byte_opcode(COMPARE_XMM), lhs, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    
                    enum prefix prefix; 
                    if(lhs->size == 8){
                        prefix = SSE_PREFIX_NON_PACKED_OP_double;
                    }else{
                        assert(lhs->size == 4);
                        prefix = SSE_PREFIX_NON_PACKED_OP_float;
                    }
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
            struct ast_conditional_expression *conditional = cast(struct ast_conditional_expression *)ast;
            // @note: conditional expressions are not l-values
            
            // @cleanup: this does not work with structs @incomplete:
            
            enum register_kind register_kind = get_register_kind_for_type(conditional->base.resolved_type);
            enum register_encoding reg = allocate_register(context, register_kind);
            
            struct jump_context jump_context = emit_code_for_if_condition(context, conditional->condition);
            struct emit_location *if_true = emit_code_for_ast(context, conditional->if_true);
            
            b32 is_float = (if_true->register_kind_when_loaded == REGISTER_KIND_xmm);
            
            struct emit_location *dumb;
            if(is_float){
                dumb = emit_load_float_into_specific_register(context, if_true, reg);
            }else{
                dumb = emit_load_into_specific_gpr(context, if_true, reg);
            }
            
            // we have to free it here because we re assign it in the second call, this is kinda dumb
            free_emit_location(context, dumb);
            
            struct jump_context jump_over_else = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
            jump_context_emit(&jump_over_else, COMP_none);
            
            emit_end_jumps(jump_context);
            struct emit_location *if_false = emit_code_for_ast(context, conditional->if_false);
            
            struct emit_location *loaded;
            if(is_float){
                loaded = emit_load_float_into_specific_register(context, if_false, reg);
            }else{
                loaded = emit_load_into_specific_gpr(context, if_false, reg);
            }
            
            free_emit_location(context, loaded);
            
            emit_end_jumps(jump_over_else);
            
            return emit_location_loaded(context, register_kind, reg, ast->resolved_type->size);
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
                struct emit_location *lhs = emit_code_for_ast(context, assign->lhs);
                struct ast_string_literal *lit = cast(struct ast_string_literal *)assign->rhs;    
                struct emit_location *rhs = emit_location_rip_relative(context, &lit->base, lit->base.resolved_type, lit->value->size + 1);
                
                emit_memcpy(context, lhs, rhs);
                return lhs;
            }
            
            struct emit_location *rhs = emit_code_for_ast(context, assign->rhs);
            if(rhs->state == EMIT_LOCATION_conditional) rhs = emit_load(context, rhs, rhs->size);
            struct emit_location *lhs = emit_code_for_ast(context, assign->lhs);
            
            emit_store(context, lhs, rhs);
            return lhs;
        }break;
        case AST_and_assignment:{
            return emit_compound_assignment(context, ast, REG_OPCODE_AND, AND_REGM8_REG8, AND_REGM_REG);
        }break;
        case AST_plus_assignment:{
            return emit_compound_assignment(context, ast, REG_OPCODE_ADD, ADD_REGM8_REG8, ADD_REGM_REG);
        }break;
        case AST_minus_assignment:{
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
                return emit_divide_or_mod_or_multiply(context, ast, 
                                                      REG_OPCODE_IMUL_REGM_RAX, REG_OPCODE_MUL_REGM_RAX, true);
            }
        }break;
        case AST_modulo_assignment:
        case AST_divide_assignment:{
            if(ast->resolved_type->kind == AST_float_type){
                return emit_compound_assignment_xmm(context, ast, DIV_XMM);
            }else{
                return emit_divide_or_mod_or_multiply(context, ast, 
                                                      REG_OPCODE_IDIV_REGM_RAX, REG_OPCODE_DIV_REGM_RAX, true);
            }
        }break;
        case AST_return:{
            struct ast_return *ret = cast(struct ast_return *)ast;
            
            context->gpr_allocator.rolling_index = REGISTER_A;
            // @note: this is not!!! the return type, as we only have to cast implicitly to it
            //struct ast_type *return_type = ret->expr->resolved_type;
            
            struct ast_type *return_type = context->current_function->type->return_type;
            if(return_type != &globals.typedef_void){
                struct emit_location *loc = emit_code_for_ast(context, ret->expr);
                // :ReturningStructs
                if(return_type->size > 8){
                    struct emit_location *location_of_rax = emit_location_stack_relative(context, REGISTER_KIND_gpr, -16, 8);
                    enum register_encoding rax = allocate_specific_register(context, REGISTER_KIND_gpr, REGISTER_A);
                    
                    struct emit_location *address_of_return_struct = emit_load_into_specific_gpr(context, location_of_rax, rax);
                    lock_emit_location(context, address_of_return_struct);
                    struct emit_location *dest = emit_location_register_relative(context, REGISTER_KIND_gpr, address_of_return_struct, 0, 0, return_type->size);
                    emit_memcpy(context, dest, loc);
                    free_emit_location(context, loc);
                    
                    unlock_emit_location(address_of_return_struct);
                    free_emit_location(context, address_of_return_struct);
                }else{
                    if(return_type->kind == AST_float_type){
                        struct emit_location *loaded = emit_load_float_into_specific_register(context, loc, XMM0);
                        free_emit_location(context, loaded);
                    }else{
                        // @sigh: we have to get good about when to return stuff in registers and 
                        //        when to return stuff with the above :ReturningStructs code.
                        // @cleanup: this assert does not work ast 'struct random_series{u64 val;}'
                        //           should probably be on the stack.
                        //assert(return_type->kind == AST_integer_type || return_type->kind == AST_pointer_type);
                        struct emit_location *loaded = emit_load_into_specific_gpr(context, loc, REGISTER_A);
                        free_emit_location(context, loaded);
                    }
                }
            }
            
            // :function_epilog
            jump_context_emit(context->jump_to_function_epilog, COMP_none);
            
            return emit_location_invalid(context);
        }break;
        case AST_scope:{
            struct ast_scope *scope = cast(struct ast_scope *)ast;
            
            for(struct ast_list_node *it = scope->statement_list.first; it; it = it->next){
                context->tempoary_stack_allocator = 0;
                struct tempoary_memory temp = begin_tempoary_memory(&context->scratch);
                
                struct emit_location *loc = emit_code_for_ast(context, it->value);
                if(loc) free_emit_location(context, loc);
                
                for(enum register_kind a = 0; a < REGISTER_KIND_count; a++){
                    for(u32 i = 0; i < array_count(context->register_allocators[a].emit_location_map); i++){
                        assert(context->register_allocators[a].emit_location_map[i] == 0);
                    }
                }
                
                end_tempoary_memory(temp);
            }
            
            scope->scope_end_byte_offset_in_function = to_u32(get_bytes_emited(context));
            
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
            switch_on = emit_load(context, switch_on, max_of(switch_on->size, 4));
            
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
                struct emit_location *imm = emit_code_for_ast(context, ast_case->expression);
                assert(imm->state == EMIT_LOCATION_immediate);
                
                b32 is_signed = type_is_signed(ast_case->expression->resolved_type);
                if(imm->size == 8 || ((imm->size == 4) && is_signed && imm->value > s32_max)){
                    struct emit_location *loaded = emit_load(context, imm, imm->size);
                    
                    emit_register_register(context, one_byte_opcode(CMP_REG_REGM), switch_on, loaded);
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
                    emit_reg_extended_op(context, one_byte_opcode(inst), REG_OPCODE_CMP, switch_on);
                    
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
            struct ast_case *ast_case = cast(struct ast_case *)ast;
            assert(ast_case->jump);
            emit_end_jumps(*ast_case->jump);
            
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
            smm jump_back_location = get_bytes_emited(context);
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
            
            ast_for->scope_for_decl->scope_end_byte_offset_in_function = to_u32(get_bytes_emited(context));
            
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
            (cast(struct ast_label *)ast)->byte_offset_in_function = to_u32(get_bytes_emited(context));
            return emit_location_invalid(context);
        }break;
        case AST_function_call:{
            struct ast_function_call *call = cast(struct ast_function_call *)ast;
            //if(ast->token->line == 1708) os_debug_break();
            
            
            struct ast_identifier *call_to_ident;
            struct ast_function_type *function_type;
            if(call->identifier_expression->resolved_type->kind == AST_function_type){
                assert(call->identifier_expression->kind == AST_identifier); // not that it matters
                function_type = cast(struct ast_function_type *)call->identifier_expression->resolved_type;
                call_to_ident = cast(struct ast_identifier *)call->identifier_expression;
            }else{
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)call->identifier_expression->resolved_type;
                assert(pointer->pointer_to->kind == AST_function_type);
                // @clenaup: maybe make an ice here
                function_type = cast(struct ast_function_type *)pointer->pointer_to;
                call_to_ident = false;
            }
            
            if(function_type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic){
                // @note: no need to spill registers for intrinsics
                return emit_intrinsic(context, call);
            }
            
            // spill all volitile registers, so they are saved
            for(u32 i = 0; i < array_count(volitile_registers); i++){
                enum register_encoding reg = volitile_registers[i];
                if(context->gpr_allocator.emit_location_map[reg]){
                    spill_register(context, REGISTER_KIND_gpr, reg);
                }
            }
            for(enum register_encoding reg = XMM0; reg < XMM6; reg++){
                if(context->xmm_allocator.emit_location_map[reg]){
                    spill_register(context, REGISTER_KIND_xmm, reg);                   
                }
            }
            
            smm function_argument_count = function_type->argument_list.count;
            smm arg_count = call->call_arguments.count;
            
            // :ReturningStructs
            // if the function returns a big struct, there is an implicit first argument, which is the 
            // address of the return value. We memcpy in 'case AST_return'.
            
            b32 returns_big_struct = (function_type->return_type->size > 8);
            if(returns_big_struct){
                arg_count += 1;
                function_argument_count += 1;
            }
            
            context->max_amount_of_function_call_arguments = max_of(context->max_amount_of_function_call_arguments, arg_count);
            // @cleanup: draw a picture of the stack
            
            // :ReturningStructs
            struct emit_location *stack_return_location = null;
            {   
                enum register_encoding argument_registers[] = {
                    REGISTER_C, 
                    REGISTER_D,
                    REGISTER_R8,
                    REGISTER_R9
                };
                
                struct emit_location **emit_locations = push_data(&context->scratch, struct emit_location *, arg_count);
                // @cleanup: we need this below... we could _re-iterate_ but dunno
                smm *argument_sizes = push_data(&context->scratch, smm, arg_count);
                
                // first 4 arguments are passed in rcx rdx r8 r9 in this order from left to right
                // all other arguments are passed on the stack
                {
                    smm arg_at = 0;
                    
                    // :ReturningStructs
                    if(returns_big_struct){
                        struct ast_type *return_type = function_type->return_type;
                        stack_return_location = emit_allocate_tempoary_stack_location(context, REGISTER_KIND_gpr, return_type->size, return_type->alignment);
                        
                        emit_locations[arg_at] = emit_load_address(context, stack_return_location, allocate_register(context, REGISTER_KIND_gpr));
                        arg_at++;
                    }
                    
                    struct ast_list_node *it = call->call_arguments.first;
                    struct ast_list_node *type_it = function_type->argument_list.first;
                    
                    for( ;it; arg_at++, it = it->next){
                        if(arg_at < array_count(argument_registers)){
                            context->gpr_allocator.rolling_index = argument_registers[arg_at];
                        }
                        
                        struct emit_location *loc = emit_code_for_ast(context, it->value);
                        if(loc->state == EMIT_LOCATION_conditional) loc = emit_load(context, loc, loc->size);
                        
                        struct ast_type *type = null;
                        if(!type_it){
                            assert(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs);
                            assert(arg_at >= function_argument_count);
                            type = it->value->resolved_type;
                        }else{
                            assert(type_it->value->kind == AST_declaration);
                            type = (cast(struct ast_declaration*)type_it->value)->type;
                            type_it = type_it->next;
                        }
                        
                        if(type->size > 8){ // :PassingStructArguments
                            // big struct arguments are copied by the caller and passed on the stack
                            assert(type->kind == AST_struct || type->kind == AST_union);
                            
                            struct emit_location *copy_into = emit_allocate_tempoary_stack_location(context, REGISTER_KIND_gpr, type->size, type->alignment);
                            
                            emit_memcpy(context, copy_into, loc);
                            free_emit_location(context, loc);
                            loc = emit_load_address(context, copy_into, allocate_register(context, REGISTER_KIND_gpr));
                        }
                        
                        // @cleanup: what happens for like struct{b8 asd[3]}; is this passed by value?
                        emit_locations[arg_at] = loc;
                        argument_sizes[arg_at] = min_of(it->value->resolved_type->size, 8);
                    }
                }
                
                {   // store all 'emit_locations' that are not passed in registers onto the stack
                    smm stack_pass_location = 0x20;
                    for(u32 arg_at = array_count(argument_registers); arg_at < arg_count; arg_at++){
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
                for(u32 i = 0; i < array_count(argument_registers) && i < arg_count; i++){
                    
                    if(emit_locations[i]->state == EMIT_LOCATION_loaded){
                        if(emit_locations[i]->loaded_register == argument_registers[i]){
                            lock_emit_location(context, emit_locations[i]);
                            continue;
                        }
                    }
                    
                    enum register_encoding arg_reg = allocate_specific_register(context, emit_locations[i]->register_kind_when_loaded, argument_registers[i]);
                    
                    // @cleanup: maybe do a emit_load_into_specific_register, that takes a register kind?
                    if(emit_locations[i]->register_kind_when_loaded == REGISTER_KIND_gpr){
                        emit_locations[i] = emit_load_into_specific_gpr(context, emit_locations[i], arg_reg);
                    }else{
                        assert(emit_locations[i]->register_kind_when_loaded == REGISTER_KIND_xmm);
                        emit_locations[i] = emit_load_float_into_specific_register(context, emit_locations[i], arg_reg);
                        // if this is a varargs argument (and a float argument) we actually pass it in a general
                        // purpose register
                        if(i >= function_argument_count){
                            assert(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs);
                            struct emit_location *float_reg = emit_locations[i];
                            assert(float_reg->size == 8);
                            
                            enum register_encoding gpr_reg = allocate_specific_register(context, REGISTER_KIND_gpr, argument_registers[i]);
                            emit_locations[i] = emit_location_loaded(context, REGISTER_KIND_gpr, gpr_reg, 8);
                            // movq gpr, xmm
                            
                            // @cleanup: holy fuck, I really don't understand the prefix convention here...
                            emit(0x66); // prefix to load the _lower_ half (mm|xmm) and we want xmm I guess..
                            emit_register_op__internal(context, two_byte_opcode(MOVQ_REGM_XMM), float_reg->loaded_register, emit_locations[i]->loaded_register, 8);
                            free_emit_location(context, float_reg);
                        }
                    }
                    
                    lock_emit_location(context, emit_locations[i]);
                }
                
                // free all 'argument_locations'
                for(u32 i = 0; i < array_count(argument_registers) && i < arg_count; i++){
                    unlock_emit_location(emit_locations[i]);
                    free_emit_location(context, emit_locations[i]);
                }
            }
            
            
            // :patches_are_32_bit
            // @note: if we are here that means that this procedure is not yet emited. Therefore it has to be a procedure we emit for. now we will only allow .text sections to be at most ((2 << 31) - 1) big, so that we can use relative calls everywhere. Thus we know that this is a realive call. -15.19.19
            // @note: if we want an executable, all patches calls to known locations are in fact 32 bit.
            //        and we have to patch them all as we later copy the code into the .text section -16.10.19
            // @note now that we are emiting into a list now, so we have to move the memory in the end so we have to patch in every case -2.12.19
            
            if(call_to_ident){
                if(function_type->flags & FUNCTION_TYPE_FLAGS_is_dllimport){
                    struct emit_location *function_location = emit_location_rip_relative(context, &call_to_ident->decl->base, call_to_ident->decl->type, 8);
                    emit_register_relative_extended(context, 0, one_byte_opcode(REG_EXTENDED_OPCODE_FF), FF_CALL_REGM, function_location);
                }else{
                    emit(CALL_RELATIVE);
                    smm patch_offset = emit_bytes(context, sizeof(s32), 0);
                    
                    // @note: we have to lookup the identifier either here or in patching, I think if makes more sense to do it here, then in patching because string literals and static variables do not want that lookup. -3.1.2020
                    
                    emit_patch(context, PATCH_rip_relative, &call_to_ident->decl->base, 0, &context->current_function->as_decl, patch_offset, patch_offset + 4);
                }
            }else{
                struct emit_location *function_location = emit_code_for_ast(context, call->identifier_expression);
                
                switch(function_location->state){
                    case EMIT_LOCATION_register_relative:{
                        emit_register_relative_extended(context, 0, one_byte_opcode(REG_EXTENDED_OPCODE_FF), FF_CALL_REGM, function_location);
                    }break;
                    case EMIT_LOCATION_loaded:{
                        emit_reg_extended_op(context, one_byte_opcode(REG_EXTENDED_OPCODE_FF), FF_CALL_REGM, function_location);
                    }break;
                    invalid_default_case();
                }
                free_emit_location(context, function_location);
            }
            
            if(function_type->return_type == &globals.typedef_void){
                return emit_location_invalid(context);
            }
            
            // :ReturningStructs
            if(stack_return_location){
                return stack_return_location;
            }
            
            struct ast_type *return_type = function_type->return_type;
            
            // if we are a compound type or whatever we have return a 'register_relative' emit location, so we
            // spill rax to the stack.
            if(return_type->kind == AST_struct || return_type->kind == AST_union || return_type->kind == AST_array_type){
                assert(return_type->size <= 8); // otherwise it should have allready been handled
                struct emit_location *ret = emit_allocate_tempoary_stack_location(context, REGISTER_KIND_gpr, 8, 8);
                struct emit_location *rax = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_A, 8);
                // @cleanup: 
                ret->size = return_type->size;
                emit_store(context, ret, rax);
                return ret;
            }
            
            if(return_type->kind == AST_float_type){
                return emit_location_loaded(context, REGISTER_KIND_xmm, XMM0, return_type->size);
            }
            
            assert(return_type->kind == AST_integer_type || return_type->kind == AST_pointer_type);
            return emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_A, return_type->size);
        }break;
        case AST_cast:{
            struct ast_unary_op *cast = cast(struct ast_unary_op *)ast;
            struct emit_location *loc = emit_code_for_ast(context, cast->operand);
            struct ast_type *cast_to = cast->base.resolved_type;
            
            if(cast->operand->resolved_type == cast_to){
                return loc;
            }
            
            if(cast_to == &globals.typedef_void) {
                // dont free if (void)(void) a // @cleanup: is this still necessary after the check above?
                if(loc) free_emit_location(context, loc); 
                return emit_location_invalid(context);
            }
            
            if(loc->state != EMIT_LOCATION_register_relative){
                loc = emit_load(context, loc, loc->size);
            }
            
            if(loc->register_kind_when_loaded == REGISTER_KIND_xmm){
                if(cast_to == &globals.typedef_f32 || cast_to == &globals.typedef_f64){
                    assert(cast->operand->resolved_type->kind == AST_float_type);
                    
                    // @cleanup: is this the wrong way around, which argument is this prefix for?
                    enum prefix sse_prefix = (cast->operand->resolved_type == &globals.typedef_f32) ? SSE_PREFIX_float : SSE_PREFIX_double;
                    
                    //cvtsd2ss - convert_scalar_double_to_scalar_single
                    //cvtss2sd - convert_scalar_single_to_scalar_double
                    if(loc->state == EMIT_LOCATION_register_relative){
                        emit_register_relative_register(context, sse_prefix, two_byte_opcode(0x5A), loc->loaded_register, loc);
                    }else{
                        emit(sse_prefix); // ps or pd
                        emit_register_op__internal(context, two_byte_opcode(0x5A), loc->loaded_register, loc->loaded_register, 4);
                    }
                    loc->size = cast_to->size;
                    return loc;
                }
                
                // otherwise it is of integer type (cast : float -> int)
                // @cleanup: negative floating point to unsinged? (it is undefined, but what do we do?)
                struct emit_location *loaded = emit_location_loaded(context, REGISTER_KIND_gpr, allocate_register(context, REGISTER_KIND_gpr), cast_to->size);
                
                enum prefix sse_prefix = (loc->size == 4) ? SSE_PREFIX_float : SSE_PREFIX_double; 
                // cvttss2si
                if(loc->state == EMIT_LOCATION_register_relative){
                    loc->size = cast_to->size; // make sure we emit rexw if we should
                    emit_register_relative_register(context, sse_prefix, two_byte_opcode(0x2c), loaded->loaded_register, loc);
                }else{
                    emit_register_op__internal(context, two_byte_opcode(0x2c), loaded->loaded_register, loc->loaded_register, cast_to->size);
                }
                free_emit_location(context, loc);
                loc = loaded; // we might have to truncate below.
            }
            
            assert(loc->register_kind_when_loaded == REGISTER_KIND_gpr);
            
            // if it is in a gpr always load it to the desired size first
            if(loc->register_kind_when_loaded == REGISTER_KIND_gpr){
                b32 result_is_gpr = cast_to->kind == AST_integer_type || cast_to->kind == AST_pointer_type;
                if(result_is_gpr && loc->size >= cast_to->size){
                    // only truncate if we cast from integer to integer (or pointer)
                    // :little_endian
                    // nothing to change here, just truncate
                    loc->size = cast_to->size;
                    return loc;
                }else{
                    // extend into a register
                    struct opcode opcode = get_opcode_for_move_instruction_and_adjust_size(loc, cast_to->size, type_is_signed(cast->operand->resolved_type));
                    
                    if(loc->state == EMIT_LOCATION_register_relative){
                        enum register_encoding reg = allocate_register(context, REGISTER_KIND_gpr);
                        emit_register_relative_register(context, 0, opcode, reg, loc);
                        free_emit_location(context, loc);
                        loc = emit_location_loaded(context, REGISTER_KIND_gpr, reg, cast_to->size);
                    }else{
                        emit_register_register(context, opcode, loc, loc);
                        loc->size = cast_to->size;
                    }
                }
            }
            assert(loc->size == cast_to->size);
            assert(loc->state == EMIT_LOCATION_loaded);
            
            // @note: I could produce better code here by not first loading the value into a gpr and then 
            //        loading it into a float, skipping a load.
            // cast: int -> float
            if(cast_to == &globals.typedef_f32 || cast_to == &globals.typedef_f64){    
                enum register_encoding reg = allocate_register(context, REGISTER_KIND_xmm);
                struct emit_location *ret = emit_location_loaded(context, REGISTER_KIND_xmm, reg, cast_to->size);
                assert(loc->size == 4 || loc->size == 8);
                //cvtsi2ss
                //cvtsi2sd
                if(loc->size == 8) emit(REXW);
                emit((cast_to == &globals.typedef_f32) ? SSE_PREFIX_float : SSE_PREFIX_double);
                emit_register_op__internal(context, two_byte_opcode(0x2A), ret->loaded_register, loc->loaded_register, 4);
                free_emit_location(context, loc);
                return ret;
            }
            
            // else 'cast_to' and 'operand' are of integer type, so we are just done as we extended it above
            return loc;
        }break;
        
        invalid_default_case(return null);
    }
    //invalid_code_path;
}

func void emit_code_for_function(struct context *context, struct ast_function *function){
    //if(function->base.s == 1580) os_debug_break();
    //if(string_match(*function->identifier, string("static_if_evaluate_bitwise"))) os_debug_break();
    
    context->current_function = function;
    context->max_amount_of_function_call_arguments = 4;
    context->tempoary_stack_high_water_mark = 0;
    
    // stack layout before we allocate stack memory
    // | arg n | ... |  arg 1  | arg 0 | ret ptr | memory for the function | 
    //                [rsp+16]  [rsp+8]   [rsp]  ^rsp
    // see below how memory  for the function is layed out. -12.02.2020
    
    // "The first four integer or pointer parameters are passed in the rcx, rdx, r8, and r9 registers."
    // we have to save these in their slots

    enum register_encoding argument_registers[4] = {
        [0] = REGISTER_C,
        [1] = REGISTER_D,
        [2] = REGISTER_R8,
        [3] = REGISTER_R9, 
    };
    
    context->gpr_allocator.rolling_index = REGISTER_C;
    
    b32 do_first_loop_for_a_big_return = false;
    if(function->type->return_type->size > 8){ // :ReturningStructs
        do_first_loop_for_a_big_return = true;
    }
    
    // set the current code section to the prolog
    u8 *prolog_start = context->emit_pool.current;
    context->current_emit_base = prolog_start;
    function->base_of_prolog = prolog_start;
    
    emit(PUSH_REGISTER_BP);
    emit_reg_reg__(context, REXW, MOVE_REG_REGM, REGISTER_BP, REGISTER_SP);
    
    {         
        smm stack_at = -16; // at offset zero is the return pointer.
        
        struct ast_list_node *it = function->is_defined->type->argument_list.first;
        for(u32 i = 0; i < array_count(argument_registers) || it; i++){
            if(!do_first_loop_for_a_big_return && !it && !(function->type->flags & FUNCTION_TYPE_FLAGS_is_varargs)) break;
            
            // @cleanup: there must be a better way to factor this...
            
            // save the first 4 register if we need them
            if(i < array_count(argument_registers)){
                enum register_encoding at = argument_registers[i];
                
                enum register_kind register_kind = REGISTER_KIND_gpr;
                if(it){ // if it is varargs all further arguments are in gprs
                    struct ast_declaration *decl = (struct ast_declaration *)it->value;
                    register_kind = get_register_kind_for_type(decl->type);    
                }
                
                
                struct emit_location *dest = emit_location_stack_relative(context, register_kind, stack_at, 8);
                
                struct emit_location *source = emit_location_loaded(context, register_kind, allocate_specific_register(context, register_kind, at), 8);
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
            }
            
            // :MSVC_function_call_stack_increase
            stack_at -= 8;
        }
    }
    
    emit_reg_reg__(context, REXW, REG_EXTENDED_OPCODE_REGM_IMMIDIATE, REG_OPCODE_SUB, REGISTER_SP);
    u32 *stack_space_subtract_address = cast(u32 *)context->emit_pool.current;
    emit_u32(0);
    
    function->rsp_subtract_offset = get_bytes_emited(context);
    
    // stack layout:
    // high addresses ----------------------------------------------------------- low adresses
    // | return value | old rbp | declarations | tempoary stack | stack of the next function |
    // rbp ---------------------^
    // rsp -----------------------------------------------------^
    //                                                                             -12.02.2020
    
    function->size_of_prolog = get_bytes_emited(context);
    
    // main code section
    u8 *main_function_start = context->emit_pool.current;
    context->current_emit_base = main_function_start;
    function->base_of_main_function = main_function_start;
    
    struct jump_context jump_to_function_epilog = emit_begin_jumps(context, JUMP_CONTEXT_jump_on_true);
    context->jump_to_function_epilog = &jump_to_function_epilog;
    
    
    // all other arguments are allready in their place
    
    // LET'S GO:
    ////////////////////////////////////////////////
    emit_code_for_ast(context, function->scope);
    ////////////////////////////////////////////////
    
    
    // amount of stack needed for the declarations in the scope
    smm stack_memory_needed = context->current_emit_offset_of_rsp;
    
    // "Space is allocated on the call stack as a shadow store for callees to save those registers."
    u32 shadow_space = 32;
    stack_memory_needed += shadow_space;
    
    stack_memory_needed += to_u32(context->tempoary_stack_high_water_mark);
    stack_memory_needed += 8 * context->max_amount_of_function_call_arguments;
    
    // "Most structures are aligned to their natural alignment. The primary exceptions are the stack pointer and malloc or alloca memory, which are aligned to 16 bytes in order to aid performance"
    function->stack_space_needed = align_up(stack_memory_needed, 16);
    *stack_space_subtract_address = to_u32(function->stack_space_needed);
    
    
    // :function_epilog we jump to here now, instead of returning on the spot, this is so we can get canonical stack framing.
    emit_end_jumps(jump_to_function_epilog);
    
    // deallocate stack memory
    emit_reg_reg__(context, REXW, REG_EXTENDED_OPCODE_REGM_IMMIDIATE, REG_OPCODE_ADD, REGISTER_SP);
    emit_u32(function->stack_space_needed);
    
    emit(POP_REGISTER_BP);
    emit(NEAR_RET_INSTRUCTION);
    
    for_ast_list(context->goto_list){
        struct ast_goto *ast_goto = cast(struct ast_goto *)it->value;
        jump_node_end_jump(ast_goto->jump_node, ast_goto->label_to_goto->byte_offset_in_function);
    }
    
    function->byte_size_without_prolog = get_bytes_emited(context);
    
    smm function_size = align_up(function->size_of_prolog + function->byte_size_without_prolog, 16);
    
    // :AllocateFunctionLocation
    function->offset_in_text_section = atomic_add(&globals.text_section_offset_allocator, function_size);
    function->byte_size = function_size;
}


#undef emit
#undef emit_u16
#undef emit_u32
#undef emit_u64
