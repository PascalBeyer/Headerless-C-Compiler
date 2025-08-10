
//
// This file is responsible to emit the code for 'struct ast_asm_block', this can happen in two different scenarios:
//    1) Called in an actual __asm__ { ... } block.
//    2) calling an '__declspec(inline_asm)' function.
// This means that this might be called both at statement level and at expression level.
// If it is called at statement level there might be references to declarations in the function scope.
// If it is called at expression level the arguments of that function are in registers and might have to be spilled.
// In either case global variables can be referenced.
//                                                                                                       26.03.2022


//
// :asm_block_use_allocate_specific_register_but_disable_the_register_allocator @cleanup: rework this comment, now we do use the allocator -.-
// 
//
// Because 'emit_inline_asm_block' can be called at expression level, we have to call 'allocate_specific_register'
// whenever we are using a register, but this only has to happen for '__declspec(inline_asm)'.
// Further, also only in this case we have to worry about function arguments.
// On the other hand, we want to avoid using the rest of the register allocation system, as there is no register
// allocation here, all registers are specifically requested by the user!
//                                                                                           27.03.2022

struct inline_asm_function_argument{
    struct inline_asm_function_argument *next;
    
    // the function argument declaration, this is here to lookup
    struct ast_declaration *declaration;
    
    // The emit_location the argument was loaded into
    struct emit_location *loaded_location;
    
    // If the argument was an integer, then the integer is supplied in this argument as well.
    struct emit_location *integer_location;
};

// @cleanup: maybe pass an argument here to warn, if the instruction reads a register that should have been allocated already, but was not.
// @cleanup: name!
// @cleanup: name!
// @cleanup: name!
// @cleanup: name!
// @cleanup: name!
// @cleanup: name!
func struct emit_location *asm_block_load_registers_which_was_used_by_user(struct context *context, enum register_kind register_kind, enum register_encoding reg, smm size){
    //:inline_asm_user_referenced_registers
    struct emit_location *loc = context->register_allocators[register_kind].emit_location_map[reg];
    if(loc && loc->inline_asm__was_used_by_user){
        // :asm_block_the_same_register_with_different_sizes
        // 
        // We copy the 'loc' here and return it. 
        // This copy is not pointed to by the register allocator.
        // We need this for instructions like 'movsx eax, al'. 
        // Or more specifically it was biting me for
        //     vinsertf128 ymm0, ymm1, xmm0, 1
        // here the xmm0, overwrote the 32-size and it emitted an invalid instruction.
        loc->size = max_of(size, loc->size);
        
        struct emit_location *ret = push_struct(&context->scratch, struct emit_location);
        *ret = *loc;
        ret->size = size;
        return ret;
    }
    
    if(loc){
        spill_register(context, register_kind, reg);
    }
    
    loc = emit_location_loaded(context, register_kind, reg, size);
    loc->inline_asm__was_used_by_user = true;
    return loc;
}

// @cleanup: if we keep this function, eventually remove the '_' currently here to clean old usages!
func struct emit_location *_asm_block_resolve_and_allocate_operand(struct context *context, struct asm_instruction *instruction, smm operand_index){
    struct asm_operand *operand = &instruction->operands[operand_index];
    
    switch(operand->kind){
        case ASM_ARG_declaration_dereference:
        case ASM_ARG_declaration_reference:{
            // Two different scenarios:
            //     1) the operand is 'stack relative' or 'rip relative'.
            //     2) the operand is a '__declspec(inline_asm)' function argument and therefore either a 'register', 
            //        'spilled' or 'immediate'.
            
            struct emit_location *ret = null;
            struct ir *expression = operand->expr.ir;
            
            if(context->in_inline_asm_function && expression->kind == IR_identifier){
                struct ir_identifier *ident = (struct ir_identifier *)expression;
                
                for(struct inline_asm_function_argument *argument = context->inline_asm_function_arguments.first;
                        argument; argument = argument->next){
                    if(argument->declaration == ident->decl){
                        
                        struct ast_declaration *decl = ident->decl;
                        struct emit_location *loaded_location  = argument->loaded_location;
                        struct emit_location *integer_location = argument->integer_location;
                        
                        if(operand->is_inline_asm_function_argument_that_needs_to_be_an_integer){
                            if(!integer_location){
                                
                                // 
                                // Figure out the token of the function call in a stupid way.
                                // 
                                struct token *token = null;
                                for(struct declaration_reference_node *node = context->current_function->referenced_declarations.first; node; node = node->next){
                                    if(node->declaration == &context->current_inline_asm_function->as_decl){
                                        token = node->token;
                                        break;
                                    }
                                }
                                
                                assert(token);
                                
                                begin_error_report(context);
                                report_error(context, token, "Argument '%.*s' of '__declspec(inline_asm)' function needs to be an integer literal, because it contains an instruction which only supports integer literals.", decl->identifier->size, decl->identifier->data);
                                report_error(context, instruction->token, "... Here is the instruction that only supports integer literals.");
                                end_error_report(context);
                                
                                integer_location = emit_location_immediate(context, decl->type->size, 0);
                            }
                            
                            ret = integer_location;
                        }else{
                            if(integer_location && operand->is_inline_asm_function_argument_that_can_be_integer){
                                ret = integer_location;
                            }else{
                                
                                //
                                // This is part of case '2)' (globals will never be cached in registers and 
                                // this is all local to the '__declspec(inline_asm)' function).
                                // Therefore, this is either a register, or spilled.
                                //
                                ret = loaded_location;
                            }
                        }
                        break;
                    }
                }
                
                // It was not an 'inline_asm_function_argument' it might be rip-relative below!
            }
            
            if(!ret){
                //
                // We are in case '1)' either a 'stack relative' or 'rip relative' variable.
                // Evaluate the expression, which should only be members and one identifier.
                //
                smm bytes_emitted_for_assert = get_bytes_emitted(context);
                ret = get_emit_location_for_identifier(context, expression); // :ir_refactor - This should also handle . and []
                assert(bytes_emitted_for_assert == get_bytes_emitted(context));
                assert(ret->state == EMIT_LOCATION_register_relative);
            }
            
            if(operand->kind == ASM_ARG_declaration_dereference){
                // @cleanup: in the future 'index' might not always be null.
                ret = emit_location_register_relative(context, ret, null, 0, operand->size);
            }
            
            return ret;
        }break;
        case ASM_ARG_register:{
            return asm_block_load_registers_which_was_used_by_user(context, operand->register_kind_when_loaded, operand->reg, operand->size);
        }break;
        case ASM_ARG_memory_operand:{
            struct emit_location *base  = asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, operand->base, 8);
            struct emit_location *index = null;
            if(operand->index != INVALID_REGISTER){
                index = asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, operand->index, 8);
            }
            
            struct emit_location *ret = emit_location_register_relative(context, base, index, operand->offset, operand->size);
            
            switch(operand->scale){
                case 1: ret->log_index_scale = 0; break;
                case 2: ret->log_index_scale = 1; break;
                case 4: ret->log_index_scale = 2; break;
                case 8: ret->log_index_scale = 3; break;
            }
            
            return ret;
        }break;
        case ASM_ARG_immediate:{
            return emit_location_immediate(context, operand->immediate, operand->size);
        }break;
        invalid_default_case();
    }
    
    invalid_code_path;
}

// If we reference a variable like 
//     movzx a, byte ptr [rax]
// we tempoary allocate a register for 'a' (or complain if we can't) and then in the end
// store this tempoary register back into 'a', If a is not loaded to begin with.
//                                                                 06.04.2022

func void emit_inline_asm_block(struct context *context, struct ir_asm_block *asm_block){
#define allocate_specific_register "This is a syntax error to remind you that you are not allowed to use this function here!"
    
    for(struct asm_instruction *inst = asm_block->instructions.first; inst; inst = inst->next){
        context->inline_asm_mode = inst->token;
        
        if(!context->in_inline_asm_function){
            //
            // Avoid writing to a global that multiple threads could look at, as this could be bad for caching!
            // if its an actual __asm__-block we record the offset here to be able to step in the debugger.
            //
            inst->byte_offset_in_function = to_s32(get_bytes_emitted(context));
        }
        
        struct emit_location *operands[8] = zero_struct;
        assert(inst->amount_of_operands < array_count(operands));
        
        //
        // Load all operands into 'emit_locations' this makes sure all used registers are allocated.
        //
        for(smm operand_index = 0; operand_index < inst->amount_of_operands; operand_index++){
            operands[operand_index] = _asm_block_resolve_and_allocate_operand(context, inst, operand_index);
        }
        
        struct emit_location *operand = operands[0]; // one operand operations
        struct emit_location *lhs = operands[0]; // two operand operations
        struct emit_location *rhs = operands[1];
        
        struct prefixes user_prefixes = create_prefixes(inst->prefixes);
        
        switch(inst->memonic){
            
            static enum legacy_prefixes memonic_to_prefix[] = {
                
                [MEMONIC_unpcklps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_unpcklpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_unpckhps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_unpckhpd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_cmpeqss] = ASM_PREFIX_SSE_float, [MEMONIC_cmpeqps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpeqsd] = ASM_PREFIX_SSE_double, [MEMONIC_cmpeqpd] = ASM_PREFIX_SSE_packed_double,  
                [MEMONIC_cmpltss] = ASM_PREFIX_SSE_float, [MEMONIC_cmpltps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpltsd] = ASM_PREFIX_SSE_double, [MEMONIC_cmpltpd] = ASM_PREFIX_SSE_packed_double, 
                [MEMONIC_cmpless] = ASM_PREFIX_SSE_float, [MEMONIC_cmpleps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmplesd] = ASM_PREFIX_SSE_double, [MEMONIC_cmplepd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_cmpneqss] = ASM_PREFIX_SSE_float, [MEMONIC_cmpneqps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpneqsd] = ASM_PREFIX_SSE_double, [MEMONIC_cmpneqpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_cmpnltss] = ASM_PREFIX_SSE_float, [MEMONIC_cmpnltps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpnltsd] = ASM_PREFIX_SSE_double, [MEMONIC_cmpnltpd] = ASM_PREFIX_SSE_packed_double, 
                [MEMONIC_cmpnless] = ASM_PREFIX_SSE_float, [MEMONIC_cmpnleps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpnlesd] = ASM_PREFIX_SSE_double, [MEMONIC_cmpnlepd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_cmpordss]   = ASM_PREFIX_SSE_float, [MEMONIC_cmpordps]   = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpordsd]   = ASM_PREFIX_SSE_double, [MEMONIC_cmpordpd]   = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_cmpunordss] = ASM_PREFIX_SSE_float, [MEMONIC_cmpunordps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmpunordsd] = ASM_PREFIX_SSE_double, [MEMONIC_cmpunordpd] = ASM_PREFIX_SSE_packed_double, 
                
                [MEMONIC_cmpps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cmppd] = ASM_PREFIX_SSE_packed_double, [MEMONIC_cmpss] = ASM_PREFIX_SSE_float, [MEMONIC_cmpsd] = ASM_PREFIX_SSE_double,
                
                [MEMONIC_rcpps]   = ASM_PREFIX_SSE_packed_float, [MEMONIC_rcpss]   = ASM_PREFIX_SSE_float, 
                [MEMONIC_rsqrtps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_rsqrtss] = ASM_PREFIX_SSE_float, 
                
                [MEMONIC_andps]  = ASM_PREFIX_SSE_packed_float, [MEMONIC_andpd]  = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_xorps]  = ASM_PREFIX_SSE_packed_float, [MEMONIC_xorpd]  = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_orps]   = ASM_PREFIX_SSE_packed_float, [MEMONIC_orpd]   = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_andnps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_andnpd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_sqrtss] = ASM_PREFIX_SSE_float, [MEMONIC_sqrtsd] = ASM_PREFIX_SSE_double, [MEMONIC_sqrtps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_sqrtpd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_maxss] = ASM_PREFIX_SSE_float, [MEMONIC_maxsd] = ASM_PREFIX_SSE_double, [MEMONIC_maxps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_maxpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_minss] = ASM_PREFIX_SSE_float, [MEMONIC_minsd] = ASM_PREFIX_SSE_double, [MEMONIC_minps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_minpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_addss] = ASM_PREFIX_SSE_float, [MEMONIC_addsd] = ASM_PREFIX_SSE_double, [MEMONIC_addps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_addpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_subss] = ASM_PREFIX_SSE_float, [MEMONIC_subsd] = ASM_PREFIX_SSE_double, [MEMONIC_subps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_subpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_divss] = ASM_PREFIX_SSE_float, [MEMONIC_divsd] = ASM_PREFIX_SSE_double, [MEMONIC_divps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_divpd] = ASM_PREFIX_SSE_packed_double,
                [MEMONIC_mulss] = ASM_PREFIX_SSE_float, [MEMONIC_mulsd] = ASM_PREFIX_SSE_double, [MEMONIC_mulps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_mulpd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_haddps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_haddpd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_movmskps] = ASM_PREFIX_SSE_packed_float, [MEMONIC_movmskpd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_cvtsi2ss] = ASM_PREFIX_SSE_float, [MEMONIC_cvtsi2sd] = ASM_PREFIX_SSE_double, 
                [MEMONIC_cvttss2si] = ASM_PREFIX_SSE_float, [MEMONIC_cvttsd2si] = ASM_PREFIX_SSE_double, 
                [MEMONIC_cvtss2si] = ASM_PREFIX_SSE_float, [MEMONIC_cvtsd2si] = ASM_PREFIX_SSE_double, 
                
                [MEMONIC_cvtps2pd] = ASM_PREFIX_SSE_packed_float, [MEMONIC_cvtpd2ps] = ASM_PREFIX_SSE_packed_double, [MEMONIC_cvtss2sd] = ASM_PREFIX_SSE_float, [MEMONIC_cvtsd2ss] = ASM_PREFIX_SSE_double, 
                
                [MEMONIC_ucomiss] = ASM_PREFIX_none, [MEMONIC_ucomisd] = ASM_PREFIX_66,
                [MEMONIC_comiss]  = ASM_PREFIX_none, [MEMONIC_comisd]  = ASM_PREFIX_66,
                
                [MEMONIC_movsd]  = ASM_PREFIX_SSE_double,
                [MEMONIC_movss]  = ASM_PREFIX_SSE_float,
                
                [MEMONIC_movdqu] = ASM_PREFIX_F3,
                [MEMONIC_movdqa] = ASM_PREFIX_66,
                
                [MEMONIC_movq] = ASM_PREFIX_P_BIG,
                [MEMONIC_movd] = ASM_PREFIX_P_BIG,
                
                [MEMONIC_movups] = ASM_PREFIX_SSE_packed_float,
                [MEMONIC_movupd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_movaps] = ASM_PREFIX_SSE_packed_float,
                [MEMONIC_movapd] = ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_movntdq] = ASM_PREFIX_66, 
                [MEMONIC_movntpd] = ASM_PREFIX_66,
                [MEMONIC_movntps] = ASM_PREFIX_none,
                
                [MEMONIC_vmovq] = ASM_PREFIX_P_BIG | ASM_PREFIX_VEX,
                [MEMONIC_vmovd] = ASM_PREFIX_P_BIG | ASM_PREFIX_VEX,
                
                [MEMONIC_vmovdqu] = ASM_PREFIX_F3 | ASM_PREFIX_VEX,
                [MEMONIC_vmovdqa] = ASM_PREFIX_66 | ASM_PREFIX_VEX,
                
                [MEMONIC_vmovups] = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_float,
                
                [MEMONIC_ptest] = ASM_PREFIX_P_BIG,
                
                // [MEMONIC_vmovupd] = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_double,
                
                [MEMONIC_lzcnt] = ASM_PREFIX_F3,
                
                [MEMONIC_cvtdq2ps] = ASM_PREFIX_none, [MEMONIC_cvtps2dq] = ASM_PREFIX_66, [MEMONIC_cvttps2dq] = ASM_PREFIX_F3,
                [MEMONIC_cvtdq2pd] = ASM_PREFIX_F3,   [MEMONIC_cvtpd2dq] = ASM_PREFIX_F2, [MEMONIC_cvttpd2dq] = ASM_PREFIX_66,
                
                [MEMONIC_pshuflw] = ASM_PREFIX_F2, [MEMONIC_pshufhw] = ASM_PREFIX_F3, [MEMONIC_pshufd]  = ASM_PREFIX_66,
                [MEMONIC_shufps] = ASM_PREFIX_none, [MEMONIC_shufpd] = ASM_PREFIX_66,
                
                [MEMONIC_aesdec]  = ASM_PREFIX_66,
                [MEMONIC_pshufb]  = ASM_PREFIX_66,
                
                [MEMONIC_vshufps]  = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_float,
                [MEMONIC_vblendps] = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_float,
                [MEMONIC_vmulps]   = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_float,
                [MEMONIC_vaddps]   = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_float,
                [MEMONIC_vsubps]   = ASM_PREFIX_VEX | ASM_PREFIX_SSE_packed_float,
                
                // @hack: this is the middle byte of the opcode
                [MEMONIC_vpshufb]     = 0x38,
                [MEMONIC_vperm2f128]  = 0x3a,
                [MEMONIC_vinsertf128] = 0x3a,
                
                [MEMONIC_vptest]  = ASM_PREFIX_VEX | ASM_PREFIX_66,
                [MEMONIC_vpminub] = ASM_PREFIX_VEX | ASM_PREFIX_66,
                [MEMONIC_vpxor]   = ASM_PREFIX_VEX | ASM_PREFIX_66,
                [MEMONIC_vpcmpeqb] = ASM_PREFIX_VEX | ASM_PREFIX_66,
                
                [MEMONIC_pmovmskb] = ASM_PREFIX_P_BIG,
                [MEMONIC_vpmovmskb] = ASM_PREFIX_P_BIG | ASM_PREFIX_VEX,
            };
            
            static u8 memonic_to_opcode[] = {
                [MEMONIC_cmpxchg] = 0xB0,
                [MEMONIC_xadd]    = 0xC0,
                [MEMONIC_xchg]    = 0x86,
                
                [MEMONIC_movsb] = 0xA4,
                [MEMONIC_movsw] = 0xA5,
                [MEMONIC_movsq] = 0xA5,
                [MEMONIC_stosb] = 0xAA,
                [MEMONIC_stosw] = 0xAB,
                [MEMONIC_stosq] = 0xAB,
                
                [MEMONIC_bt]  = 0xA3,
                [MEMONIC_bts] = 0xAB,
                [MEMONIC_btr] = 0xB3,
                [MEMONIC_btc] = 0xBB,
                
                [MEMONIC_bsf] = 0xbc,
                [MEMONIC_bsr] = 0xbd,
                [MEMONIC_lzcnt] = 0xbd,
                
                [MEMONIC_unpcklps] = 0x14, [MEMONIC_unpcklpd] = 0x14,
                [MEMONIC_unpckhps] = 0x15, [MEMONIC_unpckhpd] = 0x15,
                
                [MEMONIC_cvtsi2ss] = 0x2a, [MEMONIC_cvtsi2sd] = 0x2a, 
                
                [MEMONIC_cvttss2si] = 0x2c, [MEMONIC_cvttsd2si] = 0x2c,
                [MEMONIC_cvtss2si] = 0x2d, [MEMONIC_cvtsd2si] = 0x2d,
                
                [MEMONIC_ucomiss] = 0x2E, [MEMONIC_ucomisd] = 0x2E,
                [MEMONIC_comiss]  = 0x2f, [MEMONIC_comisd]  = 0x2f,
                
                [MEMONIC_movmskpd] = 0x50, [MEMONIC_movmskps] = 0x50, 
                
                [MEMONIC_sqrtss] = 0x51,  [MEMONIC_sqrtps]  = 0x51, [MEMONIC_sqrtsd] = 0x51, [MEMONIC_sqrtpd] = 0x51,
                [MEMONIC_rsqrtps] = 0x52, [MEMONIC_rsqrtss] = 0x52, 
                [MEMONIC_rcpps]   = 0x53, [MEMONIC_rcpss]   = 0x53, 
                [MEMONIC_andps]  = 0x54,  [MEMONIC_andpd]   = 0x54,
                [MEMONIC_andnps] = 0x55,  [MEMONIC_andnpd]  = 0x55,
                [MEMONIC_orps]   = 0x56,  [MEMONIC_orpd]    = 0x56,
                [MEMONIC_xorps]  = 0x57,  [MEMONIC_xorpd]   = 0x57,
                
                [MEMONIC_addss] = 0x58, [MEMONIC_addps] = 0x58, [MEMONIC_addsd] = 0x58, [MEMONIC_addpd] = 0x58,
                [MEMONIC_mulss] = 0x59, [MEMONIC_mulps] = 0x59, [MEMONIC_mulsd] = 0x59, [MEMONIC_mulpd] = 0x59,
                
                [MEMONIC_haddps] = 0x7c,  [MEMONIC_haddpd] = 0x7c,
                
                [MEMONIC_cvtps2pd] = 0x5a, [MEMONIC_cvtpd2ps] = 0x5a, [MEMONIC_cvtss2sd] = 0x5a, [MEMONIC_cvtsd2ss] = 0x5a, 
                
                [MEMONIC_cvtdq2ps] = 0x5b, [MEMONIC_cvtps2dq] = 0x5b, [MEMONIC_cvttps2dq] = 0x5b,
                [MEMONIC_cvtdq2pd] = 0xe6, [MEMONIC_cvtpd2dq] = 0xe6, [MEMONIC_cvttpd2dq] = 0xe6,
                
                [MEMONIC_subss] = 0x5c, [MEMONIC_subps] = 0x5c, [MEMONIC_subsd] = 0x5c, [MEMONIC_subpd] = 0x5c,
                [MEMONIC_minss] = 0x5d, [MEMONIC_minps] = 0x5d, [MEMONIC_minsd] = 0x5d, [MEMONIC_minpd] = 0x5d,
                [MEMONIC_divss] = 0x5e, [MEMONIC_divps] = 0x5e, [MEMONIC_divsd] = 0x5e, [MEMONIC_divpd] = 0x5e,
                [MEMONIC_maxss] = 0x5f, [MEMONIC_maxps] = 0x5f, [MEMONIC_maxsd] = 0x5f, [MEMONIC_maxpd] = 0x5f,
                
                [MEMONIC_punpcklbw] = 0x60, [MEMONIC_punpcklwd] = 0x61, [MEMONIC_punpckldq] = 0x62,
                
                [MEMONIC_packsswb] = 0x63,
                
                [MEMONIC_pcmpgtb] = 0x64, [MEMONIC_pcmpgtw] = 0x65, [MEMONIC_pcmpgtd] = 0x66,
                [MEMONIC_pcmpeqb] = 0x74, [MEMONIC_pcmpeqw] = 0x75, [MEMONIC_pcmpeqd] = 0x76,
                
                [MEMONIC_packuswb] = 0x67,
                [MEMONIC_punpckhbw] = 0x68,  [MEMONIC_punpckhwd] = 0x69, [MEMONIC_punpckhdq] = 0x6a,
                [MEMONIC_packssdw] = 0x6b,
                [MEMONIC_punpcklqdq] = 0x6c, [MEMONIC_punpckhqdq] = 0x6d,
                
                [MEMONIC_pmullw]  = 0xD5,
                
                [MEMONIC_psubusb] = 0xD8,
                [MEMONIC_psubusw] = 0xD9,
                [MEMONIC_pminub]  = 0xDA,
                [MEMONIC_pand]    = 0xDB,
                [MEMONIC_paddusb] = 0xDC,
                [MEMONIC_paddusw] = 0xDD,
                [MEMONIC_pmaxub]  = 0xDE,
                [MEMONIC_pandn]   = 0xDF,
                
                [MEMONIC_pavgb] = 0xe0,
                
                [MEMONIC_pavgw]   = 0xe3,
                [MEMONIC_pmulhuw] = 0xe4,
                [MEMONIC_pmulhw]  = 0xe5,
                
                [MEMONIC_psubsb] = 0xe8,
                [MEMONIC_psubsw] = 0xe9,
                [MEMONIC_pminsw] = 0xea,
                
                [MEMONIC_por]    = 0xeb,
                [MEMONIC_paddsb] = 0xEC,
                [MEMONIC_paddsw] = 0xED,
                [MEMONIC_pmaxsw] = 0xEE,
                [MEMONIC_pxor]   = 0xef,
                
                [MEMONIC_pmuludq] = 0xF4,
                [MEMONIC_pmaddwd] = 0xF5,
                [MEMONIC_psadbw]  = 0xF6,
                
                [MEMONIC_psubb] = 0xF8, [MEMONIC_psubw] = 0xF9, [MEMONIC_psubd] = 0xFA, [MEMONIC_psubq] = 0xFB,
                [MEMONIC_paddb] = 0xFC, [MEMONIC_paddw] = 0xFD, [MEMONIC_paddd] = 0xFE, [MEMONIC_paddq] = 0xD4,
                
                [MEMONIC_movhlps] = 0x12,
                [MEMONIC_movlhps] = 0x16,
                
                [MEMONIC_psrlw] = 0xd1, [MEMONIC_psraw] = 0xe1, [MEMONIC_psllw] = 0xf1, 
                [MEMONIC_psrld] = 0xd2, [MEMONIC_psrad] = 0xe2, [MEMONIC_pslld] = 0xf2, 
                [MEMONIC_psrlq] = 0xd3, /* [MEMONIC_psraq], */  [MEMONIC_psllq] = 0xf3,
                
                [MEMONIC_maskmovdqu] = 0xf7,
                
                [MEMONIC_shufps]  = 0xc6, [MEMONIC_shufpd]  = 0xc6,
                [MEMONIC_pshuflw] = 0x70, [MEMONIC_pshufhw] = 0x70, [MEMONIC_pshufd] = 0x70,
                
                [MEMONIC_movnti]  = 0xc3,
                [MEMONIC_movntdq] = 0xe7,
                [MEMONIC_movntpd] = 0x2b, [MEMONIC_movntps] = 0x2b,
                
                [MEMONIC_movups] = 0x10, [MEMONIC_movupd] = 0x10,
                [MEMONIC_movlps] = 0x12, [MEMONIC_movlpd] = 0x12,
                [MEMONIC_movhps] = 0x16, [MEMONIC_movhpd] = 0x16,
                
                [MEMONIC_movaps] = 0x28, [MEMONIC_movapd] = 0x28,
                
                [MEMONIC_vmovups] = 0x10,
                
                [MEMONIC_vpsrlw] = 0xd1, [MEMONIC_vpsraw] = 0xe1, [MEMONIC_vpsllw] = 0xf1, 
                [MEMONIC_vpsrld] = 0xd2, [MEMONIC_vpsrad] = 0xe2, [MEMONIC_vpslld] = 0xf2, 
                [MEMONIC_vpsrlq] = 0xd3,                          [MEMONIC_vpsllq] = 0xf3,
                
                
                [MEMONIC_vaddps]      = 0x58,
                [MEMONIC_vsubps]      = 0x5C,
                [MEMONIC_vmulps]      = 0x59,
                [MEMONIC_vpminub]     = 0xDA,
                [MEMONIC_vpxor]       = 0xEF,
                [MEMONIC_vpcmpeqb]    = 0x74,
                [MEMONIC_vshufps]     = 0xc6,
                
                [MEMONIC_vblendps]    = 0x3A,
                
                [MEMONIC_vpshufb]     = 0x00,
                [MEMONIC_vperm2f128]  = 0x06,
                [MEMONIC_vinsertf128] = 0x18,
                
                
                [MEMONIC_pcmpistri] = 0x63,
                [MEMONIC_pcmpestri] = 0x61,
                [MEMONIC_palignr] = 0x0f,
                
                [MEMONIC_ptest]  = 0x17,
                [MEMONIC_vptest] = 0x17,
                [MEMONIC_aesdec] = 0xDE,
                [MEMONIC_pshufb] = 0x00,
                
                [MEMONIC_pextrb] = 0x14,
                [MEMONIC_pextrw] = 0x15,
                [MEMONIC_pextrd] = 0x16,
                [MEMONIC_pextrq] = 0x16,
                
                [MEMONIC_pinsrb] = 0x20,
                // [MEMONIC_pinsrw] = 0x15,
                [MEMONIC_pinsrd] = 0x22,
                [MEMONIC_pinsrq] = 0x22,
                
                // These are acutually the /reg-extension
                [MEMONIC_neg] = 3,
                [MEMONIC_mul] = 4,
                [MEMONIC_div] = 6,
            };
            
            case MEMONIC_mov:{
                if(lhs->state == EMIT_LOCATION_loaded){
                    //
                    // @note: we are not using the general function 'emit_load_into_specific_gpr' here, as 
                    //        it actually allocates the register into the 'emit_location_map'
                    //        This will spill the register, as we have already allocated.
                    // emit_load_into_specific_gpr(context, rhs, lhs->loaded_register);
                    
                    // @note: THIS IS ACTUALLY NECESARRY: for an instruction like 'mov al, 8' we would otherwise
                    //        emit a 'mov eax, 8', and so on (that function does not to be perticular with sizes).
                    //
                    if(rhs->state == EMIT_LOCATION_immediate){
                        enum x64_OPCODE x64_opcode = (lhs->size == 1) ? MOVE_REG8_IMMEDIATE8 : MOVE_REG_IMMEDIATE;
                        
                        if(lhs->size == 2) emit(LEGACY_OPERAND_SIZE_OVERRIDE_PREFIX);
                        
                        u8 rex = 0;
                        if(lhs->size == 8) rex |= REXW;
                        if(register_is_extended(lhs->loaded_register)) rex |= REXB;
                        if(rex) emit(rex); // this decides wheter 32 or 64 bits in this case
                        
                        emit(x64_opcode + (lhs->loaded_register & 7));
                        
                        emit_bytes(context, lhs->size, rhs->value);
                    }else if(rhs->state == EMIT_LOCATION_loaded){
                        enum x64_OPCODE x64_opcode = (lhs->size == 1) ? MOVE_REG8_REGM8 : MOVE_REG_REGM;
                        emit_register_register(context, no_prefix(), one_byte_opcode(x64_opcode), lhs, rhs);
                    }else{
                        enum x64_OPCODE x64_opcode = (lhs->size == 1) ? MOVE_REG8_REGM8 : MOVE_REG_REGM;
                        emit_register_relative_register(context, no_prefix(), one_byte_opcode(x64_opcode), lhs->loaded_register, rhs);
                    }
                }else{
                    assert(lhs->state == EMIT_LOCATION_register_relative);
                    if(rhs->state == EMIT_LOCATION_immediate) rhs->size = lhs->size; // @clenaup: This was not true. I dunno, this whole stuff should be redone.
                    emit_store(context, lhs, rhs);
                }
            }break;
            
            case MEMONIC_movsx: case MEMONIC_movzx:{
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, lhs);
                
                assert(rhs->size == 1 || rhs->size == 2);
                
                u8 base_opcode = (inst->memonic == MEMONIC_movsx) ? MOVE_WITH_SIGN_EXTENSION_REG_REGM8 : MOVE_WITH_ZERO_EXTENSION_REG_REGM8;
                struct opcode opcode = (rhs->size == 1) ? two_byte_opcode(base_opcode) : two_byte_opcode(base_opcode + 1);
                rhs->size = loaded->size;
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, no_prefix(), opcode, loaded, rhs);
                }else{
                    emit_register_relative_register(context, no_prefix(), opcode, loaded->loaded_register, rhs);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_popcnt:{
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, lhs);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, create_prefixes(ASM_PREFIX_F3), two_byte_opcode(0xb8), loaded, rhs);
                }else{
                    emit_register_relative_register(context, create_prefixes(ASM_PREFIX_F3), two_byte_opcode(0xb8), loaded->loaded_register, rhs);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_add: case MEMONIC_or:
            case MEMONIC_adc: case MEMONIC_sbb:
            case MEMONIC_and: case MEMONIC_sub:
            case MEMONIC_xor: case MEMONIC_cmp:{
                u8 memonic_index = (u8)(inst->memonic - MEMONIC_add);
                
                u8 opcode_base   = memonic_index * 8;
                
                u8 REG_OPCODE    = memonic_index;
                
                u8 OP_REGM8_REG8 = opcode_base + 0;
                u8 OP_REGM_REG   = opcode_base + 1;
                u8 OP_REG8_REGM8 = opcode_base + 2;
                u8 OP_REG_REGM   = opcode_base + 3;
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    struct emit_location *loaded = emit_load_gpr(context, rhs);
                    
                    loaded->prevent_freeing += 1;
                    emit_compound_assignment__internal(context, user_prefixes, lhs, loaded, false, REG_OPCODE, OP_REGM8_REG8, OP_REGM_REG);
                    loaded->prevent_freeing -= 1;
                    
                    if(loaded != rhs) free_emit_location(context, loaded);
                }else{
                    emit_binary_op__internal(context, user_prefixes, lhs, rhs, lhs->size, false, REG_OPCODE, OP_REG8_REGM8, OP_REG_REGM);
                }
            }break;
            
            case MEMONIC_rol: case MEMONIC_ror:
            case MEMONIC_rcl: case MEMONIC_rcr:
            case MEMONIC_shl: case MEMONIC_shr:
            case MEMONIC_sal: case MEMONIC_sar:{
                u8 REG_OPCODE = (u8)(inst->memonic - MEMONIC_rol);
                b32 compound = lhs->state == EMIT_LOCATION_register_relative;
                emit_shift_or_rotate__internal(context, user_prefixes, lhs, rhs, REG_OPCODE, compound);
            }break;
            
            //
            // instruction without operands
            //
            case MEMONIC_int3:  { emit(0xcc); } break;
            case MEMONIC_ret:   { emit(0xc3); } break;
            case MEMONIC_pause: { emit(0xf3); emit(0x90); } break;
            
            case MEMONIC_lfence: { emit(0x0f); emit(0xae); emit(0xe8); } break;
            case MEMONIC_mfence: { emit(0x0f); emit(0xae); emit(0xf0); } break;
            case MEMONIC_sfence: { emit(0x0f); emit(0xae); emit(0xf8); } break;
            
            case MEMONIC_clflush:{
                assert(operand->state == EMIT_LOCATION_register_relative);
                emit_register_relative_extended(context, no_prefix(), two_byte_opcode(0xae), 7, operand);
            }break;
            
            case MEMONIC_rdtsc:{
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_A, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_D, 8);
                
                emit(0x0f); emit(0x31);
            }break;
            case MEMONIC_rdtscp:{
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_A, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_C, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_D, 8);
                
                emit(0x0f); emit(0x01); emit(0xF9);
            }break;
            
            case MEMONIC_cpuid:{
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_A, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_B, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_C, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_D, 8);
                
                emit(0x0f); emit(0xA2);
            }break;
            
            case MEMONIC_xgetbv:{
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_A, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_C, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_D, 8);
                
                emit(0x0F); emit(0x01); emit(0xD0);
            }break;
            
            case MEMONIC_movsb: case MEMONIC_stosb:{
                emit_prefixes_and_opcode(context, user_prefixes, 0, 0, 0, 1, one_byte_opcode(memonic_to_opcode[inst->memonic]));
            }break;
            
            case MEMONIC_movsw: case MEMONIC_stosw:{
                emit_prefixes_and_opcode(context, user_prefixes, 0, 0, 0, 2, one_byte_opcode(memonic_to_opcode[inst->memonic]));
            }break;
            
            case MEMONIC_movsd:{
                if(inst->amount_of_operands != 0){
                    goto this_is_a_mov_packed_singles_and_not_a_rep_movsd;
                }
                emit_prefixes_and_opcode(context, user_prefixes, 0, 0, 0, 4, one_byte_opcode(0xA5));
            }break;
            
            case MEMONIC_stosd:{
                emit_prefixes_and_opcode(context, user_prefixes, 0, 0, 0, 4, one_byte_opcode(0xab));
            }break;
            
            case MEMONIC_movsq: case MEMONIC_stosq:{
                emit_prefixes_and_opcode(context, user_prefixes, 0, 0, 0, 8, one_byte_opcode(memonic_to_opcode[inst->memonic]));
            }break;
            
            case MEMONIC_int:{
                assert(operand->state == EMIT_LOCATION_immediate);
                emit(0xcd);
                emit(operand->value);
            }break;
            
            case MEMONIC_cmpxchg16b:{
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_A, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_C, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_B, 8);
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_D, 8);
                
                assert(operand->state == EMIT_LOCATION_register_relative);
                // @cleanup: think about what to do about sizeing with this instruction
                operand->size = 8;
                emit_register_relative_extended(context, user_prefixes, two_byte_opcode(0xc7), 1, operand);
            }break;
            
            case MEMONIC_xchg:{
                if(lhs->state == EMIT_LOCATION_loaded){
                    struct emit_location *temp = lhs;
                    lhs = rhs;
                    rhs = temp;
                }
                
                struct emit_location *loaded = emit_load_gpr(context, rhs);
                
                u8 op = memonic_to_opcode[inst->memonic];
                struct opcode opcode = (lhs->size == 1) ? one_byte_opcode(op) : one_byte_opcode(op + 1);
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, user_prefixes, opcode, loaded->loaded_register, lhs);
                }else{
                    assert(lhs->state == EMIT_LOCATION_loaded);
                    emit_register_register(context, user_prefixes, opcode, /*reg*/loaded, /*regm*/lhs);
                }
                
                // @note: rhs is also a destination in some sense
                if(loaded != rhs) emit_store(context, rhs, loaded);
            }break;
            case MEMONIC_cmpxchg: case MEMONIC_xadd:{
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, rhs);
                
                u8 op = memonic_to_opcode[inst->memonic];
                struct opcode opcode = (lhs->size == 1) ? two_byte_opcode(op) : two_byte_opcode(op + 1);
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, user_prefixes, opcode, loaded->loaded_register, lhs);
                    free_emit_location(context, lhs); // ?
                }else{
                    assert(lhs->state == EMIT_LOCATION_loaded);
                    emit_register_register(context, user_prefixes, opcode, /*reg*/loaded, /*regm*/lhs);
                }
                
                // @note: rhs is also a destination in some sense
                if(loaded != rhs) emit_store(context, rhs, loaded);
            }break;
            
            case MEMONIC_inc: case MEMONIC_dec:{
                struct opcode opcode = (operand->size == 1) ? one_byte_opcode(0xfe) : one_byte_opcode(0xff);
                u8 reg_extension = (inst->memonic == MEMONIC_dec) ? 1 : 0;
                
                if(operand->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_extended(context, user_prefixes, opcode, reg_extension, operand);
                }else{
                    emit_reg_extended_op(context, user_prefixes, opcode, reg_extension, operand);
                }
            }break;
            
            case MEMONIC_mul: case MEMONIC_div: case MEMONIC_neg:{
                struct opcode opcode = (operand->size == 1) ? one_byte_opcode(0xf6) : one_byte_opcode(0xf7);
                
                // @note: All of this sucks!
                struct emit_location *rdx = null;
                if(operand->size != 1 && inst->memonic != MEMONIC_neg) rdx = asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_D, operand->size);
                
                u8 reg_extension = memonic_to_opcode[inst->memonic];
                if(operand->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_extended(context, user_prefixes, opcode, reg_extension, operand);
                }else{
                    emit_reg_extended_op(context, user_prefixes, opcode, reg_extension, operand);
                }
            }break;
            
            case MEMONIC_bswap:{
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, operand);
                
                enum rex_encoding rex = ((operand->size == 8) ? REXW : 0) | (register_is_extended(loaded->loaded_register) ? REXB : 0);
                if(rex) emit(rex);
                emit(0x0f);
                emit(0xc8 + (loaded->loaded_register & 7));
                
                if(loaded != operand) emit_store(context, operand, loaded);
            }break;
            
            case MEMONIC_seto:  case MEMONIC_setno:
            case MEMONIC_setc:  case MEMONIC_setnc:
            case MEMONIC_setz:  case MEMONIC_setnz:
            case MEMONIC_setbe: case MEMONIC_setnbe:
            case MEMONIC_sets:  case MEMONIC_setns:
            case MEMONIC_setp:  case MEMONIC_setnp:
            case MEMONIC_setl:  case MEMONIC_setnl:
            case MEMONIC_setle: case MEMONIC_setnle:{
                struct opcode opcode = two_byte_opcode((u8)(0x90 + inst->memonic - MEMONIC_seto));
                
                if(operand->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_extended(context, user_prefixes, opcode, 0, operand);
                }else{
                    emit_reg_extended_op(context, user_prefixes, opcode, 0, operand);
                }
            }break;
            
            case MEMONIC_cmovo:  case MEMONIC_cmovc:  case MEMONIC_cmovz:  case MEMONIC_cmovbe:  case MEMONIC_cmovs:  case MEMONIC_cmovp:  case MEMONIC_cmovl:  case MEMONIC_cmovle:
            case MEMONIC_cmovno: case MEMONIC_cmovnc: case MEMONIC_cmovnz: case MEMONIC_cmovnbe: case MEMONIC_cmovns: case MEMONIC_cmovnp: case MEMONIC_cmovnl: case MEMONIC_cmovnle:{
                struct opcode opcode = two_byte_opcode((u8)(0x40 + inst->memonic - MEMONIC_cmovo));
                
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, lhs);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, no_prefix(), opcode, /*reg*/loaded, /*regm*/rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, no_prefix(), opcode, /*reg*/loaded->loaded_register, /*regm*/rhs);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_bt: case MEMONIC_bts: case MEMONIC_btr: case MEMONIC_btc:{
                u8 REG_OPCODE = 4 + (u8)(inst->memonic - MEMONIC_bt);
                u8 opcode = memonic_to_opcode[inst->memonic];
                
                if(rhs->state == EMIT_LOCATION_immediate){
                    if(lhs->state == EMIT_LOCATION_loaded){
                        emit_reg_extended_op(context, no_prefix(), two_byte_opcode(0xBA), REG_OPCODE, lhs);
                    }else{
                        assert(lhs->state == EMIT_LOCATION_register_relative);
                        emit_register_relative_extended(context, no_prefix(), two_byte_opcode(0xBA), REG_OPCODE, lhs);
                    }
                }else{
                    struct emit_location *loaded = emit_load_gpr(context, rhs);
                    if(lhs->state == EMIT_LOCATION_loaded){
                        emit_register_register(context, no_prefix(), two_byte_opcode(opcode), /*reg*/loaded, /*regm*/lhs);
                    }else{
                        assert(lhs->state == EMIT_LOCATION_register_relative);
                        emit_register_relative_register(context, no_prefix(), two_byte_opcode(opcode), /*reg*/loaded->loaded_register, /*regm*/lhs);
                    }
                    if(loaded != rhs) free_emit_location(context, loaded);
                }
            }break;
            
            case MEMONIC_lzcnt:
            case MEMONIC_bsf: case MEMONIC_bsr:{
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, lhs);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, create_prefixes(memonic_to_prefix[inst->memonic]), two_byte_opcode(memonic_to_opcode[inst->memonic]), /*reg*/loaded, /*regm*/rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, create_prefixes(memonic_to_prefix[inst->memonic]), two_byte_opcode(memonic_to_opcode[inst->memonic]), /*reg*/loaded->loaded_register, /*regm*/rhs);
                }
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_cmpeqss:  case MEMONIC_cmpltss:  case MEMONIC_cmpless:  case MEMONIC_cmpunordss:
            case MEMONIC_cmpneqss: case MEMONIC_cmpnltss: case MEMONIC_cmpnless: case MEMONIC_cmpordss:
            case MEMONIC_cmpeqsd:  case MEMONIC_cmpltsd:  case MEMONIC_cmplesd:  case MEMONIC_cmpunordsd:
            case MEMONIC_cmpneqsd: case MEMONIC_cmpnltsd: case MEMONIC_cmpnlesd: case MEMONIC_cmpordsd:
            case MEMONIC_cmpeqps:  case MEMONIC_cmpltps:  case MEMONIC_cmpleps:  case MEMONIC_cmpunordps:
            case MEMONIC_cmpneqps: case MEMONIC_cmpnltps: case MEMONIC_cmpnleps: case MEMONIC_cmpordps:
            case MEMONIC_cmpeqpd:  case MEMONIC_cmpltpd:  case MEMONIC_cmplepd:  case MEMONIC_cmpunordpd:
            case MEMONIC_cmpneqpd: case MEMONIC_cmpnltpd: case MEMONIC_cmpnlepd: case MEMONIC_cmpordpd:
            case MEMONIC_cmpps: case MEMONIC_cmppd: case MEMONIC_cmpss: case MEMONIC_cmpsd:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                assert(loaded->register_kind == REGISTER_KIND_xmm);
                
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode = two_byte_opcode(0xc2);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, prefix, opcode, loaded, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }
                
                if(operands[2]){
                    assert(operands[2]->state == EMIT_LOCATION_immediate);
                    emit((u8)operands[2]->value);
                }else{
                    emit((inst->memonic - MEMONIC_cmpeqps) & 7);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_cvtpd2ps: case MEMONIC_cvtps2pd:
            case MEMONIC_cvtsd2ss: case MEMONIC_cvtss2sd: 
            case MEMONIC_unpcklps: case MEMONIC_unpcklpd:
            case MEMONIC_unpckhps: case MEMONIC_unpckhpd:
            case MEMONIC_rcpps:    case MEMONIC_rcpss:
            case MEMONIC_rsqrtps:  case MEMONIC_rsqrtss:
            case MEMONIC_sqrtps:   case MEMONIC_sqrtpd: case MEMONIC_sqrtss: case MEMONIC_sqrtsd: 
            case MEMONIC_addps:    case MEMONIC_addpd:  case MEMONIC_addss:  case MEMONIC_addsd: 
            case MEMONIC_subps:    case MEMONIC_subpd:  case MEMONIC_subss:  case MEMONIC_subsd: 
            case MEMONIC_divps:    case MEMONIC_divpd:  case MEMONIC_divss:  case MEMONIC_divsd: 
            case MEMONIC_mulps:    case MEMONIC_mulpd:  case MEMONIC_mulss:  case MEMONIC_mulsd: 
            case MEMONIC_minps:    case MEMONIC_minpd:  case MEMONIC_minss:  case MEMONIC_minsd: 
            case MEMONIC_maxps:    case MEMONIC_maxpd:  case MEMONIC_maxss:  case MEMONIC_maxsd: 
            case MEMONIC_haddps:   case MEMONIC_haddpd:
            case MEMONIC_andps:    case MEMONIC_andpd:
            case MEMONIC_xorps:    case MEMONIC_xorpd:
            case MEMONIC_orps:     case MEMONIC_orpd:
            case MEMONIC_andnps:   case MEMONIC_andnpd:
            case MEMONIC_comiss:   case MEMONIC_comisd:
            case MEMONIC_ucomiss:  case MEMONIC_ucomisd:
            case MEMONIC_movhlps:  case MEMONIC_movlhps:
            case MEMONIC_cvtdq2pd: case MEMONIC_cvtpd2dq: case MEMONIC_cvttpd2dq:
            case MEMONIC_cvtdq2ps: case MEMONIC_cvtps2dq: case MEMONIC_cvttps2dq:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, prefix, opcode, loaded, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_pmuludq:
            case MEMONIC_maskmovdqu:
            case MEMONIC_pcmpgtb: case MEMONIC_pcmpgtw: case MEMONIC_pcmpgtd:
            // case MEMONIC_pcmpltb: case MEMONIC_pcmpltw: case MEMONIC_pcmpltd:
            
            case MEMONIC_psadbw:
            case MEMONIC_paddsw:    case MEMONIC_paddsb:
            case MEMONIC_psubsw:    case MEMONIC_psubsb:
            case MEMONIC_psubusb:   case MEMONIC_psubusw:   case MEMONIC_paddusb: case MEMONIC_paddusw:
            case MEMONIC_pminub:    case MEMONIC_pmaxub:    case MEMONIC_pminsw:  case MEMONIC_pmaxsw:
            
            case MEMONIC_pcmpeqb:   case MEMONIC_pcmpeqw:   case MEMONIC_pcmpeqd:
            case MEMONIC_pmaddwd:   case MEMONIC_pmullw:    case MEMONIC_pmulhuw:   case MEMONIC_pmulhw:
            case MEMONIC_paddb:     case MEMONIC_paddw:     case MEMONIC_paddd:     case MEMONIC_paddq:
            case MEMONIC_psubb:     case MEMONIC_psubw:     case MEMONIC_psubd:     case MEMONIC_psubq:
            case MEMONIC_pavgb:     case MEMONIC_pavgw:
            
            case MEMONIC_punpcklbw:  case MEMONIC_punpcklwd: case MEMONIC_punpckldq: 
            case MEMONIC_punpcklqdq: case MEMONIC_punpckhqdq:
            case MEMONIC_punpckhbw:  case MEMONIC_punpckhwd: case MEMONIC_punpckhdq:
            case MEMONIC_packssdw:   case MEMONIC_packsswb:  case MEMONIC_packuswb:
            case MEMONIC_pxor:       case MEMONIC_por:       case MEMONIC_pand:      case MEMONIC_pandn:{
                //
                // These are the 'pxxx' memonics of the form 'pxxx xmm, xmm/m128'.
                // They are all of the form '66 0F <memonic_to_opcode[inst->memonic]>'
                //
                
                // @cleanup: 8-byte versions?
                
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, create_prefixes(ASM_PREFIX_P_BIG), opcode, loaded, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, create_prefixes(ASM_PREFIX_P_BIG), opcode, loaded->loaded_register, rhs);
                }
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            // case MEMONIC_movsd: // has a goto instead
            case MEMONIC_movss:{
                this_is_a_mov_packed_singles_and_not_a_rep_movsd:;
                
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                
                if(lhs->state == EMIT_LOCATION_loaded){
                    if(rhs->state == EMIT_LOCATION_loaded){
                        emit_register_register(context, prefix, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), lhs, rhs);
                    }else{
                        assert(rhs->state == EMIT_LOCATION_register_relative);
                        emit_register_relative_register(context, prefix, two_byte_opcode(MOVE_UNALIGNED_XMM_REGM), lhs->loaded_register, rhs);
                    }
                }else{
                    struct emit_location *loaded = emit_load_float(context, rhs);
                    emit_register_relative_register(context, prefix, two_byte_opcode(MOVE_UNALIGNED_REGM_XMM), loaded->loaded_register, lhs);
                    if(loaded != rhs) free_emit_location(context, loaded);
                }
            }break;
            
            case MEMONIC_psllw: case MEMONIC_pslld: case MEMONIC_psllq: 
            case MEMONIC_psrlw: case MEMONIC_psrld: case MEMONIC_psrlq:
            case MEMONIC_psraw: case MEMONIC_psrad:
            case MEMONIC_psrldq: case MEMONIC_pslldq:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                
                if(rhs->state == EMIT_LOCATION_immediate){
                    // [66] 71 2   psrlw xmm, imm8
                    // [66] 71 4   psraw xmm, imm8
                    // [66] 71 6   psllw xmm, imm8
                    // [66] 72 2   psrld xmm, imm8
                    // [66] 72 4   psrad xmm, imm8
                    // [66] 72 6   pslld xmm, imm8
                    // [66] 73 2   psrlq xmm, imm8
                    // [66] 73 4   psraq xmm, imm8
                    // [66] 73 6   psllq xmm, imm8
                    
                    // [66] 73 3   psrldq xmm, imm8
                    // [66] 73 7   pslldq xmm, imm8
                    
                    u8 memonic_index = (u8)(inst->memonic - MEMONIC_psrlw);
                    
                    u8 opcode     = memonic_index / 3;
                    u8 reg_opcode = 2 * ((memonic_index % 3) + 1);
                    if(inst->memonic == MEMONIC_psrldq){ opcode = 2; reg_opcode = 3; }
                    if(inst->memonic == MEMONIC_pslldq){ opcode = 2; reg_opcode = 7; }
                    
                    emit_reg_extended_op(context, create_prefixes(ASM_PREFIX_P_BIG), two_byte_opcode(0x71 + opcode), reg_opcode, loaded);
                    
                    emit(rhs->value);
                }else if(rhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, create_prefixes(ASM_PREFIX_P_BIG), two_byte_opcode(memonic_to_opcode[inst->memonic]), loaded->loaded_register, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_loaded);
                    emit_register_register(context, create_prefixes(ASM_PREFIX_P_BIG), two_byte_opcode(memonic_to_opcode[inst->memonic]), loaded, rhs);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_cvtsi2ss: case MEMONIC_cvtsi2sd:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                
                if(rhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_loaded);
                    emit_register_op__internal(context, prefix, opcode, loaded->loaded_register, rhs->loaded_register, rhs->size);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_movmskps:  case MEMONIC_movmskpd:
            case MEMONIC_cvttss2si: case MEMONIC_cvttsd2si:
            case MEMONIC_cvtss2si:  case MEMONIC_cvtsd2si:{
                struct emit_location *loaded = emit_load_float(context, rhs);
                
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, lhs);
                }else{
                    assert(lhs->state == EMIT_LOCATION_loaded);
                    emit_register_op__internal(context, prefix, opcode, lhs->loaded_register, loaded->loaded_register, lhs->size);
                }
                
                if(loaded != rhs) free_emit_location(context, loaded);
            }break;
            
            // op xmm1, xmm2/m128, imm8
            case MEMONIC_shufps: case MEMONIC_shufpd:
            case MEMONIC_pshuflw: case MEMONIC_pshufhw: case MEMONIC_pshufd:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                
                if(rhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_loaded);
                    emit_register_register(context, prefix, opcode, loaded, rhs);
                }
                assert(operands[2]->state == EMIT_LOCATION_immediate);
                emit(operands[2]->value);
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_pcmpistri:case MEMONIC_pcmpestri:{
                // The result is in rcx.
                asm_block_load_registers_which_was_used_by_user(context, REGISTER_KIND_gpr, REGISTER_C, 8);
            }/*fall_through*/
            case MEMONIC_palignr:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                struct prefixes prefix = create_prefixes(ASM_PREFIX_P_BIG);
                struct opcode   opcode = three_byte_opcode(0x3a, memonic_to_opcode[inst->memonic]);
                
                if(rhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_loaded);
                    emit_register_register(context, prefix, opcode, loaded, rhs);
                }
                assert(operands[2]->state == EMIT_LOCATION_immediate);
                emit(operands[2]->value);
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_vmovups:
            case MEMONIC_movlps: case MEMONIC_movlpd:
            case MEMONIC_movhps: case MEMONIC_movhpd:
            case MEMONIC_movapd: case MEMONIC_movupd:
            case MEMONIC_movaps: case MEMONIC_movups:{
                //    0F 28 MOVAPS xmm1, xmm2/m128
                //    0F 29 MOVAPS xmm1/m128, xmm2
                // 66 0F 28 MOVAPD xmm1, xmm2/m128
                // 66 0F 29 MOVAPD xmm1/m128, xmm2
                // VEX.128.0F.WIG 10 /r VMOVUPS xmm1, xmm2/m128
                // VEX.128.0F.WIG 11 /r VMOVUPS xmm2/m128, xmm1
                // VEX.256.0F.WIG 10 /r VMOVUPS ymm1, ymm2/m256
                // VEX.256.0F.WIG 11 /r VMOVUPS ymm2/m256, ymm1
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    opcode = two_byte_opcode(memonic_to_opcode[inst->memonic] + 1);
                    
                    struct emit_location *loaded = emit_load_float(context, rhs);
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, lhs);
                    if(loaded != rhs) free_emit_location(context, loaded);
                }else{
                    assert(lhs->state == EMIT_LOCATION_loaded);
                    if(rhs->state == EMIT_LOCATION_register_relative){
                        emit_register_relative_register(context, prefix, opcode, lhs->loaded_register, rhs);
                    }else{
                        assert(rhs->state == EMIT_LOCATION_loaded);
                        emit_register_register(context, prefix, opcode, lhs, rhs);
                    }
                }
            }break;
            // @cleanup: maybe unify these two ^ v
            case MEMONIC_vmovdqu:
            case MEMONIC_movdqa: case MEMONIC_movdqu:{
                // 66 0F 6F /r MOVDQA xmm1, xmm2/m128
                // 66 0F 7F /r MOVDQA xmm2/m128, xmm1
                // F3 0F 6F /r MOVDQU xmm1, xmm2/m128
                // F3 0F 7F /r MOVDQU xmm2/m128, xmm1
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    struct emit_location *loaded = emit_load_float(context, rhs);
                    emit_register_relative_register(context, prefix, two_byte_opcode(0x7f), loaded->loaded_register, lhs);
                    if(loaded != rhs) free_emit_location(context, loaded);
                }else{
                    assert(lhs->state == EMIT_LOCATION_loaded);
                    if(rhs->state == EMIT_LOCATION_register_relative){
                        emit_register_relative_register(context, prefix, two_byte_opcode(0x6f), lhs->loaded_register, rhs);
                    }else{
                        assert(rhs->state == EMIT_LOCATION_loaded);
                        emit_register_register(context, prefix, two_byte_opcode(0x6f), lhs, rhs);
                    }
                }
            }break;
            
            case MEMONIC_movnti:{
                assert(lhs->state == EMIT_LOCATION_register_relative);
                struct emit_location *loaded = emit_load_float(context, rhs);
                emit_register_relative_register(context, no_prefix(), two_byte_opcode(0xc3), loaded->loaded_register, lhs);
                if(loaded != rhs) free_emit_location(context, loaded);
            }break;
            
            case MEMONIC_movntdq: case MEMONIC_movntpd: case MEMONIC_movntps:{
                assert(lhs->state == EMIT_LOCATION_register_relative);
                struct emit_location *loaded = emit_load_float(context, rhs);
                emit_register_relative_register(context, create_prefixes(memonic_to_prefix[inst->memonic]), two_byte_opcode(memonic_to_opcode[inst->memonic]), loaded->loaded_register, lhs);
                if(loaded != rhs) free_emit_location(context, loaded);
            }break;
            
            case MEMONIC_pextrb: case MEMONIC_pextrw: case MEMONIC_pextrd: case MEMONIC_pextrq:{
                struct emit_location *loaded = emit_load_float(context, rhs);
                struct prefixes prefix = create_prefixes(ASM_PREFIX_P_BIG);
                struct opcode   opcode = three_byte_opcode(0x3A, memonic_to_opcode[inst->memonic]);
                
                if(lhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, lhs);
                }else{
                    assert(lhs->state == EMIT_LOCATION_loaded);
                    emit_register_op__internal(context, prefix, opcode, loaded->loaded_register, lhs->loaded_register, lhs->size);
                }
                emit(operands[2]->value);
                
                if(rhs != loaded) free_emit_location(context, loaded);
            }break;
            
            case MEMONIC_pinsrb: case MEMONIC_pinsrw: case MEMONIC_pinsrd: case MEMONIC_pinsrq:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                struct prefixes prefix = create_prefixes(ASM_PREFIX_P_BIG);
                struct opcode   opcode = three_byte_opcode(0x3A, memonic_to_opcode[inst->memonic]);
                
                if(inst->memonic == MEMONIC_pinsrw){
                    opcode = two_byte_opcode(0xC4);
                }
                
                if(rhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_loaded);
                    emit_register_op__internal(context, prefix, opcode, loaded->loaded_register, rhs->loaded_register, rhs->size);
                }
                emit(operands[2]->value);
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_vmovd: case MEMONIC_vmovq:
            case MEMONIC_movd:  case MEMONIC_movq:{
                // 64-bit has rexw
                // 66 0F 6E /r MOVD xmm, r/m32
                // 66 0F 6E /r MOVQ xmm, r/m64
                // 66 0F 7E /r MOVD r/m32, xmm
                // 66 0F 7E /r MOVQ r/m64, xmm
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                
                if(lhs->size < 16){
                    struct emit_location *loaded = emit_load_float(context, rhs);
                    
                    if(lhs->state == EMIT_LOCATION_register_relative){
                        emit_register_relative_register(context, prefix, two_byte_opcode(0x7E), loaded->loaded_register, lhs);
                    }else{
                        emit_register_op__internal(context, prefix, two_byte_opcode(0x7E), loaded->loaded_register, lhs->loaded_register, lhs->size);
                    }
                    
                    if(loaded != rhs) free_emit_location(context, loaded);
                }else{
                    struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                    
                    if(rhs->state == EMIT_LOCATION_register_relative){
                        emit_register_relative_register(context, prefix, two_byte_opcode(0x6E), loaded->loaded_register, rhs);
                    }else{
                        emit_register_op__internal(context, prefix, two_byte_opcode(0x6E), loaded->loaded_register, rhs->loaded_register, lhs->size);
                    }
                    
                    if(loaded != lhs) emit_store(context, lhs, loaded);
                }
            }break;
            
            case MEMONIC_vpmovmskb:
            case MEMONIC_pmovmskb:{
                // pmovmskb r32, xmm
                struct emit_location *first  = emit_load_gpr(context, operands[0]);
                struct emit_location *second = emit_load_float(context, operands[1]);
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                
                emit_register_op__internal(context, prefix, two_byte_opcode(0xD7), first->loaded_register, second->loaded_register, second->size);
                
                if(second != operands[1]) free_emit_location(context, second);
                if(first  != operands[0]) emit_store(context, operands[0], first);
            }break;
            
            // 'op ymm1, ymm2, ymm3/m256, imm8' or 'op xmm1, xmm2, xmm3/m128, imm8'
            // 'op ymm1, ymm2, ymm3/m256'       or 'op xmm1, xmm2, xmm3/m128'
            case MEMONIC_vpsraq:
            case MEMONIC_vshufps: case MEMONIC_vblendps:
            case MEMONIC_vpminub: case MEMONIC_vpxor: case MEMONIC_vpcmpeqb:
            case MEMONIC_vmulps: case MEMONIC_vaddps: case MEMONIC_vsubps:{
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode = two_byte_opcode(memonic_to_opcode[inst->memonic]);
                
                // VEX.128.0F.WIG 58 /r VADDPS xmm1, xmm2, xmm3/m128
                // VEX.256.0F.WIG 58 /r VADDPS ymm1, ymm2, ymm3/m256
                struct emit_location *first = (operands[0]->state == EMIT_LOCATION_loaded) ? operands[0] : 
                        emit_location_loaded(context, REGISTER_KIND_xmm, allocate_register(context, REGISTER_KIND_xmm), operands[0]->size);
                
                struct emit_location *vex = emit_load_float(context, operands[1]);
                prefix.vex_register = vex->loaded_register;
                
                if(operands[2]->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, first->loaded_register, operands[2]);
                }else{
                    emit_register_op__internal(context, prefix, opcode, first->loaded_register, operands[2]->loaded_register, operands[0]->size);
                }
                if(operands[3]){
                    assert(operands[3]->state == EMIT_LOCATION_immediate);
                    emit(operands[3]->value);
                }
                
                if(vex   != operands[1]) free_emit_location(context, vex);
                if(first != operands[0]) emit_store(context, operands[0], first);
            }break;
            
            case MEMONIC_vperm2f128: case MEMONIC_vinsertf128:
            case MEMONIC_vpshufb:{
                // @hack: store the middle byte of the opcode in the prefix
                struct prefixes prefix = create_prefixes(ASM_PREFIX_VEX | ASM_PREFIX_P_BIG);
                struct opcode opcode   = three_byte_opcode(memonic_to_prefix[inst->memonic], memonic_to_opcode[inst->memonic]);
                
                struct emit_location *first = (operands[0]->state == EMIT_LOCATION_loaded) ? operands[0] : 
                        emit_location_loaded(context, REGISTER_KIND_xmm, allocate_register(context, REGISTER_KIND_xmm), operands[0]->size);
                
                struct emit_location *vex = emit_load_float(context, operands[1]);
                prefix.vex_register = vex->loaded_register;
                
                if(operands[2]->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, prefix, opcode, first->loaded_register, operands[2]);
                }else{
                    emit_register_op__internal(context, prefix, opcode, first->loaded_register, operands[2]->loaded_register, operands[0]->size);
                }
                if(operands[3]){
                    assert(operands[3]->state == EMIT_LOCATION_immediate);
                    emit(operands[3]->value);
                }
                
                if(vex   != operands[1]) free_emit_location(context, vex);
                if(first != operands[0]) emit_store(context, operands[0], first);
            }break;
            
            // @incomplete: this one is weird, right now only 'vmovss xmm0, xmm1/m32'
            case MEMONIC_vmovss:{
                assert(inst->amount_of_operands == 2);
                assert(lhs->state == EMIT_LOCATION_loaded);
                
                if(rhs->state == EMIT_LOCATION_register_relative){
                    emit_register_relative_register(context, create_prefixes(ASM_PREFIX_F3), two_byte_opcode(0x10), lhs->loaded_register, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_loaded);
                    emit_register_op__internal(context, create_prefixes(ASM_PREFIX_F3), two_byte_opcode(0x10), lhs->loaded_register, rhs->loaded_register, rhs->size);
                }
            }break;
            
            
            // 3byte-38-op xmm, xmm128 or ymm, ymm256
            case MEMONIC_aesdec: case MEMONIC_pshufb:
            case MEMONIC_ptest:
            case MEMONIC_vptest:{
                struct emit_location *loaded = emit_load_without_freeing_float(context, lhs);
                
                struct prefixes prefix = create_prefixes(memonic_to_prefix[inst->memonic]);
                struct opcode opcode   = three_byte_opcode(0x38, memonic_to_opcode[inst->memonic]);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, prefix, opcode, loaded, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }
                
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_crc32:{
                struct emit_location *loaded = emit_load_without_freeing_gpr(context, lhs);
                struct prefixes prefix = create_prefixes(ASM_PREFIX_F2);
                struct opcode opcode = three_byte_opcode(0x38, 0xf1);
                
                if(rhs->state == EMIT_LOCATION_loaded){
                    emit_register_register(context, prefix, opcode, loaded, rhs);
                }else{
                    assert(rhs->state == EMIT_LOCATION_register_relative);
                    emit_register_relative_register(context, prefix, opcode, loaded->loaded_register, rhs);
                }
                if(loaded != lhs) emit_store(context, lhs, loaded);
            }break;
            
            case MEMONIC_prefetch:{
                assert(lhs->state == EMIT_LOCATION_immediate && rhs->state == EMIT_LOCATION_register_relative);
                
                // #define _MM_HINT_NTA 0      vprefetchnta    0F 18 /0
                // #define _MM_HINT_T0 1       vprefetch0      0F 18 /1
                // #define _MM_HINT_T1 2       vprefetch1      0F 18 /2
                // #define _MM_HINT_T2 3       vprefetch2      0F 18 /3
                
                // @cleanup: not sure about these, investigate
                // #define _MM_HINT_ENTA 4     vprefetchenta 
                // #define _MM_HINT_ET0 5      vprefetche0 
                // #define _MM_HINT_ET1 6      vprefetche1 
                // #define _MM_HINT_ET2 7      vprefetche2 
                
                emit_register_relative_extended(context, no_prefix(), two_byte_opcode(0x18), (u8)lhs->value, rhs);
            }break;
            
            case MEMONIC_return_from_inline_asm_function:{
                assert(context->in_inline_asm_function);
                //
                // @cleanup: should this jump out (if its not the last memonic)?
                //
                if(get_register_kind_for_type(context->current_inline_asm_function->type->return_type) == REGISTER_KIND_xmm){
                    context->asm_block_return = emit_load_float(context, operand);
                }else{
                    context->asm_block_return = emit_load_gpr(context, operand);
                }
            }break;
            
            case MEMONIC_bytes:{
                for(u32 index = 0; index < inst->operands[0].size; index++){
                    emit(inst->operands[0].data[index]);
                }
            }break;
            
            default:{
                report_internal_compiler_error(inst->token, "Unimplemented instruction '%.*s'.", inst->token->size, inst->token->data);
                // os_panic(1337);
            }break;
        }
        
        for(smm operand_index = 0; operand_index < inst->amount_of_operands; operand_index++){
            if(operands[operand_index]->state != EMIT_LOCATION_freed) free_emit_location(context, operands[operand_index]);
        }
        
    }
    
    context->inline_asm_mode = null;
    
    //
    // Free all registers that were used by the user.
    //
    for(enum register_kind register_kind = 0; register_kind < REGISTER_KIND_count; register_kind++){
        for(enum register_encoding reg = 0; reg < 16; reg++){
            struct emit_location *loc = context->register_allocators[register_kind].emit_location_map[reg];
            if(loc && loc->inline_asm__was_used_by_user){
                loc->inline_asm__was_used_by_user = false;
                if(loc == context->asm_block_return){
                    // Don't free the return value!
                }else{
                    free_emit_location(context, loc);
                }
            }
        }
    }
    
#undef allocate_specific_register
}

