
//////////////////////////////////////////////////////////////////////////
//                               Parser                                 //
//////////////////////////////////////////////////////////////////////////

func b32 skip_until_tokens_are_balanced(struct context *context, struct token *initial_token, enum token_type open, enum token_type closed, char *error){
    if(!initial_token){
        initial_token = next_token(context);
    }
    
    u64 count = 1;
    while(true){
        if(!in_current_token_array(context)){
            // :Error this should report the beginning and the end
            report_error(context, initial_token, error);
            
            return false;
        }else if(peek_token(context, closed)){
            if (--count == 0) break;
        }else if(peek_token(context, open)){
            count++;
        }
        next_token(context);
    }
    next_token(context);
    
    return true;
}


// unique_serial
// unique()
// :unique
func s32 get_unique_ast_serial(struct context *context){
    s32 ret = context->ast_serializer++;
    
#if 0
    if(ret == 25) os_debug_break();
#endif
    
    return ret;
}

func void _parser_sleep(struct context *context, struct token *sleep_on, u32 line, enum sleep_purpose purpose){
    if(context->should_sleep) return;
    assert(sleep_on);
    
    
    context->should_sleep          = true;
    context->should_exit_statement = true;
    context->sleep_on              = sleep_on;
    context->sleep_purpose         = purpose;
    
    debug_only(context->sleep_line = line;);
}
#define parser_sleep(context, sleep_on, purpose) _parser_sleep(context, sleep_on, __LINE__, purpose)


////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define parser_type_push(context, type) (struct ast_##type *)_parser_type_push(context, sizeof(struct ast_##type), alignof(struct ast_##type), AST_##type)

func struct ast_type *_parser_type_push(struct context *context, smm size, u32 align, enum ast_kind kind){
    
    struct ast_type *type = push_struct_(context->arena, size, align);  // @note: No need to zero, 'arena' never has any non-zero bytes.
    
    type->kind = kind;
    
    return type;
}

//_____________________________________________________________________________________________________________________
// IR building stuff

static struct ir *push_ir(struct context *context, enum ir_kind ir_kind){
    struct ir *ir = push_struct_(&context->ir_arena, sizeof(struct ir), 1); // @note: No need to zero, 'arena' never has any non-zero bytes.
    ir->kind  = ir_kind;
    return ir;
}

static struct ir_jump_node *push_jump(struct context *context, enum ir_kind ir_kind){
    struct ir_jump_node *ir = push_struct_(&context->ir_arena, sizeof(struct ir_jump_node), 1); // @note: No need to zero, 'arena' never has any non-zero bytes.
    ir->base.kind  = ir_kind;
    return ir;
}

// Sometimes when we are constant propagating, we have to un-emit a literal.
#define pop_from_ir_arena(context, ast) pop_from_ir_arena_(context, (u8 *)(ast), sizeof(*ast))
func void pop_from_ir_arena_(struct context *context, u8 *ast_memory, smm size){
    if(!context->should_exit_statement){ // This was causing issues I dunno.
        assert(ast_memory + size == arena_current(&context->ir_arena));
        context->ir_arena.current = ast_memory;
    }
}


#define parser_compound_type_push(context, type) (struct ast_compound_type *)_parser_type_push(context, sizeof(struct ast_compound_type), alignof(struct ast_compound_type), AST_##type);

func struct ast_type *parser_push_pointer_type(struct context *context, struct ast_type *pointer_to, enum ast_kind *defined_type){
    // @cleanup: we could check here if its a basic type and then do some math to figure out the pointer
    // this is kinda complicated, as globals.basic_types is an array of pointers, not basic_types themself.
    // I think we had to iterate
    assert(pointer_to);
    struct ast_pointer_type *ptr = parser_type_push(context, pointer_type);
    ptr->pointer_to = pointer_to;
    ptr->pointer_to_defined_type = defined_type;
    ptr->base.size = 8;
    ptr->base.alignment = 8;
    return cast(struct ast_type *)ptr;
}

func b32 size_is_big_or_oddly_sized(smm size){
    return (size > 8) || (size == 3) || (size == 5) || (size == 6) || (size == 7);
}

func b32 type_is_returned_by_address(struct ast_type *type){
    smm size = type->size;
    if(type->flags & TYPE_FLAG_is_intrin_type) return false;
    if(size_is_big_or_oddly_sized(size)) return true;
    return false;
}

func struct expr ast_push_s64_literal(struct context *context, s64 value, struct token *token){
    struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
    lit->base.kind = IR_integer_literal;
    lit->_s64 = value;
    lit->type = &globals.typedef_s64;
    return (struct expr){.ir = &lit->base, token, &globals.typedef_s64, null};
}

func struct expr ast_push_u64_literal(struct context *context, u64 value, struct token *token){
    
    struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
    lit->base.kind = IR_integer_literal;
    lit->_u64 = value;
    
    enum ast_kind *defined_type = null;
    if(value <= max_u8)  defined_type = &globals.typedef_u8.kind;
    else if(value <= max_u16) defined_type = &globals.typedef_u16.kind;
    else if(value <= max_u32) defined_type = &globals.typedef_u32.kind;
    
    lit->type = &globals.typedef_u64;
    return (struct expr){.ir = &lit->base, token, &globals.typedef_u64, defined_type};
}


func struct expr ast_push_s32_literal(struct context *context, s64 value, struct token *token){
    
    assert(value <= s32_max);
    assert(value >= s32_min);
    struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
    lit->base.kind = IR_integer_literal;
    lit->_s32 = (s32)value;
    
    enum ast_kind *defined_type = null;
    if(value <= max_u8) defined_type = &globals.typedef_u8.kind;
    else if(value <= max_u16) defined_type = &globals.typedef_u16.kind;
    
    lit->type = &globals.typedef_s32;
    return (struct expr){.ir = &lit->base, token, &globals.typedef_s32, defined_type};
}

func s32 integer_literal_as_s32(struct expr *lit){
    struct ir_integer_literal *literal = (struct ir_integer_literal *)lit->ir;
    struct ast_type *type = lit->resolved_type;
    assert(type->kind == AST_integer_type || type->kind == AST_bitfield_type);
    s32 value;
    if(type_is_signed(type)){
        switch(type->size){
            case 1: value = literal->_s8;  break;
            case 2: value = literal->_s16; break;
            case 4: value = literal->_s32; break;
            invalid_default_case(value = 0);
        }
    }else{
        switch(type->size){
            case 1: value = literal->_u8;  break;
            case 2: value = literal->_u16; break;
            case 4: value = literal->_u32; break;
            invalid_default_case(value = 0);
        }
    }
    return value;
}

func u32 integer_literal_as_u32(struct expr *lit){
    struct ir_integer_literal *literal = (struct ir_integer_literal *)lit->ir;
    struct ast_type *type = lit->resolved_type;
    assert(type->kind == AST_integer_type || type->kind == AST_bitfield_type);
    u32 value;
    if(type_is_signed(type)){
        switch(type->size){
            case 1: value = literal->_s8; break;
            case 2: value = literal->_s16; break;
            case 4: value = literal->_s32; break;
            invalid_default_case(value = 0);
        }
    }else{
        switch(type->size){
            case 1: value = literal->_u8; break;
            case 2: value = literal->_u16; break;
            case 4: value = literal->_u32; break;
            invalid_default_case(value = 0);
        }
    }
    return value;
}

func s64 integer_literal_as_s64(struct expr *lit){
    struct ir_integer_literal *literal = (struct ir_integer_literal *)lit->ir;
    struct ast_type *type = lit->resolved_type;
    assert(type->kind == AST_integer_type || type->kind == AST_bitfield_type);
    
    s64 value;
    if(type_is_signed(type)){
        switch(type->size){
            case 1: value = literal->_s8; break;
            case 2: value = literal->_s16; break;
            case 4: value = literal->_s32; break;
            case 8: value = literal->_s64; break;
            invalid_default_case(value = 0);
        }
    }else{
        switch(type->size){
            case 1: value = literal->_u8; break;
            case 2: value = literal->_u16; break;
            case 4: value = literal->_u32; break;
            case 8: value = literal->_u64; break;
            invalid_default_case(value = 0);
        }
    }
    return value;
}

func u64 integer_literal_as_u64(struct expr *lit){
    struct ir_integer_literal *literal = (struct ir_integer_literal *)lit->ir;
    struct ast_type *type = lit->resolved_type;
    assert(type->kind == AST_integer_type || type->kind == AST_bitfield_type);
    u64 value;
    if(type_is_signed(type)){
        switch(type->size){
            case 1: value = (u64)literal->_s8;  break;
            case 2: value = (u64)literal->_s16; break;
            case 4: value = (u64)literal->_s32; break;
            case 8: value = (u64)literal->_s64; break;
            invalid_default_case(value = 0);
        }
    }else{
        switch(type->size){
            case 1: value = literal->_u8;  break;
            case 2: value = literal->_u16; break;
            case 4: value = literal->_u32; break;
            case 8: value = literal->_u64; break;
            invalid_default_case(value = 0);
        }
    }
    return value;
}

static struct ast_type *defined_type_to_type(enum ast_kind *defined_type){
    if(*defined_type == IR_typedef){
        return ((struct ast_declaration *)defined_type)->type;
    }else{
        return (struct ast_type *)defined_type;
    }
}

func enum ast_kind *defined_type_for_binary_op(struct expr *op_lhs, struct expr *op_rhs){
    
    // 
    // The defined type for a binary op should be the smallest defined type,
    // which fits both of the arguments.
    // 
    
    enum ast_kind *lhs = op_lhs->defined_type;
    enum ast_kind *rhs = op_rhs->defined_type;
    
    if(lhs && rhs){
        struct ast_type *lhs_type = defined_type_to_type(lhs);
        struct ast_type *rhs_type = defined_type_to_type(rhs);
        
        return (rhs_type->size > lhs_type->size) ? rhs : lhs;
    }
    
    smm lhs_size = lhs ? defined_type_to_type(lhs)->size : op_lhs->resolved_type->size;
    smm rhs_size = rhs ? defined_type_to_type(rhs)->size : op_rhs->resolved_type->size;
    
    if(lhs && lhs_size >= rhs_size) return lhs;
    if(rhs && rhs_size >= lhs_size) return rhs;
    
    return null;
}

func enum ast_kind *defined_type_for_promotion(struct expr *ast){
    // :retain_type_information_through_promotion
    // if it was promoted, we retain the information about the actual size in the defined_type
    enum ast_kind *defined_type = ast->defined_type;
    if(!defined_type) defined_type = &ast->resolved_type->kind;
    return defined_type;
}

//_____________________________________________________________________________________________________________________

func void push_cast(struct context *context, enum ast_kind lhs_or_rhs, struct ast_type *cast_to, enum ast_kind *cast_to_defined_type, struct expr *cast_what, struct token *cast_what_token){
    
    int should_skip_cast = 0;
    
    if(cast_what->resolved_type == cast_to){
        // set the 'defined_type' we might have casted (float) to (f32)
        should_skip_cast = 1;
    }else if(cast_what->ir->kind == IR_float_literal){
        struct ir_float_literal *f = (struct ir_float_literal *)cast_what->ir;
        
        if(cast_to->kind == AST_integer_type){
            
            struct ir_integer_literal *i = (struct ir_integer_literal *)cast_what->ir;
            static_assert(sizeof(struct ir_float_literal) == sizeof(struct ir_integer_literal));
            
            // @cleanup: warn?
            
            i->base.kind = IR_integer_literal;
            double f_value = f->value;
            
            i->type = cast_to;
            should_skip_cast = 1;
            
            if(type_is_signed(cast_to)){
                switch(cast_to->size){
                    case 1: i->_s8  = (s8) f_value; break;
                    case 2: i->_s16 = (s16)f_value; break;
                    case 4: i->_s32 = (s32)f_value; break;
                    case 8: i->_s64 = (s64)f_value; break;
                    invalid_default_case();
                }
            }else{
                if(f_value < 0.0){
                    report_error(context, cast_what_token, "Cast from negative a floating point number to an unsigned type is illegal.");
                }
                
                if(cast_to == &globals.typedef_Bool){
                    i->_u8 = (_Bool)f_value;
                }else{
                    switch(cast_to->size){
                        case 1: i->_u8  = (u8) f_value; break;
                        case 2: i->_u16 = (u16)f_value; break;
                        case 4: i->_u32 = (u32)f_value; break;
                        case 8: i->_u64 = (u64)f_value; break;
                        invalid_default_case();
                    }
                }
            }
        }else if(cast_to->kind == AST_float_type){
            if(cast_to == &globals.typedef_f32){
                struct ir_float_literal *lit = (struct ir_float_literal *)cast_what->ir;
                // :we_always_keep_double
                lit->value = (f32)lit->value; // cast it to f32 and then back up
            }
            
            f->type = cast_to;
            should_skip_cast = 1;
        }
    }else if(cast_what->ir->kind == IR_integer_literal){
        struct ir_integer_literal *lit = (struct ir_integer_literal *)cast_what->ir;
        if(cast_to == &globals.typedef_Bool){
            lit->_u8 = (_Bool)integer_literal_as_u64(cast_what);
            
            lit->type = cast_to;
            should_skip_cast = 1;
        }else if(cast_to->kind == AST_integer_type){
            // @hmm: I trust my self, that I thought about this...
            if(type_is_signed(cast_to)){
                s64 value = integer_literal_as_s64(cast_what);
                lit->_s64 = value;
            }else{
                u64 value = integer_literal_as_u64(cast_what);
                lit->_u64 = value;
            }
            
            lit->type = cast_to;
            should_skip_cast = 1;
        }else if(cast_to->kind == AST_float_type){
            struct ir_float_literal *f = (struct ir_float_literal *)cast_what->ir;
            
            // :we_always_keep_double
            // @cleanup: this does not really work: I don't think casting to f64 and then to f32 is the same
            //                                      as casting to f32 to begin with, but not sure...
            if(type_is_signed(cast_what->resolved_type)){
                f->value = (f64)integer_literal_as_s64(cast_what);
            }else{
                f->value = (f64)integer_literal_as_u64(cast_what);
            }
            
            f->base.kind = IR_float_literal;
            f->type = cast_to;
            should_skip_cast = 1;
        }else if(cast_to->kind == AST_pointer_type){
            // Extend the integer literal to be 64-bits.
            lit->_s64 = integer_literal_as_s64(cast_what);
            
            // Convert it to a pointer literal simply by setting its type.
            cast_what->ir->kind = IR_pointer_literal;
            static_assert(sizeof(struct ir_pointer_literal) == sizeof(struct ir_integer_literal));
            
            lit->type = cast_to;
            should_skip_cast = 1;
        }else if(cast_to->kind == AST_bitfield_type){
            // 
            // If the value is signed, we have to care about negatives in the range
            // comparison.
            // 
            
            struct ast_bitfield_type *bitfield = (struct ast_bitfield_type *)cast_to;
            
            if(type_is_signed(bitfield->base_type)){
                smm min_value = -(1ll << (bitfield->width - 1));
                smm max_value =  (1ll << (bitfield->width - 1)) - 1;
                
                if(type_is_signed(cast_what->resolved_type)){
                    s64 value = integer_literal_as_u64(cast_what);
                    if(!(min_value <= value && value <= max_value)){
                        report_warning(context, WARNING_compile_time_truncation, cast_what_token, "Integer %lld gets truncated when casted to bitfield of width %u (range is %lld to %lld).", value, bitfield->width, min_value, max_value);
                    }
                }else{
                    u64 value = integer_literal_as_u64(cast_what);
                    if(value > 0x7fffffffffffffff || (smm)value > max_value){
                        report_warning(context, WARNING_compile_time_truncation, cast_what_token, "Integer %llu gets truncated when casted to bitfield of width %u (range is %lld to %lld).", value, bitfield->width, min_value, max_value);
                    }
                }
            }else{
                u64 max_value = (1ull << bitfield->width) - 1;
                
                if(type_is_signed(cast_what->resolved_type)){
                    s64 value = integer_literal_as_u64(cast_what);
                    if(value < 0 || (u64)value > max_value){
                        report_warning(context, WARNING_compile_time_truncation, cast_what_token, "Integer %lld gets truncated when casted to bitfield of width %u (range is %d to %llu).", value, bitfield->width, 0, max_value);
                    }
                }else{
                    u64 value = integer_literal_as_u64(cast_what);
                    if(value > max_value){
                        report_warning(context, WARNING_compile_time_truncation, cast_what_token, "Integer %llu gets truncated when casted to bitfield of width %u (range is %d to %llu).", value, bitfield->width, 0, max_value);
                    }
                }
            }
            
            lit->type = cast_to; // @cleanup: What?
            should_skip_cast = 1;
        }else{
            assert(cast_to->kind == AST_void_type);
        }
    }else if(cast_what->ir->kind == IR_pointer_literal){
        if(cast_to == &globals.typedef_Bool){
            struct ir_integer_literal *lit = (struct ir_integer_literal *)cast_what->ir;
            
            cast_what->ir->kind = IR_integer_literal;
            lit->_u64 = (((struct ir_pointer_literal *)lit)->pointer != 0);
            
            lit->type = cast_to;
            should_skip_cast = 1;
        }else if(cast_to->kind == AST_integer_type){
            // the value in 'cast_what->pointer' gets truncated implicitly.
            struct ir_integer_literal *lit = (struct ir_integer_literal *)cast_what->ir;
            lit->base.kind = IR_integer_literal;
            lit->type = cast_to;
            
            should_skip_cast = 1;
        }else if(cast_to->kind == AST_pointer_type){
            // just properage the type.
            struct ir_pointer_literal *lit = (struct ir_pointer_literal *)cast_what->ir;
            lit->type = cast_to;
            should_skip_cast = 1;
        }
    }else if(cast_to->kind == AST_float_type && cast_what->resolved_type == &globals.typedef_u64){
        report_warning(context, WARNING_casting_u64_to_float, cast_what_token, "Casting an unsigned 64bit number to a floating point type is slow on x64, as x64 only has builtin signed to float support.");
    }
    
    if(!should_skip_cast){
        // no constant propagation
        struct token *token = get_current_token_for_error_report(context);
        if(cast_what) token = cast_what_token;
        
        enum ir_type ir_cast_what = ir_type_from_type(cast_what->resolved_type);
        if(ir_cast_what == IR_TYPE_pointer) ir_cast_what = IR_TYPE_u64;
        if(ir_cast_what == IR_TYPE_bool) ir_cast_what = IR_TYPE_u8;
        enum ir_type ir_cast_to   = ir_type_from_type(cast_to);
        if(ir_cast_to == IR_TYPE_pointer) ir_cast_to = IR_TYPE_u64;
        
        if(ir_cast_to == IR_TYPE_bool){
            enum ir_kind ir_cast = IR_cast_to_bool;
            
            if(lhs_or_rhs == AST_cast_lhs) ir_cast += 1;
            cast_what->ir = push_ir(context, ir_cast);
        }else{
            static enum ir_kind ir_kind_for_cast[IR_TYPE_padding1][IR_TYPE_padding1] = {
                // 
                // Sign extending integers
                // 
                [IR_TYPE_s8] [IR_TYPE_s16] = IR_sign_extend_s8_to_s16,
                [IR_TYPE_s8] [IR_TYPE_s32] = IR_sign_extend_s8_to_s32,
                [IR_TYPE_s16][IR_TYPE_s32] = IR_sign_extend_s16_to_s32,
                [IR_TYPE_s8] [IR_TYPE_s64] = IR_sign_extend_s8_to_s64,
                [IR_TYPE_s16][IR_TYPE_s32] = IR_sign_extend_s16_to_s32,
                [IR_TYPE_s16][IR_TYPE_s64] = IR_sign_extend_s16_to_s64,
                [IR_TYPE_s32][IR_TYPE_s64] = IR_sign_extend_s32_to_s64,
                [IR_TYPE_s8] [IR_TYPE_u16] = IR_sign_extend_s8_to_s16,
                [IR_TYPE_s8] [IR_TYPE_u32] = IR_sign_extend_s8_to_s32,
                [IR_TYPE_s16][IR_TYPE_u32] = IR_sign_extend_s16_to_s32,
                [IR_TYPE_s8] [IR_TYPE_u64] = IR_sign_extend_s8_to_s64,
                [IR_TYPE_s16][IR_TYPE_u32] = IR_sign_extend_s16_to_s32,
                [IR_TYPE_s16][IR_TYPE_u64] = IR_sign_extend_s16_to_s64,
                [IR_TYPE_s32][IR_TYPE_u64] = IR_sign_extend_s32_to_s64,
                
                // 
                // Zero-extending integers
                // 
                [IR_TYPE_u8] [IR_TYPE_s16] = IR_zero_extend_u8_to_u16,
                [IR_TYPE_u8] [IR_TYPE_s32] = IR_zero_extend_u8_to_u32,
                [IR_TYPE_u16][IR_TYPE_s32] = IR_zero_extend_u16_to_u32,
                [IR_TYPE_u8] [IR_TYPE_s64] = IR_zero_extend_u8_to_u64,
                [IR_TYPE_u16][IR_TYPE_s32] = IR_zero_extend_u16_to_u32,
                [IR_TYPE_u16][IR_TYPE_s64] = IR_zero_extend_u16_to_u64,
                [IR_TYPE_u32][IR_TYPE_s64] = IR_zero_extend_u32_to_u64,
                [IR_TYPE_u8] [IR_TYPE_u16] = IR_zero_extend_u8_to_u16,
                [IR_TYPE_u8] [IR_TYPE_u32] = IR_zero_extend_u8_to_u32,
                [IR_TYPE_u16][IR_TYPE_u32] = IR_zero_extend_u16_to_u32,
                [IR_TYPE_u8] [IR_TYPE_u64] = IR_zero_extend_u8_to_u64,
                [IR_TYPE_u16][IR_TYPE_u32] = IR_zero_extend_u16_to_u32,
                [IR_TYPE_u16][IR_TYPE_u64] = IR_zero_extend_u16_to_u64,
                [IR_TYPE_u32][IR_TYPE_u64] = IR_zero_extend_u32_to_u64,
                
                // 
                // Converting doubles to float and back.
                // 
                [IR_TYPE_f32][IR_TYPE_f64] = IR_cast_f32_to_f64,
                [IR_TYPE_f64][IR_TYPE_f32] = IR_cast_f64_to_f32,
                
                // 
                // Converting floats to integers.
                // 
                [IR_TYPE_f32][IR_TYPE_u8] = IR_cast_f32_to_u8,
                [IR_TYPE_f32][IR_TYPE_u16] = IR_cast_f32_to_u16,
                [IR_TYPE_f32][IR_TYPE_u32] = IR_cast_f32_to_u32,
                [IR_TYPE_f32][IR_TYPE_u64] = IR_cast_f32_to_u64,
                [IR_TYPE_f32][IR_TYPE_s8] = IR_cast_f32_to_s8,
                [IR_TYPE_f32][IR_TYPE_s16] = IR_cast_f32_to_s16,
                [IR_TYPE_f32][IR_TYPE_s32] = IR_cast_f32_to_s32,
                [IR_TYPE_f32][IR_TYPE_s64] = IR_cast_f32_to_s64,
                
                [IR_TYPE_f64][IR_TYPE_u8] = IR_cast_f64_to_u8,
                [IR_TYPE_f64][IR_TYPE_u16] = IR_cast_f64_to_u16,
                [IR_TYPE_f64][IR_TYPE_u32] = IR_cast_f64_to_u32,
                [IR_TYPE_f64][IR_TYPE_u64] = IR_cast_f64_to_u64,
                [IR_TYPE_f64][IR_TYPE_s8] = IR_cast_f64_to_s8,
                [IR_TYPE_f64][IR_TYPE_s16] = IR_cast_f64_to_s16,
                [IR_TYPE_f64][IR_TYPE_s32] = IR_cast_f64_to_s32,
                [IR_TYPE_f64][IR_TYPE_s64] = IR_cast_f64_to_s64,
                
                [IR_TYPE_u8][IR_TYPE_f32] = IR_cast_u8_to_f32,
                [IR_TYPE_u16][IR_TYPE_f32] = IR_cast_u16_to_f32,
                [IR_TYPE_u32][IR_TYPE_f32] = IR_cast_u32_to_f32,
                [IR_TYPE_u64][IR_TYPE_f32] = IR_cast_u64_to_f32,
                [IR_TYPE_s8][IR_TYPE_f32] = IR_cast_s8_to_f32,
                [IR_TYPE_s16][IR_TYPE_f32] = IR_cast_s16_to_f32,
                [IR_TYPE_s32][IR_TYPE_f32] = IR_cast_s32_to_f32,
                [IR_TYPE_s64][IR_TYPE_f32] = IR_cast_s64_to_f32,
                
                [IR_TYPE_u8][IR_TYPE_f64] = IR_cast_u8_to_f64,
                [IR_TYPE_u16][IR_TYPE_f64] = IR_cast_u16_to_f64,
                [IR_TYPE_u32][IR_TYPE_f64] = IR_cast_u32_to_f64,
                [IR_TYPE_u64][IR_TYPE_f64] = IR_cast_u64_to_f64,
                [IR_TYPE_s8][IR_TYPE_f64] = IR_cast_s8_to_f64,
                [IR_TYPE_s16][IR_TYPE_f64] = IR_cast_s16_to_f64,
                [IR_TYPE_s32][IR_TYPE_f64] = IR_cast_s32_to_f64,
                [IR_TYPE_s64][IR_TYPE_f64] = IR_cast_s64_to_f64,
                
                // 
                // @note: In the future these maybe do not have to exist.
                // 
                [IR_TYPE_u16][IR_TYPE_u8] = IR_truncate_to_u8,
                [IR_TYPE_u16][IR_TYPE_s8] = IR_truncate_to_u8,
                [IR_TYPE_s16][IR_TYPE_u8] = IR_truncate_to_u8,
                [IR_TYPE_s16][IR_TYPE_s8] = IR_truncate_to_u8,
                
                [IR_TYPE_u32][IR_TYPE_u8] = IR_truncate_to_u8,
                [IR_TYPE_u32][IR_TYPE_s8] = IR_truncate_to_u8,
                [IR_TYPE_s32][IR_TYPE_u8] = IR_truncate_to_u8,
                [IR_TYPE_s32][IR_TYPE_s8] = IR_truncate_to_u8,
                
                [IR_TYPE_u64][IR_TYPE_u8] = IR_truncate_to_u8,
                [IR_TYPE_u64][IR_TYPE_s8] = IR_truncate_to_u8,
                [IR_TYPE_s64][IR_TYPE_u8] = IR_truncate_to_u8,
                [IR_TYPE_s64][IR_TYPE_s8] = IR_truncate_to_u8,
                
                [IR_TYPE_u32][IR_TYPE_u16] = IR_truncate_to_u16,
                [IR_TYPE_u32][IR_TYPE_s16] = IR_truncate_to_u16,
                [IR_TYPE_s32][IR_TYPE_u16] = IR_truncate_to_u16,
                [IR_TYPE_s32][IR_TYPE_s16] = IR_truncate_to_u16,
                
                [IR_TYPE_u64][IR_TYPE_u16] = IR_truncate_to_u16,
                [IR_TYPE_u64][IR_TYPE_s16] = IR_truncate_to_u16,
                [IR_TYPE_s64][IR_TYPE_u16] = IR_truncate_to_u16,
                [IR_TYPE_s64][IR_TYPE_s16] = IR_truncate_to_u16,
                
                [IR_TYPE_u64][IR_TYPE_u32] = IR_truncate_to_u32,
                [IR_TYPE_u64][IR_TYPE_s32] = IR_truncate_to_u32,
                [IR_TYPE_s64][IR_TYPE_u32] = IR_truncate_to_u32,
                [IR_TYPE_s64][IR_TYPE_s32] = IR_truncate_to_u32,
            };
            
            assert(ir_cast_what < IR_TYPE_padding1 && ir_cast_to < IR_TYPE_padding1);
            enum ir_kind ir_cast = ir_kind_for_cast[ir_cast_what][ir_cast_to];
            if(ir_cast){
                if(lhs_or_rhs == AST_cast_lhs) ir_cast += 1;
                cast_what->ir = push_ir(context, ir_cast);
            }
        }
    }
    
    cast_what->resolved_type = cast_to;
    cast_what->defined_type = cast_to_defined_type;
    cast_what->token = cast_what->token;
}

//_____________________________________________________________________________________________________________________


func struct ast_type *perform_integer_promotions(struct ast_type *type){
    if(type == &globals.typedef_Bool) return &globals.typedef_s32;
    if(type == &globals.typedef_s8)   return &globals.typedef_s32;
    if(type == &globals.typedef_u8)   return &globals.typedef_s32;
    if(type == &globals.typedef_s16)  return &globals.typedef_s32;
    if(type == &globals.typedef_u16)  return &globals.typedef_s32;
    
    // "all other types are unchanged"
    return type;
}


func void maybe_insert_cast_from_special_int_to_int(struct context *context, struct expr *operand, int is_lhs){
    if(operand->resolved_type->kind == AST_bitfield_type){
        struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)operand->resolved_type;
        
        // 
        // "If an int can represent all values of the originaly type (as restricted by the width, 
        //  for a bit-field), the value is converted to an int; otherwise it is converted to an
        //  unsigned int." (This assumes all bitfields are either _Bool, signed int or unsigned int).
        // 
        
        // @cleanup: do we want to make bitfield the defined type?
        operand->defined_type = null;
        
        struct ir_load_bitfield *ir_load_bitfield = push_struct(&context->ir_arena, struct ir_load_bitfield);
        ir_load_bitfield->base.kind = IR_load_bitfield + (u8)is_lhs;
        ir_load_bitfield->bitfield_type = bitfield;
        operand->ir = &ir_load_bitfield->base;
        
        if(bitfield->width < 32){
            operand->resolved_type = &globals.typedef_s32;
        }else if(bitfield->width == 32){
            if(type_is_signed(bitfield->base_type)){
                operand->resolved_type = &globals.typedef_s32;
            }else{
                operand->resolved_type = &globals.typedef_u32;
            }
        }else{
            operand->resolved_type = bitfield->base_type;
        }
    }else if(operand->resolved_type->kind == AST_atomic_integer_type){
        // :translate_atomic_to_non_atomic_and_back
        // @note: I don't think there is a reason to actually implement atomic loads for now.
        operand->resolved_type -= (&globals.typedef_atomic_bool - &globals.typedef_Bool);
    }
}

func void perform_integer_promotion_on_literal(struct expr *expr){
    struct ir_integer_literal *lit = (struct ir_integer_literal *)expr->ir;
    if(expr->resolved_type->size >= 4) return;
    
    // sign/zero extend the value to the whole 64-bits.
    lit->_s64 = integer_literal_as_s64(expr);
    
    // :retain_type_information_through_promotion
    enum ast_kind *defined_type = expr->defined_type;
    if(!defined_type) defined_type = &expr->resolved_type->kind;
    
    lit->type = &globals.typedef_s32;
    expr->resolved_type = &globals.typedef_s32;
    expr->defined_type = defined_type;
}

func void maybe_insert_integer_promotion_cast(struct context *context, enum ast_kind lhs_or_rhs, struct expr *operand, struct token *site){
    maybe_insert_cast_from_special_int_to_int(context, operand, (lhs_or_rhs == AST_cast_lhs));
    
    struct ast_type *promoted_type = perform_integer_promotions(operand->resolved_type);
    if(operand->resolved_type != promoted_type){
        // :retain_type_information_through_promotion
        // if it was promoted, we retain the information about the actual size in the defined_type
        enum ast_kind *defined_type = operand->defined_type;
        if(!defined_type) defined_type = &operand->resolved_type->kind;
        
        push_cast(context, lhs_or_rhs, promoted_type, defined_type, operand, site);
    }
}

func struct ast_type *perform_usual_arithmetic_conversions(struct ast_type *lhs, struct ast_type *rhs){
    assert(type_is_arithmetic(lhs) && type_is_arithmetic(rhs));
    
    if(lhs == &globals.typedef_f64) return &globals.typedef_f64;
    if(rhs == &globals.typedef_f64) return &globals.typedef_f64;
    if(lhs == &globals.typedef_f32) return &globals.typedef_f32;
    if(rhs == &globals.typedef_f32) return &globals.typedef_f32;
    
    // "otherwise integer promotions are applied"
    lhs = perform_integer_promotions(lhs);
    rhs = perform_integer_promotions(rhs);
    
    // "if they are of the same type no further conversion is needed"
    if(lhs == rhs) return lhs;
    
    // @cleanup: we ignore 'long' for now
    // if they have same signedness we uppromote the lower
    if(lhs == &globals.typedef_s32 && rhs == &globals.typedef_s64) return &globals.typedef_s64;
    if(rhs == &globals.typedef_s32 && lhs == &globals.typedef_s64) return &globals.typedef_s64;
    if(lhs == &globals.typedef_u32 && rhs == &globals.typedef_u64) return &globals.typedef_u64;
    if(rhs == &globals.typedef_u32 && lhs == &globals.typedef_u64) return &globals.typedef_u64;
    
    // if the unsigned type has greater or equal rank result is the result is the unsigned type
    if(lhs == &globals.typedef_u32 && rhs == &globals.typedef_s32) return &globals.typedef_u32;
    if(rhs == &globals.typedef_u32 && lhs == &globals.typedef_s32) return &globals.typedef_u32;
    if(lhs == &globals.typedef_u64 && rhs == &globals.typedef_s32) return &globals.typedef_u64;
    if(rhs == &globals.typedef_u64 && lhs == &globals.typedef_s32) return &globals.typedef_u64;
    if(lhs == &globals.typedef_u64 && rhs == &globals.typedef_s64) return &globals.typedef_u64;
    if(rhs == &globals.typedef_u64 && lhs == &globals.typedef_s64) return &globals.typedef_u64;
    
    // if all unsigned values can be represented by the signed type we convert to the signed type
    if(lhs == &globals.typedef_u32 && rhs == &globals.typedef_s64) return &globals.typedef_s64;
    if(rhs == &globals.typedef_u32 && lhs == &globals.typedef_s64) return &globals.typedef_s64;
    
    // "otherwise both operands are converted to the unsigned type corresoponding to the signed type"
    if(lhs == &globals.typedef_s32) return &globals.typedef_u32;
    if(rhs == &globals.typedef_s32) return &globals.typedef_u32;
    if(lhs == &globals.typedef_s64) return &globals.typedef_u64;
    if(rhs == &globals.typedef_s64) return &globals.typedef_u64;
    
    invalid_code_path;
}

func void maybe_insert_arithmetic_conversion_casts(struct context *context, struct expr *op_lhs, struct expr *op_rhs, struct token *site){
    
    maybe_insert_cast_from_special_int_to_int(context, op_lhs, /*is_lhs*/1);
    maybe_insert_cast_from_special_int_to_int(context, op_rhs, /*is_lhs*/0);
    
    if(!type_is_arithmetic(op_lhs->resolved_type) || !type_is_arithmetic(op_rhs->resolved_type)) return;
    
    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op_lhs->resolved_type, op_rhs->resolved_type);
    if(!promoted_type) return;
    
    if(op_lhs->resolved_type != promoted_type){
        // :retain_type_information_through_promotion
        // if it was promoted, we retain the information about the actual size in the defined_type
        enum ast_kind *defined_type = op_lhs->defined_type;
        if(!defined_type) defined_type = &op_lhs->resolved_type->kind;
        push_cast(context, AST_cast_lhs, promoted_type, defined_type, op_lhs, site);
    }
    
    if(op_rhs->resolved_type != promoted_type){
        enum ast_kind *defined_type = op_rhs->defined_type;
        if(!defined_type) defined_type = &op_rhs->resolved_type->kind;
        push_cast(context, AST_cast, promoted_type, defined_type, op_rhs, site);
    }
}

func void maybe_cast_literal_0_to_void_pointer(struct expr *lhs, struct expr *rhs){
    
    if(lhs->resolved_type->kind == AST_pointer_type && rhs->ir->kind == IR_integer_literal){
        
        if(integer_literal_as_u64(rhs) == 0){
            rhs->ir->kind = IR_pointer_literal;
            rhs->resolved_type = lhs->resolved_type;
            rhs->defined_type  = lhs->defined_type;
            ((struct ir_pointer_literal *)rhs->ir)->type = lhs->resolved_type;
        }
        return;
    }
    
    if(rhs->resolved_type->kind == AST_pointer_type && lhs->ir->kind == IR_integer_literal){
        if(integer_literal_as_u64(lhs) == 0){
            lhs->ir->kind = IR_pointer_literal;
            lhs->resolved_type = rhs->resolved_type;
            lhs->defined_type  = rhs->defined_type;
            ((struct ir_pointer_literal *)lhs->ir)->type = rhs->resolved_type;
        }
    }
}

func void maybe_insert_cast_from_void_pointer(struct expr *lhs, struct expr *rhs){
    
    if(lhs->resolved_type->kind != AST_pointer_type) return;
    if(rhs->resolved_type->kind != AST_pointer_type) return;
    
    struct ast_pointer_type *lhs_pointer = (struct ast_pointer_type *)lhs->resolved_type;
    struct ast_pointer_type *rhs_pointer = (struct ast_pointer_type *)rhs->resolved_type;
    
    if(lhs_pointer->pointer_to == &globals.typedef_void){
        lhs->resolved_type = rhs->resolved_type;
        return;
    }
    
    if(rhs_pointer->pointer_to == &globals.typedef_void){
        rhs->resolved_type = lhs->resolved_type;
        return;
    }
}

//_____________________________________________________________________________________________________________________

func struct ast_scope *parser_push_new_scope(struct context *context, struct token *token, enum scope_flags flags){
    
    struct ast_scope *scope = push_struct(context->arena, struct ast_scope);
    scope->flags = flags;
    scope->token = token;
    
    struct ast_scope *parent = context->current_scope;
    if(parent){
        if(parent->subscopes.last){ // @note: sll_push_back does not work because of the `subscopes` member.
            parent->subscopes.last->subscopes.next = scope;
            parent->subscopes.last = scope;
        }else{
            parent->subscopes.first = scope;
            parent->subscopes.last  = scope;
        }
        parent->subscopes.count += 1;
        
        scope->parent = parent;
    }
    
    context->current_scope = scope;
    
    return scope;
}

func void parser_scope_pop(struct context *context, struct ast_scope *scope){
    assert(context->current_scope == scope);
    context->current_scope = context->current_scope->parent;
}

//_____________________________________________________________________________________________________________________

struct type_info_return{
    struct ast_type *type;
    enum ast_kind *defined_type;
};

func void sleep_or_error_on_unresolved_type(struct context *context, struct ast_type *type){
    struct ast_unresolved_type *unresolved = cast(struct ast_unresolved_type *)type;
    if(globals.compile_stage < COMPILE_STAGE_parse_function){
        parser_sleep(context, unresolved->sleeping_on, SLEEP_on_struct);
    }else{
        // :Error, used ?
        
        struct string type_prefix = type_prefix_for_unresolved_type(unresolved);
        
        report_error(context, unresolved->sleeping_on, "Undeclared %.*s.", type_prefix.size, type_prefix.data);
    }
}

func int maybe_resolve_unresolved_type_or_sleep_or_error(struct context *context, struct ast_type **in_out_type){
    //
    // @warning: @cleanup: multiple threads could be doing this at the same time...
    //
    
    struct ast_type *type = atomic_load(struct ast_type *, *in_out_type);
    
    if(type->kind == AST_unresolved_type){
        struct ast_type *resolved = lookup_unresolved_type(type);
        
        if(!resolved){
            sleep_or_error_on_unresolved_type(context, type);
            return 1;
        }else{
            if(resolved->kind == AST_enum){
                // @cleanup: this is sort of weird... this should maybe also take a defined type?
                *in_out_type = &globals.typedef_s32;
            }else{
                *in_out_type = resolved;
            }
        }
    }
    return 0;
}


func int maybe_resolve_unresolved_type(struct ast_type **in_out_type){
    
    struct ast_type *type = atomic_load(struct ast_type *, *in_out_type);
    
    if(type->kind == AST_unresolved_type){
        struct ast_type *resolved = lookup_unresolved_type(type);
        
        if(!resolved) return 1;
        
        // @threading: This should maybe be done atomically?
        *in_out_type = resolved;
    }
    
    return 0;
}


func struct ast_type *types_are_equal(struct ast_type *wanted, struct ast_type *given){
    if(wanted == given) return wanted;
    if(wanted == &globals.typedef_poison) return &globals.typedef_poison;
    if(given  == &globals.typedef_poison) return &globals.typedef_poison;
    
    // If they are both unresolved types, but the names are the same, they match.
    if(wanted->kind == AST_unresolved_type && given->kind == AST_unresolved_type){
        struct ast_unresolved_type *wanted_unresolved = cast(struct ast_unresolved_type *)wanted;
        struct ast_unresolved_type *given_unresolved  = cast(struct ast_unresolved_type *)given;
        if(atoms_match(wanted_unresolved->sleeping_on->atom, given_unresolved->sleeping_on->atom)){
            return wanted;
        }else{
            return null;
        }
    }
    
    // 
    // If either of the types is unresolved, try to look it up.
    // If we failed to look it up, it cannot match the other type, as the other type is resolved.
    // 
    // @note: Because of the check above, one of the types is resolved. This means there cannot be a race-condition here.
    // 
    if(wanted->kind == AST_unresolved_type) wanted = lookup_unresolved_type(wanted);
    if(given->kind  == AST_unresolved_type) given  = lookup_unresolved_type(given);
    if(!wanted || !given) return null;
    
    struct ast_type *ret = wanted;
    if(wanted == given) return ret;
    
    if(wanted->kind == AST_pointer_type && given->kind == AST_pointer_type){
        struct ast_pointer_type *wanted_pointer = cast(struct ast_pointer_type *)wanted;
        struct ast_pointer_type *given_pointer  = cast(struct ast_pointer_type *)given;
        
        while(wanted_pointer->pointer_to->kind == AST_pointer_type && given_pointer->pointer_to->kind == AST_pointer_type){
            wanted_pointer = cast(struct ast_pointer_type *)wanted_pointer->pointer_to;
            given_pointer  = cast(struct ast_pointer_type *)given_pointer->pointer_to;
        }
        
        // 
        // If they are both unresolved types, but the types are the same, they match.
        // 
        if(wanted_pointer->pointer_to->kind == AST_unresolved_type && given_pointer->pointer_to->kind == AST_unresolved_type){
            struct ast_unresolved_type *wanted_unresolved = cast(struct ast_unresolved_type *)wanted_pointer->pointer_to;
            struct ast_unresolved_type *given_unresolved  = cast(struct ast_unresolved_type *)given_pointer->pointer_to;
            if(atoms_match(wanted_unresolved->sleeping_on->atom, given_unresolved->sleeping_on->atom)){
                return wanted;
            }else{
                return null;
            }
        }
        
        if(maybe_resolve_unresolved_type(&wanted_pointer->pointer_to)) return null;
        if(maybe_resolve_unresolved_type(&given_pointer->pointer_to)) return null;
        
        wanted = wanted_pointer->pointer_to;
        given  = given_pointer->pointer_to;
    }
    
    assert(wanted && given);
    if(wanted == given) return ret;
    if(wanted->kind != given->kind) return null;
    
    if(wanted->kind == AST_function_type){
        struct ast_function_type *wanted_function = cast(struct ast_function_type *)wanted;
        struct ast_function_type *given_function  = cast(struct ast_function_type *)given;
        
        if(wanted_function->argument_list.count != given_function->argument_list.count) return null;
        
        if(!types_are_equal(wanted_function->return_type, given_function->return_type)){
            return null;
        }
        
        struct ast_list_node *given_it = given_function->argument_list.first;
        for_ast_list(wanted_function->argument_list){
            struct ast_declaration *wanted_decl = cast(struct ast_declaration *)it->value;
            struct ast_declaration *given_decl  = cast(struct ast_declaration *)given_it->value;
            
            // @note: these patch the pointer, if they find an unresolved type
            maybe_resolve_unresolved_type(&wanted_decl->type);
            maybe_resolve_unresolved_type(&given_decl->type);
            
            if(!types_are_equal(wanted_decl->type, given_decl->type)) return null;
            given_it = given_it->next;
        }
        assert(given_it == null);
        return ret;
    }
    
    if(wanted->kind == AST_array_type){
        struct ast_array_type *wanted_array = (struct ast_array_type *)wanted;
        struct ast_array_type *given_array  = (struct ast_array_type *)given;
        
        if(wanted_array->amount_of_elements != given_array->amount_of_elements) return null;
        
        if(!types_are_equal(wanted_array->element_type, given_array->element_type)){
            return null;
        }
        return ret;
    }
    
    if(wanted->kind == AST_struct || wanted->kind == AST_union){
        struct ast_compound_type *wanted_compound = cast(struct ast_compound_type *)wanted;
        struct ast_compound_type *given_compound  = cast(struct ast_compound_type *)given;
        
        if(!atoms_match(wanted_compound->identifier->atom, given_compound->identifier->atom)) return null;
        if(wanted_compound->amount_of_members != given_compound->amount_of_members) return null;
        
        // The 'current_max' is a function of the the 'amount_of_members' as we only ever add members.
        assert(wanted_compound->current_max_amount_of_members == given_compound->current_max_amount_of_members);
        
        for(u32 member_index = 0; member_index < wanted_compound->amount_of_members; member_index++){
            struct compound_member *wanted_member = &wanted_compound->members[member_index];
            struct compound_member *given_member  = &given_compound->members[member_index];
            
            if(!atoms_match(wanted_member->name->atom, given_member->name->atom)) return null;
            if(!types_are_equal(wanted_member->type, given_member->type)) return null;
            if(wanted_member->offset_in_type != given_member->offset_in_type) return null;
        }
        
        return ret;
    }
    
    if(wanted->kind == AST_enum){
        struct ast_compound_type *wanted_enum = cast(struct ast_compound_type *)wanted;
        struct ast_compound_type *given_enum  = cast(struct ast_compound_type *)given;
        if(!atoms_match(wanted_enum->identifier->atom, given_enum->identifier->atom)) return null;
        return ret;
    }
    
    if(wanted->kind == AST_bitfield_type){
        struct ast_bitfield_type *wanted_bitfield = cast(struct ast_bitfield_type *)wanted;
        struct ast_bitfield_type *given_bitfield  = cast(struct ast_bitfield_type *)given;
        
        if(wanted_bitfield->bit_index != given_bitfield->bit_index) return null;
        if(wanted_bitfield->width != given_bitfield->width) return null;
        if(!types_are_equal(wanted_bitfield->base_type, given_bitfield->base_type)) return null;
        return ret;
    }
    
    // @cleanup: can we assert here that the types in the end are basic tpyes?
    // they match iff they are equal in the end. (this is the case for example for int * == int *)
    return (wanted == given) ? ret : null;
}

func struct string report_type_mismatch__internal(struct context *context, char *prefix, struct ast_type *type, enum ast_kind *defined_type){
    struct string ret = zero_struct;
    
    b32 handled = false;
    if(defined_type){
        if(*defined_type == IR_typedef){
            struct ast_declaration *decl = cast(struct ast_declaration *)defined_type;
            struct string lhs_string = push_type_string(context->arena, &context->scratch, decl->type);
            ret = push_format_string(&context->scratch, "%s '%.*s' (aka %.*s)", prefix, decl->identifier->amount, decl->identifier->data, lhs_string.amount, lhs_string.data);
            handled = true;
        }else if(*defined_type == AST_enum){
            struct string lhs_string = push_type_string(context->arena, &context->scratch, cast(struct ast_type *)defined_type);
            ret = push_format_string(&context->scratch, "%s '%.*s'", prefix, lhs_string.amount, lhs_string.data);
            handled = true;
        }
    }
    
    if(!handled){
        struct string lhs_string = push_type_string(context->arena, &context->scratch, type);
        ret = push_format_string(&context->scratch, "%s '%.*s'", prefix, lhs_string.amount, lhs_string.data);
    }
    return ret;
}

func void report_type_mismatch_error(struct context *context, struct ast_type *lhs, enum ast_kind *lhs_defined_type, struct ast_type *rhs, enum ast_kind *rhs_defined_type, struct token *location){
    struct string lhs_string = report_type_mismatch__internal(context, "Wanted", lhs, lhs_defined_type);
    struct string rhs_string = report_type_mismatch__internal(context, "given", rhs, rhs_defined_type);
    
    report_error(context, location, "%.*s %.*s.", lhs_string.amount, lhs_string.data, rhs_string.amount, rhs_string.data);
}


func void report_type_mismatch_warning(struct context *context, struct ast_type *lhs, enum ast_kind *lhs_defined_type, struct ast_type *rhs, enum ast_kind *rhs_defined_type, struct token *location){
    struct string lhs_string = report_type_mismatch__internal(context, "Wanted", lhs, lhs_defined_type);
    struct string rhs_string = report_type_mismatch__internal(context, "given", rhs, rhs_defined_type);
    
    report_warning(context, WARNING_type_mismatch, location, "%.*s %.*s.", lhs_string.amount, lhs_string.data, rhs_string.amount, rhs_string.data);
}

func b32 casts_implicitly_to_bool(struct ast_type *resolved_type){
    
    if(&globals.typedef_void == resolved_type){
        return false;
    }
    
    if(resolved_type->kind == AST_struct || resolved_type->kind == AST_union){
        return false;
    }
    
    if(resolved_type->kind == AST_function_type){
        return false;
    }
    
    if(resolved_type->kind == AST_array_type){
        return false; // we should have loaded the array. (also this does not make that much sense)
    }
    
    if(resolved_type->kind == AST_bitfield_type){
        return false; // we should have loaded the bitfield.
    }
    
    return true;
}

//_____________________________________________________________________________________________________________________
// 

enum type_qualifiers{
    QUALIFIER_const     = 0x1,
    QUALIFIER_volatile  = 0x2,
    QUALIFIER_restrict  = 0x4,
    QUALIFIER_atomic    = 0x8,
    
    QUALIFIER_unaligned = 0x10,
    QUALIFIER_ptr32     = 0x20,
    QUALIFIER_ptr64     = 0x40,
};

struct declarator_return{
    struct token *ident;
    struct ast_type *type;
    enum ast_kind *defined_type;
};

func struct ast_declaration *push_declaration_for_declarator(struct context *context, struct declarator_return declarator){
    struct ast_declaration *decl = push_struct(context->arena, struct ast_declaration);
    decl->kind         = IR_declaration;
    decl->type         = declarator.type;
    decl->defined_type = declarator.defined_type;
    decl->identifier   = declarator.ident;
    decl->offset_on_stack = -1;
    return decl;
}

enum declarator_kind_flags{
    DECLARATOR_type_name  = 1,
    DECLARATOR_identifier = 2,
};

enum declaration_specifier_flag{
    // 
    // C storage-class specifiers.
    // 
    SPECIFIER_typedef  = 0x1,
    SPECIFIER_extern   = 0x2,
    SPECIFIER_static   = 0x4,
    SPECIFIER_register = 0x8,
    // SPECIFIER_auto
    SPECIFIER_thread_local = 0x10,
    
    // 
    // __declspec storage-class specifiers.
    // 
    SPECIFIER_dllimport = 0x20,
    SPECIFIER_dllexport = 0x40,
    SPECIFIER_selectany = 0x80,
    
    SPECIFIER_packed    = 0x100,
    
    // 
    // function-specifiers.
    // 
    SPECIFIER_inline     = 0x200,
    SPECIFIER_noreturn   = 0x400,
    SPECIFIER_noinline   = 0x800,
    
    // 
    // HLC-specific declspecs.
    // 
    SPECIFIER_inline_asm = 0x1000,
    SPECIFIER_printlike  = 0x2000,
    
};

struct declaration_specifiers{
    
    enum ast_kind *defined_type_specifier;
    struct ast_type *type_specifier;
    u64 specifier_flags;
    
    u64 alignment;
};

func struct expr invalid_ast(struct context *context){
    struct ir *invalid = push_ir(context, AST_invalid);
    return (struct expr){.ir = invalid, get_current_token_for_error_report(context), &globals.typedef_poison};
}

// :forward_declarations :predeclarations
func struct declaration_specifiers parse_declaration_specifiers(struct context *context, struct ast_type *optional_type_specifier, enum ast_kind *optional_defined_type_speicifer);
func struct expr parse_expression(struct context *context, b32 should_skip_comma_expression);
func struct declaration_list parse_declaration_list(struct context *context, struct ast_type *optional_type, enum ast_kind *optional_defined_type);
func struct declarator_return parse_declarator(struct context *context, struct ast_type *initial_type, enum ast_kind *initial_defined_type, enum declarator_kind_flags declarator_kind_flags);

func int parse_declaration_list_in_imperative_scope(struct context *context, struct ast_type *optional_type, enum ast_kind *optional_defined_type){
    assert(context->current_scope);
    
    struct declaration_list declaration_list = parse_declaration_list(context, optional_type, optional_defined_type);
    return declaration_list.first && declaration_list.first->decl->kind == IR_function;
}

func struct ast_type *push_unresolved_type(struct context *context, struct token *sleep_on, enum ast_kind kind){
    
    // :unresolved_types
    //
    // An 'unresolved_type' is one of
    //     1) struct unresolved;
    //     2) union  unresolved;
    //     3) enum   unresolved;
    // These have to be allowed to exists in a bunch of places without ever being defined.
    // We will try to look up the 'unresolved_type' when we ever use it.
    // If it is not defined then, we either sleep or error!
    //
    
    struct ast_unresolved_type *unresolved = parser_type_push(context, unresolved_type);
    unresolved->base.size = -1; // hopefully we fuck up the system enough so that it asserts if we ever accidently use this
    unresolved->containing_scope = context->current_scope;
    unresolved->kind = kind;
    unresolved->sleeping_on = sleep_on;
    
    return &unresolved->base;
}

//_____________________________________________________________________________________________________________________
// expressions 


enum CHECK_flag{
    CHECK_none    = 0,
    CHECK_integer = 0x1,
    CHECK_float   = 0x2,
    CHECK_pointer = 0x4,
    //CHECK_array   = 0x8,
    
    // @cleanup: CHECK_scalar, CHECK_arithmetic and so on
    
    CHECK_basic   = CHECK_integer | CHECK_float | CHECK_pointer, // @cleanup: I think this is scalar?
};

func b32 check_for_basic_types__internal(struct context *context, struct ast_type *type, u32 flags, char *prefix, struct token *token){
    
    b32 okay = false;
    if((flags & CHECK_integer) && (type->kind == AST_integer_type))        okay = true;
    if((flags & CHECK_integer) && (type->kind == AST_bitfield_type))       okay = true;
    if((flags & CHECK_integer) && (type->kind == AST_atomic_integer_type)) okay = true;
    if((flags & CHECK_pointer) && (type->kind == AST_pointer_type))        okay = true;
    if((flags & CHECK_pointer) && (type->kind == AST_array_type))          okay = true;
    if((flags & CHECK_float)   && (type->kind == AST_float_type))          okay = true;
    
    if(!okay){
        char *msg;
        if(flags == CHECK_basic){
            msg = "basic";
        }else if(flags == (CHECK_integer | CHECK_pointer)){
            msg = "integer or pointer";
        }else if(flags == CHECK_integer){
            msg = "integer";
        }else if(flags == (CHECK_integer | CHECK_float)){
            msg = "integer or floating point";
        }else{
            msg = "";
            not_implemented;
        }
        
        struct string type_string = push_type_string(context->arena, &context->scratch, type);
        struct string token_string = token_get_string(token);
        report_error(context, token, "%s '%.*s' has to be of %s type, but is of type '%.*s'.", prefix, token_string.amount, token_string.data, msg, type_string.size, type_string.data);
        return false;
    }
    return true;
}

// @hmm name..., this does not take the unary but the operand... this is different from check_binary
func b32 check_unary_for_basic_types(struct context *context, struct ast_type *type, u32 flags, struct token *location){
    return check_for_basic_types__internal(context, type, flags, "Operand of unary", location);
}

func b32 check_binary_for_basic_types(struct context *context, struct ast_type *lhs_type, struct ast_type *rhs_type, struct token *site, u32 flags){
    b32 lhs_ok = check_for_basic_types__internal(context, lhs_type, flags, "Left of", site);
    b32 rhs_ok = check_for_basic_types__internal(context, rhs_type, flags, "Right of", site);
    return (lhs_ok && rhs_ok);
}

#if defined(__clang__)
#define __readeflags __builtin_ia32_readeflags_u64
#endif

func void push_dot_or_arrow(struct context *context, struct compound_member *found, struct expr *operand, enum ir_kind ir_kind){
    
    // :unresolved_types
    //
    // we used to care about unresolved types here. This is not necessary anymore:
    //     1) If you are worried about the 'operand' being a pointer to an unresolved type:
    //        This is handled by the outside. It calls 
    //        'maybe_resolve_pointer_to_unresolved_type_or_sleep_or_error'.
    //     2) If you are worried about the 'member' we return being 'unresolved':
    //        It can only ever be a pointer to an unresolved type as stuff like:
    //             struct asd { struct unresolved a; };
    //        Sleeps on 'unresovled' when parsing the structure.
    //                                                                        07.02.2023
    
    struct ir *ret = 0;
    
    if(operand->ir->kind == IR_pointer_literal){
        assert(ir_kind == IR_member_deref); // We cannot use '.' on a pointer.
        
        struct ir_pointer_literal *pointer = (struct ir_pointer_literal *)operand->ir;
        pointer->base.kind = IR_pointer_literal_deref;
        pointer->pointer += found->offset_in_type;
        
        ret = &pointer->base;
        pointer->type = found->type;
    }else if(operand->ir->kind == IR_pointer_literal_deref && ir_kind == IR_member){
        // We are here --------------v
        // ((struct arst *)0)->member.submember
        
        struct ir_pointer_literal *pointer_literal_deref = (struct ir_pointer_literal *)operand->ir;
        pointer_literal_deref->pointer += found->offset_in_type;
        ret = &pointer_literal_deref->base;
        pointer_literal_deref->type = found->type;
    }else{
        struct ir_dot_or_arrow *op = push_struct(&context->ir_arena, struct ir_dot_or_arrow);
        op->base.kind = ir_kind;
        op->member = found;
        ret = &op->base;
    }
    
    operand->ir = ret;
    operand->resolved_type = found->type;
    operand->defined_type  = found->defined_type;
}

func struct compound_member *find_member_in_compound(struct ast_compound_type *compound, struct atom ident){
    
    for(u32 index = 0; index < compound->amount_of_members; index++){
        if(atoms_match(compound->members[index].name->atom, ident)){
            return &compound->members[index];
        }
    }
    
    // The ident is not part of the compound.
    return null;
}

func void handle_dot_or_arrow(struct context *context, struct ast_compound_type *compound, struct expr *operand, enum ir_kind ir_kind){
    
    char *error = "Expected an identifier following '->'.";
    if(ir_kind == IR_member){
        error = "Expected an identifier following '.'.";
    }
    
    struct token *member = expect_token(context, TOKEN_identifier, error);
    if(context->should_exit_statement) return;
    
    struct compound_member *found = find_member_in_compound(compound, member->atom);
    if(!found){
        char *struct_or_union = (compound->base.kind == AST_struct) ? "structure" : "union";
        
        begin_error_report(context);
        report_error(context, member, "Identifier is not a member of %s.", struct_or_union);
        report_error(context, compound->identifier, "... Here is the definition of the %s.", struct_or_union);
        end_error_report(context);
        return;
    }
    
    push_dot_or_arrow(context, found, operand, ir_kind);
}

//_____________________________________________________________________________________________________________________
// Ast stack

func struct ast_stack_entry *ast_stack_push(struct context *context, enum ast_kind kind, struct token *token, struct expr *operand){
    if(context->ast_stack_at + 1 > array_count(context->ast_stack)){
        
        smm i = context->ast_stack_at - 1;
        while(token->type == TOKEN_invalid && i >= 0){
            token = context->ast_stack[i--].token;
        }
        
        assert(token->type != TOKEN_invalid);
        report_error(context, token, "Expression nests to deep.");
        return &context->ast_stack[0]; // @cleanup: not sure.
    }
    
    struct ast_stack_entry *entry = &context->ast_stack[context->ast_stack_at++];
    entry->ast_kind = kind;
    entry->token    = token;
    if(operand) entry->operand  = *operand;
    return entry;
}

func struct ast_stack_entry *ast_stack_pop(struct context *context){
    assert(context->ast_stack_at > 0);
    return &context->ast_stack[--context->ast_stack_at];
}

func enum ast_kind ast_stack_current(struct context *context){
    if(context->ast_stack_at > 0){
        return context->ast_stack[context->ast_stack_at - 1].ast_kind;
    }
    return AST_invalid;
}

//_____________________________________________________________________________________________________________________

// :function_line_information
void function_maybe_add_line_information(struct context *context, struct token *token){
    if(token->file_index == context->function_file_index && token->line != context->last_line_pushed){
        context->last_line_pushed = token->line;
        
        struct ast_function *current_function = context->current_function;
        
        u32 offset = (u32)(arena_current(&context->ir_arena) - current_function->start_in_ir_arena);
        
        if(context->last_offset_pushed == offset){
            // @note: If the last statement did not produce code, we update the line.
            
            struct function_line_information *last_entry = current_function->line_information.data + current_function->line_information.size - 1;
            last_entry->line = token->line;
        }else{
            dynarray_maybe_grow(struct function_line_information, context->arena, current_function->line_information.data, current_function->line_information.size, current_function->line_information.capacity);
            
            struct function_line_information *new_entry = current_function->line_information.data + current_function->line_information.size++;
            
            new_entry->line   = token->line;
            new_entry->offset = offset;
        }
        
        context->last_offset_pushed = offset;
    }
}

//_____________________________________________________________________________________________________________________

func struct expr parse_constant_integer_expression(struct context *context, int should_skip_comma_expression, char *message){
    
    struct expr constant_expression = parse_expression(context, should_skip_comma_expression);
    
    struct ir_integer_literal *ret = push_struct(context->arena, struct ir_integer_literal);
    
    if(constant_expression.ir->kind != IR_integer_literal){
        report_error(context, constant_expression.token, message);
        ret->base.kind = IR_integer_literal;
        ret->_u64 = 0;
        ret->type = &globals.typedef_s32;
        constant_expression.resolved_type = &globals.typedef_s32;
    }else{
        *ret = *(struct ir_integer_literal *)constant_expression.ir;
        pop_from_ir_arena(context, (struct ir_integer_literal *)constant_expression.ir);
    }
    
    constant_expression.ir = &ret->base;
    return constant_expression;
}


// @cleanup: Rename to 'maybe_parse_type_name' ?
static struct type_info_return maybe_parse_type_for_cast_or_sizeof(struct context *context){
    
    struct type_info_return ret = zero_struct;
    
    // 
    // type-name:
    //     specifier-qualifier-list abstract-declarator_opt
    // 
    // specifier-qualifier-list:
    //     type-qualifier specifier-qualifier-list_opt
    //     type-specifier specifier-qualifier-list_opt
    //     
    
    struct ast_type *type_specifier = null;
    enum ast_kind *defined_type_specifier = null;
    
    struct token *token = get_current_token(context);
    switch(token->type){
        case TOKEN_identifier:{
            struct ast_declaration *ast_typedef = lookup_typedef(context, context->current_compilation_unit, token, /*silent*/true);
            
            // This was not a typedef, hence '(identifier' does not start a type-name.
            if(!ast_typedef) return ret;
            
            next_token(context);
            type_specifier = ast_typedef->type;
            defined_type_specifier = &ast_typedef->kind;
        }break;
        
        
        // 
        // C23 allows storage class specifiers for compound literals. @incomplete:
        // 
        // case TOKEN_static: case TOKEN_register:
        
        
        // 
        // All tokens staring a type-name:
        // 
        case TOKEN_volatile: case TOKEN_const: case TOKEN_unaligned: case TOKEN_restrict: case TOKEN_atomic:
        
        case TOKEN_signed: case TOKEN_unsigned:
        
        case TOKEN_void: case TOKEN_Bool:
        case TOKEN_char: case TOKEN_short: case TOKEN_int:   case TOKEN_long:
        case TOKEN_int8: case TOKEN_int16: case TOKEN_int32: case TOKEN_int64:
        
        case TOKEN_float: case TOKEN_double:
        
        case TOKEN_struct: case TOKEN_union: case TOKEN_enum: break;
        
        // 
        // All other token do not start a type name, and thus it is not a '(type-name)'
        // 
        default: return ret;
    }
    
    
    struct declaration_specifiers specifiers = parse_declaration_specifiers(context, type_specifier, defined_type_specifier);
    if(context->should_exit_statement) return ret;
    
    ret.type = specifiers.type_specifier;
    ret.defined_type = specifiers.defined_type_specifier;
    
    // 
    // Check if there is no abstract-declarator.
    // 
    if(!peek_token(context, TOKEN_closed_paren)){
        // 
        // Parse the abstract-declarator.
        // 
        struct declarator_return declarator = parse_declarator(context, ret.type, ret.defined_type, DECLARATOR_type_name);
        
        ret.type = declarator.type;
        ret.defined_type = declarator.defined_type;
    }
    
    maybe_resolve_unresolved_type_or_sleep_or_error(context, &ret.type);
    
    return ret;
}

func void maybe_load_address_for_array_or_function(struct context *context, enum ir_kind rhs_or_lhs, struct expr *operand){
    struct ir *ir = operand->ir;
    
    if(operand->resolved_type->kind == AST_array_type){
        
        struct ast_array_type *array = (struct ast_array_type *)operand->resolved_type;
        struct ast_type *pointer_type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type);
        
        if(ir->kind == IR_pointer_literal_deref){
            // @hack: constant propergate an edge case needed for 'FIELD_OFFSET(struct s, array[1])';
            //        or written out: '(smm)&(((struct s *)0)->array[1])'.
            //        here '((struct s *)0)->array' gets subscripted and  therefore we load the address of it.
            ir->kind = IR_pointer_literal;
            
            struct ir_pointer_literal *lit = (struct ir_pointer_literal *)ir;
            lit->type = pointer_type;
        }else{
            ir = push_ir(context, rhs_or_lhs);
        }
        
        operand->ir = ir;
        operand->resolved_type = pointer_type;
        operand->defined_type = null;
    }else if(operand->resolved_type->kind == AST_function_type){
        
        struct ast_type *pointer = parser_push_pointer_type(context, operand->resolved_type, null);
        
        operand->ir = push_ir(context, rhs_or_lhs);
        operand->resolved_type = pointer;
        operand->defined_type  = null;
    }
}

func void handle_pointer_arithmetic(struct context *context, enum ir_kind ir_kind, struct token *token, struct expr *op_lhs, struct expr *op_rhs, struct expr *operand, int need_swap){
    
    // @cleanup: where do we check for void?
    
    // op should probably be either '+', '-', '+=' or '-=' but whatever
    
    struct expr *pointer = op_lhs;
    struct expr *integer = op_rhs;
    
    assert(pointer->resolved_type->kind == AST_pointer_type);
    assert(integer->resolved_type->kind == AST_integer_type);
    
    struct ast_pointer_type *pointer_type = (struct ast_pointer_type *)pointer->resolved_type;
    
    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer_type->pointer_to)) return;
    
    struct ast_type *deref_type = pointer_type->pointer_to;
    
    if(deref_type == &globals.typedef_void){
        report_error(context, token, "Cannot use pointer arithmetic on pointer to 'void'.");
        return;
    }
    
    if(deref_type->kind == AST_function_type){
        report_error(context, token, "Cannot use pointer arithmetic on function-pointer.");
        return;
    }
    
    assert(deref_type->size >= 0);
    
    if(integer->ir->kind == IR_integer_literal && pointer->ir->kind == IR_pointer_literal){
        struct ir_integer_literal *lit = (struct ir_integer_literal *)integer->ir;
        
        struct ir_pointer_literal *pointer_lit = (struct ir_pointer_literal *)pointer->ir;
        if(ir_kind == IR_add_u64){
            pointer_lit->pointer += integer_literal_as_s64(integer);
        }else{
            pointer_lit->pointer -= integer_literal_as_s64(integer);
        }
        
        if(integer->ir < pointer->ir){
            *(struct ir_pointer_literal *)lit = *pointer_lit;
            pop_from_ir_arena(context, pointer_lit);
            operand->ir = &lit->base;
        }else{
            pop_from_ir_arena(context, lit);
            operand->ir = &pointer_lit->base;
        }
    }else if(integer->ir->kind == IR_integer_literal){
        struct ir_integer_literal *lit = (struct ir_integer_literal *)integer->ir;
        
        // Upconvert to s64.
        lit->_s64 = integer_literal_as_s64(integer);
        
        // @cleanup: Check for compile time overflow?
        if(type_is_signed(integer->resolved_type)){
            lit->_s64 *= deref_type->size;
        }else{
            lit->_u64 *= deref_type->size;
        }
        lit->type = &globals.typedef_u64;
        
        operand->ir = push_ir(context, ir_kind);
    }else{
        
        if(need_swap) push_ir(context, IR_swap_lhs_rhs);
        
        if(integer->resolved_type != &globals.typedef_s64){
            push_cast(context, AST_cast, &globals.typedef_s64, null, /*integer-expr*/op_rhs, token);
        }
        
        if(deref_type->size != 1){
            ast_push_s64_literal(context, deref_type->size, token);
            
            push_ir(context, IR_multiply_u64);
        }
        
        operand->ir = push_ir(context, ir_kind);
    }
    
    operand->resolved_type = pointer->resolved_type;
    operand->defined_type = pointer->defined_type;
    operand->token = token;
}

func void push_nodes_for_subscript(struct context *context, struct expr *lhs, struct expr *index, struct token *token){
    struct ast_type *lhs_type = lhs->resolved_type;
    assert(lhs_type->kind == AST_array_type || lhs_type->kind == AST_pointer_type);
    
    enum ir_kind subscript_kind = AST_none;
    struct ast_type *dereferenced_resolved_type = null;
    enum ast_kind   *dereferenced_defined_type  = null;
    
    if(lhs_type->kind == AST_pointer_type){
        subscript_kind = IR_pointer_subscript;
        
        struct ast_pointer_type *pointer = (struct ast_pointer_type *)lhs_type;
        dereferenced_resolved_type = pointer->pointer_to;
        dereferenced_defined_type  = pointer->pointer_to_defined_type;
    }else if(lhs_type->kind == AST_array_type){
        subscript_kind = IR_array_subscript;
        
        struct ast_array_type *array = (struct ast_array_type *)lhs_type;
        
        if(lhs->ir->kind == IR_pointer_literal_deref && index->ir->kind == IR_integer_literal){
            
            // Transform
            //    (*((int (*) [])0))[3]
            // Into
            //    *((int *) 12)
            
            struct ir_pointer_literal *pointer_literal_deref = (struct ir_pointer_literal *)lhs->ir;
            pointer_literal_deref->pointer += array->element_type->size * integer_literal_as_u64(index); // @cleanup: overflow?
            
            struct ast_type *pointer_type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type);
            pointer_literal_deref->type = pointer_type;
            
            lhs->resolved_type = pointer_type;
            lhs->defined_type = null;
            
            pop_from_ir_arena(context, (struct ir_integer_literal *)index->ir);
            return;
        }
        
        dereferenced_resolved_type = array->element_type;
        dereferenced_defined_type  = array->element_type_defined_type;
    }else{
        invalid_code_path;
    }
    
    maybe_insert_cast_from_special_int_to_int(context, index, /*is_lhs*/0);
    push_cast(context, AST_cast, &globals.typedef_s64, null, index, token);
    
    struct ir_subscript *subscript = push_struct(&context->ir_arena, struct ir_subscript);
    subscript->base.kind = subscript_kind;
    subscript->type = dereferenced_resolved_type;
    
    lhs->ir = &subscript->base;
    lhs->resolved_type = dereferenced_resolved_type;
    lhs->defined_type = dereferenced_defined_type;
}

func void maybe_insert_implicit_assignment_cast_and_check_that_types_match(struct context *context, struct ast_type *lhs_type, enum ast_kind *lhs_defined_type, struct expr *rhs, struct token *assignment){
    
    maybe_insert_cast_from_special_int_to_int(context, rhs, /*is_lhs*/0);
    
    // :translate_atomic_to_non_atomic_and_back
    // 
    // If the lhs_type is an atomic integer type, we want to do the exact same stuff as if it was non-atomic.
    // The atomic storing is handled by the back-end.
    if(lhs_type->kind == AST_atomic_integer_type) lhs_type = lhs_type - (&globals.typedef_atomic_bool - &globals.typedef_Bool);
    
    if(lhs_type == rhs->resolved_type) return;
    if(rhs->resolved_type == &globals.typedef_poison) return;
    
    //@cleanup: should we spread here? we could corrupt a global
    if(lhs_type == &globals.typedef_poison) return;
    
    struct ast_type *rhs_type = rhs->resolved_type;
    
    b32 should_report_warning = false;
    b32 should_push_cast = false;
    b32 should_skip_check = false;
    
    if(lhs_type == &globals.typedef_Bool){
        if(type_is_arithmetic(rhs_type) || rhs_type->kind == AST_pointer_type){
            // these are fine, never warn, but also never report a warning.
            should_push_cast = true;
        }
        
    }else if(lhs_type->kind == AST_bitfield_type && (rhs_type->kind == AST_integer_type || rhs_type->kind == AST_float_type)){
        // 
        // Assigning from a integer or float to a bitfield.
        // 
        
        struct ast_bitfield_type *bitfield = (struct ast_bitfield_type *)lhs_type;
        if(rhs_type != bitfield->base_type){
            push_cast(context, AST_cast, bitfield->base_type, null, rhs, assignment);
        }
        
        should_skip_check = true;
    }else if(lhs_type->kind == AST_pointer_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        
        // 'pointer' = 'integer'
        if(rhs->ir->kind == IR_integer_literal){
            if(integer_literal_as_u64(rhs) != 0){
                should_report_warning  = true;
            }
        }else{
            should_report_warning  = true;
        }
    }else if(lhs_type->kind == AST_pointer_type && rhs_type->kind == AST_pointer_type){
        // 'pointer' = 'pointer'
        struct ast_pointer_type *wanted_pointer = cast(struct ast_pointer_type *)lhs_type;
        struct ast_pointer_type *given_pointer  = cast(struct ast_pointer_type *)rhs_type;
        
        if(wanted_pointer->pointer_to == &globals.typedef_void || given_pointer->pointer_to == &globals.typedef_void){
            // One of the sides is 'void *'. This is fine.
            should_skip_check = true;
        }else{
            while(wanted_pointer->pointer_to->kind == AST_pointer_type && given_pointer->pointer_to->kind == AST_pointer_type){
                wanted_pointer = cast(struct ast_pointer_type *)wanted_pointer->pointer_to;
                given_pointer  = cast(struct ast_pointer_type *)given_pointer->pointer_to;
            }
            
            // 
            // @note: MSVC does not warn on 
            //    'void ** = int **' 
            // but does warn on
            //    'int ** = void **'
            // clang and gcc warn on both.
            // 
            // If we want to follow MSVC, we could check 'wanted->pointer_to' for void here.
            // 
            
            if(!types_are_equal(wanted_pointer->pointer_to, given_pointer->pointer_to)){
                should_report_warning  = true;
                should_skip_check = true;
            }
        }
    }else if(lhs_type->kind == AST_float_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        if(rhs->ir->kind != IR_integer_literal){
            should_report_warning  = true;
        }
    }else if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_float_type){
        // 'float' = 'integer' and 'integer' = 'float'
        should_report_warning  = true;
        should_push_cast = true;
    }else if(lhs_type->kind == AST_float_type && rhs_type->kind == AST_float_type){
        should_push_cast = true;
        if(lhs_type->size < rhs_type->size){
            should_report_warning  = true;
        }else if(lhs_type->size > rhs_type->size){
            // this is fine 'double' = 'float'
        }else{
            invalid_code_path; // should not be here... Their sizes are equal, thus they should be equal.
        }
    }else if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        
        if(rhs->ir->kind == IR_integer_literal && assignment->type != TOKEN_and_equals){
            b32 fits;
            if(type_is_signed(lhs_type)){
                s64 value = integer_literal_as_s64(rhs);
                switch(lhs_type->size){
                    case 1: fits = (min_s8  <= value && value <= max_s8); break;
                    case 2: fits = (min_s16 <= value && value <= max_s16); break;
                    case 4: fits = (min_s32 <= value && value <= max_s32); break;
                    case 8: fits = (min_s64 <= value && value <= max_s64); break;
                    invalid_default_case(fits = false);
                }
            }else{
                u64 value = integer_literal_as_u64(rhs);
                switch(lhs_type->size){
                    case 1: fits = (value <= max_u8); break;
                    case 2: fits = (value <= max_u16); break;
                    case 4: fits = (value <= max_u32); break;
                    case 8: fits = (value <= max_u64); break;
                    invalid_default_case(fits = false);
                }
            }
            
            if(!fits){
                report_warning(context, WARNING_compile_time_truncation, assignment, "Compile time truncation of integer.");
            }
        }else{
            // :retain_type_information_through_promotion
            
            struct ast_type *type = rhs->defined_type ? defined_type_to_type(rhs->defined_type) : rhs_type;
            assert(type->size <= rhs_type->size);
            
            if(lhs_type->size > type->size){
                if(!type_is_signed(type)){
                    // these are always fine 's32' = 'u8'
                }else if(type_is_signed(lhs_type) == type_is_signed(type)){
                    // these are fine: 'u32' = 'u16' or 's32' = 's16'.
                }else{
                    should_report_warning = true;
                }
            }else{
                if(lhs_type->size == type->size){
                    // @cleanup: how do we want to handle this...?
                    //           for my own code I would just require this, but other peoples code is kinda bad
                    //           about signed vs unsigned.
                }else{
                    // either size mismatch or signedness mismatch, as the types are unequal
                    should_report_warning  = true;
                }
            }
        }
    }
    
    if(should_report_warning){
        report_type_mismatch_warning(context, lhs_type, lhs_defined_type, rhs->resolved_type, rhs->defined_type, assignment);
    }
    
    if(should_push_cast){
        push_cast(context, AST_cast, lhs_type, lhs_defined_type, rhs, assignment);
    }else if(!should_skip_check){
        if(!types_are_equal(lhs_type, rhs->resolved_type)){
            report_type_mismatch_error(context, lhs_type, lhs_defined_type, rhs->resolved_type, rhs->defined_type, assignment);
        }
    }
}

struct designator_node{
    struct designator_node *next;
    struct ast_type *lhs_type;
    smm offset_at;
    union{
        smm member_at;
        smm array_at;
    };
};

func struct ast_type *get_current_object_type_for_designator_node(struct designator_node *designator_node){
    
    struct ast_type *current_object_type = designator_node->lhs_type;
    if(current_object_type->kind == AST_array_type){
        struct ast_array_type *array = (struct ast_array_type *)current_object_type;
        current_object_type = array->element_type;
    }else{
        struct ast_compound_type *compound = (struct ast_compound_type *)current_object_type;
        current_object_type = compound->members[designator_node->member_at].type;
    }
    return current_object_type;
}

func void parse_initializer_list(struct context *context, struct ast_type *type_to_initialize, u64 base_offset, smm *out_trailing_initializer_size){
    
    struct token *initial_open_curly = next_token(context);
    assert(initial_open_curly->type == TOKEN_open_curly);
    
    if(type_to_initialize->kind != AST_array_type && type_to_initialize->kind != AST_struct && type_to_initialize->kind != AST_union){
        // 
        // For an initializer like:
        //     int a = {1};
        //     (int){1};
        // We need this special handling, as the code below assumes it is initializing an array 
        // or a structure/union.
        // 
        
        if(peek_token(context, TOKEN_open_curly)){
            // 
            // We found an initializer like 'int a = {{1}};', or '(int){{1}}'.
            // This is sort of stupid, but legal.
            // 
            parse_initializer_list(context, type_to_initialize, base_offset, out_trailing_initializer_size);
            expect_token(context, TOKEN_closed_curly, "Expected '}'."); // :Error, but it does not matter, noones ever going to see this.
            return;
        }
        
        struct expr initializer = zero_struct;
        if(peek_token(context, TOKEN_closed_curly)){
            // :Extension
            // 
            // This is an extension, we allow 'int a = {}' and '(int){}'.
            // GCC/Clang complain about empty scalar initializer, but I think its pretty obvious 
            // what you want here and makes some type-agnostic macros work.
            initializer = ast_push_s32_literal(context, 0, initial_open_curly);
        }else{
            // 
            // 'int a = {1, 2, 3}' only generates a warning in gcc and clang.
            // but the resulting value of a is '1'.
            // So we error on it.
            // 
            initializer = parse_expression(context, /*should_skip_comma_expression*/true);
        }
        
        peek_token_eat(context, TOKEN_comma); // Technically, an initializer list is allowed to have a trailing comma.
        
        expect_token(context, TOKEN_closed_curly, "More than one member in initializer-list for scalar type.");
        
        maybe_load_address_for_array_or_function(context, IR_load_address, &initializer);
        maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, type_to_initialize, /*ast_to_initialize->defined_type*/null, &initializer, initial_open_curly); // @cleanup:
        
        struct ir_initializer *ir_initializer = push_struct(&context->ir_arena, struct ir_initializer);
        ir_initializer->base.kind = IR_initializer;
        ir_initializer->offset = base_offset;
        ir_initializer->lhs_type = type_to_initialize;
        return;
    }
    
    //
    // 3 cases:
    //    1) {
    //       -> parse an initializer for the 'current_object'
    //    2) designator
    //       -> reset the 'current_object' and parse the designator
    //    3) <expression>
    //       -> propagate the 'current_object' until it is no longer a struct.
    // 
    // current_object is always given implicitly as:
    //    array  => designator.lhs[designator.array_at]
    //    struct => designator.lhs."designator.declaration_in_struct"
    //
    
    struct designator_node initial_node = {
        .lhs_type = type_to_initialize,
        .offset_at = base_offset,
    };
    
    if(type_to_initialize->kind == AST_struct || type_to_initialize->kind == AST_union){
        struct ast_compound_type *ast_struct = (struct ast_compound_type *)type_to_initialize;
        
        if(!ast_struct->amount_of_members){
            //
            // struct {} x = {}; is allowed, struct {} x = {10}; is not!
            //
            if(!peek_token(context, TOKEN_closed_curly)){
                begin_error_report(context);
                report_error(context, get_current_token_for_error_report(context), "Cannot initialize an empty structure with non-empty initializer-list.");
                report_error(context, ast_struct->identifier, "... Here is the definition of the empty structure.");
                end_error_report(context);
                goto error;
            }
        }
        
        initial_node.member_at = 0;
    }
    
    // :trailing_arrays
    // 
    // We are keeping track of the 'trailing_initializer_size'.
    // The trailing array size is here for both arrays of unknown size,
    // as well as trailing array members in structures.
    // We want to be able to parse:
    // 
    //     int array_of_unknown_size[]      = {1, 2, 3, 4};
    //     struct { int trailing_array[]; } = {1, 2, 3, 4};
    //     union {int trailing_array[]; }   = {1, 2, 3, 4};
    //     struct { struct { int trailing_array[];} substructure; } = {1, 2, 3, 4};
    // 
    // And a whole bunch of other 
    // 
    smm trailing_initializer_size = 0;
    
    struct designator_node initial_list_node = initial_node;
    
    struct{
        struct designator_node *first;
        struct designator_node *last;
    } designator_stack = {.first = &initial_list_node, .last = &initial_list_node};
    
    do{
        if(peek_token(context, TOKEN_closed_curly)) break;
        
        // initializer-list:
        //     designation_opt initializer
        //     initializer-list, designation_opt initializer
        //     
        // desination:
        //     designator-list =
        // 
        // designator-list:
        //     designator
        //     designator-list designator
        //     
        // designator:
        //     [constant-expression]
        //     .identifier
        // 
        
        // 
        // Figure out the current object.
        // It is either given by an explicit designator, or by the top of the designator stack.
        // 
        struct ast_type *current_object_type = null;
        struct token *site = get_current_token_for_error_report(context);
        
        if(peek_token(context, TOKEN_dot) || peek_token(context, TOKEN_open_index)){
            //
            // Parse a designator.
            //
            
            // Reset the 'current_object'.
            initial_list_node = initial_node;
            designator_stack.first = &initial_list_node;
            designator_stack.last  = &initial_list_node;
            
            do{
                struct token *token = next_token(context);
                current_object_type = designator_stack.first->lhs_type;
                
                if(token->type == TOKEN_dot){
                    struct token *identifier = expect_token(context, TOKEN_identifier, "Expected an identifier after '.' in initializer list.");
                    
                    struct ast_type *type = current_object_type;
                    if(type->kind != AST_struct && type->kind != AST_union){
                        struct string type_string = push_type_string(context->arena, &context->scratch, type);
                        report_error(context, token, "Field designator '.%.*s' can only be used for structs or unions and not '%.*s'.", identifier->size, identifier->data, type_string.size, type_string.data);
                        goto error;
                    }
                    
                    struct ast_compound_type *compound = (struct ast_compound_type *)type;
                    
                    struct compound_member *found = find_member_in_compound(compound, identifier->atom);
                    if(!found){
                        char *struct_or_union = (compound->base.kind == AST_struct) ? "structure" : "union";
                        
                        begin_error_report(context);
                        report_error(context, identifier, "Identifier is not a member of %s.", struct_or_union);
                        report_error(context, compound->identifier, "... Here is the definition of the %s.", struct_or_union);
                        end_error_report(context);
                        goto error;
                    }
                    
                    u64 new_offset = designator_stack.first->offset_at + found->offset_in_type;
                    
                    designator_stack.first->member_at = found - compound->members;
                    designator_stack.first->offset_at = new_offset;
                    
                    struct designator_node *new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->lhs_type  = found->type;
                    new_node->offset_at = new_offset;
                    sll_push_front(designator_stack, new_node);
                    
                }else if(token->type == TOKEN_open_index){
                    
                    if(current_object_type->kind != AST_array_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, current_object_type);
                        report_error(context, token, "Array designator can only be used for arrays and not '%.*s'.", type_string.size, type_string.data);
                        goto error;
                    }
                    
                    struct ast_array_type *array = (struct ast_array_type *)current_object_type;
                    
                    struct expr index = parse_constant_integer_expression(context, /*should_skip_comma_expression*/false, "Expected a constant integer expression in array subscript.");
                    
                    u64 value;
                    if(type_is_signed(index.resolved_type)){
                        s64 signed_value = integer_literal_as_s64(&index);
                        if(signed_value < 0){
                            report_error(context, index.token, "Array subscript cannot be negative.");
                            goto error;
                        }
                        
                        value = (u64)signed_value;
                    }else{
                        value = integer_literal_as_u64(&index);
                    }
                    
                    if(value >= 0x7fffffffffffffff){
                        report_error(context, index.token, "Array subscript is to big.");
                        goto error;
                    }
                    
#if 0
                    // :ir_refactor 
                    if(peek_token(context, TOKEN_dotdotdot)){
                        struct token *dotdotdot = next_token(context);
                        // Gnu extension:
                        // 
                        //    [1 ... 5] = 1,
                        // 
                        // We parse this as [<const integer expression> ... <const integer expression>] = <expression>,
                        // and restart the loop.
                        // 
                        
                        struct ast *end_index = parse_constant_integer_expression(context, /*should_skip_comma_expression*/false);
                        if(end_index->kind != IR_integer_literal){
                            report_error(context, end_index->token, "Expected a constant integer expression in range designator. (e.g.: [1 ... moose]).");
                            goto error;
                        }
                        
                        u64 end_value;
                        if(type_is_signed(end_index->resolved_type)){
                            s64 signed_value = integer_literal_as_s64(end_index);
                            if(signed_value < 0){
                                report_error(context, index->token, "Array subscript cannot be negative.");
                                goto error;
                            }
                            
                            end_value = (u64)signed_value;
                        }else{
                            end_value = integer_literal_as_u64(end_index);
                        }
                        
                        expect_token(context, TOKEN_closed_index, "Expected ']' at the end of range-designator (e.g.: `[1 ... `).");
                        
                        if(end_value <= value){
                            report_error(context, end_index->token, "Invalid range [%llu, %llu] specified, second index must be larger than the first.", value, end_value);
                            end_value = value + 1;
                        }
                        
                        if(!array->is_of_unknown_size && end_value >= (u64)array->amount_of_elements){
                            report_error(context, index->token, "Range designator [%llu, %llu] overflows array bounds. Array has only '%lld' elements.", value, end_value, array->amount_of_elements);
                            goto error;
                        }
                        
                        if(peek_token(context, TOKEN_dot) || peek_token(context, TOKEN_open_index)){
                            report_error(context, get_current_token(context), "Nested range designators are unsupported. (e.g.: `[1 .. 5][1 .. 3] = 1`).");
                        }
                        
                        u64 new_offset = designator_stack.first->offset_at + end_value * array->element_type->size;
                        designator_stack.first->array_at  = end_value;
                        designator_stack.first->offset_at = new_offset;
                        
                        struct ast_range_initializer *range_initializer = push_expression(context, dotdotdot, array_range);
                        range_initializer->offset_at = new_offset;
                        range_initializer->
                        range_initializer->start_index = value;
                        range_initializer->end_index = end_value;
                        
                        set_resolved_type(&range_initializer->base, array->element_type, array->element_type_defined_type);
                        
                        check_for_basic_types__internal(context, &range_initializer->base, CHECK_basic, "@incomplete: Range designator", dotdotdot);
                        
                        struct designator_node *new_node = push_struct(&context->scratch, struct designator_node);
                        new_node->lhs = &range_initializer->base;
                        sll_push_front(designator_stack, new_node);
                        
                    }else
#endif
                    
                    {
                        
                        // 
                        // "Normal" array designator:
                        //    [1] = 1,
                        // 
                        
                        expect_token(context, TOKEN_closed_index, "Expected ']' at the end of array designator.");
                        
                        if(!array->is_of_unknown_size && value >= (u64)array->amount_of_elements){
                            report_error(context, index.token, "Index '%llu' out of bounds. Array has only '%lld' elements.", value, array->amount_of_elements);
                            goto error;
                        }
                        
                        u64 new_offset = designator_stack.first->offset_at + value * array->element_type->size;
                        designator_stack.first->array_at  = value;
                        designator_stack.first->offset_at = new_offset;
                        
                        struct designator_node *new_node = push_struct(&context->scratch, struct designator_node);
                        new_node->lhs_type  = array->element_type;
                        new_node->offset_at = new_offset;
                        sll_push_front(designator_stack, new_node);
                    }
                }
            } while(peek_token(context, TOKEN_dot) || peek_token(context, TOKEN_open_index));
            
            site = expect_token(context, TOKEN_equals, "Expected an '=' after initializer designation.");
            
            // 
            // At this point, the new current object type is the top of the designator stack.
            // This means that for the rest of the code, the 'designator_stack' is one too far.
            // Hence, we get the current object, and then pop it off the stack.
            // 
            current_object_type = designator_stack.first->lhs_type;
            sll_pop_front(designator_stack);
        }else{
            
            if(!designator_stack.first){
                // :too_many_initializers
                //
                // here we are in a case where we ran out of designators (meaning we initialized everything)
                // but then the initializer list did not end and also did not have a designator, to reset 
                // the current_object, therefore report an error!
                //
                struct ast_type *type = type_to_initialize;
                
                if(type->kind == AST_struct || type->kind == AST_union){
                    struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
                    char *structure_or_union = (compound->base.kind == AST_struct) ? "structure" : "union";
                    
                    begin_error_report(context);
                    report_error(context, site, "Too many initializers for %s '%.*s'.", structure_or_union, compound->identifier->size, compound->identifier->data);
                    report_error(context, compound->identifier, "... Here was the %s defined.", structure_or_union);
                    end_error_report(context);
                }else{
                    assert(type->kind == AST_array_type);
                    report_error(context, site, "Too many initializers for array.");
                }
                goto error;
            }
            
            current_object_type = get_current_object_type_for_designator_node(designator_stack.first);
        }
        
        // 
        // We are at the point where we are parsing an 'initializer' again.
        // An initializer is again either an expression or an initializer list.
        // We cannot actually use 'parse_initializer' here, as it requires knowing 
        // the 'lhs' of the initalizer, but in the 'expression' case we don't.
        // Hence, we handle these two cases explicitly here ourselves.
        // 
        //                                                  15.10.2023
        
        if(peek_token(context, TOKEN_open_curly)){
            
            // 
            // Parse a sub-initializer-list, like 'struct{ struct v2 v; } asd = {{1.0f, 2.0f}};'
            // This also handles the case of 'struct{int a;} asd = {{1}};
            // 
            
            smm trailing_sub_initializer_size = 0;
            
            parse_initializer_list(context, current_object_type, designator_stack.first->offset_at, &trailing_sub_initializer_size);
            
            // :trailing_arrays
            // 
            // This handles stuff like
            //     struct { int a; int trailling_array[]; } asd = {1, {1, 2, 3}};
            //
            trailing_initializer_size = max_of(trailing_initializer_size, trailing_sub_initializer_size);
            
        }else if(peek_token(context, TOKEN_embed)){
            // 
            // This is data specified by a #embed directive.
            // The actual data is specified by the 'string' from the token.
            // Currently, we are only allowing initializing 'char[]' and 'unsigned char[]'.
            // 
            struct token *embed = next_token(context);
            struct string file_data = embed->string;
            struct designator_node *designator_node = designator_stack.first;
            struct ast_array_type *array_type = (struct ast_array_type *)designator_node->lhs_type;
            
            if(array_type->base.kind != AST_array_type || array_type->element_type->size != 1){
                // @cleanup: How correct is this?
                // :Error
                report_error(context, embed, "Can only initialize 'char []' and 'unsigned char[]' with #embed.");
                goto error;
            }
            
            if(!array_type->is_of_unknown_size && (designator_node->array_at + file_data.size > array_type->amount_of_elements)){
                report_error(context, embed, "#embed produces too many initializers for the array initialized. The #embed produces %lld initializers at offset %lld in the array and the array has %lld elements.", file_data.size, designator_node->array_at, array_type->amount_of_elements);
                goto error;
            }
            
            struct ir_embed *ir_embed = push_struct(&context->ir_arena, struct ir_embed);
            ir_embed->base.kind = IR_embed;
            ir_embed->token = embed;
            
            struct ir_initializer *initializer = push_struct(&context->ir_arena, struct ir_initializer);
            initializer->base.kind = IR_initializer;
            initializer->offset = designator_node->offset_at;
            initializer->lhs_type = current_object_type;
            
            // @note: We need to add -1 here, because the code below assumes it was not incremented.
            designator_node->array_at  += (file_data.size - 1); 
            designator_node->offset_at += (file_data.size - 1);
        }else{
            struct expr expression = parse_expression(context, /*skip_comma_expression*/true);
            
            if(designator_stack.first->member_at == 0 && expression.ir->kind == IR_string_literal && designator_stack.first->lhs_type->kind == AST_array_type){
                // 
                // The C-spec allows the initializer of a character array to be optionally brace enclosed.
                // In this case, we end up here.
                // 
                //      char arst[10] = {"arst"};
                //      
                //      struct s{
                //          char arst[10];
                //      } arst = {
                //          .arst = {"arst"},
                //      };
                //  
                // 
                
                struct ast_array_type *wanted_array = (struct ast_array_type *)designator_stack.first->lhs_type;
                struct ast_array_type *given_array  = (struct ast_array_type *)expression.resolved_type;
                
                if(wanted_array->element_type->size == given_array->element_type->size){
                    
                    // Pop one layer off the designator stack, making `designator_stack.first->lhs_type` the current object type.
                    current_object_type = designator_stack.first->lhs_type;
                    sll_pop_front(designator_stack);
                    
                    if(wanted_array->is_of_unknown_size){
                        trailing_initializer_size = max_of(trailing_initializer_size, given_array->base.size);
                    }else if(wanted_array->amount_of_elements < given_array->amount_of_elements - /*zero-terminator*/1){
                        report_error(context, site, "Array initialized to string literal of size %lld, but the array size is only %lld.", given_array->amount_of_elements, wanted_array->amount_of_elements);
                        goto error;
                    }
                    
                    struct ir_initializer *initializer = push_struct(&context->ir_arena, struct ir_initializer);
                    initializer->base.kind = IR_initializer;
                    initializer->offset = designator_stack.first ? designator_stack.first->offset_at : base_offset;
                    initializer->lhs_type = current_object_type;
                    
                    continue;
                }
            }
            
            if(expression.ir->kind == IR_compound_literal){
                struct ir_compound_literal *compound = (struct ir_compound_literal *)expression.ir;
                
                if(compound->trailing_array_size){
                    report_error(context, expression.token, "Initialization of member to struct literal looses initialization of trailing array member.");
                }
            }
            
            while(true){
                
                if(current_object_type->kind == AST_array_type && expression.ir->kind == IR_string_literal){
                    // 
                    // This is the case for:
                    //      struct { u8 array[8]; } asd = {"asd"};
                    // 
                    struct ast_array_type *wanted_array = (struct ast_array_type *)current_object_type;
                    struct ast_array_type *given_array = (struct ast_array_type *)expression.resolved_type;
                    
                    if(wanted_array->element_type->size == given_array->element_type->size){
                        
                        if(wanted_array->is_of_unknown_size){
                            trailing_initializer_size = max_of(trailing_initializer_size, given_array->base.size);
                        }else if(wanted_array->amount_of_elements < given_array->amount_of_elements - /*zero-terminator*/1){
                            report_error(context, site, "Array initialized to string literal of size %lld, but the array size is only %lld.", given_array->amount_of_elements, wanted_array->amount_of_elements);
                            goto error;
                        }
                        
                        struct ir_initializer *initializer = push_struct(&context->ir_arena, struct ir_initializer);
                        initializer->base.kind = IR_initializer;
                        initializer->offset = designator_stack.first->offset_at;
                        initializer->lhs_type = current_object_type;
                        break;
                    }
                }
                
                if(types_are_equal(expression.resolved_type, current_object_type)){
                    //
                    // This checks for struct {v2 a;} a = { return_v2() };
                    //
                    
                    struct ir_initializer *initializer = push_struct(&context->ir_arena, struct ir_initializer);
                    initializer->base.kind = IR_initializer;
                    initializer->offset = designator_stack.first->offset_at;
                    initializer->lhs_type = current_object_type;
                    break;
                }
                
                struct designator_node *new_node = null;
                struct ast_type *type = current_object_type;
                
                if(type->kind == AST_struct || type->kind == AST_union){
                    struct ast_compound_type *compound = (struct ast_compound_type *)type;
                    
                    if(!compound->amount_of_members){
                        report_error(context, site, "Initializing empty structure or union requires explicit '%s'.", "{}");
                        goto error;
                    }
                    
                    // 
                    // Recurse into the sub struct or union.
                    // 
                    new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->lhs_type = current_object_type;
                    new_node->offset_at = designator_stack.first->offset_at;
                    new_node->member_at = 0;
                }else if(type->kind == AST_array_type){
                    new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->offset_at = designator_stack.first->offset_at;
                    new_node->lhs_type = current_object_type;
                }else{
                    maybe_load_address_for_array_or_function(context, IR_load_address, &expression);
                    maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, current_object_type, /*@cleanup*/null, &expression, site);
                    
                    struct ir_initializer *initializer = push_struct(&context->ir_arena, struct ir_initializer);
                    initializer->base.kind = IR_initializer;
                    initializer->offset = designator_stack.first->offset_at;
                    initializer->lhs_type = current_object_type;
                    break;
                }
                
                sll_push_front(designator_stack, new_node);
                current_object_type = get_current_object_type_for_designator_node(designator_stack.first);
            }
        }
        
        int at_end = peek_token(context, TOKEN_closed_curly) != 0;
        
        while(true){
            struct designator_node *node = designator_stack.first;
            struct ast_type *type = node->lhs_type;
            
            if(type->kind == AST_struct){
                struct ast_compound_type *compound = (struct ast_compound_type *)type;
                struct compound_member *member = &compound->members[node->member_at];
                
                // :next_member_increment and :member_list_contains_both_linear_and_nested
                // 
                // Skip the linear members once we are done with the nested type.
                node->member_at += member->next_member_increment;
                
                if(node->member_at < compound->amount_of_members){
                    struct compound_member *new_member = &compound->members[node->member_at];
                    node->offset_at += new_member->offset_in_type - member->offset_in_type;
                }
                
                if(!at_end && (node->member_at < compound->amount_of_members)) break;
                
            }else if(type->kind == AST_union){
                // pop the union! Only ever initialize one member of the union.
                // The current object should be the next thing after the union.
            }else{
                struct ast_array_type *array = (struct ast_array_type *)type;
                
                node->array_at++;
                node->offset_at += array->element_type->size;
                
                if(array->is_of_unknown_size){
                    
                    // :trailing_arrays
                    // 
                    // If we are initializing an array of unknown size, we are at the trailing 
                    // array member of the structure, otherwise the parsing code would have rejected
                    // the structure.
                    // 
                    
                    smm amount_of_elements = node->array_at;
                    smm element_size = array->element_type->size;
                    
                    smm array_size = amount_of_elements * element_size; // @cleanup: overflow
                    
                    trailing_initializer_size = max_of(trailing_initializer_size, array_size);
                    break;
                }
                if(!at_end && (node->array_at < array->amount_of_elements)) break;
            }
            
            sll_pop_front(designator_stack);
            
            if(!designator_stack.first) break;
        }
        
        // :too_many_initializers
        //
        // @note: we cannot check syntactically here that we have to be at the end
        //        if 'designator_stack.first == null', as there could be entries
        //        using array or struct designators.
        //
        
    } while(peek_token_eat(context, TOKEN_comma));
    
    expect_token(context, TOKEN_closed_curly, "Expected '}' at the end of initializer list.");
    
    *out_trailing_initializer_size = trailing_initializer_size;
    
    error:; // @cleanup: Why do we have this?
    return;
}


// This routine patches 'decl->assign_expr' and 'decl->type' instead of returning them.
func void parse_initializer(struct context *context, struct ast_declaration *decl, struct token *equals){
    
    struct ir_skip *ir_skip = null;
    if(decl->flags & DECLARATION_FLAGS_is_local_persist){
        // 
        // For local static declarations (local presist declarations), we don't want to execute their initializer.
        // Hence, we add a _skip_ node which just tells us how many bytes to skip in the `ir_arena`.
        // In the future, there may be a better solution, where we immediately evaluate the initializer
        // and therefor do not have to do this junk.
        //                                                       - Pascal Beyer 02.03.2025
        ir_skip = push_struct(&context->ir_arena, struct ir_skip);
        ir_skip->base.kind = IR_skip;
    }
    
    // 
    // We call this function for
    //     1) int a = ?;
    //     2) int a = {?};
    //     3) (int){?}
    //     4) (struct s){?}
    //     5) struct s a = ?;
    //     6) struct s a = {?};
    // 
    struct ir *initializer_start = null;
    struct ast_type *type = decl->type;
    
    if(peek_token(context, TOKEN_open_curly)){
        // 
        // Parse an initializer like 'struct s asd = {1,2,3};' or '(struct s){1,2,3};'.
        // 
        struct token *open_curly = get_current_token(context);
        
        struct ir_compound_literal *compound_literal = push_struct(&context->ir_arena, struct ir_compound_literal);
        compound_literal->base.kind = IR_compound_literal;
        compound_literal->decl = decl;
        
        parse_initializer_list(context, decl->type, 0, &compound_literal->trailing_array_size);
        
        initializer_start = &compound_literal->base;
        
        compound_literal->initializer_size = arena_current(&context->ir_arena) - (u8 *)(compound_literal + 1);
        
        if(type_is_array_of_unknown_size(type)){
            // 
            // If we initialized an array of unknown type, we have to patch in the type.
            // @copy_and_paste from :struct_literal
            // 
            
            struct ast_array_type *array_of_unknown_size = (struct ast_array_type *)type;
            smm array_length = 0;
            if(compound_literal->trailing_array_size){
                array_length = compound_literal->trailing_array_size/array_of_unknown_size->element_type->size;
            }
            
            // 
            // Create a new array type, with the newly discovered bounds.
            struct ast_array_type *array_type = parser_type_push(context, array_type); // @cleanup: Better token.
            array_type->element_type              = array_of_unknown_size->element_type;
            array_type->element_type_defined_type = array_of_unknown_size->element_type_defined_type;
            
            patch_array_size(context, array_type, array_length, open_curly);
            
            // Set the new type.
            type = &array_type->base;
            
            // Reset the trailing array size, to not overallocate later on.
            compound_literal->trailing_array_size = 0;
        }
        
    }else{
        struct ir_identifier *lhs = push_struct(&context->ir_arena, struct ir_identifier);
        lhs->base.kind = IR_identifier;
        lhs->decl = decl;
        
        // 
        // Parse an initializer like:
        //     struct s a = <expr>
        //     int a = <expr>
        // 
        
        //
        // 'int a = 1, 2, 3;' Is not legal.
        //
        struct expr expr = parse_expression(context, /*skip_comma_expression*/true);
        
        if(expr.ir->kind == IR_compound_literal && types_are_equal(type, expr.resolved_type)){
            
            // For assignments like
            // 
            //    struct arst{
            //         int a, b, c;
            //    } arst = (struct arst){1, 2, 3);
            // 
            // we essentially _remove_ the (struct arst) and transform it into `arst = {1, 2, 3}`.
            
            static_assert(sizeof(struct ir_skip) <= sizeof(*lhs));
            
            struct ir_skip *skip = (struct ir_skip *)lhs;
            skip->base.kind = IR_skip;
            skip->size_to_skip = sizeof(*lhs);
            
            struct ir_compound_literal *compound_literal = (struct ir_compound_literal *)expr.ir;
            compound_literal->decl = decl;
            
            initializer_start = expr.ir;
        }else{
            
            if(type->kind == AST_array_type){
                // 
                // The left hand side is an array, but we don't have an initializer list,
                // so the right hand side has to be a string literal.
                // 
                // Cases:
                //     char a[]  = "asd";
                //     char a[4] = "asd";
                //     char a[3] = "asd";
                //     
                //     wchar_t a[]  = L"asd";
                //     wchar_t a[4] = L"asd";
                //     wchar_t a[3] = L"asd";
                // 
                //     int a[]  = U"asd";
                //     int a[4] = U"asd";
                //     int a[3] = U"asd";
                // 
                
                if(expr.ir->kind != IR_string_literal){
                    // :Error print the string literal thing only for integer arrays?
                    report_error(context, equals, "Array can only be initialized by {initializer-list} or \"string literal\".");
                    return;
                }
                
                // If type is an array of unknown size, patch in the size
                // we get in this for example for 'char asd[] = "asd";' or struct literals.
                struct ast_array_type *lhs_type = (struct ast_array_type *)type;
                struct ast_array_type *rhs_type = (struct ast_array_type *)expr.resolved_type;
                
                if(lhs_type->element_type->kind != AST_integer_type){
                    report_error(context, equals, "Array initialized by string literal has to be of integer type.");
                    return;
                }
                
                if(lhs_type->element_type->size != rhs_type->element_type->size){
                    // :error
                    report_error(context, equals, "Mismatching array element types.");
                    return;
                }
                
                // @note: For string literals we can right now have signed unsigned mismatches.
                
                if(lhs_type->is_of_unknown_size){
                    // 
                    // We want the type to be patched to the type of the string literal.
                    // 
                    type = &rhs_type->base;
                }else if(lhs_type->amount_of_elements == (rhs_type->amount_of_elements - 1)){
                    // 
                    // Here we are in the 'char a[3] = "asd"' case.
                    // This should not be an error, we just don't append the zero to the array.
                    // 
                }else if(lhs_type->amount_of_elements < rhs_type->amount_of_elements){
                    report_error(context, equals, "Array initialized to string literal of size %lld, but the array size is only %lld.", rhs_type->amount_of_elements, lhs_type->amount_of_elements);
                    return;
                }
                
            }else{
                maybe_load_address_for_array_or_function(context, IR_load_address, &expr);
                maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, type, decl->defined_type, &expr, equals);
            }
            
            struct ir_initializer *ast_initializer = push_struct(&context->ir_arena, struct ir_initializer);
            ast_initializer->base.kind = IR_initializer;
            ast_initializer->offset = 0;
            ast_initializer->lhs_type = decl->type;
            
            // The 'assign_expr' is only used for 'evaluate_static_initializer'.
            // In this case we have to re-iterate the whole expression.
            // This starts right after the lhs and ends at the 'ast_initializer'.
            initializer_start = &lhs->base;
        }
    }
    
    push_ir(context, IR_pop_expression);
    
    if(ir_skip){
        ir_skip->size_to_skip = (u32)(arena_current(&context->ir_arena) - (u8*)ir_skip);
    }
    
    if(!context->current_scope){
        if(context->should_exit_statement) return;
        
        // If we are at global scope, submit that this is actually a definition.
        parser_register_definition(context, decl, initializer_start, equals, type);
    }else{
        // If we are at local scope, we control this declaration, thus we can just plug in
        // its expression and type slot.
        decl->assign_expr = initializer_start;
        decl->type = type;
    }
}

// @note: for compare_exressions we set the resolved_type afterwards
func void perform_float_operation(struct context *context, enum ast_kind binary_op, struct expr *op_lhs, struct expr *op_rhs){
    
    struct ir_float_literal *lhs = (struct ir_float_literal *)op_lhs->ir;
    struct ir_float_literal *rhs = (struct ir_float_literal *)op_rhs->ir;
    
    assert(lhs->base.kind == IR_float_literal && rhs->base.kind == IR_float_literal);
    // @cleanup: how to deal with float vs double
    
    s32 compare_value = -1;
    
    switch(binary_op){
        case AST_binary_times:            lhs->value *= rhs->value; break;
        case AST_binary_divide:           lhs->value /= rhs->value; break;
        case AST_binary_plus:             lhs->value += rhs->value; break;
        case AST_binary_minus:            lhs->value -= rhs->value; break;
        case AST_binary_logical_equals:   compare_value = (lhs->value == rhs->value); break;
        case AST_binary_logical_unequals: compare_value = (lhs->value != rhs->value); break;
        case AST_binary_bigger_equals:    compare_value = (lhs->value >= rhs->value); break;
        case AST_binary_smaller_equals:   compare_value = (lhs->value <= rhs->value); break;
        case AST_binary_bigger:           compare_value = (lhs->value >  rhs->value); break;
        case AST_binary_smaller:          compare_value = (lhs->value <  rhs->value); break;
        invalid_default_case();
    }
    
    pop_from_ir_arena(context, rhs);
    
    if(compare_value != -1){
        // "The result has type int".
        struct ir_integer_literal *lit = (struct ir_integer_literal *)lhs;
        lit->base.kind = IR_integer_literal;
        lit->_u64 = 1;
        lit->type = &globals.typedef_s32;
        op_rhs->resolved_type = &globals.typedef_s32;
        op_rhs->defined_type = &globals.typedef_u8.kind;
    }
    
    op_rhs->ir = &lhs->base; // @note: type is already correct.
}

// @note: for compare_exressions we set the resolved_type afterwards
// @WARNING: This frees the 'op_rhs' from the `ir_arena`.
// @cleanup: Revert this to take an ast_kind as well.
// @note: returns in op_rhs.
func void perform_integer_operation(struct context *context, struct token *operation, struct expr *lhs_expr, struct expr *rhs_expr){
    struct ir_integer_literal *lhs = cast(struct ir_integer_literal *)lhs_expr->ir;
    struct ir_integer_literal *rhs = cast(struct ir_integer_literal *)rhs_expr->ir;
    
    assert(lhs->base.kind == IR_integer_literal && rhs->base.kind == IR_integer_literal);
    
    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(lhs_expr->resolved_type, rhs_expr->resolved_type);
    
#define __perform_integer_operation(basic_type)                           \
basic_type lhs_val = integer_literal_as_##basic_type(lhs_expr);         \
basic_type rhs_val = integer_literal_as_##basic_type(rhs_expr);         \
switch(operation->type){                                                  \
    case TOKEN_times:   lhs->_##basic_type = lhs_val * rhs_val; break;    \
    case TOKEN_slash:{                                                    \
        if(rhs_val == 0){                                                 \
            report_error(context, operation, "Integer division by 0.");   \
        }else{                                                            \
            lhs->_##basic_type = lhs_val / rhs_val;                       \
        }                                                                 \
    }break;                                                               \
    case TOKEN_mod:{                                                      \
        if(rhs_val == 0){                                                 \
            report_error(context, operation, "Integer modulation by 0."); \
        }else{                                                            \
            lhs->_##basic_type = lhs_val % rhs_val;                       \
        }                                                                 \
    }break;                                                               \
    case TOKEN_plus:    lhs->_##basic_type = lhs_val + rhs_val; break;    \
    case TOKEN_minus:   lhs->_##basic_type = lhs_val - rhs_val; break;    \
    \
    case TOKEN_logical_equals:   lhs->_u64 = (lhs_val == rhs_val); break; \
    case TOKEN_logical_unequals: lhs->_u64 = (lhs_val != rhs_val); break; \
    case TOKEN_bigger_equals:    lhs->_u64 = (lhs_val >= rhs_val); break; \
    case TOKEN_smaller_equals:   lhs->_u64 = (lhs_val <= rhs_val); break; \
    case TOKEN_bigger:           lhs->_u64 = (lhs_val > rhs_val);  break; \
    case TOKEN_smaller:          lhs->_u64 = (lhs_val < rhs_val);  break; \
    invalid_default_case();                                               \
}                                                                         \

    u64 value;
    
    if(promoted_type->size == 4){
        
        if(promoted_type == &globals.typedef_u32){
            __perform_integer_operation(u32);
            
            value = lhs->_u32;
        }else{
            assert(promoted_type == &globals.typedef_s32);
            __perform_integer_operation(s32);
            
            value = (lhs->_s32 < 0) ? -lhs->_s32 : lhs->_s32;
        }
    }else{
        assert(promoted_type->size == 8);
        
        if(promoted_type == &globals.typedef_u64){
            __perform_integer_operation(u64);
            
            value = lhs->_u64;
        }else{
            assert(promoted_type == &globals.typedef_s64);
            __perform_integer_operation(s64);
            
            value = (lhs->_s32 < 0) ? -lhs->_s64 : lhs->_s64;
        }
    }
    
#undef __perform_integer_operation
    
    enum ast_kind *defined_type = null;
    if(value <= max_u8)  defined_type = &globals.typedef_u8.kind;
    else if(value <= max_u16) defined_type = &globals.typedef_u16.kind;
    else if(value <= max_u32) defined_type = &globals.typedef_u32.kind;
    
    lhs->type = promoted_type;
    
    pop_from_ir_arena(context, rhs);
    
    rhs_expr->ir = &lhs->base;
    rhs_expr->resolved_type = promoted_type;
    rhs_expr->defined_type = defined_type;
}


func b32 check_types_for_increment_or_decrement(struct context *context, struct ast_type *type, struct token *site, char *token){
    
    // @cleanup: if we canonicalize pointers this could be globals.void_pointer
    if(type->kind == AST_pointer_type){
        struct ast_pointer_type *pointer = (struct ast_pointer_type *)type;
        if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return true;
        
        if(pointer->pointer_to == &globals.typedef_void){
            report_error(context, site, "Operand of '%s' cannot be a void-pointer.", token);
            return true;
        }
        
        if(pointer->pointer_to->kind == AST_function_type){
            report_error(context, site, "Operand of '%s' cannot be a function-pointer.", token);
            return true;
        }
    }
    
    if(!context->in_lhs_expression){
        report_error(context, site, "Operand of '%s' must be an L-value.", token);
        return true;
    }
    
    if(type->kind == AST_function_type){
        report_error(context, site, "Operand of '%s' cannot be a function.", token);
        return true;
    }
    
    if(type->kind == AST_array_type){
        report_error(context, site, "Operand of '%s' cannot be an array.", token);
        return true;
    }
    
    return false;
}

// pushes a declaration without a name with type 'type' and 'defined_type', this declaration is not initialized, and the declaration 
// is not reachable. It then returns an identifier that references this declaration.
// This is used for struct literal kind of situations.
func struct ast_declaration *push_unnamed_declaration(struct context *context, struct ast_type *type, enum ast_kind *defined_type, struct token *site){
    struct declarator_return declarator = {
        .ident = site,
        .type  =  type,
        .defined_type = defined_type,
    };
    struct ast_declaration *decl = push_declaration_for_declarator(context, declarator);
    decl->flags |= DECLARATION_FLAGS_is_unnamed;
    
    if(!(type->flags & TYPE_FLAG_ends_in_array_of_unknown_size)){
        // :compounds_with_trailing_array
        // 
        // We have to wait for the initializer to know the final size.
        parser_emit_memory_location(context, decl);
    }
    
    return decl;
}

void maybe_insert_implicit_nodes_for_varargs_argument(struct context *context, struct expr *expr, struct token *token){
    maybe_load_address_for_array_or_function(context, IR_load_address, expr);
    
    if(expr->resolved_type->kind == AST_integer_type || expr->resolved_type->kind == AST_bitfield_type){
        // "The integer promotions are performed on each argument."
        maybe_insert_integer_promotion_cast(context, AST_cast, expr, token);
    }
    
    if(expr->resolved_type == &globals.typedef_f32){
        // "Arguments that have type float are promoted to double."
        push_ir(context, IR_cast_f32_to_f64); // @hmm: no constant propagation.
        
        // :retain_type_information_through_promotion
        expr->resolved_type = &globals.typedef_f64;
        expr->defined_type = &globals.typedef_f32.kind;
    }
}


func void get_pretty_print_string_for_type(struct context *context, struct string_list *format_list, struct ast_type *type, struct string field_width, struct string precision, struct string type_specifiers){
    
    switch(type->kind){
        case AST_enum:{
            // @cleanup:
            string_list_postfix(format_list, &context->scratch, string("enum(%d)"));
            // not_implemented;
        }break;
        case AST_pointer_type:{
            struct ast_pointer_type *pointer = (struct ast_pointer_type *)type;
            
            int is_string = 0;
            
            if(pointer->pointer_to == &globals.typedef_s8){
                for(u32 index = 0; index < type_specifiers.size; index++){
                    if(type_specifiers.data[index] == 's') is_string = 1;
                }
            }
            
            struct string format_string = string("%p");
            
            if(is_string){
                char *dot = precision.size ? "." : "";
                format_string = push_format_string(&context->scratch, "%%%.*s%s%.*ss", field_width.size, field_width.data, dot, precision.size, precision.data);
            }
            
            string_list_postfix(format_list, &context->scratch, format_string);
        }break;
        case AST_bitfield_type:
        case AST_integer_type:{
            
            // struct string field_width, struct string precision, struct string type_specifiers
            
            b32 is_signed = type_is_signed(type);
            char *length_modifier = (type->size == 8) ? "ll" : "";
            char *type_specifier = is_signed ? "d" : "x";
            
            for(u32 index = 0; index < type_specifiers.size; index++){
                if(type_specifiers.data[index] == 'n') type_specifier = is_signed ? "d" : "u";
                if(type_specifiers.data[index] == 'x') type_specifier = "x";
                if(type_specifiers.data[index] == 'u') type_specifier = "u";
                if(type_specifiers.data[index] == 'd') type_specifier = "d";
                if(type_specifiers.data[index] == 'o') type_specifier = "o";
            }
            
            char *dot = precision.size ? "." : "";
            char *prefix = (*type_specifier == 'x') ? "0x" : ((*type_specifier == 'o') ? "0o" : "");
            
            struct string format_string = push_format_string(&context->scratch, "%s%%%.*s%s%.*s%s%s", prefix, field_width.size, field_width.data, dot, precision.size, precision.data, length_modifier, type_specifier);
            
            string_list_postfix(format_list, &context->scratch, format_string);
        }break;
        case AST_float_type:{
            
            char *dot = precision.size ? "." : "";
            char *postfix = type->size == 4 ? "f" : "";
            
            struct string format_string = push_format_string(&context->scratch, "%%%.*s%s%.*sf%s", field_width.size, field_width.data, dot, precision.size, precision.data, postfix);
            
            string_list_postfix(format_list, &context->scratch, format_string);
        }break;
        case AST_void_type:{
            assert(context->error);
        }break;
        invalid_default_case();
    }
}

// 
// @cleanup: This function is sort of useless now.
// 
void printlike__infer_format_string_and_arguments_for_argument(struct context *context, struct expr *argument, struct string_list *pretty_print_list, struct string flags, struct string field_width, struct string precision, struct string type_specifiers, smm *call_arguments_count){
    
    struct ast_type *argument_type = argument->resolved_type;
    
    if(argument_type->kind == AST_struct || argument_type->kind == AST_union){
        //
        // Check if we should print in a multi line fashion.
        //
        s32 depth_or_minus_one = -1;
        for(smm flag_index = 0; flag_index < flags.size; flag_index++){
            if(flags.data[flag_index] == '#'){
                depth_or_minus_one = 0;
                break;
            }
        }
        
        struct ast_compound_type *root_compound = (struct ast_compound_type *)argument_type;
        
        // @cleanup: Check the 'defined_type'?
        string_list_postfix_no_copy(pretty_print_list, &context->scratch, argument_type->kind == AST_struct ? string("(struct ") : string("(union "));
        string_list_postfix_no_copy(pretty_print_list, &context->scratch, root_compound->identifier->string);
        string_list_postfix_no_copy(pretty_print_list, &context->scratch, (depth_or_minus_one == -1) ? string("){") : string("){\n"));
        
        struct stack_node{
            struct stack_node *next;
            struct stack_node *prev;
            
            struct ast_type *lhs_type;
            union{
                smm member_at;
                smm array_at;
            };
        } initial_node = {
            .lhs_type = argument_type,
        };
        
        struct{
            struct stack_node *first;
            struct stack_node *last;
        } designator_stack = {.first = &initial_node, .last = &initial_node};
        
        while(designator_stack.first){
            
            while(true){
                // 
                // Go as deep as we can!
                // 
                struct stack_node *node = designator_stack.first;
                struct ast_type *type = node->lhs_type;
                
                struct stack_node *new_node = null;
                
                if(type->kind == AST_struct || type->kind == AST_union){
                    struct ast_compound_type *compound = (struct ast_compound_type *)type;
                    type = compound->members[node->member_at].type; // @cleanup: empty structures.
                    
                    new_node = push_struct(&context->scratch, struct stack_node);
                    new_node->lhs_type = type;
                    new_node->member_at = 0;
                }else if(type->kind == AST_array_type){
                    struct ast_array_type *array_type = (struct ast_array_type *)type;
                    type = array_type->element_type;// @cleanup: empty arrays.
                    
                    new_node = push_struct(&context->scratch, struct stack_node);
                    new_node->lhs_type = type;
                    new_node->array_at = 0;
                }else{
                    break;
                }
                
                dll_push_front(designator_stack, new_node);
            }
            
            // 
            // Duplicate the emit_location on the stack.
            // 
            push_ir(context, IR_duplicate);
            
            if(depth_or_minus_one != -1){
                string_list_postfix_no_copy(pretty_print_list, &context->scratch, string("    "));
            }
            
            // 
            // Create expression.
            // 
            for(struct stack_node *node = designator_stack.last; node; node = node->prev){
                struct ast_type *type = node->lhs_type;
                
                if(type->kind == AST_struct || type->kind == AST_union){
                    struct ast_compound_type *compound = (struct ast_compound_type *)type;
                    
                    struct ir_dot_or_arrow *dot = push_struct(&context->ir_arena, struct ir_dot_or_arrow);
                    dot->base.kind = IR_member;
                    dot->member = &compound->members[node->member_at];
                    
                    string_list_postfix_no_copy(pretty_print_list, &context->scratch, string("."));
                    string_list_postfix_no_copy(pretty_print_list, &context->scratch, dot->member->name->string);
                    
                }else if(type->kind == AST_array_type){
                    struct ast_array_type *array_type = (struct ast_array_type *)type;
                    
                    ast_push_u64_literal(context, node->array_at, null);
                    
                    struct ir_subscript *subscript = push_struct(&context->ir_arena, struct ir_subscript);
                    subscript->base.kind = IR_array_subscript;
                    subscript->type = array_type->element_type;
                    
                    struct string string = push_format_string(&context->scratch, "[%llu]", node->array_at);
                    string_list_postfix_no_copy(pretty_print_list, &context->scratch, string);
                }
            }
            
            string_list_postfix_no_copy(pretty_print_list, &context->scratch, string(" = "));
            
            //
            // Actually pretty print the argument to string.
            //
            get_pretty_print_string_for_type(context, pretty_print_list, designator_stack.first->lhs_type, field_width, precision, type_specifiers);
            *call_arguments_count += 1;
            
            // 
            // Swap in the correct result.
            // 
            push_ir(context, IR_swap_lhs_rhs);
            
            dll_remove(designator_stack, designator_stack.first);
            
            while(designator_stack.first){
                struct stack_node *node = designator_stack.first;
                struct ast_type *type = node->lhs_type;
                
                if(type->kind == AST_struct){
                    struct ast_compound_type *compound = (struct ast_compound_type *)type;
                    struct compound_member *member = &compound->members[node->member_at];
                    
                    node->member_at += member->next_member_increment;
                    
                    if(node->member_at < compound->amount_of_members) break; // We found the next member!
                }else if(type->kind == AST_union){
                    // Pop the union, only ever print one member.
                }else{
                    struct ast_array_type *array = (struct ast_array_type *)type;
                    
                    node->array_at++;
                    
                    if(node->array_at < array->amount_of_elements) break; // We found the next member!
                }
                
                dll_remove(designator_stack, designator_stack.first);
            }
            
            if(designator_stack.first){
                string_list_postfix_no_copy(pretty_print_list, &context->scratch, (depth_or_minus_one == -1) ? string(", ") : string(",\n"));
            }else{
                if(depth_or_minus_one != -1) string_list_postfix_no_copy(pretty_print_list, &context->scratch, string(",\n"));
            }
        }
        
        string_list_postfix_no_copy(pretty_print_list, &context->scratch, string("}"));
        
        // 
        // Pop on the expression.
        // 
        push_ir(context, IR_pop_expression);
        
    }else{
        // No need to push a declaration for these, they are just read once anyway, 
        // also no need to specify a depth.
        get_pretty_print_string_for_type(context, pretty_print_list, argument->resolved_type, field_width, precision, type_specifiers);
        *call_arguments_count += 1;
    }
}

// @WARNING: identifier_expression->ast might not be accurate.
static void parse_call_to_printlike_function_arguments(struct context *context, struct ast_function_type *printlike_function_type, struct expr *identifier_expression, struct expr *format_string_argument, smm *call_arguments_count){
    
    (void)printlike_function_type;
    
    if(format_string_argument->ir->kind != IR_string_literal){
        
        // 
        // The first argument in the 'variable_arguments' is the format string, or if not present
        // the format string should be either {} or {:#}.
        // For large structures, more than 3 members we use the {:#}.
        // For small structures we use {}. If there is more than one argument the format string should be always {}.
        //
        
        struct expr expr = *format_string_argument;
        
        // 
        // Build the string literal and then swap it into the correct position.
        // 
        struct ir_string_literal *format_string_literal = push_struct(&context->ir_arena, struct ir_string_literal);
        format_string_literal->base.kind = IR_string_literal;
        format_string_literal->string_kind = STRING_KIND_utf8;
        
        push_ir(context, IR_load_address);
        
        push_ir(context, IR_swap_lhs_rhs);
        
        struct string_list pretty_print_list = zero_struct;
        
        struct string flags = zero_struct, field_width = zero_struct, precision = zero_struct, type_specifiers = zero_struct;
        
        while(true){
            printlike__infer_format_string_and_arguments_for_argument(context, &expr, &pretty_print_list, flags, field_width, precision, type_specifiers, call_arguments_count);
            
            if(!peek_token_eat(context, TOKEN_comma)) break;
            
            expr = parse_expression(context, /*should_skip_comma_expression*/true);
        }
        
        format_string_literal->value = string_list_flatten(pretty_print_list, context->arena);
        return;
    }
    
    if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)){
        report_error(context, get_current_token_for_error_report(context), "Expected ',' or ')' after format string argument of __declspec(printlike)-function.");
        return;
    }
    
    struct ir_string_literal *format_string_literal = (struct ir_string_literal *)format_string_argument->ir;
    struct string format_string = format_string_literal->value;
    
    // Once we have the string literal, we can add the `implicit_address_conversion`.
    maybe_load_address_for_array_or_function(context, IR_load_address, format_string_argument);
    
    // 
    // Start parsing the format string.
    // 
    
    struct string_list pretty_print_list = zero_struct;
    
    while(format_string.size){
        struct string start = string_eat_until_characters_front(&format_string, "{%");
        
        if(!format_string.size){
            //
            // We have 'eat_delimiter = false' this means if 'format_string.size == 0', 
            // we did not find a '{' or '%'. Just copy the _start_ to the 'string_list' and we are done!
            //
            string_list_postfix(&pretty_print_list, &context->scratch, start);
            break;
        }
        
        if(format_string.data[0] == '%'){
            //
            // We got a C style format specifier!
            //
            
            //
            // printf syntax (from c-spec):
            //
            // %<flags><field-width><precision><length-modifier><conversion-specifier>
            // 
            // flags: 
            //    '+' - add a plus if a signed value is read (printf("%+d", 3) -> +3)
            //    '-' - the result is left-justified
            //    ' ' - same as + but with a space instead of a +
            //    '#' - not sure what this does, it says the result is in _alternate form_
            //    '0' - the result is padded with 0 instead of with spaces.
            // field-width:
            //    Non-negative integer or '*'.
            //    If the print would be less than this, instead the result will be padded.
            // precision:
            //    A '.' followed by a non-negative integer or '*'.
            //    specifies the amount of digest to appear.
            // length-modifier:
            //    Specifies the length of the argument:
            //        hh -> signed or unsigned char
            //        h  -> signed or unsigned short
            //        ll -> long long or unsigned long long int
            //        l  -> long or unsigned long int
            //        j  -> intmax_t or uintmax_t ?
            //        z  -> size_t
            //        t  -> ptrdiff_t
            //        L  -> long double
            // conversion-specifier:
            //    The actual type we want to print:
            //        d, i -> int, decimal
            //        u    -> unsigned int, decimal
            //        o    -> unsigned int, octal 
            //        x    -> unsigned int, lower case hex
            //        X    -> unsigned int, upper case hex
            //        f, F -> floating point, F produces INF instead of inf
            //        e, E -> floating point, scientific notation, E makes the e in 1e10 be upper case
            //        g, G -> floating point, mix of scientific and usual notation
            //        a, A -> floating point, maybe hexfloat?
            //        c    -> char, if l is present wchar_t
            //        s    -> char *, if l is present wchar_t *
            //        p    -> pointer
            //        n    -> pointer to signed integer writes number of characters written
            //        %    -> just writes '%'
            
            if(format_string.size >= 2 && format_string.data[1] == '%'){
                //
                // '%%' inserts one '%' into the string, but we need to keep both in the format string.
                //
                string_eat_front(&format_string, 2); // eat both '%%'.
                start.size += 2;                     // keep both of the '%'.
                string_list_postfix(&pretty_print_list, &context->scratch, start);
                continue;
            }
            
            //
            // Copy every thing that happened before the argument to the _pretty print format string._
            //
            string_list_postfix(&pretty_print_list, &context->scratch, start);
            
            struct string it = format_string;
            string_eat_front(&it, 1); // eat the '%'
            
            struct string flags = string_eat_characters_front(&it, "+- #0");
            struct string field_width = zero_struct;
            struct string precision   = zero_struct;
            
            // 
            // Parse field width.
            // 
            if(it.size && it.data[0] == '*'){
                field_width = string_eat_front(&it, 1);
                
                if(peek_token(context, TOKEN_closed_paren)) goto too_few_args_for_print;
                
                struct expr argument = parse_expression(context, /*should_skip_comma_expression*/true);
                *call_arguments_count += 1;
                
                if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)) break;
                
                maybe_insert_implicit_nodes_for_varargs_argument(context, &argument, argument.token);
                
                if(argument.resolved_type->kind != AST_integer_type){
                    struct string type_string = push_type_string(context->arena, &context->scratch, argument.resolved_type);
                    report_error(context,  argument.token, "Field width specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                    return;
                }
            }else{
                field_width = string_eat_characters_front(&it, "0123456789");
            }
            
            // 
            // Parse precision.
            // 
            if(it.size && it.data[0] == '.'){
                string_eat_front(&it, 1); // eat the '.'
                
                if(it.size && it.data[0] == '*'){
                    precision = string_eat_front(&it, 1);
                    
                    if(peek_token(context, TOKEN_closed_paren)) goto too_few_args_for_print;
                    
                    struct expr argument = parse_expression(context, /*should_skip_comma_expression*/true);
                    *call_arguments_count += 1;
                    
                    if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)) break;
                    
                    maybe_insert_implicit_nodes_for_varargs_argument(context, &argument, argument.token);
                    
                    if(argument.resolved_type->kind != AST_integer_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, argument.resolved_type);
                        report_error(context,  argument.token, "Precision specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                        return;
                    }
                }else{
                    precision = string_eat_characters_front(&it, "0123456789");
                }
            }
            
            //
            // Parse length modifiers.
            //
            int length_modifier = 0;
            if(string_front_match_eat(&it, "hh")){
                length_modifier = 'hh';
            }else if(string_front_match_eat(&it, "h")){
                length_modifier = 'h';
            }else if(string_front_match_eat(&it, "ll")){
                length_modifier = 'll';
            }else if(string_front_match_eat(&it, "l")){
                length_modifier = 'l';
            }else if(string_front_match_eat(&it, "L")){
                length_modifier = 'L';
            }else if(string_front_match_eat(&it, "j")){
                length_modifier = 'j';
            }else if(string_front_match_eat(&it, "z")){
                length_modifier = 'z';
            }else if(string_front_match_eat(&it, "t")){
                length_modifier = 't';
            }
            
            if(!it.size){
                report_error(context, format_string_argument->token, "Missing format specifier after '%.*s' in format string.", format_string.size, format_string.data);
                return;
            }
            
            u8 conversion_specifier = it.data[0];
            string_eat_front(&it, 1);
            
            struct string format_specifier = {
                .data = format_string.data,
                .size = it.data - format_string.data,
            };
            
            // We are done with parsing _commit_ the format string. 
            // This is sort of stupid, we should probably get rid of 'it'
            format_string = it;
            
            if(peek_token(context, TOKEN_closed_paren)) goto too_few_args_for_print;
            
            struct expr argument = parse_expression(context, /*should_skip_comma_expression*/true);
            *call_arguments_count += 1;
            
            if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)) break;
            
            if(conversion_specifier == '?'){
                //
                // Special format specifier '?'. Infer the type!
                //
                if(length_modifier){
                    report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Length specifiers (hh, h, l, ll etc.) do not apply to format specifier '?'.");
                }
                
                struct string type_specifiers = zero_struct;
                printlike__infer_format_string_and_arguments_for_argument(context, &argument, &pretty_print_list, flags, field_width, precision, type_specifiers, call_arguments_count);
                continue;
            }
            
            //
            // Classic C format specifiers. Append the format specifier to the pretty print list as the user put it there.
            // Append the argument to the argument list. Check the format string for possible errors.
            //
            
            string_list_postfix(&pretty_print_list, &context->scratch, format_specifier);
            
            //
            // d,   i,   o,   u,   x,   X   (signed or unsigned int)
            // hhd, hhi, hho, hhu, hhx, hhX (signed or unsigned char)
            // hd,  hi,  ho,  hu,  hx,  hX  (signed or unsigned short)
            // ld,  li,  lo,  lu,  lx,  lX  (signed or unsigned long int)
            // lld, lli, llo, llu, llx, llX (signed or unsigned long long int)
            // jd,  ji,  jo,  ju,  jx,  jX  (intmax_t or uintmax_t)
            // zd,  zi,  zo,  zu,  zx,  zX  (signed size_t or size_t)
            // td,  ti,  to,  tu,  tx,  tX  (ptrdiff_t or unsigned ptrdiff_t)
            // 
            // n same as the above, but pointer to that type.
            // 
            // lc wint_t
            // ls wchar_t 
            // l has no effect on aA eE fF gG
            // L coverts to long double for aA eE fF gG.
            // 
            
            enum ast_kind *defined_type = argument.defined_type;
            struct ast_type *promoted_type = argument.resolved_type;
            struct ast_type *unpromoted_type = promoted_type;
            if(conversion_specifier == 'n'){
                if(promoted_type->kind != AST_pointer_type){
                    report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%s%c' expects a pointer.", (char *)&length_modifier, conversion_specifier);
                }else{
                    struct ast_pointer_type *pointer = (struct ast_pointer_type *)promoted_type;
                    defined_type    = pointer->pointer_to_defined_type;
                    unpromoted_type = pointer->pointer_to;
                    
                    conversion_specifier = 'd';
                }
            }
            
            // :retain_type_information_through_promotion
            if(defined_type){
                struct ast_type *original = unpromoted_type;
                
                if(*defined_type == IR_typedef){
                    unpromoted_type = ((struct ast_declaration *)defined_type)->type;
                }else{
                    unpromoted_type = (struct ast_type *)defined_type;
                }
                
                if(unpromoted_type->kind != AST_integer_type && unpromoted_type->kind != AST_float_type){
                    // might have been like an enum or something just go back to the defined type.
                    unpromoted_type = original;
                }
            }
            
            switch(conversion_specifier){
                case 'd': case 'i': case 'u': case 'o': case 'x': case 'X':{
                    
                    // @cleanup: For now we don't report warnings for things that have precision.
                    if(precision.size) break;
                    
                    if(length_modifier == 'j' || length_modifier == 'z' || length_modifier == 't'){
                        
                        char *should_warn = null;
                        
                        struct string type_string = string("");
                        
                        if(defined_type && *defined_type == IR_typedef){
                            struct ast_declaration *ast_typedef = (struct ast_declaration *)defined_type;
                            type_string = ast_typedef->identifier->string;
                        }
                        
                        if(length_modifier == 'j'){
                            if((conversion_specifier == 'd' || conversion_specifier == 'i')){
                                if(!string_match(type_string, string("intmax_t"))) should_warn = "intmax_t";
                            }else if(conversion_specifier == 'u'){
                                if(!string_match(type_string, string("uintmax_t"))) should_warn = "uintmax_t";
                            }else{
                                // for the rest (o, x, X) allow either
                                if(!string_match(type_string, string("intmax_t")) && !string_match(type_string, string("uintmax_t")))  should_warn = "intmax_t or uintmax_t";
                            }
                        }else if(length_modifier == 'z'){
                            if(!string_match(type_string, string("size_t"))) should_warn = "size_t";
                        }else if(length_modifier == 't'){
                            if(!string_match(type_string, string("ptrdiff_t"))) should_warn = "ptrdiff_t";
                        }
                        
                        if(should_warn){
                            report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%s%c' expects %s.", (char *)&length_modifier, conversion_specifier, should_warn);
                        }
                    }else if(length_modifier == 0 || length_modifier == 'hh' || length_modifier == 'h' || length_modifier == 'l' || length_modifier == 'll'){
                        //
                        // Integral conversion specifier, go by the list of expected types.
                        //
                        
                        if(conversion_specifier == 'd' || conversion_specifier == 'i'){
                            struct ast_type *expected_type = &globals.typedef_s32;
                            if(length_modifier == 'hh') expected_type = &globals.typedef_s8;
                            if(length_modifier == 'h')  expected_type = &globals.typedef_s16;
                            if(length_modifier == 'l')  expected_type = &globals.typedef_s32;
                            if(length_modifier == 'll') expected_type = &globals.typedef_s64;
                            
                            if(expected_type != unpromoted_type && expected_type != promoted_type){
                                struct string expected_type_string = push_type_string(context->arena, &context->scratch, expected_type);
                                struct string type_string          = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%s%c' expects %.*s, but the argument is of type '%.*s'.", (char *)&length_modifier, conversion_specifier, expected_type_string.size, expected_type_string.data, type_string.size, type_string.data);
                            }
                        }else if(conversion_specifier == 'u'){
                            struct ast_type *expected_type = &globals.typedef_u32;
                            if(length_modifier == 'hh') expected_type = &globals.typedef_u8;
                            if(length_modifier == 'h')  expected_type = &globals.typedef_u16;
                            if(length_modifier == 'l')  expected_type = &globals.typedef_u32;
                            if(length_modifier == 'll') expected_type = &globals.typedef_u64;
                            
                            if(expected_type != unpromoted_type && expected_type != promoted_type){
                                struct string expected_type_string = push_type_string(context->arena, &context->scratch, expected_type);
                                struct string type_string          = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%s%c' expects %.*s, but the argument is of type '%.*s'.", (char *)&length_modifier, conversion_specifier, expected_type_string.size, expected_type_string.data, type_string.size, type_string.data);
                            }
                        }else{
                            //
                            // Allow either signed an unsigned for 'o' 'x' 'X'.
                            //
                            struct ast_type *expected_signed_type = &globals.typedef_s32;
                            if(length_modifier == 'hh') expected_signed_type = &globals.typedef_s8;
                            if(length_modifier == 'h')  expected_signed_type = &globals.typedef_s16;
                            if(length_modifier == 'l')  expected_signed_type = &globals.typedef_s32;
                            if(length_modifier == 'll') expected_signed_type = &globals.typedef_s64;
                            
                            struct ast_type *expected_unsigned_type = &globals.typedef_u32;
                            if(length_modifier == 'hh') expected_unsigned_type = &globals.typedef_u8;
                            if(length_modifier == 'h')  expected_unsigned_type = &globals.typedef_u16;
                            if(length_modifier == 'l')  expected_unsigned_type = &globals.typedef_u32;
                            if(length_modifier == 'll') expected_unsigned_type = &globals.typedef_u64;
                            
                            if(expected_signed_type != unpromoted_type && expected_unsigned_type != unpromoted_type && expected_signed_type != promoted_type && expected_unsigned_type != promoted_type){
                                struct string signed_type_string   = push_type_string(context->arena, &context->scratch, expected_signed_type);
                                struct string type_string          = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%s%c' expects signed or unsigned %.*s, but the argument is of type '%.*s'.", (char *)&length_modifier, conversion_specifier, signed_type_string.size, signed_type_string.data, type_string.size, type_string.data);
                            }
                        }
                    }else{
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                    }
                }break;
                
                case 'c':{
                    if(length_modifier == 0){
                        if(unpromoted_type != &globals.typedef_u8 && unpromoted_type != &globals.typedef_s8){
                            struct string type_string = push_type_string(context->arena, &context->scratch, unpromoted_type);
                            report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%c' expects signed or unsigned character, but the argument is of type '%.*s'.", conversion_specifier, type_string.size, type_string.data);
                        }
                    }else if(length_modifier == 'l'){
                        if(unpromoted_type != &globals.typedef_u16 && unpromoted_type != &globals.typedef_s16){
                            struct string type_string = push_type_string(context->arena, &context->scratch, unpromoted_type);
                            report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%%c' expects signed or unsigned wide character (wchar_t), but the argument is of type '%.*s'.", conversion_specifier, type_string.size, type_string.data);
                        }
                    }else{
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                    }
                }break;
                
                case 'p':{
                    if(length_modifier != 0){
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                    }
                    
                    if(promoted_type->kind != AST_pointer_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, promoted_type);
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%p' requires a pointer, but the argument is of type '%.*s'.", type_string.size, type_string.data);
                    }
                }break;
                
                case 'e': case 'E':
                case 'a': case 'A':
                case 'g': case 'G':
                case 'f': case 'F':{
                    
                    if(length_modifier != 0 && length_modifier != 'l' && length_modifier != 'L'){
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                    }
                    
                    if(promoted_type->kind != AST_float_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, promoted_type);
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%f' requires a float, but the argument is of type '%.*s'.", type_string.size, type_string.data);
                    }
                }break;
                
                case 's':{
                    struct ast_type *element_type = null;
                    if(promoted_type->kind == AST_pointer_type){
                        struct ast_pointer_type *pointer = (struct ast_pointer_type *)promoted_type;
                        element_type = pointer->pointer_to;
                    }else if(promoted_type->kind == AST_array_type){
                        struct ast_array_type *array = (struct ast_array_type *)promoted_type;
                        element_type = array->element_type;
                    }
                    
                    b32 should_warn = true;
                    if(length_modifier == 'l'){
                        if(element_type == &globals.typedef_s16 || element_type == &globals.typedef_u16) should_warn = false;
                    }else if(length_modifier == 0){
                        if(element_type == &globals.typedef_s8 || element_type == &globals.typedef_u8) should_warn = false;
                    }else{
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                    }
                    
                    if(should_warn){
                        struct string type_string = push_type_string(context->arena, &context->scratch, promoted_type);
                        report_warning(context, WARNING_incorrect_format_specifier, argument.token, "Format specifier '%%s' requires 'char *', but the argument is of type '%.*s'.", type_string.size, type_string.data);
                    }
                }break;
                
                default:{
                    report_warning(context, WARNING_unknown_format_specifier, format_string_argument->token, "Unknown format specifier '%%%c'.", conversion_specifier);
                }break;
            }
            
            continue;
        }
        
        // Make sure we have an argument.
        assert(format_string.data[0] == '{');
        string_eat_front(&format_string, 1); // Eat the '{'.
        
        //
        // Rust style format specifier Syntax:
        //
        // {[integer|identifier[=]][:[flags][field-width][.precision][type]]} 
        //
        // special cases are {identifier} which evaluates to _"{}", identifier_,
        // {identifier=}, which evaluates to _"identifier = {}", identifier_
        // If the type of the argument is a compound, all format specifiers are 
        // applied to all the fields.
        //
        // We want to allow random '{' in the string without complaining: 
        // Things like:
        //    print("{");
        //    print("{ hello = %u }", 10);
        // etc, should not be treated as rust format strings.
        // Hence, we have to be able to "fail" here at many points.
        // 
        
        struct string identifier = eat_identifier(&format_string);
        
        int have_equals = 0;
        if(identifier.size && format_string.size && format_string.data[0] == '='){
            have_equals = 1;
            string_eat_front(&format_string, 1);
        }
        
        struct string flags           = zero_struct;
        struct string field_width     = zero_struct;
        struct string precision       = zero_struct;
        struct string type_specifiers = zero_struct;
        
        int this_is_probably_supposed_to_be_a_rust_format_string = 0;
        
        
        // 
        // Allow for '{.*s}', '{*s}', '{#}'
        // 
        if(format_string.size && (format_string.data[0] == ':' || format_string.data[0] == '.' || format_string.data[0] == '#' || format_string.data[0] == '*')){
            if(format_string.data[0] == ':') string_eat_front(&format_string, 1);
            
            flags = string_eat_characters_front(&format_string, "+- #0");
            
            this_is_probably_supposed_to_be_a_rust_format_string |= (string_strip_whitespace(flags).size != 0);
            
            // 
            // Parse field width.
            // 
            if(format_string.size && format_string.data[0] == '*'){
                field_width = string_eat_front(&format_string, 1);
                
                if(peek_token(context, TOKEN_closed_paren)) goto too_few_args_for_print;
                
                struct expr argument = parse_expression(context, /*should_skip_comma_expression*/true);
                *call_arguments_count += 1;
                
                if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)) break;
                
                maybe_insert_implicit_nodes_for_varargs_argument(context, &argument, argument.token);
                
                if(argument.resolved_type->kind != AST_integer_type){
                    struct string type_string = push_type_string(context->arena, &context->scratch, argument.resolved_type);
                    report_error(context,  argument.token, "Field width specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                    return;
                }
            }else{
                field_width = string_eat_characters_front(&format_string, "0123456789");
            }
            
            // 
            // Parse precision.
            // 
            if(format_string.size && format_string.data[0] == '.'){
                string_eat_front(&format_string, 1); // eat the '.'
                
                if(format_string.size && format_string.data[0] == '*'){
                    precision = string_eat_front(&format_string, 1);
                    
                    if(peek_token(context, TOKEN_closed_paren)) goto too_few_args_for_print;
                    
                    struct expr argument = parse_expression(context, /*should_skip_comma_expression*/true);
                    *call_arguments_count += 1;
                    
                    if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)) break;
                    
                    maybe_insert_implicit_nodes_for_varargs_argument(context, &argument, argument.token);
                    
                    if(argument.resolved_type->kind != AST_integer_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, argument.resolved_type);
                        report_error(context,  argument.token, "Precision specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                        return;
                    }
                }else{
                    precision = string_eat_characters_front(&format_string, "0123456789");
                }
            }
            
            type_specifiers = eat_identifier(&format_string);
        }
        
        if((format_string.size == 0) || format_string.data[0] != '}'){
            
            if(this_is_probably_supposed_to_be_a_rust_format_string){
                // 
                // We think the intent of this string was to be a {}-format string, but it was malformed.
                // 
                
                struct string format_specifier = {
                    .data = start.data + start.size,
                    .size = (format_string.data + format_string.size) - start.data + start.size,
                };
                
                // Scan up to the end of the specifier.
                format_specifier = eat_until_char(&format_specifier, '}', /*eat delimiter*/true);
                
                char *dotdotdot = "";
                
                if(format_specifier.data[format_specifier.size-1] != '}'){
                    dotdotdot = "...";
                    format_specifier.size = min_of(format_specifier.size, 10);
                }
                
                report_error(context, format_string_argument->token, "Incorrectly formatted format string '%.*s%s' for __declspec(printlike) procedure.", format_specifier.size, format_specifier.data, dotdotdot);
                return;
            }
            
            // 
            // It ended up not being a rust string I guess.
            // Eat everything we have parsed in 'it', 
            // as it does not contain either '{' or '%' by the way we parsed the string.
            // 
            start.size += (format_string.data - (start.data + start.size));
            string_list_postfix(&pretty_print_list, &context->scratch, start);
            continue;
        }
        
        //
        // We got a Rust style format specifier!
        // Copy every thing that happened before the argument to the _pretty print format string_.
        //
        string_eat_front(&format_string, 1); // eat the '}'.
        string_list_postfix(&pretty_print_list, &context->scratch, start);
        
        struct string format_specifier = {
            .data = start.data + start.size,
            .size = (format_string.data + format_string.size) - start.data + start.size,
        };
        
        if(identifier.size){
            //
            // This was a format specifier like `print("{arst}")`
            // It should "translate" to `print("{}", arst)`
            // and from there further depending on what 'arst' is.
            //
            struct ast_declaration *declaration = lookup_declaration(context->current_scope, context->current_compilation_unit, atom_for_string(identifier));
            
            if(!declaration && (string_match(identifier, string("s")) || string_match(identifier, string("x")))){
                // Allow '{s}', '{x}'.
                type_specifiers = identifier;
            }else{
                if(!declaration){
                    report_error(context, format_string_argument->token, "Identifier in format specifier '%.*s', is undeclared.", format_specifier.size, format_specifier.data);
                    return;
                }
                
                struct ir_identifier *ident = push_struct(&context->ir_arena, struct ir_identifier);
                ident->base.kind = IR_identifier;
                ident->decl = declaration;
                
                *call_arguments_count += 1;
                
                if(have_equals){
                    struct string identifier_equals = push_format_string(&context->scratch, "%.*s = ", identifier.size, identifier.data);
                    string_list_postfix(&pretty_print_list, &context->scratch, identifier_equals);
                }
                
                struct expr argument = {.ir = &ident->base, format_string_argument->token, declaration->type, declaration->defined_type};
                
                printlike__infer_format_string_and_arguments_for_argument(context, &argument, &pretty_print_list, flags, field_width, precision, type_specifiers, call_arguments_count);
                continue;
            }
        }
        
        if(peek_token(context, TOKEN_closed_paren)){
            too_few_args_for_print:;
            
            report_error(context, identifier_expression->token, "Less arguments for __declspec(printlike)-function, than indicated by the format string.");
            return;
        }
        
        struct expr argument = parse_expression(context, /*should_skip_comma_expression*/true);
        printlike__infer_format_string_and_arguments_for_argument(context, &argument, &pretty_print_list, flags, field_width, precision, type_specifiers, call_arguments_count);
        
        if(!peek_token_eat(context, TOKEN_comma) && !peek_token(context, TOKEN_closed_paren)) break;
    }
    
    format_string_literal->value = string_list_flatten(pretty_print_list, context->arena);
}

// :compound_assignments
func void punt_compound_assignment(struct context *context, struct expr *lhs, struct expr *rhs, struct token *site, enum ast_kind ast_kind){
    // 
    // IR_duplicate_lhs:
    //     {lhs, rhs} -> {lhs, lhs, rhs}
    // IR_op:
    //     {lhs, lhs, rhs} -> {lhs, result}
    // IR_store:
    //     {lhs, result} -> {result}
    //     
    
    struct ir *dup = push_ir(context, IR_duplicate_lhs);
    
    struct expr dup_expr = {.ir = dup, site, lhs->resolved_type, lhs->defined_type};
    
    maybe_insert_arithmetic_conversion_casts(context, &dup_expr, rhs, site);
    
    enum ir_type ir_type = ir_type_from_type(dup_expr.resolved_type);
    enum ir_kind op_ir_kind;
    
    switch(ast_kind){
        case AST_plus_assignment:{
            u32 type_index = ((ir_type - IR_TYPE_s32) >> 1) + (ir_type == IR_TYPE_f64);
            op_ir_kind = IR_add_u32 + type_index;
        }break;
        case AST_minus_assignment:{
            u32 type_index = ((ir_type - IR_TYPE_s32) >> 1) + (ir_type == IR_TYPE_f64);
            op_ir_kind = IR_subtract_u32 + type_index;
        }break;
        case AST_and_assignment:{
            op_ir_kind = IR_and_u32 + ((ir_type - IR_TYPE_s32) >> 1);
        }break;
        case AST_or_assignment:{
            op_ir_kind = IR_or_u32 + ((ir_type - IR_TYPE_s32) >> 1);
        }break;
        case AST_xor_assignment:{
            op_ir_kind = IR_xor_u32 + ((ir_type - IR_TYPE_s32) >> 1);
        }break;
        case AST_modulo_assignment:{
            op_ir_kind = IR_mod_s32 + (ir_type - IR_TYPE_s32);
        }break;
        case AST_divide_assignment:{
            op_ir_kind = IR_divide_s32 + (ir_type - IR_TYPE_s32);
        }break;
        case AST_times_assignment:{
            op_ir_kind = IR_multiply_s32 + (ir_type - IR_TYPE_s32);
        }break;
        case AST_left_shift_assignment:{
            op_ir_kind = IR_left_shift_s32 + (ir_type - IR_TYPE_s32);
        }break;
        case AST_right_shift_assignment:{
            op_ir_kind = IR_right_shift_s32 + (ir_type - IR_TYPE_s32);
        }break;
        invalid_default_case(op_ir_kind = IR_invalid);
    }
    
    rhs->ir = push_ir(context, op_ir_kind);
    rhs->resolved_type = dup_expr.resolved_type; // Might have changed in the conversion cast.
    rhs->defined_type = dup_expr.defined_type;
    
    maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, rhs, site);
    
    // @copy and paste from AST_assignment case.
    struct ast_type *lhs_type = lhs->resolved_type;
    if(lhs_type->kind == AST_bitfield_type){
        struct ir_store_bitfield *bitfield_assignment = push_struct(&context->ir_arena, struct ir_store_bitfield);
        bitfield_assignment->base.kind = IR_store_bitfield;
        bitfield_assignment->bitfield_type = (struct ast_bitfield_type *)lhs_type;
        rhs->ir = &bitfield_assignment->base;
    }else{
        enum ir_kind ir_kind = (lhs_type->flags & TYPE_FLAG_is_atomic) ? IR_store_atomic : IR_store;
        rhs->ir = push_ir(context, ir_kind);
    }
    
    rhs->resolved_type = lhs->resolved_type;
    rhs->defined_type = lhs->defined_type;
}

// @cleanup: Eventually, we should implement this ourselves.
__declspec(dllimport) double strtod(const char *str, char **endptr);

func struct expr parse_expression(struct context *context, b32 should_skip_comma_expression){
    
    if(maybe_report_error_for_stack_exhaustion(context, get_current_token_for_error_report(context), "Expression nests to deep.")){
        return invalid_ast(context);
    }
    
    // We push an invalid ast onto the stack, to 'shield' the ast stack, when calling 'parse_expression' recursively. We assert that this works in two ways here.
    
    smm ast_stack_before = context->ast_stack_at;
    b32 did_push = false;
    if(ast_stack_before){ // we only have to push if we are not the first thing
        if(context->ast_stack_at < array_count(context->ast_stack)){
            ast_stack_push(context, AST_invalid, &globals.invalid_token, null);
            did_push = true; // @cleanup: this could probably be simplified.
        }
    }
    
    restart:;
    
    if(context->should_exit_statement) return invalid_ast(context);
    
    struct expr operand = zero_struct;
    
    switch(get_current_token(context)->type){
        
        // 
        // :prefix_expression parse_prefix_expression
        // 
        
        // @note: Integer promotion get's handled in the second part.
        
        case TOKEN_increment:   ast_stack_push(context, AST_unary_preinc,      next_token(context), null); goto restart;
        case TOKEN_decrement:   ast_stack_push(context, AST_unary_predec,      next_token(context), null); goto restart;
        case TOKEN_logical_not: ast_stack_push(context, AST_unary_logical_not, next_token(context), null); goto restart;
        case TOKEN_bitwise_not: ast_stack_push(context, AST_unary_bitwise_not, next_token(context), null); goto restart;
        case TOKEN_times:       ast_stack_push(context, AST_unary_deref,       next_token(context), null); goto restart;
        case TOKEN_minus:       ast_stack_push(context, AST_unary_minus,       next_token(context), null); goto restart;
        case TOKEN_plus:        ast_stack_push(context, AST_unary_plus,        next_token(context), null); goto restart;
        case TOKEN_and:         ast_stack_push(context, AST_unary_address,     next_token(context), null); goto restart;
        
        // :sizeof/alignof threading bug
        case TOKEN_sizeof:{
            struct ast_stack_entry *entry = ast_stack_push(context, AST_sizeof, next_token(context), null); 
            entry->other = arena_current(&context->ir_arena); // For sizeof(expression), we have to delete the whole expression.
            goto restart;
        }
        case TOKEN_alignof:{
            struct ast_stack_entry *entry = ast_stack_push(context, AST_alignof, next_token(context), null); 
            entry->other = arena_current(&context->ir_arena); // For alignof(expression), we have to delete the whole expression.
            goto restart;
        }
        
        case TOKEN_open_paren:{
            struct token *open_paren = next_token(context);
            
            // 
            // :cast_expression parse_cast_expression
            // 
            
            context->maybe_in_cast = peek_token(context, TOKEN_identifier);
            
            struct type_info_return type_to_cast_to = maybe_parse_type_for_cast_or_sizeof(context);
            if(context->should_exit_statement) return invalid_ast(context);
            
            if(type_to_cast_to.type){
                
                context->maybe_in_cast = null;
                
                expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of a cast.");
                if(peek_token(context, TOKEN_open_curly)){
                    // 
                    // :struct_literal :parse_struct_literal :parse_struct_or_array_literal :compound_literal :parse_compund_literal
                    // 
                    struct token *open_curly = get_current_token(context);
                    
                    if(type_to_cast_to.type->kind == AST_function_type){
                        report_error(context, open_paren, "Cast to function is illegal."); // @cleanup: Should we allow this as weird version of lambas?
                        return invalid_ast(context);
                    }
                    
                    struct ast_declaration *compound_literal_declaration = push_unnamed_declaration(context, type_to_cast_to.type, type_to_cast_to.defined_type, open_curly);
                    
                    struct ir_compound_literal *compound_literal = push_struct(&context->ir_arena, struct ir_compound_literal);
                    compound_literal->base.kind = IR_compound_literal;
                    compound_literal->decl = compound_literal_declaration;
                    compound_literal->decl->assign_expr = &compound_literal->base;
                    
                    parse_initializer_list(context, compound_literal_declaration->type, 0, &compound_literal->trailing_array_size);
                    
                    if(context->should_exit_statement) return invalid_ast(context);
                    
                    compound_literal->initializer_size = arena_current(&context->ir_arena) - (u8 *)(compound_literal + 1);
                    
                    if(type_is_array_of_unknown_size(type_to_cast_to.type)){
                        struct ast_array_type *array_of_unknown_size = (struct ast_array_type *)type_to_cast_to.type;
                        smm array_length = 0;
                        if(compound_literal->trailing_array_size){
                            array_length = compound_literal->trailing_array_size/array_of_unknown_size->element_type->size;
                        }
                        
                        // 
                        // Create a new array type, with the newly discovered bounds.
                        struct ast_array_type *array_type = parser_type_push(context, array_type); // @cleanup: Better token.
                        array_type->element_type              = array_of_unknown_size->element_type;
                        array_type->element_type_defined_type = array_of_unknown_size->element_type_defined_type;
                        
                        patch_array_size(context, array_type, array_length, open_curly);
                        
                        // Set the new type.
                        compound_literal_declaration->type = &array_type->base;
                        
                        // Reset the trailing array size, to not overallocate later on.
                        compound_literal->trailing_array_size = 0;
                    }
                    
                    if(type_to_cast_to.type->flags & TYPE_FLAG_ends_in_array_of_unknown_size){
                        // :compounds_with_trailing_array
                        parser_emit_memory_location(context, compound_literal_declaration);
                    }
                    
                    operand.ir = &compound_literal->base;
                    operand.token = open_curly;
                    operand.resolved_type = compound_literal_declaration->type;
                    operand.defined_type = compound_literal_declaration->defined_type;
                    
                    // struct literals should be assignable? msvc does that.
                    context->in_lhs_expression = true;
                    goto skip_primary_expression_because_we_got_a_struct_literal;
                }
                
                // 
                // @note: 'sizeof (struct a){0}.member' should be interpreted as 'sizeof((struct a){0}.member)'.
                // 
                
                if(ast_stack_current(context) == AST_alignof || ast_stack_current(context) == AST_sizeof){
                    struct ast_stack_entry *size_or_alignof = ast_stack_pop(context);
                    b32 is_alignof = (size_or_alignof->ast_kind == AST_alignof);
                    struct ast_type *type = type_to_cast_to.type;
                    
                    if(type == &globals.typedef_void){
                        report_error(context, size_or_alignof->token, "Type void has undefined %s.", is_alignof ? "alignment" : "size");
                        return invalid_ast(context);
                    }
                    
                    if(type->kind == AST_function_type){
                        report_error(context, size_or_alignof->token, "Function type has undefined %s.", is_alignof ? "alignment" : "size");
                        return invalid_ast(context);
                    }
                    
                    if(!is_alignof && type_is_array_of_unknown_size(type)){
                        report_error(context, size_or_alignof->token, "Array of unknown size has undefined size.");
                    }
                    
                    // 
                    // :sizeof/alignof threading bug
                    // 
                    // Copied from todo this is the reason why these now happen here instead of the alignof/sizeof cases.
                    // 
                    // threading bug at sizeof(type):
                    // 
                    // thread one parses the sizeof. sees type, its not yet known, so it goes back one token and we start parsing a 
                    // prefix expression at '(type)'. now thread two finishes parsing 'type' and registers it.
                    // thread one now parses '(type)' as a cast.
                    
                    
                    u64 value = is_alignof ? type->alignment : type->size;
                    
                    // "The [..] type (an unsigned integer type) is 'size_t', defined in <stddef.h>."
                    operand = ast_push_u64_literal(context, value, size_or_alignof->token);
                    
                    context->in_lhs_expression = false;
                    
                    goto skip_primary_expression_because_we_got_a_sizeof_or_align_of_with_a_type;
                }
                
                if(type_to_cast_to.type->kind == AST_array_type){
                    report_error(context, open_paren, "Cast to array is illegal.");
                    return invalid_ast(context);
                }
                
                if(type_to_cast_to.type->kind == AST_function_type){
                    report_error(context, open_paren, "Cast to function is illegal.");
                    return invalid_ast(context);
                }
                
                // @note: The defined type being the operand is sort of weird.
                struct ast_stack_entry *ast_stack_entry = ast_stack_push(context, AST_cast, open_paren, &(struct expr){(struct ir *)type_to_cast_to.defined_type});
                ast_stack_entry->other = type_to_cast_to.type;
                goto restart;
            }else{
                
                // 
                // Parenthesized expression.
                // 
                
                struct expr expr = parse_expression(context, false);
                expect_token(context, TOKEN_closed_paren, "Expected a ')' ending parenthesized expression.");
                operand = expr;
                
                if(context->should_exit_statement) return expr;
                
                // context->in_lhs_expression is set the right way, coming out of the recursive call.
            }
        }break;
        
        // 
        // :primary_expression parse_primary_expression
        // 
        
        case TOKEN_character_literal:{
            struct token *lit_token = next_token(context);
            struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
            lit->base.kind = IR_integer_literal;
            
            struct escaped_string escaped = handle_escaping(context, lit_token);
            assert(escaped.string_kind != STRING_KIND_invalid); // this should not be possible as the tokenizer only allows certain sequences.
            
            if(escaped.string.size == 0){
                report_error(context, lit_token, "Empty character constants are not allowed.");
                return invalid_ast(context);
            }
            
            // Types seem to be defined, but maybe we violate that (?)
            //     "An integer character constant has type int."
            //     "A wide character constant prefixed by the letter L has type wchar_t, [...],"
            //     "a wide character constant prefixed by the letter u or U has type char16_t or char32_t, respectively [...]."
            
            // For unicode characters or more than one letters, the value is implementation defined:
            //     "The value of an integer character constant containing more than one character (e.g.,'ab'), 
            //      or containing a character or escape sequence that does not map to a single-byte
            //      execution character, is implementation-defined."
            //     "The value of a wide character constant containing more than one multibyte character 
            //      or a single multibyte character that maps to multiple members of the extended execution character set
            //      [...] is implementation-defined."
            
            // We have to sign extend character values:
            //     "If an integer character constant contains a single character or escape sequence, 
            //      its value is the one that results when an object with type char whose value is that 
            //      of the single character or escape sequence is converted to type int."
            struct ast_type *type    = null;
            enum ast_kind *defined_type = null;
            
            u64 value = 0;
            switch(escaped.string_kind){
                case STRING_KIND_utf8:{ // u8'', ''
                    
                    // 
                    // The promotion rules choosen by all compilers are really weird:
                    // 
                    //     '\xff' == -1
                    //     '\xff\xff' == 65535
                    //     '\xff\xff\xff\xff' == -1
                    //     
                    // GCC and clang also truncate '\xff\xff\xff\xff\xff\xff\xff\xff\xff' to an (int)-1.
                    // 
                    
                    if(escaped.string.size == 1){
                        type = &globals.typedef_s32;
                        defined_type = &globals.typedef_s8.kind;
                        value = *(s8 *)escaped.string.data;
                    }else if(escaped.string.size == 2){
                        type = &globals.typedef_s32;
                        defined_type = &globals.typedef_s16.kind;
                        value = *(u16 *)escaped.string.data;
                    }else if(escaped.string.size <= 4){
                        type = &globals.typedef_s32;
                        value = *(s32 *)escaped.string.data;
                    }else if(escaped.string.size <= 8){
                        // @note: this is an extension as we are supposed to give it type int.
                        report_warning(context, WARNING_extension_used, lit_token, "Character constant exceeds the size of 'int' using '__int64'.");
                        type = &globals.typedef_s64;
                        value = *(s64 *)escaped.string.data;
                    }else{
                        report_error(context, lit_token, "Character constant exceeds the size of '__int64'.");
                        return invalid_ast(context);
                    }
                }break;
                case STRING_KIND_utf16:{ // u16'', L'', u''
                    
                    if(escaped.string.size > 2){
                        // @cleanup: for u this should say char16_t
                        report_error(context, lit_token, "Wide character constant exceeds the size of 'wchar_t'.");
                        return invalid_ast(context);
                    }
                    
                    // @note: These seem to be of type u16.
                    //        All compilers agree that sizeof('\xffff') = 2 and (L/u)'\xffff' == 65535.
                    
                    type = &globals.typedef_u16;
                    value = *(u16 *)escaped.string.data;
                }break;
                case STRING_KIND_utf32:{ // u32'', U''
                    if(escaped.string.size > 4){
                        report_error(context, lit_token, "Wide character constant exceeds the size of 'char32_t'.");
                        return invalid_ast(context);
                    }
                    
                    // @note: Not surprisingly, this one is unsigned.
                    type = &globals.typedef_u32;
                    value = *(u32 *)escaped.string.data;
                }break;
                invalid_default_case();
            }
            
            lit->_u64 = value;
            lit->type = type;
            
            operand.ir = &lit->base;
            operand.token = lit_token;
            operand.resolved_type = type;
            operand.defined_type = defined_type;
            
            context->in_lhs_expression = false;
        }break;
        
#define explicit_number_kind_case(type, postfix)\
case NUMBER_KIND_##type:{ \
    if(type##_min > (smm)val || type##_max < (smm)val){ \
        report_warning(context, WARNING_compile_time_truncation, lit_token, "Literal is postfixed '" #postfix "', but does not fit into a '%s'.", #type); \
    } \
    lit->_##type = (type)val; \
    operand.resolved_type = &globals.typedef_##type;\
}break
        
        
        case TOKEN_base10_literal:{
            struct token *lit_token = next_token(context);
            
            if(lit_token->size == 1 && lit_token->data[0] == '0'){
                // @note: Fast path for 0.
                operand = ast_push_s32_literal(context, 0, lit_token);
                context->in_lhs_expression = false;
                break;
            }
            
            struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
            lit->base.kind = IR_integer_literal;
            
            struct parsed_integer parsed_integer = parse_base10_literal(context, lit_token);
            u64 val = parsed_integer.value;
            
            // "The type of an integer constant is the first of the corresponding list
            //  in which its value can be represented."
            
            
            operand.ir = &lit->base;
            operand.token = lit_token;
            
            switch(parsed_integer.number_kind){
                case NUMBER_KIND_invalid:{
                    assert(context->should_exit_statement);
                    lit->type = &globals.typedef_s32;
                    return (struct expr){.ir = &lit->base, lit_token, &globals.typedef_s32};
                }break;
                
                explicit_number_kind_case(s8, i8);
                explicit_number_kind_case(s16, i16);
                explicit_number_kind_case(s32, i32);
                explicit_number_kind_case(s64, i64);
                explicit_number_kind_case(u8, ui8);
                explicit_number_kind_case(u16, ui16);
                explicit_number_kind_case(u32, ui32);
                explicit_number_kind_case(u64, ui64);
                
                case NUMBER_KIND_long: // @cleanup: if we do long vs int think about this
                case NUMBER_KIND_int:{
                    if(val <= max_s32){
                        enum ast_kind *defined_type = 0;
                        
                        if(val <= max_u8) defined_type = &globals.typedef_u8.kind;
                        else if(val <= max_u16) defined_type = &globals.typedef_u16.kind;
                        
                        lit->_s32 = (s32)val;
                        operand.resolved_type = &globals.typedef_s32;
                        operand.defined_type  = defined_type;
                        break;
                    }
                } // fallthrough
                case NUMBER_KIND_long_long:{
                    if(val <= max_s64){
                        lit->_s64 = (s64)val;
                        operand.resolved_type = &globals.typedef_s64;
                        operand.defined_type  = null;
                        break;
                    }
                    lit->_u64 = (u64)val;
                    operand.resolved_type = &globals.typedef_u64;
                    operand.defined_type  = null;
                    report_warning(context, WARNING_integer_literal_too_large_to_be_signed, lit_token, "Integer literal exceeds the maximum value representable as a signed integer and is interpreted as unsigned.");
                }break;
                case NUMBER_KIND_unsigned_long: // @cleanup: long vs int
                case NUMBER_KIND_unsigned:{
                    if(val <= max_u32){
                        lit->_u32 = (u32)val;
                        operand.resolved_type = &globals.typedef_u32;
                        operand.defined_type  = null;
                    }else{
                        lit->_u64 = (u64)val;
                        operand.resolved_type = &globals.typedef_u64;
                        operand.defined_type  = null;
                    }
                } break;
                case NUMBER_KIND_unsigned_long_long:{
                    lit->_u64 = val;
                    operand.resolved_type = &globals.typedef_u64;
                    operand.defined_type  = null;
                }break;
                invalid_default_case();
            }
            
            lit->type = operand.resolved_type;
            context->in_lhs_expression = false;
        }break;
        
        case TOKEN_binary_literal:
        case TOKEN_hex_literal:{
            struct token *lit_token = next_token(context);
            struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
            lit->base.kind = IR_integer_literal;
            
            struct parsed_integer parsed_integer;
            if(lit_token->type == TOKEN_hex_literal){
                parsed_integer = parse_hex_literal(context, lit_token);
            }else{
                parsed_integer = parse_binary_literal(context, lit_token);
            }
            
            u64 val = parsed_integer.value;
            
            operand.ir = &lit->base;
            operand.token = lit_token;
            
            switch(parsed_integer.number_kind){
                case NUMBER_KIND_invalid:{
                    assert(context->should_exit_statement);
                    lit->type = &globals.typedef_s32;
                    return (struct expr){ .ir = &lit->base, lit_token, &globals.typedef_s32};
                }break;
                
                explicit_number_kind_case(s8, i8);
                explicit_number_kind_case(s16, i16);
                explicit_number_kind_case(s32, i32);
                explicit_number_kind_case(s64, i64);
                explicit_number_kind_case(u8, ui8);
                explicit_number_kind_case(u16, ui16);
                explicit_number_kind_case(u32, ui32);
                explicit_number_kind_case(u64, ui64);
                
                case NUMBER_KIND_s128: case NUMBER_KIND_u128:{
                    report_error(context, lit_token, "128-bit literals are not supported.");
                    return (struct expr){.ir = &lit->base, lit_token, &globals.typedef_s32};
                }
                
#undef explicit_number_kind_case
                
                case NUMBER_KIND_long:
                case NUMBER_KIND_int:{
                    
                    if(val <= max_s32){
                        
                        enum ast_kind *defined_type = 0;
                        if(val <= max_u8) defined_type = &globals.typedef_u8.kind;
                        else if(val <= max_u16) defined_type = &globals.typedef_u16.kind;
                        
                        lit->_s32 = (s32)val;
                        operand.resolved_type = &globals.typedef_s32;
                        operand.defined_type  = defined_type;
                    }else if(val <= max_u32){
                        lit->_u32 = (u32)val;
                        operand.resolved_type = &globals.typedef_u32;
                        operand.defined_type  = null;
                    }else if(val <= max_s64){
                        lit->_s64 = (s64)val;
                        operand.resolved_type = &globals.typedef_s64;
                        operand.defined_type  = null;
                    }else{
                        lit->_u64 = (u64)val;
                        operand.resolved_type = &globals.typedef_u64;
                        operand.defined_type  = null;
                    }
                }break;
                case NUMBER_KIND_long_long:{
                    if(val <= max_s64){
                        lit->_s64 = (s64)val;
                        operand.resolved_type = &globals.typedef_s64;
                        operand.defined_type  = null;
                    }else{
                        lit->_u64 = (u64)val;
                        operand.resolved_type = &globals.typedef_u64;
                        operand.defined_type  = null;
                    }
                }break;
                case NUMBER_KIND_unsigned_long:
                case NUMBER_KIND_unsigned:{
                    if(val <= max_u32){
                        lit->_u32 = (u32)val;
                        operand.resolved_type = &globals.typedef_u32;
                        operand.defined_type  = null;
                    }else{
                        lit->_u64 = val;
                        operand.resolved_type = &globals.typedef_u64;
                        operand.defined_type  = null;
                    }
                }break;
                case NUMBER_KIND_unsigned_long_long:{
                    lit->_u64 = val;
                    operand.resolved_type = &globals.typedef_u64;
                    operand.defined_type  = null;
                }break;
                invalid_default_case();
            }
            
            lit->type = operand.resolved_type;
            context->in_lhs_expression = false;
        }break;
        
        case TOKEN_string_literal:{
            struct token *string_literal_token = next_token(context);
            struct ir_string_literal *lit = push_struct(&context->ir_arena, struct ir_string_literal);
            lit->base.kind = IR_string_literal;
            
            struct token_and_string *string_base = 0, one_string_storage;
            smm total_string_size = 0;
            smm amount_of_strings = 0;
            enum string_kind composed_string_kind = STRING_KIND_invalid;
            
            if(!peek_token(context, TOKEN_string_literal)){
                
                // 
                // Only a single string literal.
                // 
                
                struct string string = token_get_string(string_literal_token);
                struct string prefix = eat_until_char(&string, '"', /*eat_delimiter*/false);
                string = strip_quotes(string);
                
                composed_string_kind    = string_prefix_to_kind(prefix);
                one_string_storage.string = string;
                one_string_storage.token  = string_literal_token;
                string_base = &one_string_storage;
                amount_of_strings = 1;
                total_string_size = string.size;
            }else{
                // 
                // We have to concatinate adjacent string literals.
                // First we figure out the 'string_kind' and all the strings.
                // 
                
                string_base = push_uninitialized_data(&context->scratch, struct token_and_string, 0);
                
                for(struct token *string_literal = string_literal_token; string_literal; string_literal = peek_token_eat(context, TOKEN_string_literal)){
                    
                    // 
                    // Each string literal is of the form:
                    //    <prefix>"string"
                    // e.g: L"asd", U"asd", "asd", u8"asd", ...
                    // 
                    struct string string = token_get_string(string_literal);
                    struct string prefix = eat_until_char(&string, '"', /*eat_delimiter*/false);
                    
                    string = strip_quotes(string);
                    
                    enum string_kind string_kind = string_prefix_to_kind(prefix);
                    
                    // 
                    // The c-spec demands that we use the largest string size. I.e.:
                    //    L"asd" "asd" == L"asdasd"
                    // 
                    if(string_kind > composed_string_kind) composed_string_kind = string_kind;
                    
                    struct token_and_string *ref = push_uninitialized_struct(&context->scratch, struct token_and_string);
                    ref->string = string;
                    ref->token  = string_literal;
                    
                    total_string_size += string.size;
                }
                
                amount_of_strings = push_uninitialized_data(&context->scratch, struct token_and_string, 0) - string_base;
            }
            
            struct string string = escape_and_convert_string_array(context, string_base, amount_of_strings, total_string_size, composed_string_kind);
            
            struct ast_type *element_type = null;
            switch(composed_string_kind){
                case STRING_KIND_utf8:  element_type = &globals.typedef_s8;  break;
                case STRING_KIND_utf16: element_type = &globals.typedef_u16; break;
                case STRING_KIND_utf32: element_type = &globals.typedef_u32; break;
                invalid_default_case();
            }
            
            lit->value = string;
            lit->string_kind = composed_string_kind;
            
            struct ast_array_type *type = parser_type_push(context, array_type);
            type->amount_of_elements = (string.size/element_type->size + 1); // plus one for the zero_terminator
            type->element_type = element_type;
            type->base.size = type->amount_of_elements * element_type->size;
            type->base.alignment = element_type->alignment;
            
            lit->type = &type->base;
            
            operand.ir = &lit->base;
            operand.token = string_literal_token;
            operand.resolved_type = &type->base;
            operand.defined_type = null;
            
            if(context->should_exit_statement) return operand;
            
            // :strings_are_lhs_expressions
            // string literals are actually lhs expression (you can take the address of them),
            // but you cannot assign to them, as they are arrays.
            context->in_lhs_expression = true;
        }break;
        
        case TOKEN_float_hex_literal:
        case TOKEN_float_literal:{
            struct token *float_token = next_token(context);
            struct ir_float_literal *f = push_struct(&context->ir_arena, struct ir_float_literal);
            f->base.kind = IR_float_literal;
            
            struct string string = token_get_string(float_token);
            assert(string.size);
            
            smm suffix_begin = 0;
            for(smm i = string.size - 1; i > 0; i--){
                if(u8_is_number(string.data[i]) || string.data[i] == '.'){
                    suffix_begin = i + 1;
                    break;
                }
            }
            
            struct string suffix = create_string(string.data + suffix_begin, string.size - suffix_begin);
            
            b32 is_32_bit = false;
            if(suffix.size == 0){
                // no suffix is fine -> double
            }else if(string_match_case_insensitive(suffix, string("f"))){
                is_32_bit = true;
            }else if(string_match_case_insensitive(suffix, string("l"))){
                // nothing (long double not supported / is double)
            }else{
                report_error(context, float_token, "Invalid suffix '%.*s' on float literal.", suffix.size, suffix.data);
            }
            
            struct string value = create_string(string.data, string.size - suffix.size);
            char *cvalue = push_cstring_from_string(&context->scratch, value);
            char *endptr;
            double val = strtod(cvalue, &endptr);
            
            // @cleanup: handle strtod errors
            
            if(endptr != cvalue + value.size){
                report_error(context, float_token, "Invalid suffix '%.*s' on float literal.", suffix.size, suffix.data);
            }
            
            struct ast_type *type = is_32_bit ? &globals.typedef_f32 : &globals.typedef_f64;
            
            f->type = type;
            f->value = val;
            
            operand.ir = &f->base;
            operand.token = float_token;
            operand.resolved_type = type;
            operand.defined_type = null;
            
            context->in_lhs_expression = false;
        }break;
        
        case TOKEN___func__:{
            // :__FUNCTION__is_resolved_during_chunking
            // 
            // If we were inside of a function, we would have fixed up this '__FUNCTION__' while 
            // chunking the file.
            // 
            
            struct token *token = next_token(context);
            
            assert(!context->current_function);
            
            report_error(context, token, "%.*s is only allowed inside a function.", token->size, token->data);
            return invalid_ast(context);
        }break;
        case TOKEN_identifier:{
            struct token *token = next_token(context);
            
            struct ast_declaration *lookup = lookup_declaration(context->current_scope, context->current_compilation_unit, token->atom);
            
            if(!lookup){
                if(globals.compile_stage < COMPILE_STAGE_parse_function){
                    parser_sleep(context, token, SLEEP_on_decl);
                }else{
                    report_error(context, token, "Undeclared identifier.");
                }
                return invalid_ast(context);
            }else if(lookup->kind != IR_declaration && lookup->kind != IR_function){
                assert(lookup->kind == IR_typedef);
                
                if(globals.compile_stage == COMPILE_STAGE_parse_global_scope_entries){
                    //
                    // welcome to the worst hack in the compiler:
                    // 
                    
                    // :sizeof/alignof threading bug
                    // 
                    // We might have parsed 'sizeof(type)' as not being a sizeof of a type one level up.
                    // Then another thread finished 'type' and registered it globally.
                    // We, now passing the nested expression for '(type)', now find a typedef and would error.
                    // so instead we detect exactly this situation one level up and set the member 
                    // 'maybe_in_cast' on 'context', which is the identifier.
                    // Sleeping on this identifier will wake us up immediately again (there is a protection against
                    // race conditions) and thus we will retry the same thing but this time the 'sizeof(type)'
                    // will be parsed correctly in the first case.
                    //                                                                               02.05.2021
                    
                    if(context->maybe_in_cast){
                        assert(context->maybe_in_cast == token);
                        parser_sleep(context, token, SLEEP_on_decl);   
                    }
                }
                
                // :Error
                report_error(context, token, "Unexpected type in expression.");
                return invalid_ast(context);
            }
            
            maybe_resolve_unresolved_type(&lookup->type);
            
            if(lookup->kind == IR_function || (lookup->kind == IR_declaration && (lookup->flags & DECLARATION_FLAGS_is_global))){
                struct ast_declaration *outer_declaration = context->current_function ? &context->current_function->as_decl : context->current_declaration;
                
                if(!outer_declaration){
                    // We are in something like this:
                    // 
                    //    int a;
                    //    struct{
                    //        int arr[a];
                    //        int arr[sizeof(a)];
                    //    };
                    //    
                    // Either, this is an error, or the reference is inside a sizeof and we don't need to add a dependency reference.
                }else{
                    struct declaration_reference_node *new_reference = push_struct(context->arena, struct declaration_reference_node);
                    new_reference->declaration = lookup;
                    new_reference->token = token;
                    
                    // :DeclarationReferenceThreadingBug
                    // 
                    // @cleanup: This should probably be atomic, as we might be parsing an initializer of a declaration
                    //           in two different threads at the same time.
                    //           This happens if there are two initializers in the code (a bug), because we `register_declaration` and then
                    //           reassign the declaration in `parse_declaration_list`.
                    //           In the future, there should probably be a guard in `parse_declaration_list` against parsing an initializer
                    //           twice at the same time.
                    //                                                                                            - 19.10.2024
                    sll_push_back(outer_declaration->referenced_declarations, new_reference);
                }
            }else{
                lookup->_times_referenced++;
            }
            
            if(lookup->flags & DECLARATION_FLAGS_is_enum_member){
                assert(lookup->assign_expr->kind == IR_integer_literal);
                // @sigh
                struct ir_integer_literal *source = (struct ir_integer_literal *)lookup->assign_expr;
                struct ir_integer_literal *lit = push_struct(&context->ir_arena, struct ir_integer_literal);
                lit->base.kind = IR_integer_literal;
                
                // @note: right now this only does enums
                lit->_s32 = source->_s32;
                lit->type = &globals.typedef_s32;
                
                operand.ir = &lit->base;
                operand.token = token;
                operand.resolved_type = &globals.typedef_s32;
                operand.defined_type = &lookup->type->kind;
                
                context->in_lhs_expression = false;
            }else{
                struct ir_identifier *ident = push_struct(&context->ir_arena, struct ir_identifier);
                ident->base.kind = IR_identifier;
                ident->decl = lookup;
                
                operand.ir = &ident->base;
                operand.token = token;
                operand.resolved_type = lookup->type;
                operand.defined_type = lookup->defined_type;
                
                context->in_lhs_expression = true;
            }
        }break;
        
        case TOKEN_generic:{
            // 
            // generic-selection:
            //      _Generic ( assignment-expression, ({type-name|default}: assignment-expression)* )
            // 
            struct token *generic = next_token(context);
            
            expect_token(context, TOKEN_open_paren, "Expected a '(' after '_Generic'.");
            
            u8 *ir_arena_start = arena_current(&context->ir_arena);
            
            struct expr choice_expression = parse_expression(context, /*skip_comma_expression*/1);
            maybe_load_address_for_array_or_function(context, IR_load_address, &choice_expression);
            maybe_insert_cast_from_special_int_to_int(context, &choice_expression, /*is_lhs*/0);
            
            struct ast_type *choice = choice_expression.resolved_type;
            
            context->ir_arena.current = ir_arena_start;
            
            struct expr default_expression = zero_struct;
            struct expr choosen_expression = zero_struct;
            
            while(in_current_token_array(context) && !peek_token_eat(context, TOKEN_closed_paren)){
                
                expect_token(context, TOKEN_comma, "Expected ')' or ',' while parsing a _Generic association list.");
                if(peek_token_eat(context, TOKEN_closed_paren)) break; // Allow trailing comma.
                
                if(peek_token_eat(context, TOKEN_default)){
                    expect_token(context, TOKEN_colon, "Expected ':' after 'default' while parsing a _Generic association list.");
                    
                    default_expression = parse_expression(context, /*skip_comma_expression*/1);
                }else{
                    struct type_info_return type_name = maybe_parse_type_for_cast_or_sizeof(context);
                    if(!type_name.type){
                        report_error(context, get_current_token_for_error_report(context), "Expected a type-name or 'default' while parsing a _Generic association list.");
                        type_name.type = &globals.typedef_s32;
                        skip_until_tokens_are_balanced(context, get_current_token(context), TOKEN_open_paren, TOKEN_closed_paren, "This error is never reported as we have an 'report_error' just before it.");
                    }
                    
                    expect_token(context, TOKEN_colon, "Expected ':' after the type-name while parsing a _Generic association list.");
                    
                    struct expr expression = parse_expression(context, /*skip_comma_expression*/1);
                    
                    if(types_are_equal(type_name.type, choice)){
                        choosen_expression = expression;
                    }else{
                        memset(ir_arena_start, 0, context->ir_arena.current - ir_arena_start);
                        context->ir_arena.current = ir_arena_start;
                    }
                }
            }
            
            if(choosen_expression.ir){
                operand = choosen_expression;
            }else if(default_expression.ir){
                operand = default_expression;
            }else{
                // :Error
                report_error(context, generic, "_Generic choice did not match any of the associations.");
                return invalid_ast(context);
            }
        }break;
        
        default:{
            report_syntax_error(context, get_current_token_for_error_report(context), "Unexpected token in expression.");
            return invalid_ast(context);
        }break;
    }
    
    context->maybe_in_cast = null;
    
    skip_primary_expression_because_we_got_a_sizeof_or_align_of_with_a_type:;
    skip_primary_expression_because_we_got_a_struct_literal:;
    
    // :postfix_expression parse_postfix_expression
    // 
    // Precedence 1: postfix expressions, Left-to-Right Associative
    // 
    //    operand++  operand--  operand() operand[]
    //    operand.   operand->
    //    
    b32 do_continue = true;
    while(do_continue && !context->should_exit_statement){
        struct token *test = next_token(context);
        switch(test->type){
            case TOKEN_increment:{
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_basic, test)) return operand;
                if(check_types_for_increment_or_decrement(context, operand.resolved_type, test, "++")) return operand;
                
                // 
                // @cleanup: Atomic integer types.
                // 
                
                if(operand.resolved_type->kind == AST_pointer_type){
                    struct ir_pointer_increment *increment = push_struct(&context->ir_arena, struct ir_pointer_increment);
                    increment->base.kind = IR_postinc_pointer;
                    increment->pointer_type = (struct ast_pointer_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else if(operand.resolved_type->kind == AST_bitfield_type){
                    struct ir_bitfield_increment *increment = push_struct(&context->ir_arena, struct ir_bitfield_increment);
                    increment->base.kind = IR_postinc_bitfield;
                    increment->bitfield_type = (struct ast_bitfield_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else{
                    enum ir_type ir_type = ir_type_from_type(operand.resolved_type);
                    
                    static u8 ir_type_to_inc_kind[] = {
                        [IR_TYPE_bool] = IR_postinc_bool,
                        
                        [IR_TYPE_s8] = IR_postinc_u8,
                        [IR_TYPE_u8] = IR_postinc_u8,
                        
                        [IR_TYPE_s16] = IR_postinc_u16,
                        [IR_TYPE_u16] = IR_postinc_u16,
                        
                        [IR_TYPE_s32] = IR_postinc_u32,
                        [IR_TYPE_u32] = IR_postinc_u32,
                        
                        [IR_TYPE_s64] = IR_postinc_u64,
                        [IR_TYPE_u64] = IR_postinc_u64,
                        
                        [IR_TYPE_f32] = IR_postinc_f32,
                        [IR_TYPE_f64] = IR_postinc_f64,
                        
                        // @incomplete: There should probably be atomic versions of these.
                        [IR_TYPE_atomic_bool] = IR_postinc_bool,
                        
                        [IR_TYPE_atomic_s8] = IR_postinc_u8,
                        [IR_TYPE_atomic_u8] = IR_postinc_u8,
                        
                        [IR_TYPE_atomic_s16] = IR_postinc_u16,
                        [IR_TYPE_atomic_u16] = IR_postinc_u16,
                        
                        [IR_TYPE_atomic_s32] = IR_postinc_u32,
                        [IR_TYPE_atomic_u32] = IR_postinc_u32,
                        
                        [IR_TYPE_atomic_s64] = IR_postinc_u64,
                        [IR_TYPE_atomic_u64] = IR_postinc_u64,
                    };
                    
                    operand.ir = push_ir(context, ir_type_to_inc_kind[ir_type]);
                }
                
                context->in_lhs_expression = false;
            }break;
            case TOKEN_decrement:{
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_basic, test)) return operand;
                if(check_types_for_increment_or_decrement(context, operand.resolved_type, test, "--")) return operand;
                
                if(operand.resolved_type->kind == AST_pointer_type){
                    struct ir_pointer_increment *increment = push_struct(&context->ir_arena, struct ir_pointer_increment);
                    increment->base.kind = IR_postdec_pointer;
                    increment->pointer_type = (struct ast_pointer_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else if(operand.resolved_type->kind == AST_bitfield_type){
                    struct ir_bitfield_increment *increment = push_struct(&context->ir_arena, struct ir_bitfield_increment);
                    increment->base.kind = IR_postdec_bitfield;
                    increment->bitfield_type = (struct ast_bitfield_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else{
                    enum ir_type ir_type = ir_type_from_type(operand.resolved_type);
                    
                    static u8 ir_type_to_inc_kind[] = {
                        [IR_TYPE_bool] = IR_postdec_bool,
                        
                        [IR_TYPE_s8] = IR_postdec_u8,
                        [IR_TYPE_u8] = IR_postdec_u8,
                        
                        [IR_TYPE_s16] = IR_postdec_u16,
                        [IR_TYPE_u16] = IR_postdec_u16,
                        
                        [IR_TYPE_s32] = IR_postdec_u32,
                        [IR_TYPE_u32] = IR_postdec_u32,
                        
                        [IR_TYPE_s64] = IR_postdec_u64,
                        [IR_TYPE_u64] = IR_postdec_u64,
                        
                        [IR_TYPE_f32] = IR_postdec_f32,
                        [IR_TYPE_f64] = IR_postdec_f64,
                        
                        // @incomplete: There should probably be atomic versions of these.
                        [IR_TYPE_atomic_bool] = IR_postdec_bool,
                        
                        [IR_TYPE_atomic_s8] = IR_postdec_u8,
                        [IR_TYPE_atomic_u8] = IR_postdec_u8,
                        
                        [IR_TYPE_atomic_s16] = IR_postdec_u16,
                        [IR_TYPE_atomic_u16] = IR_postdec_u16,
                        
                        [IR_TYPE_atomic_s32] = IR_postdec_u32,
                        [IR_TYPE_atomic_u32] = IR_postdec_u32,
                        
                        [IR_TYPE_atomic_s64] = IR_postdec_u64,
                        [IR_TYPE_atomic_u64] = IR_postdec_u64,
                    };
                    
                    operand.ir = push_ir(context, ir_type_to_inc_kind[ir_type]);
                }
                
                context->in_lhs_expression = false;
            }break;
            case TOKEN_open_index:{
                
                struct expr index = parse_expression(context, false);
                expect_token(context, TOKEN_closed_index, "Expected ']' at the end of array subscript.");
                if(context->should_exit_statement) return index;
                
                if(operand.resolved_type->kind == AST_integer_type){
                    //
                    // This is a @hack to work around some cursed stuff c is supporting.
                    // As 'a[b]' is technically just the same as '*(a + b)' expressions like
                    // '1[argv]' are legal and evaluate to the same as 'argv[1]'.
                    //                                                            -20.04.2023
                    // 
                    struct expr temp = operand;
                    operand = index;
                    index = temp;
                }
                
                if(!check_unary_for_basic_types(context, index.resolved_type, CHECK_integer, test)) return index;
                
                if(operand.resolved_type->kind == AST_struct){
                    not_implemented;
                }
                
                if(operand.resolved_type->kind != AST_pointer_type && operand.resolved_type->kind != AST_array_type){
                    report_error(context, test, "Left hand side of [] needs to be of pointer or array type.");
                    return index;
                }
                
                if(operand.resolved_type->kind == AST_pointer_type){
                    struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)operand.resolved_type;
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, test, "Cannot subscript a void-pointer");
                        return index;
                    }
                    
                    if(pointer->pointer_to->kind == AST_function_type){
                        report_error(context, test, "Cannot subscript a function-pointer.");
                        return index;
                    }
                    
                    // :unresolved_types
                    //
                    // If the type we are pointing to is unresolved, like
                    //     struct unresolved *pointer;
                    // We have to resolve it, or sleep on it/error on it.
                    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return index;
                }
                
                //
                // @cleanup: warning or error if the lhs is an array and the index is statically out of bounds?
                //
                
                push_nodes_for_subscript(context, &operand, &index, test);
                
                context->in_lhs_expression = true;
            }break;
            case TOKEN_dot:{
                if(operand.resolved_type->kind == AST_pointer_type){
                    // @note: For now I have decided that this is now an unchangable default :)
                    goto treat_dot_as_arrow_because_allow_dot_as_arrow_was_set_and_we_got_a_pointer_type_for_a_dot;
                }
                
                if(operand.resolved_type->kind == AST_unresolved_type){
                    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &operand.resolved_type)){
                        return operand;
                    }
                }
                
                if(operand.resolved_type->kind != AST_struct && operand.resolved_type->kind != AST_union){
                    report_error(context, test, "Left of '.' needs to be of struct or union type.");
                    return operand;
                }
                
                struct ast_compound_type *compound = cast(struct ast_compound_type *)operand.resolved_type;
                handle_dot_or_arrow(context, compound, &operand, IR_member);
                
                // 
                // @note: Dot does not alter the 'is_lhs_expression' flag.
                //        An expression like 'function().member' should not be a lhs expression.
                //        While usually, the lhs of a member expression will be a lhs-expression anyway.
                //        This prevents 'function2().value++' from compiling.
                //        
                // context->in_lhs_expression = true;
            }break;
            case TOKEN_arrow:{
                treat_dot_as_arrow_because_allow_dot_as_arrow_was_set_and_we_got_a_pointer_type_for_a_dot:;
                
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand); // Apperantly, you can use '->' on arrays.
                
                if(operand.resolved_type->kind == AST_unresolved_type){
                    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &operand.resolved_type)){
                        return operand;
                    }
                }
                
                struct ast_type *type = operand.resolved_type;
                
                if(type->kind != AST_pointer_type){
                    report_error(context, test, "Left of '->' needs to be of pointer type.");
                    return operand;
                }
                
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)type;
                
                // :unresolved_types
                //
                // If the type we are pointing to is unresolved, like
                //     struct unresolved *pointer;
                // We have to resolve it, or sleep on it/error on it.
                if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return operand;
                
                if(pointer->pointer_to->kind != AST_struct && pointer->pointer_to->kind != AST_union){
                    report_error(context, test, "Left of '->' needs to be of pointer to struct or union type.");
                    return operand;
                }
                
                struct ast_compound_type *compound = cast(struct ast_compound_type *)pointer->pointer_to;
                handle_dot_or_arrow(context, compound, &operand, IR_member_deref);
                
                context->in_lhs_expression = true;
            }break;
            case TOKEN_open_paren:{
                
                if(operand.ir->kind == IR_identifier) test = operand.token;
                
                struct token *call_token = test;
                
                struct ast_type *type = operand.resolved_type;
                if(type->kind == AST_pointer_type){
                    type = ((struct ast_pointer_type *)type)->pointer_to;
                }
                
                if(type->kind != AST_function_type){
                    report_error(context, call_token, "Calling a non-procedure.");
                    return operand;
                }
                
                struct ast_function_type *function_type = (struct ast_function_type *)type;
                
                // :unresolved_types
                // 
                // the return type and arguments can be unresolved. For example in a declaration like 
                //     struct unresolved_return unused_function(struct unresolved_argument argument);
                // the 'unresolved_return' and the 'unresolved_argument' are unresolved.
                // Here, we need to resolve them or sleep/error.
                //
                
                maybe_resolve_unresolved_type_or_sleep_or_error(context, &function_type->return_type);
                if(context->should_exit_statement) return operand;
                
                if(operand.ir->kind == IR_deref && operand.resolved_type->kind == AST_function_type){
                    // 
                    // Convert '(*function_pointer)()' to 'function_pointer()'.
                    // 
                    struct ir_deref *deref = (struct ir_deref *)operand.ir;
                    pop_from_ir_arena(context, deref);
                }
                
                // @note: For var_args functions the 'call_arguments_count' can differ from the 'function_type->argument_list.count'.
                smm call_arguments_count = 0;
                
                struct ast_list_node* function_argument_iterator = function_type->argument_list.first;
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    while(1){
                        struct expr expr = parse_expression(context, true);
                        if(context->should_exit_statement) return expr;
                        
                        if(function_argument_iterator){
                            
                            assert(*function_argument_iterator->value == IR_declaration);
                            struct ast_declaration *decl = cast(struct ast_declaration *)function_argument_iterator->value;
                            
                            if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) return expr;
                            
                            if((function_type->flags & FUNCTION_TYPE_FLAGS_is_printlike) && function_argument_iterator->next == null){
                                //
                                // For a printlike function we do the parameter parsing ourselves.
                                // This is because we want to be able to not have a 'format' argument. 
                                // In this case we will _infer_ a 'format' argument.
                                //
                                parse_call_to_printlike_function_arguments(context, function_type, &operand, &expr, &call_arguments_count);
                                call_arguments_count += 1;
                                break;
                            }
                            
                            // "the arguments are implicitly converted, as if by assignment"
                            maybe_load_address_for_array_or_function(context, IR_load_address, &expr);
                            maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, decl->type, decl->defined_type, &expr, expr.token);
                            
                            function_argument_iterator = function_argument_iterator->next;
                        }else{
                            if(!(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs)){
                                // :Error
                                report_error(context, call_token, "Too many arguments to function.");
                                return expr;
                            }else{
                                if(expr.resolved_type == &globals.typedef_void){
                                    report_error(context, expr.token, "Expression of type void cannot be used as function argument.");
                                }
                                
                                maybe_insert_implicit_nodes_for_varargs_argument(context, &expr, expr.token);
                            }
                        }
                        
                        call_arguments_count += 1;
                        
                        if(!peek_token_eat(context, TOKEN_comma)) break;
                        if(peek_token(context, TOKEN_closed_paren)) break; // Allow trailling ',' in function calls.
                    }
                    
                    expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of parameter list.");
                }
                
                if(function_type->argument_list.count > call_arguments_count){
                    // :Error
                    begin_error_report(context);
                    struct string type_string = push_type_string(&context->scratch, &context->scratch, &function_type->base);
                    report_error(context, call_token, "Too few arguments to function of type '%.*s'.", type_string.size, type_string.data);
                    
                    // @note: Careful here, the we pop the operand when we are converting `(*function_pointer)()` to `function_pointer()`.
                    //        Therefore, we also have to check that the token is an identifier.
                    if(call_token->type == TOKEN_identifier && operand.ir->kind == IR_identifier){
                        struct ir_identifier *ident = (struct ir_identifier *)operand.ir;
                        report_error(context, ident->decl->identifier, "... Here is the declaration of the function.");
                    }
                    end_error_report(context);
                    return operand;
                }
                
                if((function_type->flags & FUNCTION_TYPE_FLAGS_is_noreturn) && !context->in_conditional_expression){
                    context->current_statement_returns_a_value = 1;
                }
                
                struct ir_function_call *call = push_struct(&context->ir_arena, struct ir_function_call);
                call->base.kind = IR_function_call;
                call->function_type = function_type;
                call->call_arguments_count = call_arguments_count;
                
                operand.ir = &call->base;
                operand.resolved_type = function_type->return_type;
                operand.defined_type = function_type->return_type_defined_type;
                
                context->in_lhs_expression = false;
            }break;
            
            default:{
                prev_token(context);
                do_continue = false;
            }break;
        }
        
        if(do_continue) operand.token = test;
    }
    
    static enum precedence{
        PRECEDENCE_prefix         = 2,
        PRECEDENCE_multiplicative = 3,
        PRECEDENCE_additive       = 4,
        PRECEDENCE_shift          = 5,
        PRECEDENCE_relational     = 6,
        PRECEDENCE_equality       = 7,
        PRECEDENCE_bitwise_and    = 8,
        PRECEDENCE_bitwise_xor    = 9,
        PRECEDENCE_bitwise_or     = 10,
        PRECEDENCE_logical_and    = 11,
        PRECEDENCE_logical_or     = 12,
        PRECEDENCE_ternary        = 13,
        PRECEDENCE_assignment     = 14,
        PRECEDENCE_comma          = 15,
    } token_to_precedence[TOKEN_count] = {
        [TOKEN_times] = PRECEDENCE_multiplicative, 
        [TOKEN_slash] = PRECEDENCE_multiplicative, 
        [TOKEN_mod]   = PRECEDENCE_multiplicative,
        
        [TOKEN_plus]  = PRECEDENCE_additive, 
        [TOKEN_minus] = PRECEDENCE_additive,
        
        [TOKEN_left_shift]  = PRECEDENCE_shift, 
        [TOKEN_right_shift] = PRECEDENCE_shift,
        
        [TOKEN_bigger_equals]  = PRECEDENCE_relational,
        [TOKEN_smaller_equals] = PRECEDENCE_relational,
        [TOKEN_bigger]         = PRECEDENCE_relational,
        [TOKEN_smaller]        = PRECEDENCE_relational,
        
        [TOKEN_logical_equals]   = PRECEDENCE_equality,
        [TOKEN_logical_unequals] = PRECEDENCE_equality,
        
        [TOKEN_and] = PRECEDENCE_bitwise_and,
        [TOKEN_xor] = PRECEDENCE_bitwise_xor,
        [TOKEN_or]  = PRECEDENCE_bitwise_or,
        [TOKEN_logical_and] = PRECEDENCE_logical_and,
        [TOKEN_logical_or]  = PRECEDENCE_logical_or,
        
        [TOKEN_question_mark] = PRECEDENCE_ternary,
        
        [TOKEN_equals]             = PRECEDENCE_assignment,
        [TOKEN_and_equals]         = PRECEDENCE_assignment,
        [TOKEN_or_equals]          = PRECEDENCE_assignment,
        [TOKEN_xor_equals]         = PRECEDENCE_assignment,
        [TOKEN_plus_equals]        = PRECEDENCE_assignment,
        [TOKEN_minus_equals]       = PRECEDENCE_assignment, 
        [TOKEN_left_shift_equals]  = PRECEDENCE_assignment,
        [TOKEN_right_shift_equals] = PRECEDENCE_assignment,
        [TOKEN_div_equals]         = PRECEDENCE_assignment,
        [TOKEN_mod_equals]         = PRECEDENCE_assignment,
        [TOKEN_times_equals]       = PRECEDENCE_assignment,
        
        [TOKEN_comma] = PRECEDENCE_comma,
    };
    
    struct token *binary_expression = next_token(context);
    
    while(context->ast_stack_at > 0){
        static enum precedence ast_to_precedence[AST_count] = {
            [AST_unary_predec]      = PRECEDENCE_prefix,
            [AST_unary_preinc]      = PRECEDENCE_prefix,
            [AST_sizeof]            = PRECEDENCE_prefix,
            [AST_alignof]           = PRECEDENCE_prefix,
            [AST_unary_bitwise_not] = PRECEDENCE_prefix,
            [AST_unary_logical_not] = PRECEDENCE_prefix,
            [AST_unary_plus]        = PRECEDENCE_prefix,
            [AST_unary_minus]       = PRECEDENCE_prefix,
            [AST_unary_deref]       = PRECEDENCE_prefix,
            [AST_unary_address]     = PRECEDENCE_prefix,
            [AST_cast]              = PRECEDENCE_prefix,
            
            [AST_binary_times]  = PRECEDENCE_multiplicative,
            [AST_binary_divide] = PRECEDENCE_multiplicative,
            [AST_binary_mod]    = PRECEDENCE_multiplicative,
            
            [AST_binary_plus]  = PRECEDENCE_additive,
            [AST_binary_minus] = PRECEDENCE_additive,
            
            [AST_binary_left_shift]  = PRECEDENCE_shift,
            [AST_binary_right_shift] = PRECEDENCE_shift,
            
            [AST_binary_bigger_equals]  = PRECEDENCE_relational,
            [AST_binary_smaller_equals] = PRECEDENCE_relational,
            [AST_binary_bigger]         = PRECEDENCE_relational,
            [AST_binary_smaller]        = PRECEDENCE_relational,
            
            [AST_binary_logical_equals]   = PRECEDENCE_equality,
            [AST_binary_logical_unequals] = PRECEDENCE_equality,
            
            [AST_binary_and] = PRECEDENCE_bitwise_and,
            [AST_binary_xor] = PRECEDENCE_bitwise_xor,
            [AST_binary_or]  = PRECEDENCE_bitwise_or,
            
            [AST_logical_and] = PRECEDENCE_logical_and,
            [AST_logical_or]  = PRECEDENCE_logical_or,
            
            [AST_conditional_expression] = PRECEDENCE_ternary, // right-to-left associative
            [AST_conditional_expression_true] = PRECEDENCE_ternary, // right-to-left associative
            [AST_conditional_expression_false] = PRECEDENCE_ternary, // right-to-left associative
            
            [AST_assignment]             = PRECEDENCE_assignment, // right-to-left associative
            [AST_and_assignment]         = PRECEDENCE_assignment,
            [AST_or_assignment]          = PRECEDENCE_assignment,
            [AST_xor_assignment]         = PRECEDENCE_assignment,
            [AST_plus_assignment]        = PRECEDENCE_assignment,
            [AST_minus_assignment]       = PRECEDENCE_assignment,
            [AST_left_shift_assignment]  = PRECEDENCE_assignment,
            [AST_right_shift_assignment] = PRECEDENCE_assignment,
            [AST_times_assignment]       = PRECEDENCE_assignment,
            [AST_divide_assignment]      = PRECEDENCE_assignment,
            [AST_modulo_assignment]      = PRECEDENCE_assignment,
            
            [AST_comma_expression] = PRECEDENCE_comma,
        };
        
        // 
        // @note: Whether we parse '1 op 3 op 2' as '(1 op 3) op 2' (left-associative) or as 
        //        '1 op (3 op 2)' (right-associative) depends on whether we check the top of the
        //        'ast_stack' first or we check the token first.
        // 
        
        enum ast_kind ast_kind = ast_stack_current(context);
        if(ast_kind == AST_invalid) break; // We are at the guard ast.
        
        enum precedence ast_precedence   = ast_to_precedence[ast_kind];
        enum precedence token_precedence = token_to_precedence[binary_expression->type];
        
        if(ast_precedence == token_precedence && (ast_precedence == PRECEDENCE_assignment || ast_precedence == PRECEDENCE_ternary)){
            // right-associative, on equality break.
            break;   
        }
        
        // If the next token has lower precedence, wait for that one to finish, before finishing this one.
        if(token_precedence && ast_precedence > token_precedence) break;
        
        struct ast_stack_entry *stack_entry = ast_stack_pop(context);
        
        switch(ast_kind){
            
            // :prefix_expression parse_prefix_expression
            // 
            // Precedence 2: prefix expressions, Right-to-Left Associative
            // 
            //    ++operand, --operand, +operand, -operand
            //    !operand, ~operand, (type)operand, *operand
            //    &operand, sizeof operand, _Alignof operand
            //    
            
            case AST_unary_preinc:{
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_basic, stack_entry->token)) return operand;
                if(check_types_for_increment_or_decrement(context, operand.resolved_type, stack_entry->token, "++")) return operand;
                
                if(operand.resolved_type->kind == AST_pointer_type){
                    struct ir_pointer_increment *increment = push_struct(&context->ir_arena, struct ir_pointer_increment);
                    increment->base.kind = IR_preinc_pointer;
                    increment->pointer_type = (struct ast_pointer_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else if(operand.resolved_type->kind == AST_bitfield_type){
                    struct ir_bitfield_increment *increment = push_struct(&context->ir_arena, struct ir_bitfield_increment);
                    increment->base.kind = IR_preinc_bitfield;
                    increment->bitfield_type = (struct ast_bitfield_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else{
                    enum ir_type ir_type = ir_type_from_type(operand.resolved_type);
                    
                    static u8 ir_type_to_inc_kind[] = {
                        [IR_TYPE_bool] = IR_preinc_bool,
                        
                        [IR_TYPE_s8] = IR_preinc_u8,
                        [IR_TYPE_u8] = IR_preinc_u8,
                        
                        [IR_TYPE_s16] = IR_preinc_u16,
                        [IR_TYPE_u16] = IR_preinc_u16,
                        
                        [IR_TYPE_s32] = IR_preinc_u32,
                        [IR_TYPE_u32] = IR_preinc_u32,
                        
                        [IR_TYPE_s64] = IR_preinc_u64,
                        [IR_TYPE_u64] = IR_preinc_u64,
                        
                        [IR_TYPE_f32] = IR_preinc_f32,
                        [IR_TYPE_f64] = IR_preinc_f64,
                        
                        // @incomplete: There should probably be atomic versions of these.
                        [IR_TYPE_atomic_bool] = IR_preinc_bool,
                        
                        [IR_TYPE_atomic_s8] = IR_preinc_u8,
                        [IR_TYPE_atomic_u8] = IR_preinc_u8,
                        
                        [IR_TYPE_atomic_s16] = IR_preinc_u16,
                        [IR_TYPE_atomic_u16] = IR_preinc_u16,
                        
                        [IR_TYPE_atomic_s32] = IR_preinc_u32,
                        [IR_TYPE_atomic_u32] = IR_preinc_u32,
                        
                        [IR_TYPE_atomic_s64] = IR_preinc_u64,
                        [IR_TYPE_atomic_u64] = IR_preinc_u64,
                    };
                    
                    operand.ir = push_ir(context, ir_type_to_inc_kind[ir_type]);
                }
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_unary_predec:{
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_basic, stack_entry->token)) return operand;
                if(check_types_for_increment_or_decrement(context, operand.resolved_type, stack_entry->token, "--")) return operand;
                
                if(operand.resolved_type->kind == AST_pointer_type){
                    struct ir_pointer_increment *increment = push_struct(&context->ir_arena, struct ir_pointer_increment);
                    increment->base.kind = IR_predec_pointer;
                    increment->pointer_type = (struct ast_pointer_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else if(operand.resolved_type->kind == AST_bitfield_type){
                    struct ir_bitfield_increment *increment = push_struct(&context->ir_arena, struct ir_bitfield_increment);
                    increment->base.kind = IR_predec_bitfield;
                    increment->bitfield_type = (struct ast_bitfield_type *)operand.resolved_type;
                    
                    operand.ir = &increment->base;
                }else{
                    enum ir_type ir_type = ir_type_from_type(operand.resolved_type);
                    
                    static u8 ir_type_to_inc_kind[] = {
                        [IR_TYPE_bool] = IR_predec_bool,
                        
                        [IR_TYPE_s8] = IR_predec_u8,
                        [IR_TYPE_u8] = IR_predec_u8,
                        
                        [IR_TYPE_s16] = IR_predec_u16,
                        [IR_TYPE_u16] = IR_predec_u16,
                        
                        [IR_TYPE_s32] = IR_predec_u32,
                        [IR_TYPE_u32] = IR_predec_u32,
                        
                        [IR_TYPE_s64] = IR_predec_u64,
                        [IR_TYPE_u64] = IR_predec_u64,
                        
                        [IR_TYPE_f32] = IR_predec_f32,
                        [IR_TYPE_f64] = IR_predec_f64,
                        
                        // @incomplete: There should probably be atomic versions of these.
                        [IR_TYPE_atomic_bool] = IR_predec_bool,
                        
                        [IR_TYPE_atomic_s8] = IR_predec_u8,
                        [IR_TYPE_atomic_u8] = IR_predec_u8,
                        
                        [IR_TYPE_atomic_s16] = IR_predec_u16,
                        [IR_TYPE_atomic_u16] = IR_predec_u16,
                        
                        [IR_TYPE_atomic_s32] = IR_predec_u32,
                        [IR_TYPE_atomic_u32] = IR_predec_u32,
                        
                        [IR_TYPE_atomic_s64] = IR_predec_u64,
                        [IR_TYPE_atomic_u64] = IR_predec_u64,
                    };
                    
                    operand.ir = push_ir(context, ir_type_to_inc_kind[ir_type]);
                }
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_sizeof:{
                
                if(operand.resolved_type->kind == AST_function_type){
                    report_error(context, stack_entry->token, "Expression of function type has undefined size.");
                    return operand;
                }
                
                if(operand.resolved_type == &globals.typedef_void){
                    report_error(context, stack_entry->token, "Expression of type void has undefined size.");
                    return operand;
                }
                
                if(type_is_array_of_unknown_size(operand.resolved_type)){
                    // @cleanup: This for now prevents us to emit the wrong code, without complaining,
                    //           but this isn't really correct.
                    //           If we have the following code:
                    //           
                    //               char array_of_unknown_size[];
                    //               char I_need_the_size[sizeof(array_of_unknown_size)];
                    //               char array_of_unknown_size[4];
                    //           
                    //           Then, because of order-independence, the only reasonable solution
                    //           is that 'sizeof(array_of_unknown_size)' should yield 4.
                    //           This would mean, we have to sleep on the array, and then wake up,
                    //           if the array size is resolved.
                    //           
                    // @cleanup: This might also just be a threading issue, if we have 
                    //               
                    //               char array_of_unknown_size[] = {1, 2, 3};
                    //               char I_need_the_size[sizeof(array_of_unknown_size)];
                    //               
                    //           The second array might parse _between_ the parsing of the declarator
                    //           and the parsing of the initializer, resulting in this error.
                    //           
                    // :Error
                    report_error(context, stack_entry->token, "'sizeof' cannot be applied to array of unknown size.");
                    return operand;
                }
                
                u64 size = operand.resolved_type->size;
                
                // @note: Delete the whole <expression> off of the `ir_arena`.
                context->ir_arena.current = stack_entry->other;
                
                // Sets in op->operand in the next iteration.
                operand = ast_push_u64_literal(context, size, operand.token);
                
                context->in_lhs_expression = false;
            }break;
            case AST_alignof:{
                
                if(operand.resolved_type->kind == AST_function_type){
                    report_error(context, stack_entry->token, "Expression of function type has undefined alignment.");
                    return operand;
                }
                
                if(operand.resolved_type == &globals.typedef_void){
                    report_error(context, stack_entry->token, "Expression of type void has undefined alignment.");
                    return operand;
                }
                
                u64 alignment = operand.resolved_type->alignment;
                
                // @note: Delete the whole <expression> off of the `ir_arena`.
                context->ir_arena.current = stack_entry->other;
                
                // Sets in op->operand in the next iteration.
                operand = ast_push_u64_literal(context, alignment, operand.token);
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_unary_bitwise_not:{
                
                if(operand.ir->kind == IR_integer_literal){
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)operand.ir;
                    
                    // "The result of the ~ is the bitwise complement of its (promoted) operand."
                    perform_integer_promotion_on_literal(&operand);
                    
                    lit->_u64 = ~lit->_u64; // singed does not matter and should stay the same
                    break;
                }
                
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_integer, stack_entry->token)) return operand;
                
                maybe_insert_integer_promotion_cast(context, AST_cast, &operand, stack_entry->token);
                
                assert(operand.resolved_type == &globals.typedef_s32 || operand.resolved_type == &globals.typedef_u32 || operand.resolved_type == &globals.typedef_s64 || operand.resolved_type == &globals.typedef_u64);
                
                enum ir_kind ir_kind = (operand.resolved_type->size == 4) ? IR_bitwise_not_u32 : IR_bitwise_not_u64;
                operand.ir = push_ir(context, ir_kind); // Sets in op->operand in the next iteration. Type does not change.
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_unary_logical_not:{
                
                if(operand.ir->kind == IR_integer_literal){
                    struct ir_integer_literal *lit = cast(struct ir_integer_literal *)operand.ir;
                    lit->_u64 = !integer_literal_as_u64(&operand);
                    
                    // "The result has type int."
                    lit->type = &globals.typedef_s32;
                    operand.resolved_type = &globals.typedef_s32;
                    operand.defined_type  = &globals.typedef_u8.kind;
                    break;
                }
                
                // @cleanup: Const prop for pointer literals.
                
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                maybe_insert_integer_promotion_cast(context, AST_cast, &operand, operand.token);
                
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_basic, stack_entry->token)) return operand;
                
                enum ir_type ir_type = ir_type_from_type(operand.resolved_type);
                static enum ir_kind ir_kind_for_type[] = {
                    [IR_TYPE_s32] = IR_logical_not_u32,
                    [IR_TYPE_u32] = IR_logical_not_u32,
                    
                    [IR_TYPE_s64] = IR_logical_not_u64,
                    [IR_TYPE_u64] = IR_logical_not_u64,
                    
                    [IR_TYPE_f32] = IR_logical_not_f32,
                    [IR_TYPE_f64] = IR_logical_not_f64,
                    [IR_TYPE_pointer] = IR_logical_not_u64,
                };
                
                operand.ir = push_ir(context, ir_kind_for_type[ir_type]); // Sets in op->operand in the next iteration.
                operand.resolved_type = &globals.typedef_s32;
                operand.defined_type = &globals.typedef_u8.kind;
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_unary_plus:{
                
                if(operand.ir->kind == IR_integer_literal){
                    // "The result of the unary operator + is the value of its (promoted) operand."
                    perform_integer_promotion_on_literal(&operand);
                    
                    // Nothing to do here, 'operand' is in the contant propagated value.
                    break;
                }else if(operand.ir->kind == IR_float_literal){
                    // Nothing to do here, 'operand' is in the contant propagated value.
                    break;
                }
                
                maybe_insert_integer_promotion_cast(context, AST_cast, &operand, operand.token);
                
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_integer | CHECK_float, stack_entry->token)) return operand;
                
                // @cleanup: Why are we pushing this expresssion?
                // struct ast *prefix = ast_push_unary_expression(context, ast_kind, operand);
                // set_resolved_type(prefix, operand.resolved_type, operand.defined_type);
                // operand.ast = prefix; // Sets in op->operand in the next iteration. Type does not change.
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_unary_minus:{
                
                if(operand.ir->kind == IR_integer_literal){
                    
                    // "The result of the unary operator - is the negative of its (promoted) operand."
                    perform_integer_promotion_on_literal(&operand);
                    
                    // @cleanup: warn here unsigned stays unsigned
                    if(!type_is_signed(operand.resolved_type)){
                        report_warning(context, WARNING_unsigned_negation, operand.token, "Negation of an unsigned number is still unsigned.");
                    }
                    
                    // @note: Negation is the same of signed and unsigned values.
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)operand.ir;
                    switch(operand.resolved_type->size){
                        case 1: lit->_s8  = -lit->_s8;  break;
                        case 2: lit->_s16 = -lit->_s16; break;
                        case 4: lit->_s32 = -lit->_s32; break;
                        case 8: lit->_s64 = -lit->_s64; break;
                        invalid_default_case();
                    }
                    
                    // We are done here, 'operand' is in the contant propagated value.
                    break;
                }else if(operand.ir->kind == IR_float_literal){
                    struct ir_float_literal *lit = (struct ir_float_literal *)operand.ir;
                    lit->value = -lit->value;
                    
                    // We are done here, 'operand' is in the contant propagated value.
                    break;
                }
                
                maybe_insert_integer_promotion_cast(context, AST_cast, &operand, operand.token);
                
                if(!check_unary_for_basic_types(context, operand.resolved_type, CHECK_integer | CHECK_float, stack_entry->token)) return operand;
                
                if(operand.resolved_type->kind == AST_integer_type && !type_is_signed(operand.resolved_type)){
                    report_warning(context, WARNING_unsigned_negation, operand.token, "Negation of an unsigned number is still unsigned.");
                }
                
                enum ir_type ir_type = ir_type_from_type(operand.resolved_type);
                static enum ir_kind ir_kind_for_type[] = {
                    [IR_TYPE_s32] = IR_negate_u32,
                    [IR_TYPE_u32] = IR_negate_u32,
                    
                    [IR_TYPE_s64] = IR_negate_u64,
                    [IR_TYPE_u64] = IR_negate_u64,
                    
                    [IR_TYPE_f32] = IR_negate_f32,
                    [IR_TYPE_f64] = IR_negate_f64,
                };
                
                operand.ir = push_ir(context, ir_kind_for_type[ir_type]); // Sets in op->operand in the next iteration. Type does not change.
                
                context->in_lhs_expression = false;
            }break;
            case AST_unary_deref:{
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                
                if(operand.resolved_type->kind != AST_pointer_type){
                    report_error(context, stack_entry->token, "Can only use '*' on a pointer type.");
                    return operand;
                }
                
                if(operand.ir->kind == IR_pointer_literal){
                    operand.ir->kind = IR_pointer_literal_deref;
                    struct ast_pointer_type *pointer_type = (struct ast_pointer_type *)operand.resolved_type;
                    operand.resolved_type = pointer_type->pointer_to;
                    operand.defined_type = pointer_type->pointer_to_defined_type;
                    
                    struct ir_pointer_literal *lit = (struct ir_pointer_literal *)operand.ir;
                    lit->type = pointer_type->pointer_to;
                }else{
                    
                    struct ast_pointer_type *pointer = (struct ast_pointer_type *)operand.resolved_type;
                    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return operand;
                    
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, stack_entry->token, "Dereferencing a void-pointer.");
                        return operand;
                    }
                    
                    struct ir_deref *deref = push_struct(&context->ir_arena, struct ir_deref);
                    deref->base.kind = IR_deref;
                    deref->type = pointer->pointer_to;
                    
                    operand.ir = &deref->base;
                    operand.resolved_type = pointer->pointer_to;
                    operand.defined_type = pointer->pointer_to_defined_type;
                }
                
                context->in_lhs_expression = true;
            }break;
            
            case AST_unary_address:{
                
                if(!context->in_lhs_expression){
                    report_error(context, operand.token, "Operand of unary '&' must be a lhs expression.");
                    return operand;
                }
                
                if(operand.ir->kind == IR_pointer_literal_deref){
                    operand.ir->kind = IR_pointer_literal;
                    
                    struct ast_type *ptr = parser_push_pointer_type(context, operand.resolved_type, operand.defined_type);
                    operand.resolved_type = ptr;
                    operand.defined_type = null;
                    
                    struct ir_pointer_literal *lit = (struct ir_pointer_literal *)operand.ir;
                    lit->type = ptr;
                }else{
                    operand.ir = push_ir(context, IR_load_address); // Sets in op->operand in the next iteration.
                    operand.resolved_type = parser_push_pointer_type(context, operand.resolved_type, operand.defined_type);
                    operand.defined_type = null;
                }
                
                context->in_lhs_expression = false;
            }break;
            
            case AST_cast:{
                struct ast_type *type_to_cast_to = stack_entry->other;
                enum ast_kind *defined_type_to_cast_to = (enum ast_kind *)stack_entry->operand.ir;
                
                // @cleanup: is this correct?
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
                
                enum ast_kind kind_to_cast_to = type_to_cast_to->kind;
                
                if(kind_to_cast_to == AST_union || kind_to_cast_to == AST_struct){
                    if(!types_are_equal(type_to_cast_to, operand.resolved_type)){
                        struct string operand_type_string = push_type_string(context->arena, &context->scratch, operand.resolved_type);
                        struct string cast_to_type_string = push_type_string(context->arena, &context->scratch, type_to_cast_to);
                        
                        report_error(context, operand.token, "Cast of '%.*s' to '%.*s' is illegal.", operand_type_string.length, operand_type_string.data, cast_to_type_string.length, cast_to_type_string.data);
                    }
                    // @note: Do not set `operand = prefix`.
                    break;
                }
                
                if(operand.resolved_type == &globals.typedef_void){
                    if(kind_to_cast_to != AST_void_type){
                        report_error(context, operand.token, "cast of 'void' to 'non-void'.");
                        return operand;
                    }
                }else if(operand.resolved_type->kind == AST_integer_type){
                    // @note: these are fine, can cast to everything
                }else if(operand.resolved_type->kind == AST_float_type){
                    if(kind_to_cast_to == AST_pointer_type){
                        report_error(context, operand.token, "Cast from float to pointer is illegal.");
                        return operand;
                    }
                }else if(operand.resolved_type->kind == AST_pointer_type){
                    if(kind_to_cast_to == AST_float_type){
                        report_error(context, operand.token, "Cast from pointer to float is illegal.");
                        return operand;
                    }
                }else{
                    if(kind_to_cast_to != AST_void_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, operand.resolved_type);
                        report_error(context, operand.token, "Cannot cast operand of type '%.*s' to something other than 'void' or itself.", type_string.length, type_string.data);
                        return operand;
                    }
                }
                
                if(kind_to_cast_to == AST_void_type){
                    // @note: This is necessary to insert, as (void)0 should not be an integer literal anymore.
                    operand.ir = push_ir(context, IR_cast_to_void);
                    operand.resolved_type = &globals.typedef_void;
                    operand.defined_type = defined_type_to_cast_to;
                }else{
                    push_cast(context, AST_cast, type_to_cast_to, defined_type_to_cast_to, &operand, operand.token);
                }
                context->in_lhs_expression = false;
            }break;
            
            // :multiplicative_expression parse_multiplicative_expression
            // 
            // Precedence 3: Multiplicative expression, Left-to-Right Associative
            //    
            //    lhs * rhs, lhs / rhs, lhs % rhs
            //    
            case AST_binary_times: case AST_binary_divide: case AST_binary_mod:{
                
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    perform_integer_operation(context, stack_entry->token, op_lhs, op_rhs);
                }else{
                    maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                    
                    // @note: only checking the rhs works, because of the arithmetic conversion casts.
                    if(ast_kind == AST_binary_mod && op_rhs->resolved_type->kind == AST_float_type){
                        report_error(context, operand.token, "Operator '%%' is illegal for floats.");
                        return operand;
                    }
                    
                    if(stack_entry->operand.ir->kind == IR_float_literal && operand.ir->kind == IR_float_literal){
                        perform_float_operation(context, ast_kind, op_lhs, op_rhs);
                    }else{
                        
                        if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_integer | CHECK_float)) return operand;
                        
                        if(op_lhs->resolved_type != op_rhs->resolved_type){
                            report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                            return operand;
                        }
                        
                        enum ir_kind base = 0;
                        if(ast_kind == AST_binary_times)  base = IR_multiply_s32;
                        if(ast_kind == AST_binary_divide) base = IR_divide_s32;
                        if(ast_kind == AST_binary_mod)    base = IR_mod_s32;
                        
                        enum ir_type ir_type = ir_type_from_type(op_lhs->resolved_type);
                        enum ir_kind ir_kind = base + (ir_type - IR_TYPE_s32);
                        
                        // @note: For '%', the right hand side defined type "bounds" the lhs type as this is an and.
                        //        Hence, we simply reuse the defined type of the rhs.
                        enum ast_kind *defined_type = (ast_kind == AST_binary_mod) ? op_rhs->defined_type : defined_type_for_binary_op(op_lhs, op_rhs);
                        
                        operand.ir = push_ir(context, ir_kind);
                        operand.resolved_type = op_lhs->resolved_type;
                        operand.defined_type = defined_type;
                    }
                }
                context->in_lhs_expression = false;
            }break;
            
            // :additive_expression parse_additive_expression
            // 
            // Precedence 4: Additive expression, Left-to-Right Associative
            // 
            //    lhs + rhs, lhs - rhs
            //    
            case AST_binary_plus: case AST_binary_minus:{
                
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    perform_integer_operation(context, stack_entry->token, op_lhs, op_rhs);
                }else{
                    maybe_load_address_for_array_or_function(context, IR_load_address_lhs, op_lhs);
                    maybe_load_address_for_array_or_function(context, IR_load_address, op_rhs);
                    
                    maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                    
                    // @note: interger + pointer is legal, integer - pointer is not.
                    if(op_lhs->resolved_type->kind == AST_pointer_type && op_rhs->resolved_type->kind == AST_integer_type){
                        // 'pointer + integer', 'pointer - integer'
                        enum ir_kind ir_kind = ast_kind == AST_binary_plus ? IR_add_u64 : IR_subtract_u64;
                        handle_pointer_arithmetic(context, ir_kind, stack_entry->token, op_lhs, op_rhs, &operand, /*need_swap*/false);
                    }else if(ast_kind == AST_binary_minus && op_lhs->resolved_type->kind == AST_pointer_type && op_rhs->resolved_type->kind == AST_pointer_type){
                        // 'pointer - pointer'
                        
                        if(!types_are_equal(op_lhs->resolved_type, op_rhs->resolved_type)){
                            report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                            return operand;
                        }
                        
                        // @cleanup: Constant propagate!
                        
                        // Subtract the two pointers.
                        operand.ir = push_ir(context, IR_subtract_u64);
                        
                        // Divide by the size.
                        struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)op_lhs->resolved_type;
                        if(pointer->pointer_to->size != 1){ // @cleanup: 0?
                            ast_push_s64_literal(context, pointer->pointer_to->size, null);
                            operand.ir = push_ir(context, IR_divide_u64);
                        }
                        
                        operand.resolved_type = &globals.typedef_s64;
                        operand.defined_type = null;
                    }else if(op_lhs->resolved_type->kind == AST_integer_type && op_rhs->resolved_type->kind == AST_pointer_type){
                        
                        if(ast_kind == AST_binary_minus){
                            report_error(context, stack_entry->token, "Cannot subtract pointer from integer.");
                        }
                        
                        handle_pointer_arithmetic(context, IR_add_u64, stack_entry->token, op_rhs, op_lhs, &operand, /*need_swap*/true);
                    }else if(op_lhs->ir->kind == IR_float_literal && op_rhs->ir->kind == IR_float_literal){
                        perform_float_operation(context, ast_kind, op_lhs, op_rhs);
                    }else{
                        
                        if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_basic)) return operand;
                        
                        if(op_lhs->resolved_type != op_rhs->resolved_type){
                            report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                            return operand;
                        }
                        
                        enum ir_type ir_type = ir_type_from_type(op_lhs->resolved_type);
                        // 0 - u32, 1 - u64, 2 - f32, 3 - f64
                        u32 type_index = ((ir_type - IR_TYPE_s32) >> 1) + (ir_type == IR_TYPE_f64);
                        enum ir_kind ir_base = (ast_kind == AST_binary_plus) ? IR_add_u32 : IR_subtract_u32;
                        enum ir_kind ir_kind = ir_base + type_index;
                        
                        operand.ir = push_ir(context, ir_kind);
                        operand.resolved_type = op_lhs->resolved_type;
                        operand.defined_type = defined_type_for_binary_op(op_lhs, op_rhs);
                    }
                }
                
                context->in_lhs_expression = false;
            }break;
            
            // :shift_expression parse_shift_expression
            // 
            // Precedence 5: Shift expression, Left-to-Right Associative
            // 
            //    lhs << rhs, lhs >> rhs
            //    
            case AST_binary_left_shift: case AST_binary_right_shift:{
                
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                // "The integer promotions are performed on each of the operands."
                maybe_insert_integer_promotion_cast(context, AST_cast_lhs, op_lhs, stack_entry->token);
                maybe_insert_integer_promotion_cast(context, AST_cast, op_rhs, operand.token);
                
                if(op_rhs->ir->kind == IR_integer_literal){
                    struct ir_integer_literal *literal =(struct ir_integer_literal *)op_rhs->ir;
                    
                    if(type_is_signed(op_rhs->resolved_type)){
                        b32 should_error = false;
                        switch(op_rhs->resolved_type->size){
                            case 1: should_error = (literal->_s8  < 0); break;
                            case 2: should_error = (literal->_s16 < 0); break;
                            case 4: should_error = (literal->_s32 < 0); break;
                            case 8: should_error = (literal->_s64 < 0); break;
                            invalid_default_case();
                        }
                        if(should_error){
                            report_error(context, operand.token, "Cannot shift by a negative amount.");
                            return operand;
                        }
                    }
                    
                    u64 value = integer_literal_as_u64(op_rhs);
                    
                    // @cleanup: overflow on the right hand side?
                    // we only have to check the u8 part as that is the only thing that gets used.
                    if(value > (u64)op_lhs->resolved_type->size * 8){
                        report_error(context, stack_entry->token, "Shift by '%llu' but lhs has only '%lld' bits.", value, op_lhs->resolved_type->size * 8);
                        return operand;
                    }
                    
                    if(op_lhs->ir->kind == IR_integer_literal){
                        struct ir_integer_literal *lhs_lit = cast(struct ir_integer_literal *)op_lhs->ir;
                        struct ir_integer_literal *rhs_lit = literal;
                        
                        // @note: I think these shifts are implicitly arithmetic when they have to be.
                        
                        struct ast_type *lhs_type = op_lhs->resolved_type;
                        
                        if(ast_kind == AST_binary_left_shift){
                            if(lhs_type == &globals.typedef_s32){
                                lhs_lit->_s32 <<= rhs_lit->_u8;
                            }else if(lhs_type == &globals.typedef_u32){
                                lhs_lit->_u32 <<= rhs_lit->_u8;
                            }else if(lhs_type == &globals.typedef_s64){
                                lhs_lit->_s64 <<= rhs_lit->_u8;
                            }else if(lhs_type == &globals.typedef_u64){
                                lhs_lit->_u64 <<= rhs_lit->_u8;
                            }
                        }else{
                            if(lhs_type == &globals.typedef_s32){
                                lhs_lit->_s32 >>= rhs_lit->_u8;
                            }else if(lhs_type == &globals.typedef_u32){
                                lhs_lit->_u32 >>= rhs_lit->_u8;
                            }else if(lhs_type == &globals.typedef_s64){
                                lhs_lit->_s64 >>= rhs_lit->_u8;
                            }else if(lhs_type == &globals.typedef_u64){
                                lhs_lit->_u64 >>= rhs_lit->_u8;
                            }
                        }
                        
                        operand.ir = &lhs_lit->base;
                        operand.resolved_type = lhs_type;
                        operand.defined_type = null; // ?
                        
                        pop_from_ir_arena(context, literal);
                        break;
                    }
                }
                
                if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_integer)) return operand;
                
                // @hmm: we require op->rhs->size == 1...
                push_cast(context, AST_cast, &globals.typedef_u8, null, op_rhs, operand.token);
                
                enum ir_kind ir_base = (ast_kind == AST_binary_right_shift) ? IR_right_shift_s32 : IR_left_shift_s32;
                enum ir_type ir_type = ir_type_from_type(op_lhs->resolved_type);
                enum ir_kind ir_kind = ir_base + (ir_type - IR_TYPE_s32);
                operand.ir = push_ir(context, ir_kind);
                
                // "the type of the result is that of the promoted left operand"
                operand.resolved_type = op_lhs->resolved_type;
                operand.defined_type = op_lhs->defined_type;
                
                context->in_lhs_expression = false;
            }break;
            
            
            // :relational_expression parse_relational_expression
            // 
            // Precedence 6: Relational expression, Left-to-Right Associative
            // 
            //    lhs < rhs, lhs > rhs, lhs <= rhs, lhs >= rhs
            //    
            case AST_binary_bigger_equals:
            case AST_binary_smaller_equals:
            case AST_binary_bigger:
            case AST_binary_smaller:{
                
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    perform_integer_operation(context, stack_entry->token, op_lhs, op_rhs);
                    
                    // "The result has type int."
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)op_rhs->ir;
                    lit->type = &globals.typedef_s32;
                    operand.resolved_type = &globals.typedef_s32;
                    operand.defined_type = null;
                    break;
                }
                
                maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                
                if(op_lhs->ir->kind == IR_float_literal && op_rhs->ir->kind == IR_float_literal){
                    // @note: perform_float_operation returns an 'IR_integer_literal' in this case
                    perform_float_operation(context, ast_kind, op_lhs, op_rhs);
                    break;
                }
                
                maybe_load_address_for_array_or_function(context, IR_load_address_lhs, op_lhs);
                maybe_load_address_for_array_or_function(context, IR_load_address, op_rhs);
                
                if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_basic)) return operand;
                
                maybe_cast_literal_0_to_void_pointer(op_lhs, op_rhs); // @cleanup: this seems sus to me, but all compilers seem to do this.
                
                struct ast_type *match = types_are_equal(op_lhs->resolved_type, op_rhs->resolved_type);
                if(!match){
                    if(op_lhs->resolved_type->kind == AST_pointer_type && op_rhs->resolved_type->kind == AST_pointer_type){
                        report_type_mismatch_warning(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                    }else{
                        report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                        return operand;
                    }
                }
                
                // :smaller_bigger_ir_encoding
                // 
                // 20 = 5 bit:
                //         1 bit - bigger / smaller
                //         1 bit - equals / proper
                //         3 bit - type (s32, u32, s64, u64, f32, f64)
                //         
                
                enum ir_type match_type = match ? ir_type_from_type(match) : IR_TYPE_u64;
                if(match_type == IR_TYPE_pointer) match_type = IR_TYPE_u64;
                
                // 0 - bigger equals, 1 - smaller equals, 2 - bigger, 3 - smaller
                u32 condition_information = ast_kind - AST_binary_bigger_equals; 
                
                u32 information = ((match_type - IR_TYPE_s32) << 2) | condition_information;
                
                enum ir_kind ir_kind = IR_bigger_equals_s32 + information;
                
                operand.ir = push_ir(context, ir_kind);
                operand.resolved_type = &globals.typedef_s32;
                operand.defined_type  = &globals.typedef_u8.kind;
                
                context->in_lhs_expression = false;
            }break;
            
            // :compare_expression parse_compare_expression
            // 
            // Precedence 7: Equality expression, Left-to-Right Associative
            // 
            //    lhs == rhs, lhs != rhs
            //    
            // @cleanup: @copy_and_paste.
            case AST_binary_logical_equals:
            case AST_binary_logical_unequals:{
                
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                struct token *op_token = stack_entry->token;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    perform_integer_operation(context, op_token, op_lhs, op_rhs);
                    // "The result has type int."
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)op_rhs->ir;
                    lit->type = &globals.typedef_s32;
                    operand.resolved_type = &globals.typedef_s32;
                    operand.defined_type = null;
                    break;
                }
                
                maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                
                if(op_lhs->ir->kind == IR_float_literal && op_rhs->ir->kind == IR_float_literal){
                    // @note: perform_float_operation returns an 'IR_integer_literal' in this case
                    perform_float_operation(context, ast_kind, op_lhs, op_rhs);
                    break;
                }
                
                maybe_load_address_for_array_or_function(context, IR_load_address_lhs, op_lhs);
                maybe_load_address_for_array_or_function(context, IR_load_address,     op_rhs);
                
                if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_basic)) return operand;
                
                maybe_insert_cast_from_void_pointer(op_lhs, op_rhs);
                maybe_cast_literal_0_to_void_pointer(op_lhs, op_rhs);
                
                struct ast_type *match = types_are_equal(op_lhs->resolved_type, op_rhs->resolved_type);
                if(!match){
                    if(op_lhs->resolved_type->kind == AST_pointer_type && op_rhs->resolved_type->kind == AST_pointer_type){
                        report_type_mismatch_warning(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, op_token);
                    }else{
                        report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, op_token);
                        return operand;
                    }
                }
                
                // 1 bit equals vs unequals
                // 1 bit float vs integer
                // 1 bit 32 vs 64
                // 
                // IR_equals_u32, IR_unequals_u32, IR_equals_u64, IR_unequals_u64,
                // IR_equals_f32, IR_unequals_f32, IR_equals_f64, IR_unequals_f64,
                
                u32 equals_or_unequals = ast_kind - AST_binary_logical_equals;
                u32 size_bit = (match == null) || (match->size == 8);
                u32 float_vs_integer = match && match->kind == AST_float_type;
                u32 information = (float_vs_integer << 2) | (size_bit << 1) | equals_or_unequals;
                
                enum ir_kind ir_kind = IR_equals_u32 + information;
                operand.ir = push_ir(context, ir_kind);
                operand.resolved_type = &globals.typedef_s32;
                operand.defined_type  = &globals.typedef_u8.kind;
                
                context->in_lhs_expression = false;
            }break;
            
            // :bitwise_and_expression parse_bitwise_and_expression
            // 
            // Precedence 8: Bitwise AND, Left-to-Right Associative
            // 
            //    lhs & rhs
            //    
            case AST_binary_and:{
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    u64 lhs = integer_literal_as_u64(op_lhs);
                    u64 rhs = integer_literal_as_u64(op_rhs);
                    
                    lhs &= rhs;
                    
                    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op_lhs->resolved_type, op_rhs->resolved_type);
                    
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)op_lhs->ir;
                    lit->type = promoted_type;
                    lit->_u64 = lhs;
                    
                    pop_from_ir_arena(context, (struct ir_integer_literal *)operand.ir);
                    
                    operand.ir = op_lhs->ir;
                    operand.resolved_type = promoted_type;
                    operand.defined_type = null;
                }else{
                    
                    maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                    
                    if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_integer)) return operand;
                    
                    if(op_lhs->resolved_type != op_rhs->resolved_type){
                        report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                        return operand;
                    }
                    
                    enum ir_kind ir_kind = (op_lhs->resolved_type->size == 4) ? IR_and_u32 : IR_and_u64;
                    operand.ir = push_ir(context, ir_kind);
                    
                    // @note: The right hand side defined type "bounds" the lhs type as this is an and.
                    //        Hence, we simply reuse the defined type of the rhs. @cleanup: This seems weird, feel like it should be the _smaller_ defined type.
                    
                    operand.resolved_type = op_lhs->resolved_type;
                    operand.defined_type = op_rhs->defined_type;
                    
                    context->in_lhs_expression = false;
                }
            }break;
            
            // :bitwise_xor_expression parse_bitwise_xor_expression
            // 
            // Precedence 9: Bitwise XOR, Left-to-Right Associative
            // 
            //    lhs ^ rhs
            //    
            // @cleanup: copy and paste
            case AST_binary_xor:{
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    
                    u64 lhs = integer_literal_as_u64(op_lhs);
                    u64 rhs = integer_literal_as_u64(op_rhs);
                    
                    lhs ^= rhs;
                    
                    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op_lhs->resolved_type, op_rhs->resolved_type);
                    
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)op_lhs->ir;
                    lit->type = promoted_type;
                    lit->_u64 = lhs;
                    
                    pop_from_ir_arena(context, (struct ir_integer_literal *)operand.ir);
                    
                    operand.ir = op_lhs->ir;
                    operand.resolved_type = promoted_type;
                    operand.defined_type = null;
                }else{
                    if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_integer)) return operand;
                    
                    maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                    
                    if(op_lhs->resolved_type != op_rhs->resolved_type){
                        report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                        return operand;
                    }
                    
                    enum ir_kind ir_kind = (op_lhs->resolved_type->size == 4) ? IR_xor_u32 : IR_xor_u64;
                    operand.ir = push_ir(context, ir_kind);
                    
                    operand.resolved_type = op_lhs->resolved_type;
                    operand.defined_type = defined_type_for_binary_op(op_lhs, op_rhs);
                    
                    context->in_lhs_expression = false;
                }
            }break;
            
            // :bitwise_or_expression parse_bitwise_or_expression
            // 
            // Precedence 10: Bitwise OR, Left-to-Right Associative
            // 
            //    lhs | rhs
            //    
            case AST_binary_or:{
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    
                    u64 lhs = integer_literal_as_u64(op_lhs);
                    u64 rhs = integer_literal_as_u64(op_rhs);
                    
                    lhs |= rhs;
                    
                    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op_lhs->resolved_type, op_rhs->resolved_type);
                    
                    struct ir_integer_literal *lit = (struct ir_integer_literal *)op_lhs->ir;
                    lit->type = promoted_type;
                    lit->_u64 = lhs;
                    
                    pop_from_ir_arena(context, (struct ir_integer_literal *)operand.ir);
                    
                    operand.ir = op_lhs->ir;
                    operand.resolved_type = promoted_type;
                    operand.defined_type  = null;
                }else{
                    if(!check_binary_for_basic_types(context, op_lhs->resolved_type, op_rhs->resolved_type, stack_entry->token, CHECK_integer)) return operand;
                    
                    maybe_insert_arithmetic_conversion_casts(context, op_lhs, op_rhs, stack_entry->token);
                    
                    if(op_lhs->resolved_type != op_rhs->resolved_type){
                        report_type_mismatch_error(context, op_lhs->resolved_type, op_lhs->defined_type, op_rhs->resolved_type, op_rhs->defined_type, stack_entry->token);
                        return operand;
                    }
                    
                    
                    enum ir_kind ir_kind = (op_lhs->resolved_type->size == 4) ? IR_or_u32 : IR_or_u64;
                    operand.ir = push_ir(context, ir_kind);
                    
                    operand.resolved_type = op_lhs->resolved_type;
                    operand.defined_type = defined_type_for_binary_op(op_lhs, op_rhs);
                    
                    context->in_lhs_expression = false;
                }
            }break;
            
            // :logical_and_expression parse_logical_and_expression
            // 
            // Precedence 11: Logical AND, Left-to-Right Associative
            // 
            //    lhs && rhs
            //    
            case AST_logical_and:{
                maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                
                if(!casts_implicitly_to_bool(operand.resolved_type)){
                    // :Error pick a way to say this an stick to it
                    report_error(context, operand.token, "Right of '&&' has to be convertible to _Bool.");
                    return operand;
                }
                
                // 
                // Code for &&
                //  
                //    lhs
                //    jump if false .false
                //    rhs
                //    jump if false .false
                //    temp
                //    1
                //    assign
                //    jump .end
                //  .false:
                //    0
                //    assign
                //  .end:
                // 
                
                struct expr *op_lhs = &stack_entry->operand;
                struct expr *op_rhs = &operand;
                struct ir_jump_node *conditional_jump = stack_entry->other;
                
                if(op_lhs->ir->kind == IR_integer_literal && op_rhs->ir->kind == IR_integer_literal){
                    struct ir_integer_literal *lit = cast(struct ir_integer_literal *)op_lhs->ir;
                    if(integer_literal_as_u64(op_lhs) && integer_literal_as_u64(op_rhs)){
                        lit->_u64 = 1;
                    }else{
                        lit->_u64 = 0;
                    }
                    
                    pop_from_ir_arena(context, (struct ir_integer_literal *)operand.ir);
                    pop_from_ir_arena(context, conditional_jump);
                    
                    lit->type = &globals.typedef_s32;
                    operand.ir = &lit->base;
                }else{
                    
                    // 
                    // Okay, this is somewhat aweful. 
                    // Something like this is necessary for expressions like a + (b && c),
                    // but for the case where a logical and / logical or leads into a condition
                    // we should have special cases, that do not load the condition.
                    // 
                    
                    struct ir_jump_node *rhs_jump = push_jump(context, IR_jump_if_false);
                    rhs_jump->label_number = conditional_jump->label_number;
                    
                    struct ast_declaration *temp = push_unnamed_declaration(context, &globals.typedef_s32, null, stack_entry->token);
                    struct ir_identifier *lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp;
                    
                    ast_push_s32_literal(context, 1, null);
                    push_ir(context, IR_store);
                    push_ir(context, IR_pop_expression);
                    
                    struct ir_jump_node *jump_end = push_jump(context, IR_jump);
                    jump_end->label_number = context->jump_label_index++;
                    
                    struct ir_jump_node *false_label = push_jump(context, IR_jump_label);
                    false_label->label_number = conditional_jump->label_number;
                    
                    lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp;
                    ast_push_s32_literal(context, 0, null);
                    push_ir(context, IR_store);
                    push_ir(context, IR_pop_expression);
                    
                    struct ir_jump_node *end_label = push_jump(context, IR_jump_label);
                    end_label->label_number = jump_end->label_number;
                    
                    // ... in the end, temp is on top of the stack, the expression we return.
                    lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp;
                    operand.ir = &lhs->base;
                }
                
                operand.resolved_type = &globals.typedef_s32;
                operand.defined_type = &globals.typedef_u8.kind;
                context->in_lhs_expression = false;
            }break;
            
            // :logical_or_expression parse_logical_or_expression
            // 
            // Precedence 12: Logical OR, Left-to-Right Associative
            // 
            //    lhs || rhs
            //    
            case AST_logical_or:{
                
                // Code for logical or:
                //     lhs 
                //     jump if true .if_true
                //     rhs
                //     jump if true .if_true
                //     temp
                //     0
                //     assign
                //     jump .end
                //   .if_true
                //     1
                //     assign
                //   .end:
                // 
                
                maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                
                if(!casts_implicitly_to_bool(operand.resolved_type)){
                    report_error(context, operand.token, "Right of '||' has to be convertible to _Bool.");
                    return operand;
                }
                
                struct expr *op_lhs_expr = &stack_entry->operand;
                struct ir *op_lhs = stack_entry->operand.ir;
                
                struct expr *op_rhs_expr = &operand;
                struct ir *op_rhs = operand.ir;
                struct ir_jump_node *conditional_jump = stack_entry->other;
                
                // 
                // @note: For a condition like '0 || (print("hello!\n"), 0)' we should not constant propagate.
                //        At least for now, in theory, the first part (`0 ||`) could be removed.
                // 
                
                if(op_lhs->kind == IR_integer_literal && op_rhs->kind == IR_integer_literal){
                    struct ir_integer_literal *lit = cast(struct ir_integer_literal *)op_lhs;
                    if(integer_literal_as_u64(op_lhs_expr) || integer_literal_as_u64(op_rhs_expr)){
                        lit->_u64 = 1;
                    }else{
                        lit->_u64 = 0;
                    }
                    
                    pop_from_ir_arena(context, (struct ir_integer_literal *)operand.ir);
                    pop_from_ir_arena(context, conditional_jump);
                    
                    lit->type = &globals.typedef_s32;
                    operand.ir = &lit->base;
                }else{
                    
                    // 
                    // @copy and paste from ast_logical_and:
                    // 
                    // Something like this is necessary for expressions like a + (b && c),
                    // but for the case where a logical and / logical or leads into a condition
                    // we should have special cases, that do not load the condition.
                    // 
                    
                    struct ir_jump_node *rhs_jump = push_jump(context, IR_jump_if_true);
                    rhs_jump->label_number = conditional_jump->label_number;
                    
                    struct ast_declaration *temp = push_unnamed_declaration(context, &globals.typedef_s32, null, stack_entry->token);
                    struct ir_identifier *lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp;
                    
                    ast_push_s32_literal(context, 0, null);
                    push_ir(context, IR_store);
                    push_ir(context, IR_pop_expression);
                    
                    struct ir_jump_node *jump_end = push_jump(context, IR_jump);
                    jump_end->label_number = context->jump_label_index++;
                    
                    struct ir_jump_node *true_label = push_jump(context, IR_jump_label);
                    true_label->label_number = conditional_jump->label_number;
                    
                    lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp;
                    
                    ast_push_s32_literal(context, 1, null);
                    push_ir(context, IR_store);
                    push_ir(context, IR_pop_expression);
                    
                    struct ir_jump_node *end_label = push_jump(context, IR_jump_label);
                    end_label->label_number = jump_end->label_number;
                    
                    lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp;
                    operand.ir = &lhs->base;
                    
                    operand.ir = &lhs->base;
                }
                
                operand.resolved_type = &globals.typedef_s32;
                operand.defined_type  = &globals.typedef_u8.kind;
                context->in_lhs_expression = false;
            }break;
            
            // :conditional_expression parse_conditional_expression
            // 
            // Precedence 13: Ternary operator, Right-to-Left Associative (the middle operator is parsed as if it was in parenthesis)
            // 
            //    lhs ? true : false
            //    
            case AST_conditional_expression:
            case AST_conditional_expression_true:
            case AST_conditional_expression_false:{
                
                // @cleanup: this has a lot of code that any binary expression also has e.g.
                //              'maybe_cast_literal_0_to_void_pointer'
                //              'maybe_insert_arithmetic_conversion_casts'
                //           maybe we could make it compatible with binary ops, to use these functions
                
                // 
                // Code for ternary:
                //    
                //    condition     (if this is constant, it has been passed through 'AST_condtional_expression_true / AST_condition_expression_false')
                //    jumpcc .false
                //    if_true
                //    jump .end
                //  .false:
                //    if_false
                //  .end:
                // 
                
                struct token *question_mark = stack_entry->token;
                
                struct expr *if_true   = &stack_entry->operand;
                struct expr *if_false  = &operand;
                struct conditional_expression_information *information = stack_entry->other;
                
                struct ir *condition = information->condition;
                struct ir_identifier *temp1 = information->temp;
                
                struct ir_jump_node *end_jump = information->end_jump;
                struct ir_jump_node *if_false_label = (struct ir_jump_node *)(end_jump + 1);
                struct ir_identifier *temp2 = (struct ir_identifier *)(if_false_label + 1);
                
                maybe_load_address_for_array_or_function(context, IR_load_address, if_false);
                maybe_insert_cast_from_special_int_to_int(context, if_false, /*is_lhs*/0);
                
                // "one of the following shall hold":
                
                // "- both are arithmetic types"
                if(type_is_arithmetic(if_true->resolved_type) && type_is_arithmetic(if_false->resolved_type)){
                    struct ast_type *arithmetic_type = perform_usual_arithmetic_conversions(if_true->resolved_type, if_false->resolved_type);
                    
                    if(if_true->resolved_type != arithmetic_type){
                        u8 *start = context->ir_arena.current;
                        push_cast(context, AST_cast, arithmetic_type, defined_type_for_promotion(if_true), if_true, stack_entry->token);
                        if(start != context->ir_arena.current){
                            *information->cast = *if_true->ir;
                            if_true->ir->kind = IR_nop;
                        }
                    }
                    if(if_false->resolved_type != arithmetic_type){
                        push_cast(context, AST_cast, arithmetic_type, defined_type_for_promotion(if_false), if_false, stack_entry->token);
                    }
                    
                }else{
                    
                    // "- one is a pointer to an object and the other is a pointer to void"
                    maybe_insert_cast_from_void_pointer(if_true, if_false);
                    
                    // "- one operand is a pointer and the other is a null pointer constant"
                    maybe_cast_literal_0_to_void_pointer(if_true, if_false);
                    
                    // "- both operands have the same union or structure type"
                    // "- both operands have void type"
                    // "- both operands are pointers to compatible types"
                    if(!types_are_equal(if_false->resolved_type, if_true->resolved_type)){
                        report_type_mismatch_error(context, if_true->resolved_type, if_true->defined_type, if_false->resolved_type, if_false->defined_type, question_mark);
                    }
                }
                
                int constant_propagated = 0;
                if(ast_kind != AST_conditional_expression){
                    
                    // The condition is constant, we only turn this into a constant if the "true" path is also const.
                    // @cleanup: Only do this if both paths are const?
                    
                    struct expr *expr = (ast_kind == AST_conditional_expression_true) ? if_true : if_false;
                    struct ir *ir = expr->ir;
                    
                    if(ir->kind == IR_integer_literal || ir->kind == IR_pointer_literal || ir->kind == IR_float_literal){
                        // 
                        // We have to constant-propergate this.
                        // Copy it into condition and delete everything afterwards.
                        // 
                        // This whole thing feels sort-of bad, but it is the best I could come up with.
                        constant_propagated = 1;
                        
                        static_assert(sizeof(struct ir_integer_literal) == sizeof(struct ir_float_literal));
                        static_assert(sizeof(struct ir_integer_literal) == sizeof(struct ir_pointer_literal));
                        memcpy(condition, ir, sizeof(struct ir_integer_literal));
                        
                        u8 *past_const = (u8 *)condition + sizeof(struct ir_integer_literal);
                        context->ir_arena.current = past_const;
                        
                        operand.ir = condition;
                        operand.resolved_type = expr->resolved_type;
                        operand.defined_type  = expr->defined_type;
                    }
                }
                
                if(!constant_propagated){
                    // @cleanup: how should these defined_types propagate in this case?
                    enum ast_kind *defined_type = if_true->defined_type ? if_true->defined_type : if_false->defined_type;
                    
                    // 
                    // @cleanup: This is probably wrong.
                    // 
                    struct ast_type *bigger_type = if_true->resolved_type->size > if_false->resolved_type->size ? if_true->resolved_type : if_false->resolved_type;
                    
                    struct ast_declaration *temp_decl = push_unnamed_declaration(context, bigger_type, null, question_mark);
                    
                    temp1->decl = temp_decl;
                    temp2->decl = temp_decl;
                    
                    push_ir(context, IR_store);
                    push_ir(context, IR_pop_expression);
                    
                    struct ir_jump_node *end_label = push_jump(context, IR_jump_label);
                    end_label->label_number = end_jump->label_number;
                    
                    
                    struct ir_identifier *lhs = push_struct(&context->ir_arena, struct ir_identifier);
                    lhs->base.kind = IR_identifier;
                    lhs->decl = temp_decl;
                    operand.ir = &lhs->base;
                    operand.resolved_type = if_true->resolved_type;
                    operand.defined_type = defined_type;
                }
                
                context->in_lhs_expression = false;
                context->in_conditional_expression -= 1;
            }break;
            
            // :assign_expression parse_assign_expression :assignment_expression parse_assignment_expression
            // :compound_assignment
            // 
            // Precedence 14: Assignment expression, Right-to-Left Associative
            // 
            //    lhs = rhs, lhs += rhs, lhs -= rhs, lhs *= rhs, lhs /= rhs, lhs %= rhs
            //    lhs <<= rhs, lhs >>= rhs, lhs &= rhs, lhs ^= rhs, lhs |= rhs
            //    
            case AST_assignment:{
                struct expr *lhs = &stack_entry->operand;
                
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, &operand, stack_entry->token);
                
                struct ast_type *lhs_type = lhs->resolved_type;
                
                if(lhs_type->kind == AST_bitfield_type){
                    struct ir_store_bitfield *bitfield_assignment = push_struct(&context->ir_arena, struct ir_store_bitfield);
                    bitfield_assignment->base.kind = IR_store_bitfield;
                    bitfield_assignment->bitfield_type = (struct ast_bitfield_type *)lhs_type;
                    operand.ir = &bitfield_assignment->base;
                }else{
                    enum ir_kind ir_kind = (lhs_type->flags & TYPE_FLAG_is_atomic) ? IR_store_atomic : IR_store;
                    operand.ir = push_ir(context, ir_kind);
                }
                
                operand.resolved_type = lhs->resolved_type;
                operand.defined_type = lhs->defined_type;
                
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            // :compound_assignments
            //
            // "A compound assignment of the form E1 op= E2" is equivalent ot the simple assignment expression
            //  E1 = E1 op E2, except that the lvalue E1 is evaluated only once [...]."
            //
            // This means for example that if 'i = 1000' and 'f = 1.234f', then 'i *= f' gives 'i == 1234'.
            // this is because the resulting expression is supposed to be:
            //     i = (int)( (float)i * f);
            // This also happens for '+': 
            //     int i = 0x4000000; i += 1.0f;
            // at the end 'i' is still '0x4000000'.
            // Similarly, this also happens for integer % integer:
            //     char a = 1; int b = 0x100; a %= b; 
            // This does not crash, meaning a gets up-converted to 'int' and then modulated by 0x100.
            
            // For this problem we have the hack/solution 'punt_compound_assignment',
            // which transforms the assignment 'a op= b' into '(unnamed = &a, *unnamed = *unnamed op b)'
            // with assignment cast and arithmetic conversions applied.
            // This is probably pretty slow and produces terrible code, so only use it if you have to!
            //                                                                                   -19.09.2021
            
            case AST_plus_assignment:
            case AST_minus_assignment:{
                struct expr *lhs = &stack_entry->operand;
                struct expr *rhs = &operand;
                maybe_insert_cast_from_special_int_to_int(context, rhs, /*is_lhs*/0);
                
                struct ast_type *lhs_type = lhs->resolved_type;
                struct ast_type *rhs_type = rhs->resolved_type;
                
                if(lhs_type->kind == AST_pointer_type){
                    if(rhs_type->kind != AST_integer_type){
                        report_error(context, stack_entry->token, "Left is a pointer type and the right is not an integer.");
                        return operand;
                    }
                    
                    enum ir_kind ir_kind = ast_kind == AST_plus_assignment ? IR_add_assignment_u64 : IR_subtract_assignment_u64;
                    handle_pointer_arithmetic(context, ir_kind, stack_entry->token, lhs, rhs, &operand, /*need_swap*/false);
                    context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
                    break;
                }
                
                maybe_load_address_for_array_or_function(context, IR_load_address, rhs);
                
                if(rhs->resolved_type->kind == AST_pointer_type){
                    // The rhs should never be a pointer.
                    report_error(context, operand.token, "Right of '%s' cannot be a pointer.", ast_kind == AST_plus_assignment ? "+=" : "-=");
                    return operand;
                }
                
                if(!check_binary_for_basic_types(context, lhs->resolved_type, rhs->resolved_type, stack_entry->token, CHECK_basic)) return operand;
                
                b32 punt = false;
                
                if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_float_type){
                    // punt on 'integer += float' and 'integer -= float'.
                    // this is necessary as for example 'int i = 0x4000000; i += 1.0f;' yields 'i == 0x4000000'.
                    punt = true;
                }else if(lhs->resolved_type == &globals.typedef_f32 && rhs->resolved_type == &globals.typedef_f64){
                    // punt on 'float += double', this is necessary for example
                    //     double var_00 = -1183312983.13;
                    //     float  var_08 =  -282911132.13f;
                    //     var_08 -= var_00;
                    // @cleanup: search for a better example!
                    punt = true;
                }
                
                if(lhs->resolved_type == &globals.typedef_Bool || lhs->resolved_type->kind == AST_bitfield_type) punt = 1;
                
                if(punt){
                    punt_compound_assignment(context, lhs, rhs, stack_entry->token, ast_kind);
                }else{
                    maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, rhs, stack_entry->token);
                    
                    enum ir_type ir_type = ir_type_from_type(lhs->resolved_type);
                    // 0 - u8, 1 - u16, 2 - u32, 3 - u64, 4 - f32, 5 - f64
                    // 6 - atomic bool, 7 - atomic u8, 8 - atomic u16, 9 atomic u32, 10 - atomic u64
                    u32 type_index = ((ir_type - IR_TYPE_s8) >> 1) + (ir_type == IR_TYPE_f64);
                    
                    enum ir_kind ir_base = (ast_kind == AST_plus_assignment) ? IR_add_assignment_u8 : IR_subtract_assignment_u8;
                    enum ir_kind ir_kind = ir_base + type_index;
                    
                    operand.ir = push_ir(context, ir_kind);
                    operand.resolved_type = lhs->resolved_type;
                    operand.defined_type = lhs->defined_type;
                }
                
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_modulo_assignment:{
                
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                
                if(!check_binary_for_basic_types(context, stack_entry->operand.resolved_type, operand.resolved_type, stack_entry->token, CHECK_integer)) return operand;
                
                struct expr *lhs = &stack_entry->operand;
                struct expr *rhs = &operand;
                maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
                
                // Punt if the right hand side is bigger than the lhs, because we could have situation like
                //     char a = 1; int b = 0x100; a %= b; 
                // in which case 'b' cant be truncated to char, or it will crash.
                b32 punt = (lhs->resolved_type->size < rhs->resolved_type->size);
                
                if(rhs->ir->kind == IR_integer_literal){
                    u64 value = integer_literal_as_u64(rhs);
                    if(value == 0){
                        report_error(context, stack_entry->token, "Compile time '%%=' by 0.");
                        return operand;
                    }
                    
                    if(value < (1ull << lhs->resolved_type->size)){
                        // @cleanup: you could see how we could report an error here instead of punting in some cases
                        //           but this hits the common case of % by some small integer (like 10).
                        punt = false;
                    }
                }
                
                if(lhs->resolved_type == &globals.typedef_Bool || (lhs->resolved_type->kind == AST_bitfield_type)) punt = 1;
                
                if(punt){
                    punt_compound_assignment(context, lhs, rhs, stack_entry->token, ast_kind);
                }else{
                    maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, rhs, stack_entry->token);
                    
                    enum ir_type ir_type = ir_type_from_type(lhs->resolved_type);
                    enum ir_kind ir_kind = IR_modulo_assignment_s8 + (ir_type - IR_TYPE_s8);
                    
                    operand.ir = push_ir(context, ir_kind);
                    operand.resolved_type = lhs->resolved_type;
                    operand.defined_type = lhs->defined_type;
                }
                
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_and_assignment:
            case AST_or_assignment:
            case AST_xor_assignment:{
                
                if(!check_binary_for_basic_types(context, stack_entry->operand.resolved_type, operand.resolved_type, stack_entry->token, CHECK_integer)) return operand;
                
                struct expr *lhs = &stack_entry->operand;
                struct expr *rhs = &operand;
                maybe_insert_cast_from_special_int_to_int(context, rhs, /*is_lhs*/0);
                
                if(lhs->resolved_type == &globals.typedef_Bool || lhs->resolved_type->kind == AST_bitfield_type){
                    punt_compound_assignment(context, lhs, rhs, stack_entry->token, ast_kind);
                }else{
                    // These are always fine, we never have to punt.
                    maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, rhs, stack_entry->token);
                    
                    struct ast_type *lhs_type = lhs->resolved_type;
                    if(lhs_type == &globals.typedef_atomic_bool) lhs_type = &globals.typedef_atomic_u8;
                    
                    enum ir_kind ir_base = 0;
                    if(ast_kind == AST_and_assignment) ir_base = IR_and_assignment_u8;
                    if(ast_kind == AST_or_assignment)  ir_base = IR_or_assignment_u8;
                    if(ast_kind == AST_xor_assignment) ir_base = IR_xor_assignment_u8;
                    
                    enum ir_type ir_type = ir_type_from_type(lhs->resolved_type);
                    u32 type_index = (ir_type - IR_TYPE_s8) >> 1;
                    if(ir_type >= IR_TYPE_atomic_bool) type_index -= 3;
                    
                    enum ir_kind ir_kind = ir_base + type_index;
                    operand.ir = push_ir(context, ir_kind);
                    operand.resolved_type = lhs->resolved_type;
                    operand.defined_type = lhs->defined_type;
                }
                
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_divide_assignment:
            case AST_times_assignment:{
                
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                
                if(!check_binary_for_basic_types(context, stack_entry->operand.resolved_type, operand.resolved_type, stack_entry->token, CHECK_integer|CHECK_float)) return operand;
                
                struct expr *lhs = &stack_entry->operand;
                struct expr *rhs = &operand;
                
                maybe_insert_cast_from_special_int_to_int(context, rhs, /*is_lhs*/0);
                
                struct ast_type *lhs_type = lhs->resolved_type;
                struct ast_type *rhs_type = rhs->resolved_type;
                
                int punt = false;
                if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_float_type) punt = true;
                
                //
                // Punt if the lhs type is smaller than the rhs.
                // This is prevents the truncation of the rhs, when in reality the rhs should be promoted.
                // 
                if(lhs_type->size < rhs_type->size) punt = true;
                
                if(rhs->ir->kind == IR_integer_literal){
                    // 
                    // @copy and paste from above.
                    // 
                    
                    u64 value = integer_literal_as_u64(rhs);
                    if(value == 0){
                        if(ast_kind == AST_divide_assignment){
                            report_error(context, stack_entry->token, "Compile time '/=' by 0.");
                            return operand;
                        }else{
                            report_warning(context, WARNING_compile_time_multiplication_by_zero, stack_entry->token, "Compile time '*=' by 0.");
                        }
                    }
                    
                    if(value < (1ull << lhs->resolved_type->size)){
                        // @cleanup: you could see how we could report an error here instead of punting in some cases
                        //           but this hits the common case of % by some small integer (like 10).
                        punt = false;
                    }
                }
                
                if(lhs->resolved_type == &globals.typedef_Bool || lhs->resolved_type->kind == AST_bitfield_type) punt = 1;
                
                if(punt){
                    punt_compound_assignment(context, lhs, rhs, stack_entry->token, ast_kind);
                }else{
                    maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, rhs, stack_entry->token);
                    
                    if(lhs->resolved_type->kind == AST_atomic_integer_type){
                        report_error(context, stack_entry->token, "Atomic integer operation currently not supported.");
                        return operand;
                    }
                    
                    enum ir_kind ir_base = (ast_kind == AST_divide_assignment) ? IR_divide_assignment_s8 : IR_multiply_assignment_s8;
                    enum ir_type ir_type = ir_type_from_type(lhs->resolved_type);
                    operand.ir = push_ir(context, ir_base + ir_type - IR_TYPE_s8);
                    operand.resolved_type = lhs->resolved_type;
                    operand.defined_type = lhs->defined_type;
                }
                
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_left_shift_assignment:
            case AST_right_shift_assignment:{
                maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
                
                if(!check_binary_for_basic_types(context, stack_entry->operand.resolved_type, operand.resolved_type, stack_entry->token, CHECK_integer)) return operand;
                
                struct expr *lhs = &stack_entry->operand;
                struct expr *rhs = &operand;
                maybe_insert_cast_from_special_int_to_int(context, rhs, /*is_lhs*/0);
                
                if(lhs->resolved_type == &globals.typedef_Bool || lhs->resolved_type->kind == AST_bitfield_type){
                    punt_compound_assignment(context, lhs, rhs, stack_entry->token, ast_kind);
                }else{
                    push_cast(context, AST_cast, &globals.typedef_u8, null, rhs, operand.token);
                    
                    if(lhs->resolved_type->kind == AST_atomic_integer_type){
                        report_error(context, stack_entry->token, "Atomic integer operation currently not supported.");
                        return operand;
                    }
                    
                    enum ir_kind ir_base = (ast_kind == AST_left_shift_assignment) ? IR_left_shift_assignment_s8 : IR_right_shift_assignment_s8;
                    enum ir_type ir_type = ir_type_from_type(lhs->resolved_type);
                    operand.ir = push_ir(context, ir_base + ir_type - IR_TYPE_s8);
                    operand.resolved_type = lhs->resolved_type;
                    operand.defined_type = lhs->defined_type;
                }
                
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_comma_expression:{
                assert(!should_skip_comma_expression);
                
                // We have to change the operand, as otherwise we may try to const propagate in situations where we should not.
                operand.ir = push_ir(context, IR_nop);
                
                context->in_lhs_expression = false; // @hmm: should we allow (3, a) = 3; probably not
            }break;
            
            invalid_default_case();
        }
        
        // After we have handled the expression, the current token is the one from the stack_entry.
        operand.token = stack_entry->token;
    }
    
    switch(binary_expression->type){
        
        // Precedence 3
        case TOKEN_times: ast_stack_push(context, AST_binary_times,  binary_expression, &operand); goto restart;
        case TOKEN_slash: ast_stack_push(context, AST_binary_divide, binary_expression, &operand); goto restart;
        case TOKEN_mod:   ast_stack_push(context, AST_binary_mod,    binary_expression, &operand); goto restart;
        
        // Precedence 4
        case TOKEN_plus:  ast_stack_push(context, AST_binary_plus,  binary_expression, &operand); goto restart;
        case TOKEN_minus: ast_stack_push(context, AST_binary_minus, binary_expression, &operand); goto restart;
        
        // Precedence 5
        case TOKEN_left_shift:  ast_stack_push(context, AST_binary_left_shift,  binary_expression, &operand); goto restart;
        case TOKEN_right_shift: ast_stack_push(context, AST_binary_right_shift, binary_expression, &operand); goto restart;
        
        // Precedence 6
        case TOKEN_bigger_equals:  ast_stack_push(context, AST_binary_bigger_equals,  binary_expression, &operand); goto restart;
        case TOKEN_smaller_equals: ast_stack_push(context, AST_binary_smaller_equals, binary_expression, &operand); goto restart;
        
        case TOKEN_bigger:  ast_stack_push(context, AST_binary_bigger,  binary_expression, &operand); goto restart;
        case TOKEN_smaller: ast_stack_push(context, AST_binary_smaller, binary_expression, &operand); goto restart;
        
        // Precedence 7
        case TOKEN_logical_equals:   ast_stack_push(context, AST_binary_logical_equals,   binary_expression, &operand); goto restart;
        case TOKEN_logical_unequals: ast_stack_push(context, AST_binary_logical_unequals, binary_expression, &operand); goto restart;
        
        // Precedence 8
        case TOKEN_and: ast_stack_push(context, AST_binary_and, binary_expression, &operand); goto restart;
        
        // Precedence 9
        case TOKEN_xor: ast_stack_push(context, AST_binary_xor, binary_expression, &operand); goto restart;
        
        // Precedence 10
        case TOKEN_or: ast_stack_push(context, AST_binary_or, binary_expression, &operand); goto restart;
        
        // Precedence 11
        case TOKEN_logical_and:{
            maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
            maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
            
            if(!casts_implicitly_to_bool(operand.resolved_type)){
                report_error(context, next_token(context), "Left of '&&' has to be convertible to _Bool.");
                return operand;
            }
            
            struct ir_jump_node *conditional_jump = push_jump(context, IR_jump_if_false);
            conditional_jump->label_number = context->jump_label_index++;
            
            struct ast_stack_entry *stack_entry = ast_stack_push(context, AST_logical_and, binary_expression, &operand);
            stack_entry->other = conditional_jump;
            goto restart;
        }break;
        
        // Precedence 12
        case TOKEN_logical_or:{
            maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
            maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
            
            if(!casts_implicitly_to_bool(operand.resolved_type)){
                report_error(context, next_token(context), "Left of '||' has to be convertible to _Bool.");
                return operand;
            }
            
            struct ir_jump_node *conditional_jump = push_jump(context, IR_jump_if_true);
            conditional_jump->label_number = context->jump_label_index++;
            
            struct ast_stack_entry *stack_entry = ast_stack_push(context, AST_logical_or, binary_expression, &operand);
            stack_entry->other = conditional_jump;
            goto restart;
        }break;
        
        // Precedence 13
        case TOKEN_question_mark:{
            // conditional_expression = logical_or_expression ? expression : conditional_expression;
            
            struct token *question_mark = binary_expression;
            
            maybe_load_address_for_array_or_function(context, IR_load_address, &operand);
            maybe_insert_cast_from_special_int_to_int(context, &operand, /*is_lhs*/0);
            if(!casts_implicitly_to_bool(operand.resolved_type)){
                report_error(context, question_mark, "Left hand side of '?' has no implicit conversion to bool.");
                return operand;
            }
            
            context->in_conditional_expression += 1; // :tracking_conditional_expression_depth_for_noreturn_functions
            
            struct ir_jump_node *conditional_jump = push_jump(context, IR_jump_if_false);
            conditional_jump->label_number = context->jump_label_index++;
            
            struct ir_identifier *temp1 = push_struct(&context->ir_arena, struct ir_identifier);
            temp1->base.kind = IR_identifier;
            
            struct expr if_true = parse_expression(context, false);
            maybe_load_address_for_array_or_function(context, IR_load_address, &if_true);
            maybe_insert_cast_from_special_int_to_int(context, &if_true, /*is_lhs*/0);
            expect_token(context, TOKEN_colon, "Expected ':' in conditional expression.");
            if(context->should_exit_statement) return operand;
            
            enum ast_kind conditional_expression_kind = AST_conditional_expression;
            
            if(operand.ir->kind == IR_integer_literal || operand.ir->kind == IR_pointer_literal || operand.ir->kind == IR_float_literal){
                // 
                // The condition is constant, we emit one of:
                //     AST_conditional_expression_true
                //     AST_conditional_expression_false
                // 
                
                int is_true;
                switch(operand.ir->kind){
                    case IR_integer_literal: is_true = integer_literal_as_u64(&operand) ? 1 : 0;                  break;
                    case IR_pointer_literal: is_true = ((struct ir_pointer_literal *)operand.ir)->pointer ? 1 : 0; break;
                    case IR_float_literal:   is_true = ((struct ir_float_literal *)operand.ir)->value ? 1 : 0;     break;
                    invalid_default_case(is_true = false);
                }
                
                conditional_expression_kind = is_true ? AST_conditional_expression_true : AST_conditional_expression_false;
            }
            
            struct ir *cast = null;
            if(type_is_arithmetic(if_true.resolved_type)){
                cast = push_ir(context, IR_nop);
            }
            push_ir(context, IR_store);
            push_ir(context, IR_pop_expression);
            
            // 
            // @WARINING: We assume we know the order of these nodes to get temp2 from end_jump.
            // 
            
            struct ir_jump_node *end_jump = push_jump(context, IR_jump);
            end_jump->label_number = context->jump_label_index++;
            
            struct ir_jump_node *if_false_label = push_jump(context, IR_jump_label);
            if_false_label->label_number = conditional_jump->label_number;
            
            struct ir_identifier *temp2 = push_struct(&context->ir_arena, struct ir_identifier);
            temp2->base.kind = IR_identifier;
            
            struct conditional_expression_information *conditional_expression_information = push_struct(&context->scratch, struct conditional_expression_information);
            conditional_expression_information->condition = operand.ir;
            conditional_expression_information->temp = temp1;
            conditional_expression_information->end_jump = end_jump;
            conditional_expression_information->cast = cast;
            
            struct ast_stack_entry *entry = ast_stack_push(context, conditional_expression_kind, question_mark, &if_true);
            entry->other = conditional_expression_information;
            goto restart;
        }break;
        
        // Precedence 14
        case TOKEN_equals:
        case TOKEN_and_equals:
        case TOKEN_or_equals:
        case TOKEN_xor_equals:
        case TOKEN_plus_equals:
        case TOKEN_minus_equals:
        case TOKEN_left_shift_equals:
        case TOKEN_right_shift_equals:
        case TOKEN_div_equals:
        case TOKEN_mod_equals:
        case TOKEN_times_equals:{
            
            if(!context->in_lhs_expression){
                report_error(context, binary_expression, "Left is not assignable.");
                return operand;
            }
            
            if(operand.resolved_type->kind == AST_array_type){
                report_error(context, binary_expression, "Cannot assign to an array.");
                return operand;
            }
            
            if(operand.resolved_type->kind == AST_function_type){
                report_error(context, binary_expression, "Cannot assign to a function.");
                return operand;
            }
            
            if(operand.resolved_type == &globals.typedef_Bool && binary_expression->type != TOKEN_equals && binary_expression->type != TOKEN_xor_equals && binary_expression->type != TOKEN_or_equals && binary_expression->type != TOKEN_and_equals){
                report_error(context, binary_expression, "Only the operators '=', '|=', '&=' and '^=' are supported, when the left is of type _Bool.");
                return operand;
            }
            
            if(operand.resolved_type == &globals.typedef_void){
                report_error(context, binary_expression, "Cannot assign to something of type 'void'.");
                return operand;
            }
            
            switch(binary_expression->type){
                case TOKEN_equals:{
                    ast_stack_push(context, AST_assignment, binary_expression, &operand);
                }break;
                case TOKEN_and_equals:{
                    ast_stack_push(context, AST_and_assignment, binary_expression, &operand);
                }break;
                case TOKEN_or_equals:{
                    ast_stack_push(context, AST_or_assignment, binary_expression, &operand);
                }break;
                case TOKEN_xor_equals:{
                    ast_stack_push(context, AST_xor_assignment, binary_expression, &operand);
                }break;
                case TOKEN_plus_equals:{
                    ast_stack_push(context, AST_plus_assignment, binary_expression, &operand);
                }break;
                case TOKEN_minus_equals:{
                    ast_stack_push(context, AST_minus_assignment, binary_expression, &operand);
                }break;
                case TOKEN_left_shift_equals:{
                    ast_stack_push(context, AST_left_shift_assignment, binary_expression, &operand);
                }break;
                case TOKEN_right_shift_equals:{
                    ast_stack_push(context, AST_right_shift_assignment, binary_expression, &operand);
                }break;
                case TOKEN_div_equals:{
                    ast_stack_push(context, AST_divide_assignment, binary_expression, &operand);
                }break;
                case TOKEN_mod_equals:{
                    ast_stack_push(context, AST_modulo_assignment, binary_expression, &operand);
                }break;
                case TOKEN_times_equals:{
                    ast_stack_push(context, AST_times_assignment, binary_expression, &operand);
                }break;
                invalid_default_case();
            };
            
            goto restart;
        }break;
        
        // Precedence 15
        case TOKEN_comma:{
            if(should_skip_comma_expression){
                prev_token(context);
                break;
            }
            
            push_ir(context, IR_pop_expression);
            
            ast_stack_push(context, AST_comma_expression, binary_expression, &operand);
            goto restart;
        }break;
        
        // The 'binary_expression' token was not part of this expression.
        default: prev_token(context); break;
    }
    
    // Remove the artificial invalid ast we put on in the beginning.
    if(did_push){
        struct ast_stack_entry *guard_ast = ast_stack_pop(context);
        assert(guard_ast->ast_kind == AST_invalid);
    }
    
    // @note: This should never fire as we want to return early if there is an error deeper in the stack.
    assert(ast_stack_before == context->ast_stack_at);
    
    return operand;
}

func void check_and_set_declaration_specifier_flag(struct context *context, struct declaration_specifiers *specifiers, enum declaration_specifier_flag flag, struct token *site, char *error_name){
    if(specifiers->specifier_flags & flag){
        report_warning(context, WARNING_double_specifier, site, "Double '%s'.", error_name);
    }else{
        specifiers->specifier_flags |= flag;
    }
}

func struct declaration_specifiers parse_declaration_specifiers(struct context *context, struct ast_type *optional_type_specifier, enum ast_kind *optional_defined_type_specifier){
    
    struct declaration_specifiers specifiers = zero_struct;
    specifiers.type_specifier         = optional_type_specifier;
    specifiers.defined_type_specifier = optional_defined_type_specifier;
    
    if(maybe_report_error_for_stack_exhaustion(context, get_current_token_for_error_report(context), "Type nests too deep.")){
        specifiers.type_specifier = &globals.typedef_poison;
        specifiers.defined_type_specifier = 0;
        return specifiers;
    }
    
    enum{
        C_TYPE_none      = 0x0,
        C_TYPE_unsigned  = 0x1,
        C_TYPE_signed    = 0x2,
        C_TYPE_Bool      = 0x4,
        C_TYPE_char      = 0x8,
        C_TYPE_short     = 0x10,
        C_TYPE_int       = 0x20,
        C_TYPE_long      = 0x40,
        C_TYPE_long_long = 0x80,
        C_TYPE_int8      = 0x100,
        C_TYPE_int16     = 0x200,
        C_TYPE_int32     = 0x400,
        C_TYPE_int64     = 0x800,
        C_TYPE_float     = 0x1000,
        C_TYPE_double    = 0x2000,
        C_TYPE_void      = 0x4000,
    } c_type = 0;
    
    enum type_qualifiers type_qualifiers = 0;
    
    // 
    // declaration-specifiers:
    //     storage-class-specifier declaration-specifiers_opt
    //     type-specifier          declaration-specifiers_opt
    //     type-qualifier          declaration-specifiers_opt
    //     function-specifier      declaration-specifiers_opt
    //     alignment-specifier     declaration-specifiers_opt
    // 
    
    b32 should_break = false;
    while(in_current_token_array(context) && !should_break){
        struct token *token = next_token(context);
        
        switch(token->type){
            
            //
            // Storage class specifiers (typedef, extern, static, _Thread_local, auto, register)
            //
            case TOKEN_typedef:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_typedef, token, "typedef");
            }break;
            case TOKEN_extern:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_extern, token, "extern");
            }break;
            case TOKEN_static:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_static, token, "static");
            }break;
            case TOKEN_register:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_register, token, "register");
            }break;
            case TOKEN_thread_local:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_thread_local, token, "thread_local");
            }break;
            
            //
            // Function specifiers (inline, _Noreturn) @cleanup: these should only be allowed for functions
            //
            case TOKEN_noreturn:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_noreturn, token, "noreturn");
            }break;
            case TOKEN_inline:
            case TOKEN_forceinline:{
                check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_inline, token, "inline");
            }break;
            
            //
            // Type qualifiers (const, restrict, volatile, _Atomic)
            //
            case TOKEN_atomic:{
                type_qualifiers |= QUALIFIER_atomic;
                
                if(peek_token_eat(context, TOKEN_open_paren)){
                    struct type_info_return type_info = maybe_parse_type_for_cast_or_sizeof(context);
                    
                    if(!type_info.type){
                        report_error(context, get_current_token_for_error_report(context), "Expected a type after '_Atomic('.");
                        break;
                    }
                    
                    if(type_info.type->kind != AST_integer_type && type_info.type->kind != AST_pointer_type){
                        report_error(context, token, "_Atomic is currently only implemented for integer and pointer types.");
                    }
                    
                    expect_token(context, TOKEN_closed_paren, "Expected a ')' after '_Atomic(<...>'");
                    
                    if(specifiers.type_specifier || c_type != C_TYPE_none){
                        report_error(context, token, "Declaration specifies more than one data type.");
                    }
                    
                    // @note: The type will be adjust later on.
                    specifiers.type_specifier = type_info.type;
                    specifiers.defined_type_specifier = type_info.defined_type;
                }
            }break;
            
            case TOKEN_restrict:  type_qualifiers |= QUALIFIER_restrict;  break;
            case TOKEN_const:     type_qualifiers |= QUALIFIER_const;     break;
            case TOKEN_volatile:  type_qualifiers |= QUALIFIER_volatile;  break;
            case TOKEN_unaligned: type_qualifiers |= QUALIFIER_unaligned; break;
            
            // 
            // Alignment-specifiers
            // 
            case TOKEN_alignas:{
                expect_token(context, TOKEN_open_paren, "Expected '(' to follow '_Alignas'.");
                
                struct type_info_return type_name = maybe_parse_type_for_cast_or_sizeof(context);
                
                u64 value = 0;
                if(type_name.type){
                    value = type_name.type->alignment;
                }else{
                    struct expr const_expr = parse_constant_integer_expression(context, /*should_skip_comma_expression*/false, "Argument of '_Alignas' must be a type name or a constant expression.");
                    value = integer_literal_as_u64(&const_expr);
                }
                
                // "An alignment specification of zero has no effect."
                if(value != 0){
                    
                    if(!is_power_of_two(value)){
                        report_error(context, token, "Alignment must be a power of two.");
                        value = 1;
                    }
                    
                    if(specifiers.alignment){
                        // @cleanup: warn here!
                        // report_warning(context, WARNING_alignment_specified_more_then_once, "Alignment");
                        specifiers.alignment = max_of(specifiers.alignment, value);
                    }else{
                        specifiers.alignment = value;
                    }
                }
                
                expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of '_Alignas'.");
            }break;
            
            //
            // __declspec (storage-class specifier).
            //
            case TOKEN_declspec:{
                struct token *open  = expect_token(context, TOKEN_open_paren, "Expected '(' after '__declspec'.");
                struct token *directive = peek_token(context, TOKEN_restrict) ? next_token(context) : expect_token(context, TOKEN_identifier, "Expected a directive after '__declspec'.");
                
                b32 skip = false;
                
                struct atom directive_string = directive->atom;
                
                if(atoms_match(directive_string, globals.keyword_dllimport)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_dllimport, directive, "__declspec(dllimport)");
                }else if(atoms_match(directive_string, globals.keyword_dllexport)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_dllexport, directive, "__declspec(dllexport)");
                }else if(atoms_match(directive_string, globals.keyword_noreturn)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_noreturn, directive, "__declspec(noreturn)");
                }else if(atoms_match(directive_string, globals.keyword_noinline)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_noinline, directive, "__declspec(noinline)");
                }else if(atoms_match(directive_string, globals.keyword_align)){
                    expect_token(context, TOKEN_open_paren, "Expected '(' to follow 'align'.");
                    
                    struct expr const_expr = parse_constant_integer_expression(context, /*should_skip_comma_expression*/false, "Expected a constant expression in '__declspec(align(_))'.");
                    
                    u64 value = integer_literal_as_u64(&const_expr);
                    
                    //
                    // struct __declspec(align(0x100)) asd;
                    //
                    // @note: the difference between '__declspec(align(#)) struct asd {int asd; } asd;'
                    //        and                    'struct __declspec(align(#)) asd {int asd; } asd;'
                    // is whether all instances of 'struct asd' are #-aligned or just 'asd'.
                    //
                    
                    if(!is_power_of_two(value)){
                        report_error(context, directive, "Alignment must be a power of two.");
                        value = 1;
                    }
                    
                    if(specifiers.alignment){
                        // @cleanup: warn here!
                        // report_warning(context, WARNING_alignment_specified_more_then_once, "Alignment");
                        specifiers.alignment = max_of(specifiers.alignment, value);
                    }else{
                        specifiers.alignment = value;
                    }
                    
                    expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of 'align'.");
                }else if(atoms_match(directive_string, globals.keyword_selectany)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_selectany, directive, "__declspec(selectany)");
                }else if(atoms_match(directive_string, globals.keyword_inline_asm)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_inline_asm, directive, "__declspec(inline_asm)");
                }else if(atoms_match(directive_string, globals.keyword_packed)){
                    report_error(context, directive, "A declaration cannot be '__declspec(packed)'. Please declare a __declspec(packed)-type instead. (I.e please use 'struct __declspec(packed) packed_type' instead of '__declspec(packed) struct packed_type'.");
                }else if(atoms_match(directive_string, globals.keyword_printlike)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_printlike, directive, "__declspec(printlike)");
                }else if(atoms_match(directive_string, globals.keyword_thread)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_thread_local, directive, "__declspec(thread)");
                }else{
                    skip = true;
                    report_warning(context, WARNING_unsupported_declspec, directive, "Unsupported '__declspec' ignored.");
                    skip_until_tokens_are_balanced(context, open, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' in unsupported '__declspec'.");
                }
                
                if(!skip){
                    expect_token(context, TOKEN_closed_paren, "Expected ')' ending '__declspec'.");
                }
            }break;
            
#define case_c_type(type_name)                                \
case TOKEN_##type_name:{                                                 \
    if(c_type & C_TYPE_##type_name){                                     \
        report_syntax_error(context, token, "Double '%s'.", #type_name); \
    }else{                                                               \
        c_type |= C_TYPE_##type_name;                                    \
    }                                                                    \
}break;
            case_c_type(unsigned);
            case_c_type(signed);
            case_c_type(Bool);
            case_c_type(char);
            case_c_type(short);
            case_c_type(int);
            
            case TOKEN_long:{
                if(c_type & C_TYPE_long_long){
                    report_syntax_error(context, token, "Tripple 'long'.");
                }else if(c_type & C_TYPE_long){
                    c_type |= C_TYPE_long_long;
                }else{
                    c_type |= C_TYPE_long;
                }
            }break;
            
            case_c_type(int8);
            case_c_type(int16);
            case_c_type(int32);
            case_c_type(int64);
            case_c_type(float);
            case_c_type(double);
            case_c_type(void);
            
            case TOKEN_struct: case TOKEN_union:{
                if(specifiers.type_specifier || c_type != C_TYPE_none){
                    report_error(context, token, "Declaration specifies more than one data type.");
                }
                
                // 
                // :struct :parse_struct :union :parse_union
                // 
                
                b32 is_union = (token->type == TOKEN_union);
                
                u64 declspec_alignment = 1;
                b32 is_intrin_type = false;
                b32 is_packed = false;
                b32 has_declspec_alignment = false;
                while(peek_token(context, TOKEN_declspec)){
                    next_token(context);
                    struct token *open = expect_token(context, TOKEN_open_paren, "Expected '(' to follow '__declspec'.");
                    
                    struct token *declspec = expect_token(context, TOKEN_identifier, "Expected an identifier after '__declspec'.");
                    b32 unsupported = false;
                    
                    if(atoms_match(declspec->atom, globals.keyword_align)){
                        expect_token(context, TOKEN_open_paren, "Expected '(' to follow 'align'.");
                        
                        struct expr const_expr = parse_constant_integer_expression(context, /*should_skip_comma_expression*/false, "Expected a constant expression in '__declspec(align(_))'.");
                        
                        u64 value = integer_literal_as_u64(&const_expr);
                        
                        has_declspec_alignment = true;
                        
                        //
                        // struct __declspec(align(0x100)) asd;
                        //
                        // @note: the difference between '__declspec(align(#)) struct asd {int asd; } asd;'
                        //        and                    'struct __declspec(align(#)) asd {int asd; } asd;'
                        // is whether all instances of 'struct asd' are #-aligned or just 'asd'.
                        //
                        
                        if(!is_power_of_two(value)){
                            report_error(context, declspec, "Alignment must be a power of two.");
                            value = 1;
                        }
                        
                        if(declspec_alignment != 1){
                            // @cleanup: warning.
                            declspec_alignment = max_of(declspec_alignment, value);
                        }else{
                            declspec_alignment = value;
                        }
                        
                        expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of 'align'.");
                    }else if(atoms_match(declspec->atom, globals.keyword_intrin_type)){
                        is_intrin_type = true;
                    }else if(atoms_match(declspec->atom, globals.keyword_packed)){
                        is_packed = true;
                    }else{
                        unsupported = true;
                    }
                    
                    if(!unsupported){
                        expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of '__declspec'.");
                    }
                    
                    if(unsupported){
                        report_warning(context, WARNING_unsupported_declspec, declspec, "Unsupported '__declspec' ignored.");
                        skip_until_tokens_are_balanced(context, open, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' in unsupported '__declspec'.");
                    }
                }
                
                if(context->should_exit_statement) break;
                
                // 
                // struct-or-union specifier:
                //     struct-or-union identifier_opt { struct-declaration-list }
                //     struct-or-union identifier
                //     
                // struct-or-union:
                //     struct
                //     union
                // 
                
                struct token *name = peek_token_eat(context, TOKEN_identifier);
                if(!peek_token_eat(context, TOKEN_open_curly)){
                    // 
                    // We are in the 'struct-or-union identifier' case.
                    // 
                    
                    char *union_or_struct = is_union ? "union" : "struct";
                    enum ast_kind ast_kind = is_union ? AST_union : AST_struct;
                    
                    if(!name){
                        report_error(context, get_current_token_for_error_report(context), "Expected an identifier or '{' after '%s'.", union_or_struct);
                    }else{
                        struct ast_type *lookup = lookup_compound_type(context, name->atom);
                        
                        if(!lookup){
                            lookup = push_unresolved_type(context, name, ast_kind);
                        }else if(lookup->kind != ast_kind){
                            char *error = null;
                            if(lookup->kind == AST_union)  error = "a union";
                            if(lookup->kind == AST_struct) error = "a struct";
                            if(lookup->kind == AST_enum)   error = "an enum";
                            assert(error);
                            
                            report_error(context, name, "Got '%s %.*s', but '%.*s' is %s.", union_or_struct, name->size, name->data, name->size, name->data, error);
                        }
                        
                        specifiers.type_specifier         = lookup;
                        specifiers.defined_type_specifier = null;
                    }
                }else{
                    if(!context->sleeping_ident && name) context->sleeping_ident = name;
                    
                    struct ast_compound_type *compound;
                    if(is_union){
                        compound = parser_compound_type_push(context, union);
                    }else{
                        compound = parser_compound_type_push(context, struct);
                    }
                    
                    if(is_intrin_type) compound->base.flags |= TYPE_FLAG_is_intrin_type;
                    if(has_declspec_alignment) compound->base.flags |= TYPE_FLAG_is_user_aligned;
                    if(name){
                        compound->identifier = name;
                    }else{
                        struct token *hacky_token = push_struct(context->arena, struct token);
                        *hacky_token = *token;
                        hacky_token->atom = globals.unnamed_tag;
                        compound->identifier = hacky_token;
                    }   
                    
                    smm size = 0;
                    smm alignment = declspec_alignment;
                    
                    struct ast_type *current_bitfield_type = null;
                    s64 current_bitfield_index = 0;
                    
                    struct token *had_array_of_unknown_size = null;
                    
                    while(!peek_token_eat(context, TOKEN_closed_curly)){
                        
                        if(peek_token(context, TOKEN_semicolon)){
                            report_warning(context, WARNING_extraneous_semicolon, next_token(context), "Extraneous ';' in struct.");
                            continue;
                        }
                        
                        if(!is_union && had_array_of_unknown_size){
                            report_error(context, had_array_of_unknown_size, "Array of unknown size has to be the last member of struct.");
                        }
                        
                        // 
                        // struct-declaration-list:
                        //     struct-declaration
                        //     struct-declaration-list struct-declaration
                        //     
                        // struct-declaration:
                        //     specifier-qualifier-list struct-declarator-list_opt;
                        //     static_assert-declaration (@cleanup:)
                        // 
                        // specifier-qualifier-list:
                        //     type-specifier specifier-qualifier-list_opt
                        //     type-qualifier specifier-qualifier-list_opt
                        // 
                        
                        struct declaration_specifiers struct_specifiers = parse_declaration_specifiers(context, null, null);
                        struct ast_type *lhs_type    = struct_specifiers.type_specifier;
                        enum ast_kind *lhs_defined_type = struct_specifiers.defined_type_specifier;
                        
                        // @cleanup: Disallow extern typedef static register thread_local...
                        
                        // :unresolved_types
                        // 
                        // At this point, the lhs_type could be unresolved, but this does not necessarily 
                        // imply an error, because the declarator could (for example) be a pointer:
                        //     
                        //     struct unresolved *pointer_to_unresolved_type;
                        // 
                        
                        do{
                            // 
                            // struct-declarator-list:
                            //     struct-declarator
                            //     struct-declarator-list, struct-declarator
                            //     
                            // struct-declarator:
                            //     declarator
                            //     declarator_opt : constant-expression
                            //     
                            
                            struct declarator_return declarator = zero_struct;
                            if(peek_token(context, TOKEN_colon) || peek_token(context, TOKEN_semicolon)){
                                //
                                // This is here to handle 'struct{ int : 3; }', which is legal, 
                                // so you can skip some bits without having to name them.
                                // 
                                // This also handles struct inclusions like 'struct{ struct other_struct; }'.
                                //
                                declarator.ident = globals.invalid_identifier_token;
                                declarator.type  = lhs_type;
                                declarator.defined_type = lhs_defined_type;
                            }else{
                                declarator = parse_declarator(context, lhs_type, lhs_defined_type, DECLARATOR_identifier);
                            }
                            
                            struct token *ident = (declarator.ident != globals.invalid_identifier_token) ? declarator.ident : get_current_token(context);
                            
                            if(declarator.type->kind == AST_unresolved_type){
                                // :unresolved_types
                                // 
                                // At this point there is an unresolved struct member, e.g.:
                                //     struct unresolved unresolved;
                                //     union unresolved;
                                // Hence, the structure we are currently trying to parse
                                // needs to wait for the structure.
                                
                                sleep_or_error_on_unresolved_type(context, declarator.type);
                                declarator.type = &globals.typedef_s32;
                            }
                            
                            if(declarator.type == &globals.typedef_void){
                                report_error(context, ident, "Cannot declare a struct member with type void.");
                                declarator.type = &globals.typedef_s32;
                            }
                            
                            if(declarator.type->kind == AST_function_type){
                                report_error(context, ident, "Cannot declare a function in a struct.");
                                declarator.type = &globals.typedef_s32;
                            }
                            
                            if(declarator.type->flags & TYPE_FLAG_ends_in_array_of_unknown_size){
                                had_array_of_unknown_size = ident;
                                compound->base.flags |= TYPE_FLAG_ends_in_array_of_unknown_size;
                            }
                            
                            // :bitfields :parse_bitfields
                            b32 this_was_a_bitfield_type_that_fits = false;
                            if(peek_token(context, TOKEN_colon)){
                                struct token *colon = next_token(context);
                                
                                if(declarator.type->kind != AST_integer_type){
                                    report_error(context, colon, "Bitfield must be of integer type.");
                                }
                                
                                struct expr constant = parse_constant_integer_expression(context, true, "Bitfield width must be constant.");
                                s64 width = integer_literal_as_s64(&constant);
                                
                                if(width < 0){
                                    report_error(context, constant.token, "Bitfield width must be non-negative.");
                                    width = 0;
                                }
                                
                                // "As a special case, a bit-field struture member with a width of 0 indicates 
                                //  that no further bit-field is to be packed into the unit in which the previous 
                                //  bitfield, if any, was placed."
                                if(width == 0){
                                    current_bitfield_type = null;
                                    current_bitfield_index = 0;
                                    
                                    if(declarator.ident != globals.invalid_identifier_token){
                                        report_error(context, colon, "Named bitfield cannot have zero width.");
                                    }
                                    
                                    // If there is another thing after this continue the declarator loop. 
                                    if(peek_token(context, TOKEN_comma)) continue;
                                    
                                    // No more declarators.
                                    break;
                                }
                                
                                if(width > (declarator.type->size * 8)){
                                    struct string type_string = report_type_mismatch__internal(context, "", declarator.type, declarator.defined_type);
                                    report_error(context, colon, "Bitfield width for type '%.*s' must be '<= %lld', but is '%lld'.", type_string.size, type_string.data, (declarator.type->size * 8), width);
                                    width = 1;
                                }
                                
                                u64 bitfield_base = current_bitfield_index;
                                
                                if(current_bitfield_type && current_bitfield_type->size == declarator.type->size && current_bitfield_index + width <= declarator.type->size * 8){
                                    // 
                                    // If there was no prior named bitfield, it was not a bitfield that fits. This can happen for e.g.:
                                    //   
                                    //   struct { int : 4; int a: 4; };
                                    // 
                                    if(compound->amount_of_members) this_was_a_bitfield_type_that_fits = true;
                                    
                                    current_bitfield_index += width;
                                }else{
                                    bitfield_base = 0;
                                    current_bitfield_type  = declarator.type;
                                    current_bitfield_index = width;
                                }
                                
                                if(declarator.ident == globals.invalid_identifier_token){
                                    // 
                                    // If this was an anonymous bitfield, do not create a declaration for it.
                                    // This will cause it to get _skipped_ during initialization:
                                    // 
                                    //     struct{
                                    //         int a : 4;
                                    //         int   : 4;
                                    //         int b : 4;
                                    //     } bitfield = {1, 1};
                                    // 
                                    // Should have 'bitfield.a == 1' and 'bitfield.b == 1'.
                                    // 
                                    
                                    // If there is another thing after this continue the declarator loop. 
                                    if(peek_token(context, TOKEN_comma)) continue;
                                    
                                    // No more declarators.
                                    break;
                                }
                                
                                struct ast_bitfield_type *bitfield = parser_type_push(context, bitfield_type);
                                bitfield->width      = to_u32(width);
                                bitfield->bit_index  = to_u32(bitfield_base);
                                bitfield->base_type  = declarator.type;
                                bitfield->base.size  = declarator.type->size;
                                bitfield->base.alignment = declarator.type->alignment;
                                
                                declarator.type = &bitfield->base;
                            }else{
                                current_bitfield_type = null;
                                current_bitfield_index = 0;
                            }
                            
                            smm decl_alignment = declarator.type->alignment;
                            if(struct_specifiers.alignment){ 
                                decl_alignment = struct_specifiers.alignment; // Member specifiers are the most inner so they should take precedence.
                                compound->base.flags |= TYPE_FLAG_is_user_aligned;
                            }else if(is_packed){
                                decl_alignment = 1; // If the struct is __declspec(packed), overwrite the _natural_ alignment.
                            }else if(!(declarator.type->flags & TYPE_FLAG_is_user_aligned) && decl_alignment > context->pragma_alignment){
                                decl_alignment = context->pragma_alignment;
                            }
                            
                            smm offset_in_type;
                            
                            if(this_was_a_bitfield_type_that_fits){
                                // 
                                // Copy the offset from the previous bitfield type.
                                // 
                                assert(compound->members[compound->amount_of_members-1].type->kind == AST_bitfield_type);
                                offset_in_type = compound->members[compound->amount_of_members-1].offset_in_type;
                            }else if(is_union){
                                offset_in_type = 0;
                                size = max_of(declarator.type->size, size);
                            }else{
                                // 
                                // @note: first align up!
                                // 
                                size = align_up(size, decl_alignment);
                                
                                offset_in_type = size;
                                size += declarator.type->size;
                            }
                            
                            // The alignment is the strictest alignment for any of the members.
                            alignment = max_of(decl_alignment, alignment);
                            
                            if(!context->should_exit_statement){
                                
                                if(declarator.ident != globals.invalid_identifier_token){
                                    
                                    if(find_member_in_compound(compound, ident->atom)){
                                        report_error(context, ident, "Redeclaration of member '%.*s'.", ident->size, ident->data);
                                    }
                                    
                                    register_compound_member(context, compound, ident, declarator.type, offset_in_type, /*next_member_increment*/1);
                                }else if(declarator.type->kind != AST_struct && declarator.type->kind != AST_union){
                                    report_error(context, ident, "Declaration does not declare anything.");
                                }else{
                                    struct ast_compound_type *nested_compound = (struct ast_compound_type *)declarator.type;
                                    
                                    // :member_list_contains_both_linear_and_nested
                                    // 
                                    // First register the "anonymous" nested member, then each of the members of the substructure.
                                    register_compound_member(context, compound, declarator.ident, declarator.type, offset_in_type, nested_compound->amount_of_members + /*the nested compound itself*/1);
                                    
                                    for(u32 member_index = 0; member_index < nested_compound->amount_of_members; member_index++){
                                        struct compound_member *member = &nested_compound->members[member_index];
                                        
                                        if(member->name != globals.invalid_identifier_token){
                                            if(find_member_in_compound(compound, member->name->atom)){
                                                report_error(context, ident, "Redeclaration of member '%.*s'.", member->name->size, member->name->data);
                                                break;
                                            }
                                        }
                                        
                                        smm next_member_increment = (nested_compound->base.kind == AST_union) ? nested_compound->amount_of_members - member_index : 1;
                                        register_compound_member(context, compound, member->name, member->type, offset_in_type + member->offset_in_type, next_member_increment);
                                    }
                                }
                                
                            }
                        }while(peek_token_eat(context, TOKEN_comma));
                        
                        // If we should sleep, break such that we don't waste time parsing the rest
                        // of the structure, otherwise, try to parse the rest of the stucture.
                        if(context->should_sleep) break;
                        
                        if(expect_token(context, TOKEN_semicolon, "Expected ';' after member declaration.")->type != TOKEN_semicolon){
                            // 
                            // If we were not at the ';' Of this declaration, try to get there.
                            // This hopefully gets us in a good state again.
                            // 
                            while(in_current_token_array(context)){
                                struct token *test = next_token(context);
                                if(test->type == TOKEN_semicolon) break;
                                if(test->type == TOKEN_closed_curly){
                                    prev_token(context);
                                    break;
                                }
                            }
                            
                            // Ensure we leave the loop when we are at the end.
                            if(!in_current_token_array(context)) break;
                        }
                    }
                    
                    if(context->should_exit_statement){
                        compound->base.size      = -1;
                        compound->base.alignment = -1;
                    }else{
                        assert(size >= 0);
                        assert(is_power_of_two(alignment));
                        
                        // Align up the size at the very end.
                        // This adjusts cases like 
                        // struct {
                        //     u64 has_alignment_of_8;
                        //     u32 has_alignment_of_4;
                        // };
                        // This is supposed to be of size 16. 
                        
                        compound->base.size = (size + (alignment - 1)) & ~(alignment - 1); // @cleanup: overflow
                        compound->base.alignment = alignment;
                        if(name){
                            compound->compilation_unit = context->current_compilation_unit;
                            register_compound_type(context, &compound->base, compound->identifier);
                        }
                    }
                    
                    if(is_intrin_type){
                        // @incomplete: Check the type to be a known intrin type.
                    }
                    
                    specifiers.type_specifier = &compound->base;
                }
            }break;
            
            case TOKEN_enum:{
                if(specifiers.type_specifier || c_type != C_TYPE_none){
                    report_error(context, token, "Declaration specifies more than one data type.");
                }
                
                struct token *name = peek_token_eat(context, TOKEN_identifier);
                
                if(peek_token_eat(context, TOKEN_open_curly)){
                    struct ast_compound_type *ast_enum = parser_compound_type_push(context, enum);
                    
                    // "each enumerated type should be compatible with a signed or unsinged integer type. Implementation
                    //  defined but should be able to hold all of the members" -> we are lazy: int
                    ast_enum->base.size = 4;
                    ast_enum->base.alignment = 4;
                    if(name){
                        ast_enum->identifier = name;
                    }else{
                        struct token *hacky_token = push_struct(context->arena, struct token);
                        *hacky_token = *token;
                        hacky_token->atom = globals.unnamed_enum;
                        ast_enum->identifier = hacky_token;
                    }   
                    
                    if(!context->sleeping_ident && name) context->sleeping_ident = name;
                    
                    s64 current_value = 0;
                    do{
                        if(peek_token_eat(context, TOKEN_closed_curly)) goto enum_skip_curly;
                        
                        struct token *ident = expect_token(context, TOKEN_identifier, "Expected an identifier in enum declaration.");
                        
                        if(peek_token_eat(context, TOKEN_equals)){
                            struct expr const_expr = parse_constant_integer_expression(context, true, "Enum expression needs to be a constant integer.");
                            
                            current_value = integer_literal_as_s64(&const_expr);
                        }
                        
                        // @note: allow to u32_max, as enums are often used as flag-constants and it just does not matter
                        //        whether its s32 or u32.
                        
                        if(s32_min <= current_value && current_value <= s32_max){
                            // we are fine!
                        }else if(s32_max < current_value && current_value <= u32_max){
                            
                            // @cleanup: report a warning?
                            current_value = (s32)current_value;
                        }else{
                            report_error(context, token, "Enum members are ints, this member is outside of the range of the enum. Member is %lld range is [%d, %u].", current_value, s32_min, u32_max);
                        }
                        
                        
                        // "an identifier declared as an enumeration constant has type int".
                        struct ir_integer_literal *literal = push_struct(context->arena, struct ir_integer_literal);
                        literal->base.kind = IR_integer_literal;
                        literal->_s32 = (s32)current_value;
                        literal->type = &globals.typedef_s32;
                        
                        //
                        // the c-spec says: enum members are ints, and the enum is of a size that can hold all enum members
                        struct ast_declaration *decl = push_struct(context->arena, struct ast_declaration);
                        decl->kind = IR_declaration;
                        decl->identifier = ident;
                        decl->flags |= DECLARATION_FLAGS_is_enum_member;
                        decl->type = &ast_enum->base;
                        decl->assign_expr = (struct ir *)&literal->base;
                        decl->offset_on_stack = -1;
                        decl->compilation_unit = context->current_compilation_unit;
                        
                        decl = register_declaration(context, decl);
                        
                        register_compound_member(context, ast_enum, ident, &ast_enum->base, current_value, /*next_member_increment*/0);
                        
                        current_value++;
                    }while(peek_token_eat(context, TOKEN_comma));
                    
                    expect_token(context, TOKEN_closed_curly, "Expected '}' ending enum declaration or ',' to keep going.");
                    
                    enum_skip_curly:;
                    
                    if(name){
                        ast_enum->compilation_unit = context->current_compilation_unit;
                        register_compound_type(context, &ast_enum->base, ast_enum->identifier);
                    }
                    
                    specifiers.defined_type_specifier = &ast_enum->base.kind;
                    specifiers.type_specifier = &globals.typedef_s32;
                }else{ // no open curly
                    if(!name){
                        report_syntax_error(context, get_current_token_for_error_report(context), "Expected an identifier or '{' after 'enum'.");
                    }else{
                        struct ast_type *lookup = lookup_compound_type(context, name->atom);
                        
                        if(!lookup){
                            lookup = push_unresolved_type(context, name, AST_enum);
                        }else if(lookup->kind != AST_enum){
                            char *error = null;
                            if(lookup->kind == AST_union)  error = "a union";
                            if(lookup->kind == AST_struct) error = "a struct";
                            if(lookup->kind == AST_enum)   error = "an enum";
                            assert(error);
                            
                            report_error(context, name, "Got 'enum %.*s' but '%.*s' is %s.", name->size, name->data, name->size, name->data, error);
                        }
                        
                        specifiers.defined_type_specifier = &lookup->kind;
                        specifiers.type_specifier = &globals.typedef_s32;
                    }
                }
            }break;
            
            case TOKEN_identifier:{
                // 
                // Enable shadowing of typedefs.
                // 
                if(specifiers.type_specifier || c_type){
                    prev_token(context);
                    should_break = 1;
                    break;
                }
                
                struct ast_declaration *ast_typedef = lookup_typedef(context, context->current_compilation_unit, token, /*silent*/false);
                if(ast_typedef){
                    // 
                    // :unresolved_types
                    //
                    // If it is a typedef to an unresolved type, we just give back an unresolved type and its fine.
                    // This is treated the same way 'struct unresolved' would be.
                    
                    specifiers.defined_type_specifier = &ast_typedef->kind;
                    specifiers.type_specifier = ast_typedef->type;
                }else{
                    if(globals.compile_stage < COMPILE_STAGE_parse_function){
                        parser_sleep(context, token, SLEEP_on_decl);
                    }else{
                        report_error(context, token, "Undeclared identifier.");
                    }
                    
                    specifiers.type_specifier = &globals.typedef_poison;
                }
            }break;
            
            default:{
                prev_token(context);
                should_break = 1;
            }break;
        }
    }
    
    if(specifiers.type_specifier && c_type != C_TYPE_none){
        // @note: This can happen for 'size_t int' or something.
        report_error(context, get_current_token_for_error_report(context), "Declaration specifies more than one data type.");
    }
    
    if(!specifiers.type_specifier && c_type == C_TYPE_none){
        report_error(context, get_current_token_for_error_report(context), "Expected a type.");
    }
    
    if(context->should_exit_statement){
        specifiers.type_specifier = &globals.typedef_poison;
        specifiers.defined_type_specifier = null;
        return specifiers;
    }
    
    if(c_type != C_TYPE_none){
        
        switch((int)c_type){
            
            case C_TYPE_void: specifiers.type_specifier = &globals.typedef_void; break;
            
            case C_TYPE_Bool: specifiers.type_specifier = &globals.typedef_Bool; break;
            
            case C_TYPE_unsigned: specifiers.type_specifier = &globals.typedef_u32; break;
            case C_TYPE_signed:   specifiers.type_specifier = &globals.typedef_s32; break;
            case C_TYPE_char:     specifiers.type_specifier = &globals.typedef_s8; break;
            case C_TYPE_short:    specifiers.type_specifier = &globals.typedef_s16; break;
            case C_TYPE_int:      specifiers.type_specifier = &globals.typedef_s32; break;
            case C_TYPE_long:     specifiers.type_specifier = &globals.typedef_s32; break;
            
            case (C_TYPE_short | C_TYPE_int): specifiers.type_specifier = &globals.typedef_s16; break;
            case (C_TYPE_long  | C_TYPE_int): specifiers.type_specifier = &globals.typedef_s32; break;
            case (C_TYPE_long  | C_TYPE_long_long): specifiers.type_specifier = &globals.typedef_s64; break;
            case (C_TYPE_long  | C_TYPE_long_long | C_TYPE_int): specifiers.type_specifier = &globals.typedef_s64; break;
            
            case (C_TYPE_unsigned | C_TYPE_char):  specifiers.type_specifier = &globals.typedef_u8; break;
            case (C_TYPE_unsigned | C_TYPE_short): specifiers.type_specifier = &globals.typedef_u16; break;
            case (C_TYPE_unsigned | C_TYPE_int):   specifiers.type_specifier = &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_long):  specifiers.type_specifier = &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_long | C_TYPE_long_long): specifiers.type_specifier = &globals.typedef_u64; break;
            
            case (C_TYPE_unsigned | C_TYPE_short | C_TYPE_int): specifiers.type_specifier = &globals.typedef_u16; break;
            case (C_TYPE_unsigned | C_TYPE_long  | C_TYPE_int): specifiers.type_specifier = &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_long | C_TYPE_long_long | C_TYPE_int): specifiers.type_specifier = &globals.typedef_u64; break;
            
            case (C_TYPE_signed | C_TYPE_char):  specifiers.type_specifier = &globals.typedef_s8; break;
            case (C_TYPE_signed | C_TYPE_short): specifiers.type_specifier = &globals.typedef_s16; break;
            case (C_TYPE_signed | C_TYPE_int):   specifiers.type_specifier = &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_long):  specifiers.type_specifier = &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_long | C_TYPE_long_long): specifiers.type_specifier = &globals.typedef_s64; break;
            
            case (C_TYPE_signed | C_TYPE_short | C_TYPE_int): specifiers.type_specifier = &globals.typedef_s16; break;
            case (C_TYPE_signed | C_TYPE_long  | C_TYPE_int): specifiers.type_specifier = &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_long | C_TYPE_long_long | C_TYPE_int): specifiers.type_specifier = &globals.typedef_s64; break;
            
            case C_TYPE_int8:  specifiers.type_specifier = &globals.typedef_s8; break;
            case C_TYPE_int16: specifiers.type_specifier = &globals.typedef_s16; break;
            case C_TYPE_int32: specifiers.type_specifier = &globals.typedef_s32; break;
            case C_TYPE_int64: specifiers.type_specifier = &globals.typedef_s64; break;
            
            case (C_TYPE_signed | C_TYPE_int8):  specifiers.type_specifier = &globals.typedef_s8; break;
            case (C_TYPE_signed | C_TYPE_int16): specifiers.type_specifier = &globals.typedef_s16; break;
            case (C_TYPE_signed | C_TYPE_int32): specifiers.type_specifier = &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_int64): specifiers.type_specifier = &globals.typedef_s64; break;
            
            case (C_TYPE_unsigned | C_TYPE_int8):  specifiers.type_specifier = &globals.typedef_u8; break;
            case (C_TYPE_unsigned | C_TYPE_int16): specifiers.type_specifier = &globals.typedef_u16; break;
            case (C_TYPE_unsigned | C_TYPE_int32): specifiers.type_specifier = &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_int64): specifiers.type_specifier = &globals.typedef_u64; break;
            
            case C_TYPE_float:    specifiers.type_specifier = &globals.typedef_f32; break;
            case C_TYPE_double:   specifiers.type_specifier = &globals.typedef_f64; break;
            case (C_TYPE_long | C_TYPE_double): specifiers.type_specifier = &globals.typedef_f64; break;
            
            default:{
                prev_token(context); // This token is valid, as we just now got a c type.
                report_error(context, next_token(context), "Invalid combination of base types.");
                specifiers.type_specifier = &globals.typedef_poison;
            }break;
        }
    }
    
    if(type_qualifiers & QUALIFIER_atomic){
        
        if(specifiers.type_specifier->kind != AST_integer_type && specifiers.type_specifier->kind != AST_pointer_type){
            report_error(context, get_current_token_for_error_report(context), "_Atomic is currently only implemented for integer and pointer types.");
            specifiers.type_specifier = &globals.typedef_poison;
        }else{
            if(specifiers.type_specifier->kind == AST_integer_type){
                // :translate_atomic_to_non_atomic_and_back
                specifiers.type_specifier = specifiers.type_specifier + (&globals.typedef_atomic_bool - &globals.typedef_Bool);
            }else{
                // Atomic pointer type
                struct ast_pointer_type *copied_pointer = push_struct(context->arena, struct ast_pointer_type);
                *copied_pointer = *(struct ast_pointer_type *)specifiers.type_specifier;
                copied_pointer->base.flags |= TYPE_FLAG_is_atomic;
                specifiers.type_specifier = &copied_pointer->base;
                specifiers.defined_type_specifier = null;
            }
        }
    }
    
    if(specifiers.specifier_flags & SPECIFIER_inline){
        if(specifiers.specifier_flags & SPECIFIER_extern){
            // 
            // extern inline acts as select any.
            // 
            specifiers.specifier_flags |= SPECIFIER_selectany;
        }else{
            //
            // If not explicitly extern, inline implies static.
            //
            specifiers.specifier_flags |= SPECIFIER_static;
        }   
    }
    
    return specifiers;
}

func struct declaration_list parse_declaration_list(struct context *context, struct ast_type *type_specifier, enum ast_kind *defined_type_specifier){
    
    struct declaration_list ret = zero_struct;
    
    // @cleanup: explain this again
    b32 should_set_sleeping_ident = !context->sleeping_ident;
    
    struct declaration_specifiers specifiers = parse_declaration_specifiers(context, type_specifier, defined_type_specifier);
    if(context->should_exit_statement) goto end;
    
    // :unresolved_types
    //
    // The 'specifiers.type_specifier' might be an unresolved type, i.e
    //     'struct unresolved', 'union unresolved' or 'enum unresolved'.
    // If this is the case this might be fine, if the declarator makes them
    // into a pointer type, or the return type of a function or something like that.
    // It is also fine for typedefs.
    // If the unresolved type turn out to be the type of a declaration in the end, we have to error.
    //                                                                                   07.02.2023
    
    ret.defined_type_specifier = specifiers.defined_type_specifier;
    ret.type_specifier         = specifiers.type_specifier;
    if(peek_token(context, TOKEN_semicolon)){
        // :unresolved_types
        // 
        // This codepath mostly happens for predeclarations like 'struct unresolved;'
        // We could think about saving the information _that_ the type was declared, to 
        // provide a better error message, when it is used.
        goto end; // @cleanup: the 'does not declare anything' check should be here.
    }
    
    do{
        struct declarator_return declarator = parse_declarator(context, ret.type_specifier, ret.defined_type_specifier, DECLARATOR_identifier);
        if(should_set_sleeping_ident) context->sleeping_ident = declarator.ident;
        
        if(context->should_exit_statement) goto end;
        
        if(context->current_scope && declarator.type->kind == AST_function_type && !(specifiers.specifier_flags & SPECIFIER_typedef)){
            // @cleanup: this feels really hacky.
            //           The use case here is that a function declaration at local scope, e.g.
            //               int asd();
            //           has to be extern, and thus take the 'is_extern' code path below.
            //           While this is not true for a normal variable. (lookup exactly what the c-spec says here).
            //           MSVC does not seem to care whether or not the local declaration is static.
            //               int main(){
            //                   static int asd();
            //                   asd();
            //               }
            //               int asd(){}
            //           works just fine.
            //           Thus we will choose to use the 'is_extern' code path below, whenever the function
            //           is not a local function definition.
            //           This is sort of unclean.                                        -07.08.2021
            if(!peek_token(context, TOKEN_open_curly)){
                specifiers.specifier_flags |= SPECIFIER_extern;
            }
        }
        
        if(!context->current_scope){
            // If we are at global scope, look in the 'token_to_is_static' table, if this identifier is
            // actually defined as static later on (or previously) in the file.
            // We overwrite the usual is static.                                         -20.11.2020
            if(declarator.ident != globals.invalid_identifier_token){
                if(compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(context->current_compilation_unit, declarator.ident->atom) == IDENTIFIER_is_static){
                    specifiers.specifier_flags |= SPECIFIER_static;
                }
            }
        }
        
        b32 should_break = false;
        struct ast_declaration *decl;
        
        if(context->current_scope && (specifiers.specifier_flags & SPECIFIER_extern) && !(specifiers.specifier_flags & SPECIFIER_dllimport)){
            
            // 
            // An extern variable at local scope has to exist at global scope, or this is an error.
            // This handles the cases
            //     extern int asd;
            //     int asd();
            // We know all the global declarations at this point, as we are at local scope and thus we
            // are at 'COMPILE_STAGE_parse_function', meaning we parsed all global scope entries.
            //                                                                             07.08.2021
            
            struct ast_table *table = compilation_unit_get_declaration_table_for_ident(context->current_compilation_unit, declarator.ident->atom);
            decl = (struct ast_declaration *)ast_table_get(table, declarator.ident->atom);
            
            if(!decl){
                // Push the declaration so we can continue as if it was defined.
                decl = push_declaration_for_declarator(context, declarator);
                if(declarator.type->kind == AST_function_type) decl->kind = IR_function;
                decl->flags |= DECLARATION_FLAGS_is_global;
                
                if(globals.output_file_type == OUTPUT_FILE_obj){
                    // If we are compiling to an object, this is fine. 
                    // The declaration is just an unresolved external in the end.
                    struct ast_declaration *redecl = (struct ast_declaration *)ast_table_add_or_return_previous_entry(&globals.external_declarations_at_function_scope, &decl->kind, declarator.ident);
                    if(redecl) decl = redecl;
                }else{
                    report_error(context, declarator.ident, "This 'extern' variable was never defined.");
                }
            }
            
            if(!types_are_equal(decl->type, declarator.type)){
                begin_error_report(context);
                report_error(context, declarator.ident, "'extern' declaration inside function has different type from original definition.");
                report_error(context, decl->identifier, "... Here is the original definition.");
                end_error_report(context);
            }
            
            // @cleanup: Declaration specifiers.
            
            // We have to add the declaration here for a function like:
            //        int decl = 1337;
            //        int main(int argc, char *argv[]){
            //            int decl = 0;
            //            {
            //                extern int decl;
            //                return decl;
            //            }
            //        }
            //    Should return '1337' and not '0', the 'extern int decl' shadows 'int decl = 0;'.
            //                                                                      - 03.10.2021
            parser_register_declaration_in_scope(context, context->current_scope, decl);
            
        }else if(!(specifiers.specifier_flags & SPECIFIER_typedef) && declarator.type->kind == AST_function_type){
            
            // 
            // This is a function declaration or definition at global or local scope.
            // 
            
            struct ast_function *function = push_struct(context->arena, struct ast_function);
            function->kind = IR_function;
            function->type = cast(struct ast_function_type *)declarator.type;
            function->identifier = declarator.ident;
            function->memory_location = null;
            function->offset_in_text_section = -1; // Set atomically when done emitting.
            function->compilation_unit = context->current_compilation_unit;
            
            if(!context->current_scope) function->as_decl.flags |= DECLARATION_FLAGS_is_global;
            
            if(specifiers.specifier_flags & SPECIFIER_static)    function->as_decl.flags |= DECLARATION_FLAGS_is_static;
            if(specifiers.specifier_flags & SPECIFIER_selectany) function->as_decl.flags |= DECLARATION_FLAGS_is_selectany;
            
            if(specifiers.specifier_flags & SPECIFIER_inline_asm){
                
                // @cleanup: dllexport and such
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_varargs){
                    report_error(context, function->identifier, "A function declared '__declspec(inline_asm)' cannot be varargs.\n");
                }
                
                function->type->flags |= FUNCTION_TYPE_FLAGS_is_inline_asm;
            }
            
            if(specifiers.specifier_flags & SPECIFIER_noreturn)  function->type->flags |= FUNCTION_TYPE_FLAGS_is_noreturn;
            
            if(specifiers.specifier_flags & SPECIFIER_dllexport){
                if(specifiers.specifier_flags & SPECIFIER_static){
                    // :error
                    report_error(context, function->identifier, "Cannot export static function.");
                    goto end;
                }
                
                // @cleanup: dllexport function in another function? And dllexport declaration, but not definition in another function?
                
                function->as_decl.flags |= DECLARATION_FLAGS_is_dllexport;
            }
            
            if(specifiers.alignment > 0){
                report_warning(context, WARNING_function_alignment, function->identifier, "'__declspec(align(_))' is ignored for functions.");
            }
            
            if(specifiers.specifier_flags & SPECIFIER_dllimport){
                if(specifiers.specifier_flags & SPECIFIER_static){
                    report_error(context, function->identifier, "Cannot '__declspec(dllimport)' a static function.");
                    goto end;
                }
                
                // We check that this declaration is actually contained in some dll only for the ones 
                // we actually use. This is done in 'explain.c'.
                function->as_decl.flags |= DECLARATION_FLAGS_is_dllimport;
                
                if(context->current_scope){
                    // @incomplete: For now disallow __declspec(dllimport) at local scope.
                    report_error(context, function->identifier, "@incomplete: Currently, __declspec(dllimport) is not allowed inside a function.");
                }
            }
            
            if(specifiers.specifier_flags & SPECIFIER_printlike){
                //
                // Validate that this 'printlike' function is varargs and has a format string as the 
                // last named argument.
                //
                struct ast_function_type *function_type = function->type;
                
                if(!(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs)){
                    report_error(context, function->identifier, "'__declspec(printlike)' function needs to be varargs.");
                    goto end;
                }
                
                int error = 0;
                
                if(function_type->argument_list.count == 0){
                    error = 1;
                }else{
                    struct ast_declaration *last_argument = (struct ast_declaration *)function_type->argument_list.last->value;
                    struct ast_type *argument_type = last_argument->type;
                    
                    if(argument_type->kind != AST_pointer_type){
                        error = 1;
                    }else{
                        struct ast_pointer_type *pointer = (struct ast_pointer_type *)argument_type;
                        if(pointer->pointer_to != &globals.typedef_s8) error = 1;
                    }
                }
                
                if(error){
                    report_error(context, function->identifier, "'__declspec(printlike)' function needs to have an argument of type 'char *' as its last named argument.");
                    goto end;
                }
                
                function_type->flags |= FUNCTION_TYPE_FLAGS_is_printlike;
            }
            
            struct ast_scope *scope = null;
            if(peek_token(context, TOKEN_open_curly)){
                
                if(specifiers.specifier_flags & SPECIFIER_dllimport){
                    report_error(context, function->identifier, "Cannot define a function that is declared '__declspec(dllimport)'.");
                    goto end;
                }
                
                if(!sll_is_empty(ret)){
                    // :Error
                    report_error(context, function->identifier, "Cannot define a function in a compound declaration list.");
                    return ret;
                }
                
                scope = parser_push_new_scope(context, get_current_token(context), SCOPE_FLAG_is_function_scope);
                parser_scope_pop(context, scope);
                
                if(context->current_scope){
                    struct token *open = next_token(context);
                    
                    // @cleanup: disallow some declaration specifiers.
                    
                    smm begin_marker = context->token_at;
                    
                    if(!skip_until_tokens_are_balanced(context, open, TOKEN_open_curly, TOKEN_closed_curly, "Unmatched '{'.")){
                        goto end;
                    }
                    smm end_marker = context->token_at;
                    
                    struct token_array array = {
                        .data = context->tokens.data + begin_marker,
                        .size = end_marker - begin_marker,
                    };
                    
                    struct parse_work *parse_work = push_parse_work(context, array, context->current_compilation_unit, function);
                    work_queue_push_work(context, &globals.work_queue_parse_functions, parse_work);
                    
                    function->as_decl.flags |= DECLARATION_FLAGS_is_local_persist;
                    function->scope = scope;
                    ast_list_append(&context->local_functions, context->arena, &function->kind);
                }
                
                should_break = true; // disallow int a(){}, b;
            }
            
            // @note: After this, 'decl' might not be 'function', 
            //        because there might have been a prior declaration.
            decl = register_declaration(context, &function->as_decl);
            if(context->should_sleep) goto end; // @cleanup: should_sleep?
            
            // @cleanup: We have to validate here that the declarations `FUNCTION_TYPE_flags` and declaration flags match.
            
            if(!context->current_scope && scope){
                // This is a global declaration and we define it!
                
                int we_have_defined_the_function = parser_register_definition(context, decl, (void *)scope, scope->token, declarator.type);
                
                if(!we_have_defined_the_function){
                    // If we have not defined the function, we are either `__declspec(selectany) or there was an error. 
                    // In either case, we return the old 'function' instead of the global 'decl' that we got back.
                    // The other option here would be to set a flag and not even parse the function, 
                    // but for now I am will parse both copies.
                    //                                                                              - 19.10.2024
                    function->scope = scope;
                    decl = &function->as_decl;
                }else if(specifiers.specifier_flags & SPECIFIER_dllexport){
                    add_global_reference_for_declaration(context->arena, &function->as_decl);
                }
            }
        }else{
            
            if(!(specifiers.specifier_flags & SPECIFIER_typedef) && declarator.type == &globals.typedef_void){
                report_error(context, declarator.ident, "Cannot declare a variable of type void.");
                goto end;
            }
            
            if(specifiers.specifier_flags & SPECIFIER_inline){
                report_error(context, declarator.ident, "Data declaration cannot be marked 'inline'.");
                goto end;
            }
            
            if(specifiers.specifier_flags & SPECIFIER_noreturn){
                report_error(context, declarator.ident, "Data declaration cannot be marked '_Noreturn'.");
                goto end;
            }
            
            if(specifiers.specifier_flags & SPECIFIER_static){
                if(specifiers.specifier_flags & SPECIFIER_dllexport){
                    report_error(context, declarator.ident, "Variable declared '__declspec(dllexport)' cannot also be declared 'static'.");
                    goto end;
                }
                
                if(specifiers.specifier_flags & SPECIFIER_dllimport){
                    report_error(context, declarator.ident, "Variable declared '__declspec(dllimport)' cannot also be declared 'static'.");
                    goto end;
                }
                
                if(specifiers.specifier_flags & SPECIFIER_extern){
                    report_error(context, declarator.ident, "Variable declared 'extern' cannot also be declared 'static'.");
                    goto end;
                }
            }
            
            if(context->current_scope && (specifiers.specifier_flags & SPECIFIER_dllexport) && !(specifiers.specifier_flags & SPECIFIER_extern)){
                report_error(context, declarator.ident, "Cannot define a variable declared '__declspec(dllexport)' inside of a function. Did you forget to add 'extern'?");
                goto end;
            }
            
            decl = push_declaration_for_declarator(context, declarator);
            if(specifiers.specifier_flags & SPECIFIER_typedef) decl->kind = IR_typedef;
            
            if(specifiers.alignment > 0) decl->overwrite_alignment = specifiers.alignment;
            
            if(specifiers.specifier_flags & SPECIFIER_static){ // @note: flag before registering
                if(context->current_scope){
                    ast_list_append(&context->current_function->static_variables, context->arena, &decl->kind);
                    decl->flags |= DECLARATION_FLAGS_is_local_persist;
                }else{
                    decl->flags |= DECLARATION_FLAGS_is_static;
                }
            }
            
            if(specifiers.specifier_flags & SPECIFIER_extern)    decl->flags |= DECLARATION_FLAGS_is_extern;
            if(specifiers.specifier_flags & SPECIFIER_selectany) decl->flags |= DECLARATION_FLAGS_is_selectany;
            
            if(specifiers.specifier_flags & SPECIFIER_dllimport) decl->flags |= DECLARATION_FLAGS_is_dllimport;
            if(specifiers.specifier_flags & SPECIFIER_dllexport) decl->flags |= DECLARATION_FLAGS_is_dllexport;
            
            if(specifiers.specifier_flags & SPECIFIER_thread_local) decl->flags |= DECLARATION_FLAGS_is_thread_local;
            
            if(!context->current_scope) decl->flags |= DECLARATION_FLAGS_is_global;
            
            decl->compilation_unit = context->current_compilation_unit;
            
            // If the declaration is not explicitly extern, it cannot have unresolved type.
            if(!(specifiers.specifier_flags & SPECIFIER_extern) && !(specifiers.specifier_flags & SPECIFIER_typedef)){
                if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) goto end;
            }
            
            if(context->current_scope && specifiers.specifier_flags & SPECIFIER_dllimport){
                // @incomplete: For now disallow __declspec(dllimport) at local scope.
                report_error(context, decl->identifier, "@incomplete: Currently, __declspec(dllimport) is not allowed inside a function.");
            }
            
            // 
            // Register it up here already in case of 'void *asd = &asd;'
            // this means that for right now the declaration is 'not initialized' even if it has an
            // initializer. If we want to propagate const declarations, we might want to think about this.
            //                                                     -20.11.2020
            
            // 
            // We assign to 'decl' here, and potentially replace it with a previous one, such that
            //    int a;
            //    int a = 1;
            // works (we would otherwise not call 'evaluate_initializer' on the right decl).
            // 
            decl = register_declaration(context, decl);
            if(context->should_exit_statement) goto end;
            
            if(peek_token(context, TOKEN_equals)){
                struct token *equals = next_token(context);
                
                if(specifiers.specifier_flags & SPECIFIER_typedef){
                    // :Error
                    report_error(context, equals, "Cannot initialize a typedef.");
                    goto end;
                }
                
                // If the declaration has an initializer, it cannot have unresolved type.
                if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) goto end;
                
                if(specifiers.specifier_flags & SPECIFIER_dllimport){
                    report_error(context, declarator.ident, "Variable declared '__declspec(dllimport)' cannot be initialized.");
                    goto end;
                }
                
                if(context->current_scope && (specifiers.specifier_flags & SPECIFIER_extern)){
                    // "If the declaration of an identifier has block scope, and the identifier has external or internal linkage, 
                    //  then declaration shall have no initializer for the identifier."
                    report_error(context, equals, "Cannot initialize an 'extern' variable inside of a function.");
                    goto end;
                }
                
                if(specifiers.specifier_flags & SPECIFIER_dllexport){
                    add_global_reference_for_declaration(context->arena, decl);
                }
                
                // :DeclarationReferenceThreadingBug
                context->current_declaration = decl;
                
                parse_initializer(context, decl, equals);
                if(context->should_exit_statement) goto end;
            }
            
            if(!(specifiers.specifier_flags & SPECIFIER_typedef) && !(decl->flags & DECLARATION_FLAGS_is_local_persist) && !(decl->flags & DECLARATION_FLAGS_is_global)){
                parser_emit_memory_location(context, decl);
            }
        }
        
        // This is in arena, as we now have 'ast_declaration_list', maybe this is not necessary, 
        // as we dont use 'ast_declaration_list' at global scope. @leak @cleanup: Is this comment still accurate?
        struct declaration_node *node = push_uninitialized_struct(context->arena, struct declaration_node);
        node->decl = decl;
        sll_push_back(ret, node);
        
        // Don't allow int a(){}, b(){} because that's stupid.
        if(should_break) break;
    }while(peek_token_eat(context, TOKEN_comma));
    
    end:;
    
    return ret;
}

#include "parse_asm_block.c"

void maybe_report_warning_for_assignment_in_condition(struct context *context, struct ir *condition, struct token *token){
    (void)context, (void)condition, (void)token;
    #if 0 
    if(condition->kind == AST_assignment){
        report_warning(context, WARNING_assignment_in_condition, token, "Assignment withing condition, did you mean '=='?");
    }
    #endif
}

func struct ast *parse_imperative_scope(struct context *context);

void parse_static_assert(struct context *context, struct token *static_assert_token){
    
    expect_token(context, TOKEN_open_paren, "Expected a '(' after _Static_assert.");
    struct expr constant_expression = parse_constant_integer_expression(context, /*should_skip_comma_expression*/true, "First argument to '_Static_assert' is not constant.");
    
    s64 value = integer_literal_as_s64(&constant_expression);
    
    struct string string_literal = {0};
    
    if(peek_token_eat(context, TOKEN_comma)){
        struct expr string_literal_expression = parse_expression(context, /*should_skip_comma_expression*/true);
        if(string_literal_expression.ir->kind != IR_string_literal){
            report_error(context, string_literal_expression.token, "Second argument to _Static_assert needs to be a string literal.");
        }else{
            struct ir_string_literal *ir_string_literal = (struct ir_string_literal *)string_literal_expression.ir;
            string_literal = ir_string_literal->value;
            pop_from_ir_arena(context, ir_string_literal);
        }
    }
    
    if(!context->error){
        if(value == 0){
            if(string_literal.data && string_literal.size){
                // @cleanup: wide characters?
                report_error(context, static_assert_token, "%.*s", string_literal.size, string_literal.data);
            }else{
                report_error(context, static_assert_token, "_Static_assert fired.");
            }
        }
    }
    
    expect_token(context, TOKEN_closed_paren, "Expected a ')' at the end of _Static_assert.");
}

func void parse_statement(struct context *context){
    
    struct token *initial_token = next_token(context); // Returns the current token. Do this before the stack exhaustion check to "make progress".
    
    if(maybe_report_error_for_stack_exhaustion(context, get_current_token_for_error_report(context), "Scoping nests to deep.")) return;
    
    b32 needs_semicolon = true;
    
    function_maybe_add_line_information(context, initial_token);
    
    switch(initial_token->type){
        case TOKEN_semicolon:{
            needs_semicolon = false;
        }break;
        case TOKEN_if:{
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'if'.");
            
            // Allocate an _if-false_ label that parse_expression can use.
            smm if_false_label = /*context->if_false_label = */context->jump_label_index++;
            
            struct expr condition = parse_expression(context, false);
            
            maybe_report_warning_for_assignment_in_condition(context, condition.ir, condition.token);
            maybe_load_address_for_array_or_function(context, IR_load_address, &condition);
            maybe_insert_cast_from_special_int_to_int(context, &condition, /*is_lhs*/0);
            if(!casts_implicitly_to_bool(condition.resolved_type)){
                // :Error
                report_error(context, condition.token, "'if' condition has to cast to bool.");
                
            }
            expect_token(context, TOKEN_closed_paren, "Expected ')' ending 'if' condition.");
            
            // Get the initial "returns a value state".
            int statement_returns_a_value = context->current_statement_returns_a_value;
            
            // :ir_refactor - new
            struct ir_jump_node *condition_jump = push_jump(context, IR_jump_if_false);
            condition_jump->label_number = if_false_label;
            
            // if(context->if_true_label != -1){
            //     struct ast_jump_label *if_true_label = push_expression(context, get_current_token_for_error_report(context), jump_label);
            //     if_true_label->label_number = context->if_true_label;
            // }
            
            parse_statement(context);
            
            // Check if the "if" part returns a value and reset the to the old state.
            int if_statement_returns_a_value = context->current_statement_returns_a_value;
            context->current_statement_returns_a_value = statement_returns_a_value;
            
            if(peek_token_eat(context, TOKEN_else)){
                
                struct ir_jump_node *jump_over_else = push_jump(context, IR_jump);
                jump_over_else->label_number = context->jump_label_index++;
                
                struct ir_jump_node *else_label = push_jump(context, IR_jump_label);
                else_label->label_number = if_false_label;
                
                parse_statement(context);
                
                struct ir_jump_node *end_label = push_jump(context, IR_jump_label);
                end_label->label_number = jump_over_else->label_number;
                
                
                // Check if the "else" part returns a value and we now return a value, if we returned a value before, or both sides returned a value.
                int else_statement_returns_a_value = context->current_statement_returns_a_value;
                context->current_statement_returns_a_value = statement_returns_a_value | (else_statement_returns_a_value & if_statement_returns_a_value);
            }else{
                struct ir_jump_node *end_label = push_jump(context, IR_jump_label);
                end_label->label_number = if_false_label;
            }
            
            needs_semicolon = false;
        }break;
        case TOKEN_for:{
            // @note: the 'comma separated lists' you see in for loops are actually just the comma operator
            
            expect_token(context, TOKEN_open_paren, "Expected '(' after 'for'.");
            
            // Get the initial "returns a value state".
            int statement_returns_a_value = context->current_statement_returns_a_value;
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            
            struct expr condition;
            {
                if(!peek_token_eat(context, TOKEN_semicolon)){
                    // @hack: @cleanup: this should be either a declaration or an expression, for now any statement is valid....
                    parse_statement(context);
                }
                
                // 
                // We delay parsing the 'post condition' to after we have parsed the body of the for.
                // This is because we want to emit the for to look like:
                // 
                //    declaration
                //  .for:
                //      condition
                //      cond-jump .break
                //      body
                //  .continue:
                //      increment
                //    jump .for
                //  .break:
                // 
                
                smm for_label_index      = context->jump_label_index++;
                smm continue_label_index = context->jump_label_index++;
                smm break_label_index    = context->jump_label_index++;
                
                smm old_continue_label_index = context->current_continue_label;
                smm old_break_label_index    = context->current_break_label;
                context->current_continue_label = continue_label_index;
                context->current_break_label    = break_label_index;
                
                struct ir_jump_node *for_label = push_jump(context, IR_jump_label);
                for_label->label_number = for_label_index;
                
                if(peek_token(context, TOKEN_semicolon)){
                    condition = ast_push_s32_literal(context, 1, next_token(context)); // desugars to true
                }else{
                    condition = parse_expression(context, false);
                    maybe_report_warning_for_assignment_in_condition(context, condition.ir, condition.token);
                    
                    maybe_load_address_for_array_or_function(context, IR_load_address, &condition);
                    maybe_insert_cast_from_special_int_to_int(context, &condition, /*is_lhs*/0);
                    if(!casts_implicitly_to_bool(condition.resolved_type)){
                        // :Error
                        report_error(context, condition.token, "'for' condition has to cast to bool.");
                        parser_scope_pop(context, scope);
                        return;
                    }
                    expect_token(context, TOKEN_semicolon, "Expected ';' after 'for'-condition.");
                }
                
                // :ir_refactor - new
                struct ir_jump_node *condition_jump = push_jump(context, IR_jump_if_false);
                condition_jump->label_number = break_label_index;
                
                smm increment_token_at = -1;
                
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    increment_token_at = context->token_at;
                    skip_until_tokens_are_balanced(context, get_current_token(context), TOKEN_open_paren, TOKEN_closed_paren, "Expected ')' at the end of 'for'.");
                }
                
                parse_statement(context);
                
                struct ir_jump_node *continue_label = push_jump(context, IR_jump_label);
                continue_label->label_number = continue_label_index;
                
                if(increment_token_at != -1){
                    smm past_body = context->token_at;
                    context->token_at = increment_token_at;
                    
                    parse_expression(context, false);
                    
                    // Because of how expressions work, this is also where skip_until_tokens_are_balanced moved us.
                    expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of 'for'.");
                    
                    push_ir(context, IR_pop_expression);
                    
                    context->token_at = past_body;
                }
                
                struct ir_jump_node *loop_jump = push_jump(context, IR_jump);
                loop_jump->label_number = for_label_index;
                
                struct ir_jump_node *break_label = push_jump(context, IR_jump_label);
                break_label->label_number = break_label_index;
                
                context->current_continue_label = old_continue_label_index;
                context->current_break_label    = old_break_label_index;
            }
            parser_scope_pop(context, scope);
            
            // This is an infinite loop, if we found no alive break and the condition is constant != 0.
            int is_infinite_loop = ((scope->flags & SCOPE_FLAG_found_an_alive_break) == 0) && condition.ir->kind == IR_integer_literal && integer_literal_as_u64(&condition) != 0;
            
            // @note: For a loop line `for(int i = 0; i < n; i++){ return i; }` The "returns a value" would be true, 
            // but we don't know if 'i < n', so we disregard the loops "returns a value".
            context->current_statement_returns_a_value = statement_returns_a_value | is_infinite_loop;
            
            needs_semicolon = false;
        }break;
        case TOKEN_while:{
            
            smm continue_label_index = context->jump_label_index++;
            smm break_label_index    = context->jump_label_index++;
            
            smm old_continue_label_index = context->current_continue_label;
            smm old_break_label_index    = context->current_break_label;
            context->current_continue_label = continue_label_index;
            context->current_break_label    = break_label_index;
            
            // @note: We desugar 'while' to 'for(;condition;)'.
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'while'.");
            
            struct ir_jump_node *continue_label = push_jump(context, IR_jump_label);
            continue_label->label_number = continue_label_index;
            
            struct expr condition = parse_expression(context, false);
            maybe_report_warning_for_assignment_in_condition(context, condition.ir, condition.token);
            
            maybe_load_address_for_array_or_function(context, IR_load_address, &condition);
            maybe_insert_cast_from_special_int_to_int(context, &condition, /*is_lhs*/0);
            if(!casts_implicitly_to_bool(condition.resolved_type)){
                report_error(context, condition.token, "'while' condition has to cast to bool.");
                return;
            }
            expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of 'while'.");
            
            // :ir_refactor - new
            struct ir_jump_node *condition_jump = push_jump(context, IR_jump_if_false);
            condition_jump->label_number = break_label_index;
            
            // Get the initial "returns a value state".
            int statement_returns_a_value = context->current_statement_returns_a_value;
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            
            parse_statement(context);
            
            parser_scope_pop(context, scope);
            
            struct ir_jump_node *loop_jump = push_jump(context, IR_jump);
            loop_jump->label_number = continue_label_index;
            
            struct ir_jump_node *break_label = push_jump(context, IR_jump_label);
            break_label->label_number = break_label_index;
            
            // This is an infinite loop, if we found no alive break and the condition is constant != 0.
            int is_infinite_loop = ((scope->flags & SCOPE_FLAG_found_an_alive_break) == 0) && condition.ir->kind == IR_integer_literal && integer_literal_as_u64(&condition) != 0;
            context->current_statement_returns_a_value = statement_returns_a_value | is_infinite_loop; // See 'for' case for comment.
            
            needs_semicolon = false;
            
            context->current_continue_label = old_continue_label_index;
            context->current_break_label    = old_break_label_index;
        }break;
        case TOKEN_do:{
            smm loop_label_index      = context->jump_label_index++;
            smm continue_label_index = context->jump_label_index++;
            smm break_label_index    = context->jump_label_index++;
            
            smm old_continue_label_index = context->current_continue_label;
            smm old_break_label_index    = context->current_break_label;
            context->current_continue_label = continue_label_index;
            context->current_break_label    = break_label_index;
            
            // loop:
            //    body
            //  continue:
            //    condition
            //    cond-jump loop
            //  break:
            
            struct ir_jump_node *loop_label = push_jump(context, IR_jump_label);
            loop_label->label_number = loop_label_index;
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            
            parse_statement(context);
            parser_scope_pop(context, scope);
            
            expect_token(context, TOKEN_while, "Missing 'while' in do-while statement.");
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'while'.");
            
            struct ir_jump_node *continue_label = push_jump(context, IR_jump_label);
            continue_label->label_number = continue_label_index;
            
            struct expr condition = parse_expression(context, false);
            maybe_report_warning_for_assignment_in_condition(context, condition.ir, condition.token);
            
            maybe_load_address_for_array_or_function(context, IR_load_address, &condition);            
            maybe_insert_cast_from_special_int_to_int(context, &condition, /*is_lhs*/0);
            if(!casts_implicitly_to_bool(condition.resolved_type)){
                report_error(context, condition.token, "'while' condition has to cast to bool.");
                return;
            }
            expect_token(context, TOKEN_closed_paren, "Expected ')' ending 'while'-condition.");
            
            // :ir_refactor - new
            struct ir_jump_node *condition_jump = push_jump(context, IR_jump_if_true);
            condition_jump->label_number = loop_label_index;
            
            struct ir_jump_node *break_label = push_jump(context, IR_jump_label);
            break_label->label_number = break_label_index;
            
            // This is an infinite loop, if we found no alive break and the condition is constant != 0.
            int is_infinite_loop = ((scope->flags & SCOPE_FLAG_found_an_alive_break) == 0) && condition.ir->kind == IR_integer_literal && integer_literal_as_u64(&condition) != 0;
            
            // do <> while(); loops are a little bit different to the other loops, as their body is guaranteed to be executed one time.
            // Hence, we can say they return a value, if their body (or condition) returns a value, or its an infinite loop.
            context->current_statement_returns_a_value |= is_infinite_loop;
            
            context->current_continue_label = old_continue_label_index;
            context->current_break_label    = old_break_label_index;
        }break;
        
        case TOKEN_break:{
            
            // @cleanup: This loop is stupid. Hmm maybe not 100%.
            struct ast_scope *scope = context->current_scope;
            for(; scope; scope = scope->parent){
                if(scope->flags & SCOPE_FLAG_can_break) break;
            }
            
            if(!scope){
                report_syntax_error(context, initial_token, "'break' is not inside of a breakable scope.");
                return;
            }
            
            if(!context->current_statement_returns_a_value){
                // If the scope we are currently in does not return a value and there is a break, 
                // we can be sure that the breakable scope does not return a value.
                scope->flags |= SCOPE_FLAG_found_an_alive_break;
            }
            
            struct ir_jump_node *loop_jump = push_jump(context, IR_jump);
            loop_jump->label_number = context->current_break_label;
            
        }break;
        case TOKEN_continue:{
            
            // @cleanup: This search is stupid.
            struct ast_scope *scope = context->current_scope;
            for(; scope; scope = scope->parent){
                if(scope->flags & SCOPE_FLAG_can_continue) break;
            }
            
            if(!scope){
                report_syntax_error(context, initial_token, "'continue' is not inside of a continue-able scope.");
                return;
            }
            
            struct ir_jump_node *loop_jump = push_jump(context, IR_jump);
            loop_jump->label_number = context->current_continue_label;
            
            // We cannot escape infinte loops through a 'continue'.
            context->current_statement_returns_a_value = 1;
            
        }break;
        case TOKEN_switch:{
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'switch'.");
            
            struct expr switch_on = parse_expression(context, false);
            maybe_insert_cast_from_special_int_to_int(context, &switch_on, /*is_lhs*/0);
            
            if(switch_on.resolved_type->kind != AST_integer_type){
                report_error(context, switch_on.token, "Can only switch on integers.");
                return;
            }
            
            // "The integer promotions are performed on the controlling expression"
            maybe_insert_integer_promotion_cast(context, AST_cast, &switch_on, switch_on.token);
            
            expect_token(context, TOKEN_closed_paren, "Expected ')' ending 'switch'-condition.");
            
            smm break_label_index        = context->jump_label_index++;
            smm old_break_label_index    = context->current_break_label;
            context->current_break_label = break_label_index;
            
            struct ir_switch *ir_switch = push_struct(&context->ir_arena, struct ir_switch);
            ir_switch->base.kind = IR_switch;
            
            struct ir_switch *previous_switch = context->current_switch;
            struct expr       previous_switch_on = context->current_switch_on;
            struct token     *previous_switch_default_label_token = context->current_switch_default_label_token;
            context->current_switch = ir_switch;
            context->current_switch_on = switch_on;
            context->current_switch_default_label_token = 0;
            
            // Get the initial "returns a value state".
            int statement_returns_a_value = context->current_statement_returns_a_value;
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_break);
            
            parse_statement(context);
            parser_scope_pop(context, scope);
            
            context->current_switch    = previous_switch;
            context->current_switch_on = previous_switch_on;
            context->current_switch_default_label_token = previous_switch_default_label_token;
            
            // The switch returns a value, if its statement does, there is a default case (or in the future if its exhaustive ) and there was no alive break.
            int switch_returns_a_value = context->current_statement_returns_a_value && ir_switch->default_jump_label && !(scope->flags & SCOPE_FLAG_found_an_alive_break);
            context->current_statement_returns_a_value = statement_returns_a_value | switch_returns_a_value;
            
            struct ir_jump_node *break_label = push_jump(context, IR_jump_label);
            break_label->label_number = break_label_index;
            
            if(!ir_switch->default_jump_label){
                ir_switch->default_jump_label = break_label;
            }
            
            needs_semicolon = false;
            context->current_break_label = old_break_label_index;
        }break;
        case TOKEN_case:{
            if(!context->current_switch){
                report_syntax_error(context, initial_token, "'case' is only allowed within 'switch'-statement.");
                return;
            }
            
            // We assume we can jump here arbitrarily so the containing scope should not return a value anymore.
            context->current_statement_returns_a_value = 0;
            
            struct expr const_expr = parse_constant_integer_expression(context, false, "Operand of 'case' has to be constant.");
            
            struct expr *switch_on = &context->current_switch_on;
            maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, switch_on->resolved_type, switch_on->defined_type, &const_expr, const_expr.token);
            
            assert(const_expr.ir->kind == IR_integer_literal);
            assert(const_expr.resolved_type == context->current_switch_on.resolved_type);
            
            u64 value = integer_literal_as_u64(&const_expr);
            
            for(struct ir_case *ir_case = context->current_switch->case_list.first; ir_case; ir_case = ir_case->next){
                if(ir_case->value == value){
                    begin_error_report(context);
                    if(type_is_signed(switch_on->resolved_type)){
                        report_error(context, initial_token, "Case '%lld' already handled.", (smm)value);
                    }else{
                        report_error(context, initial_token, "Case '%llu' already handled.", value);
                    }
                    report_error(context, ir_case->token, "... Here is the previous case.");
                    end_error_report(context);
                }
            }
            
            struct ir_case *ir_case = push_struct(&context->ir_arena, struct ir_case);
            ir_case->base.kind = IR_case;
            ir_case->value = value;
            ir_case->token = initial_token;
            expect_token(context, TOKEN_colon, "Expected ':' after 'case'-label.");
            needs_semicolon = false;
            
            sll_push_back(context->current_switch->case_list, ir_case);
            
            if(!peek_token(context, TOKEN_closed_curly)) parse_statement(context);
        }break;
        case TOKEN_default:{
            if(!context->current_switch){
                report_syntax_error(context, initial_token, "'default' is only allowed within 'switch'-statement.");
                return;
            }
            
            // We assume we can jump here arbitrarily so the containing scope should not return a value anymore.
            context->current_statement_returns_a_value = 0;
            
            if(context->current_switch->default_jump_label){
                begin_error_report(context);
                report_syntax_error(context, initial_token, "More than one 'default'-case in switch.");
                report_syntax_error(context, context->current_switch_default_label_token, "... Here was the previous 'default'-case.");
                end_error_report(context);
            }
            
            expect_token(context, TOKEN_colon, "Expected ':' after 'default'.");
            needs_semicolon = false;
            
            struct ir_jump_node *default_label = push_jump(context, IR_jump_label);
            default_label->label_number = context->jump_label_index++;
            context->current_switch->default_jump_label = default_label;
            
            if(!peek_token(context, TOKEN_closed_curly)) parse_statement(context);
            
        }break;
        case TOKEN_else:{
            report_syntax_error(context, initial_token, "'else' without matching 'if'.");
            return;
        }break;
        case TOKEN_return:{
            struct ast_function_type *current_function_type = context->current_function->type;
            
            if(peek_token(context, TOKEN_semicolon)){
                if(current_function_type->return_type != &globals.typedef_void){
                    struct string type_string = push_type_string(context->arena, &context->scratch, current_function_type->return_type);
                    report_error(context, get_current_token(context), "Expected an expression after 'return' in function with return type '%s'.", type_string.data);
                    return;
                }
            }else{
                // @note: I used to check here that a void-function does not return a value.
                //        gcc and clang allow returning a void expression from a void function.
                //        This might be useful for future compatibility I guess...?
                //        Let's support it for now!
                
                struct expr return_expression = parse_expression(context, false);
                maybe_load_address_for_array_or_function(context, IR_load_address, &return_expression);
                maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, current_function_type->return_type, current_function_type->return_type_defined_type, &return_expression, initial_token);
            }
            
            context->current_statement_returns_a_value = 1;
            
            if(current_function_type->flags & FUNCTION_TYPE_FLAGS_is_noreturn){
                report_warning(context, WARNING_return_in_noreturn_function, initial_token, "'return' in function declared as '_Noreturn'.");
            }
            
            push_ir(context, IR_return);
        }break;
        case TOKEN_open_curly:{
            struct ast_scope *scope = parser_push_new_scope(context, initial_token, SCOPE_FLAG_none);
            
            parse_imperative_scope(context);
            
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
        }break;
        
        // case TOKEN_closed_curly:{
        //      This should now be done by parse_imperative_block, before 'if(1)}' was a valid statement.
        //      We can still get here tho, for the exact reason, but it should be a syntax error.
        // }break;
        
        case TOKEN_goto:{
            struct ast_goto *ast_goto = push_struct(&context->scratch, struct ast_goto);
            struct token *ident = expect_token(context, TOKEN_identifier, "Missing identifier after 'goto'.");
            ast_goto->ident = ident->atom;
            ast_goto->token = ident;
            sll_push_back(context->goto_list, ast_goto);
            
            context->current_statement_returns_a_value = 1;
            
            // :ir_refactor - new way.
            struct ir_jump_node *goto_jump = push_jump(context, IR_jump);
            ast_goto->jump = goto_jump;
        }break;
        case TOKEN_identifier:{
            if(peek_token_eat(context, TOKEN_colon)){
                
                struct atom ident = initial_token->atom;
                needs_semicolon = false;
                
                for(struct ast_label *label = context->label_list.first; label; label = label->next){
                    if(atoms_match(label->ident, ident)){
                        begin_error_report(context);
                        report_error(context, initial_token, "Redefinition of label '%.*s'.", ident.amount, ident.data);
                        report_error(context, label->token, "... Here is the previous definition.");
                        end_error_report(context);
                        return;
                    }
                }
                
                struct ast_label *label = push_struct(&context->scratch, struct ast_label);
                label->ident = ident;
                sll_push_back(context->label_list, label);
                
                // We assume we can jump here arbitrarily so the containing scope should not return a value anymore.
                context->current_statement_returns_a_value = 0;
                
                struct ir_jump_node *jump_label = push_jump(context, IR_jump_label);
                jump_label->label_number = context->jump_label_index++;
                label->jump_label = jump_label;
                
                if(!peek_token(context, TOKEN_closed_curly)) parse_statement(context);
                
                break;
            }
            
            struct ast_declaration *lookup = lookup_typedef(context, context->current_compilation_unit, initial_token, /*silent*/true);
            if(lookup){
                int is_function_declaration = parse_declaration_list_in_imperative_scope(context, lookup->type, /*defined_type*/&lookup->kind);
                if(is_function_declaration) needs_semicolon = false;
                break;
            }
            
            prev_token(context);
            
            parse_expression(context, false);
            
            push_ir(context, IR_pop_expression);
        }break;
        
        // @cleanup: noreturn, inline, ?
        case TOKEN_alignas: case TOKEN_atomic: 
        case TOKEN_declspec:
        case TOKEN_unaligned: case TOKEN_restrict:
        case TOKEN_const:    case TOKEN_volatile: case TOKEN_static: case TOKEN_extern: case TOKEN_typedef:
        case TOKEN_register: case TOKEN_signed:   case TOKEN_unsigned:
        case TOKEN_void:     case TOKEN_Bool:     case TOKEN_char:   case TOKEN_short:  case TOKEN_int: case TOKEN_long:
        case TOKEN_int8:     case TOKEN_int16:    case TOKEN_int32:  case TOKEN_int64:
        case TOKEN_float:    case TOKEN_double:
        case TOKEN_struct:   case TOKEN_union:    case TOKEN_enum:{
            prev_token(context);
            int is_function_declaration = parse_declaration_list_in_imperative_scope(context, null, null);
            
            // @note: A function declaration (which is not a definition) does need a semicolon.
            if(is_function_declaration) needs_semicolon = false;
        }break;
        
        case TOKEN_asm:{
            struct ir_asm_block *asm_block = push_struct(&context->ir_arena, struct ir_asm_block);
            asm_block->base.kind = IR_asm_block;
            asm_block->token = initial_token;
            
            expect_token(context, TOKEN_open_curly, "Expected '{' after '__asm__'.");
            
            parse_asm_block(context, asm_block);
            
            needs_semicolon = false;
        }break;
        
        case TOKEN_static_assert:{
            parse_static_assert(context, initial_token);
        }break;
        
        default:{
            // This should not be assert(false): take for example +value; that is a valid statement.
            prev_token(context);
            
            // If it is nothing else; it's gotta be an assignment or expression.
            parse_expression(context, false);
            push_ir(context, IR_pop_expression);
        }break;
        
        case TOKEN_invalid:{
            assert(context->error);
            return;
        }break;
    }
    
    if(needs_semicolon){
        if(context->should_exit_statement){
            // If an error occurred in for example 'extern int a;' (a not being filled in) we don't want
            // want to eat the semicolon, as we want to reset 'should_exit_statement' in the 'parse_imperative_scope'
            // code path. Here we also do not know yet, whether it is in fact a semicolon, so don't reset it here either.
            // Just don't at the semicolon if it is one.                                              - 03.10.2021
            
        }else{
            // :Error we could catch special cases here.
            expect_token(context, TOKEN_semicolon, "Missing ';' ending a statement.");
        }
    }
    
    return;
}

// @note: We assume the TOKEN_open_curly was already consumed.
func struct ast *parse_imperative_scope(struct context *context){
    
    struct ast_scope *scope = context->current_scope;
    assert(scope);
    
    scope->start_line_index = (u32)context->current_function->line_information.size;
    
    // Keep parsing inputs even after an error has occurred!
    while(true){
        // Statements only occur at function scope, so there should not be any sleeping.
        assert(!context->should_sleep);
        
        if(peek_token_eat(context, TOKEN_closed_curly) || peek_token(context, TOKEN_invalid)) break;
        
        parse_statement(context);
        
        if(context->should_exit_statement){
            while(!peek_token(context, TOKEN_invalid)){
                if(peek_token_eat(context, TOKEN_semicolon)) break; // clearly a new statement begins
                if(peek_token_eat(context, TOKEN_comma))     break; // probably a new expression begins
                if(peek_token(context, TOKEN_closed_curly))  break; // probably this scope ends, compound initializers should handle themselfs
                if(peek_token(context, TOKEN_open_curly))    break; // probably a new scope begins, in which case we want to report errors again
                next_token(context);
            }
            context->should_exit_statement = false;
        }else{
            assert(scope == context->current_scope);
        }
    }
    
    for(u32 table_index = 0; table_index < scope->current_max_amount_of_declarations; table_index++){
        
        struct ast_declaration *decl = scope->declarations[table_index];
        if(!decl) continue;
        if(decl->flags & DECLARATION_FLAGS_is_global) continue; // We don't track the used information for external declarations at function scope.
        
        if(decl->kind == IR_declaration){
            if(decl->_times_referenced == 0){
                report_warning(context, WARNING_unused_local_variable, decl->identifier, "Local variable is never used.");
            }else if(decl->_times_referenced == decl->_times_written){
                report_warning(context, WARNING_local_variable_only_ever_written, decl->identifier, "Local variable is never read, only written.");
            }
        }
    }
    
    scope->end_line_index = (u32)(context->current_function->line_information.size-1);
    if(scope->start_line_index > scope->end_line_index){
        scope->start_line_index = scope->end_line_index;
    }
    
    return (struct ast *)scope;
}

func struct declarator_return parse_declarator(struct context* context, struct ast_type *_initial_type, enum ast_kind *_initial_defined_type, enum declarator_kind_flags declarator_kind_flags){
    
    struct declarator_return ret = zero_struct;
    ret.type = _initial_type;
    ret.defined_type = _initial_defined_type;
    ret.ident = globals.invalid_identifier_token;
    
    if(context->should_exit_statement) return ret;
    
    if(maybe_report_error_for_stack_exhaustion(context, get_current_token_for_error_report(context), "Declaration nests to deep.")) return ret;
    
    // 
    // Microsoft extension:
    // 
    //     Specifying the calling-convention with __cdecl or __stdcall.
    //     There are two ways this has to work:
    //         int * __stdcall function(void);
    //         int (__stdcall * function)(void);
    //     One specifies a __stdcall-function returning pointer to int,
    //     the other declaration is a pointer to a __stdcall-function.
    // 
    // @note: We just ignore these for now. All calling conventions are the same on x64-windows.
    // 
    while(true){
        struct token *begin = get_current_token(context);
        peek_token_eat(context, TOKEN_cdecl);
        peek_token_eat(context, TOKEN_stdcall);
        if(get_current_token(context) == begin) break;
    }
    
    // declarator:
    //     pointer_opt direct-declarator
    
    // abstract-declarator:
    //     pointer
    //     pointer_opt direct-abstract-declarator
    
    // pointer:
    //     * type-qualifier-list_opt
    //     * type-qualifier-list_opt pointer
    // 
    // type-qualifier-list:
    //     type-qualifier
    //     type-qualifier-list type-qualifier
    // 
    
    while(peek_token_eat(context, TOKEN_times)){
        
        ret.type = parser_push_pointer_type(context, ret.type, ret.defined_type);
        ret.defined_type = null;
        
        enum type_qualifiers type_qualifiers = 0;
        
        while(true){
            struct token *begin = get_current_token(context);
            
            // 
            // @cleanup: these are flags which should probably be modifiers on the pointer type.
            // 
            if(peek_token_eat(context, TOKEN_ptr32))     type_qualifiers |= QUALIFIER_ptr32;
            if(peek_token_eat(context, TOKEN_ptr64))     type_qualifiers |= QUALIFIER_ptr64;
            if(peek_token_eat(context, TOKEN_unaligned)) type_qualifiers |= QUALIFIER_unaligned;
            
            if(peek_token_eat(context, TOKEN_restrict))  type_qualifiers |= QUALIFIER_restrict;
            if(peek_token_eat(context, TOKEN_const))     type_qualifiers |= QUALIFIER_const;
            if(peek_token_eat(context, TOKEN_volatile))  type_qualifiers |= QUALIFIER_volatile;
            if(peek_token_eat(context, TOKEN_atomic))    type_qualifiers |= QUALIFIER_atomic;
            
            if(get_current_token(context) == begin) break;
        }
        
        // @incomplete: If there are type qualifiers, we need to set them on the pointer somehow...
    }
    
    while(true){ // @note: See above.
        struct token *begin = get_current_token(context);
        peek_token_eat(context, TOKEN_cdecl);
        peek_token_eat(context, TOKEN_stdcall);
        if(get_current_token(context) == begin) break;
    }
    
    // 
    // direct-declarator:
    //     identifier
    //     (declarator)
    //     direct-declarator[type-qualififer-list_opt assignment-expression]
    //     direct-declarator(parameter-type-list)
    // 
    // direct-abstract-declarator:
    //     (abstract-declarator)
    //     direct-abstract-declarator_opt[type-qualifier-list_opt assignment-expression_opt]
    //     direct-abstract-declarator_opt(parameter-type-list_opt)
    // 
    
    // 
    // If we need an identifier (direct-declartor), we know that a '(' will be '(declarator)', 
    // because there needs to be a direct-declarator before any 'parameter-type-list'.
    // 
    // If we need a type-name, in the case of a valid '(abstract-declarator)' we will have either
    // '(*' or '((' or '([' or '((', for pointer, '(abstract-declarator)', array and function respectively.
    // Hence, we can check for these symbols as the next token.
    // 
    // If we allow an identifier, there is the additional case of '(identifier'.
    // Here, we cannot really tell the difference between:
    //     typedef int asd;
    //     int function(int (asd)); // same as 'int function(int (*)(asd argument)).'
    // and
    //     int function(int (asd)); // same as 'int function(int asd).'
    //     typedef int asd;
    //     
    // because we cannot sleep on 'asd', as it might be the latter case, and we cannot just lookup 'asd',
    // as we support out of order compilation and it might not be ready.
    // 
    // @cleanup: Is this actually true? If we in 'stage 1' figure out all of the global declaration and
    //           typedefs (without knowing what they resolve to), we could sleep here, right?
    //           
    // For now, it will just prefer the latter by greedily assuming its the '(declarator)' case.
    // 
    
    // Remember the current position in the token array, such that we can reset later on.
    int have_nested_declarator = false;
    smm nested_declarator_marker = context->token_at;
    
    if(peek_token(context, TOKEN_identifier)){
        // 
        // This is only valid in the direct-declartor case.
        // Error, if we are only searching for a type-name.
        // 
        ret.ident = next_token(context);
        if(!(declarator_kind_flags & DECLARATOR_identifier)){
            // :Error
            report_error(context, prev_token(context), "Identifier in an abstract declarator.");
            return ret;
        }
    }else if(peek_token(context, TOKEN_open_paren)){
        // 
        // This is valid in both the 'direct-declarator' and the 'direct-abstract-declarator' cases.
        // In both cases we have to differentiate between the function parameter type list and the 
        // parenthesized declarator cases.
        // 
        
        struct token *open_paren = next_token(context);
        
        if(declarator_kind_flags == DECLARATOR_identifier){
            // 
            // We only want a direct-declarator.
            // In this case we know its a nested declarator, as any parameter-type-list 
            // needs to be preceeded by a direct-declarator.
            // 
            have_nested_declarator = true;
        }else{
            // If we only want an abstract-declarator.
            // We can check, the next token for '*', '(' and '['.
            if(peek_token(context, TOKEN_times) || peek_token(context, TOKEN_open_paren) || peek_token(context, TOKEN_open_index)){
                have_nested_declarator = true;
            }
            
            // @cleanup: Not sure how to handle these...
            if(peek_token(context, TOKEN_cdecl) || peek_token(context, TOKEN_stdcall)){
                have_nested_declarator = true;
            }
            
            if(declarator_kind_flags & DECLARATOR_identifier){
                // If we also allow direct-declarators, we have the special case of an identifier.
                // For an identifier we have the cases:
                //     
                //     int (typename a) = function returning int with argument of type 'typename'.
                //     int (a)          = integer declaration with name 'a'.
                // 
                // Where the first one is not a nested declarator, but the second one is.
                // 
                // @cleanup: Currently, this will depend on the order we are compiling stuff in, 
                //           which is bad.
                //                                                             -Pascal Beyer 04.10.2024
                struct token *identifier = peek_token(context, TOKEN_identifier);
                if(identifier){
                    struct ast_declaration *ast_typedef = lookup_typedef(context, context->current_compilation_unit, identifier, /*silent*/true);
                    if(!ast_typedef) have_nested_declarator = true;
                }
            }
        }
        
        if(have_nested_declarator){
            if(!skip_until_tokens_are_balanced(context, open_paren, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '('.")){
                return ret;
            }
        }else{
            // It was not a nested declarator, move back to the open paren and try to parse it 
            // as a function below.
            prev_token(context);
        }
        
    }else{
        // 
        // We got neither an identifier nor an open paren, this might be valid for 
        // the 'direct-abstract-declarator' case for type-names like:
        // 
        //    int[10]
        // 
        
        if(!(declarator_kind_flags & DECLARATOR_type_name)){
            // :Error
            report_syntax_error(context, get_current_token_for_error_report(context), "Expected an identifier or '(' in a direct declarator.");
            return ret;
        }
        
        if(!peek_token(context, TOKEN_open_index)){
            // 
            // There is no direct-abstract-declarator after the pointer.
            // This is valid, just return what we have got.
            // 
            return ret;
        }
    }
    
    // 
    // Here, we have to careful on the order in which we apply the declarators.
    // The declarators are left associative and applied right to left:
    // 
    //     int f[]() = int (f[])() = [] () int f = "array of functions returing int"
    //     int f()[] = int (f())[] = () [] int f = "function returing array of int"
    // 
    // (Which of course are both invalid, but need to be parsed correctly nevertheless).
    // Similarly, for arrays of arrays:
    // 
    //     int a[1][2] = int (a[1])[2] = [1] [2] int a = "array of length 1 of arrays of length 2 of ints"
    // 
    
    // 
    // We *first* parse the post types (i.e all [] and ()) from left to right, 
    // pushing each type to the front of 'post_types'.
    // 
    // Then we iterate the 'post_types' to "apply" them in reverse order (right to left).
    // 
    
    struct post_type_node{
        struct post_type_node *next;
        
        struct ast_type *type; // Either an array, or a function.
        struct token *token;
    } *post_types = null;
    
    while(in_current_token_array(context)){
        
        if(get_current_token(context)->type == TOKEN_open_paren){
            struct token *open_paren = next_token(context);
            
            // :unresolved_types
            // 
            // We don't check for unresolved types anywhere in here.
            // This is because it is fine to have unresolved arguments/return types.
            // Whenever we actually use the function, we have to check that the types are 
            // actually resolved.
            //                                                                 07.02.2023
            
            struct ast_function_type *function = parser_type_push(context, function_type);
            
            // 
            // parameter-type-list:
            //     parameter-list
            //     parameter-list, ...
            // 
            // parameter-list:
            //     parameter-declaration
            //     parameter-list, parameter-declaration
            //     
            
            if(!peek_token_eat(context, TOKEN_closed_paren)){
                do{
                    if(peek_token_eat(context, TOKEN_dotdotdot)){
                        // @cleanup: C technically requires that you have one named argument.
                        function->flags |= FUNCTION_TYPE_FLAGS_is_varargs;
                        break;
                    }
                    
                    if(peek_token(context, TOKEN_closed_paren)) break; // Allow trailling comma.
                    
                    // 
                    // parameter-declaration:
                    //     declaration-specifiers declarator
                    //     declaration-specifiers abstract-declarator
                    //     declaration-specifiers
                    // 
                    
                    struct declaration_specifiers declaration_specifiers = parse_declaration_specifiers(context, null, null);
                    if(context->should_exit_statement) return ret;
                    
                    struct declarator_return declarator = parse_declarator(context, declaration_specifiers.type_specifier, declaration_specifiers.defined_type_specifier, DECLARATOR_type_name | DECLARATOR_identifier);
                    if(context->should_exit_statement) return ret;
                    
                    if(declarator.type->kind == AST_array_type){
                        // 
                        // "A declaration of a parameter as 'array of type' shall be adjusted to
                        // 'qualified pointer to type' [...]."
                        // 
                        struct ast_array_type *array = cast(struct ast_array_type *)declarator.type;
                        declarator.type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type);
                        if(!declarator.defined_type){
                            declarator.defined_type = &array->base.kind;
                        }
                    }else if(declarator.type->kind == AST_function_type){
                        // 
                        // "A declaration of a parameter as 'function returning type' shall be adjusted to
                        //  'pointer to function returning type'."
                        //  
                        declarator.type = parser_push_pointer_type(context, declarator.type, null);
                    }
                    
                    struct ast_declaration *decl = push_declaration_for_declarator(context, declarator);
                    
                    ast_list_append(&function->argument_list, context->arena, &decl->kind);
                    
                    if(decl->assign_expr){ // @cleanup: This can never happen right? we should 'peek_token(context, TOKEN_equals)' if anything.
                        report_error(context, prev_token(context), "Assignment in function declaration. Default arguments are invalid.");
                        return ret;
                    }
                    
                }while(peek_token_eat(context, TOKEN_comma));
                
                expect_token(context, TOKEN_closed_paren, "Expected ')' ending function declarator.");
                if(context->should_exit_statement) return ret;
            }
            
            if(function->argument_list.count == 1){
                struct ast_declaration *argument = (struct ast_declaration *)function->argument_list.first->value;
                if(argument->type->kind == AST_void_type && argument->identifier == globals.invalid_identifier_token){
                    // 
                    // Transform 'int arst(void);' into 'int arst();', furthermore, if we have typedef void VOID;
                    // Also transform 'int arst(VOID);' into 'int arst();'.
                    // 
                    function->argument_list.first = function->argument_list.last = null;
                    function->argument_list.count = 0;
                }
            }
            
            
            struct post_type_node *post_type_node = push_struct(&context->scratch, struct post_type_node);
            
            post_type_node->type = &function->base;
            post_type_node->next = post_types;
            post_type_node->token = open_paren;
            post_types = post_type_node;
            
        }else if(get_current_token(context)->type == TOKEN_open_index){
            
            // @note: In theory the c-spec wants this:
            //     array-declarator:
            //         direct-declarator [ type-qualifier-list_opt assignment-expression_opt ]
            //         direct-declarator [ static type-qualifier-list_opt assignment-expression ]
            //         direct-declarator [ type-qualifier-list static assignment-expression ]
            //         direct-declarator [ type-qualifier-list_opt * ]
            // But I have never seen this used in my life, so I am going to ignore it...
            // These are only valid within a function prototype.
            // 
            
            struct token *open_index = next_token(context);
            
            struct ast_array_type *array_type = parser_type_push(context, array_type);
            
            if(peek_token_eat(context, TOKEN_closed_index)){ // u32 arr[];
                array_size_zero_also_means_array_of_unknown_size:;
                
                array_type->amount_of_elements = 0;
                array_type->is_of_unknown_size = true;
                array_type->base.size = 0;
                array_type->base.flags |= TYPE_FLAG_ends_in_array_of_unknown_size;
            }else{
                struct expr index_expression = parse_constant_integer_expression(context, false, "Array subscript is not constant.");
                
                expect_token(context, TOKEN_closed_index, "Expected ']' ending array declarator.");
                
                s64 value = integer_literal_as_s64(&index_expression);
                
                if(value == 0){
                    goto array_size_zero_also_means_array_of_unknown_size;
                }
                
                if(value <= 0){
                    report_error(context, index_expression.token, "Array size must be positive.");
                    return ret;
                }
                
                array_type->amount_of_elements = value;
                
                if(context->should_exit_statement) return ret;
            }
            
            struct post_type_node *post_type_node = push_struct(&context->scratch, struct post_type_node);
            
            post_type_node->type = &array_type->base;
            post_type_node->next = post_types;
            post_type_node->token = open_index;
            post_types = post_type_node;
        }else{
            break;
        }
    }
    
    // 
    // Apply each of the post types.
    // 
    for(struct post_type_node *post_type_node = post_types; post_type_node; post_type_node = post_type_node->next){
        struct ast_type *post_type = post_type_node->type;
        struct token *token = post_type_node->token;
        
        if(post_type->kind == AST_function_type){
            struct ast_function_type *function_type = (struct ast_function_type *)post_type;
            // 
            // We have to set the return type.
            // 
            
            if(ret.type->kind == AST_function_type){
                // :Error
                report_error(context, token, "Function returning function is illegal. Return a function pointer instead.");
                return ret;
            }else if(ret.type->kind == AST_array_type){
                // :Error
                report_error(context, token, "Function returning array is illegal. You can wrap the array inside of a struct.");
                return ret;
            }
            
            function_type->return_type = ret.type;
            function_type->return_type_defined_type = ret.defined_type;
            
        }else if(post_type->kind == AST_array_type){
            // 
            // We have to set the element type and calculate the size.
            // 
            struct ast_array_type *array_type = (struct ast_array_type *)post_type;
            
            if(ret.type->kind == AST_unresolved_type){
                // :unresolved_types
                // Disallow of 'unresolved_types'.
                sleep_or_error_on_unresolved_type(context, ret.type);
                return ret;
            }
            
            if(ret.type == &globals.typedef_void){
                report_error(context, token, "Array of type void is illegal.");
                return ret;
            }
            
            if(ret.type->kind == AST_function_type){
                report_error(context, token, "Array of functions is illegal, please use function pointers.");
                return ret;
            }
            
            if(type_is_array_of_unknown_size(ret.type)){
                // @note: only the first array in the chain can be 'unknown_size'.
                //        Meaning 'char a[][2]' is legal, 'char a[2][]' is not.
                
                // :Error find a better word for 'outer'.
                report_error(context, token, "Only the most outer array can be of unknown size. ('char a[][2]' is legal, 'char a[2][]' is not).");
                return ret;
            }
            
            array_type->element_type = ret.type;
            array_type->element_type_defined_type = ret.defined_type;
            
            if(!array_type->is_of_unknown_size){
                patch_array_size(context, array_type, array_type->amount_of_elements, token);
            }
            
            array_type->base.alignment = ret.type->alignment;
        }else{
            invalid_code_path;
        }
        
        ret.type = post_type;
        ret.defined_type = null;
    }
    
    if(have_nested_declarator){
        // 
        // We have a nested declarator.
        // Its token range begins at 'nested_declarator_marker'.
        // 
        smm saved_marker = context->token_at;
        
        context->token_at = nested_declarator_marker;
        
        // We should have reset the token iterator to the open paren.
        assert(peek_token(context, TOKEN_open_paren));
        
        next_token(context);
        
        ret = parse_declarator(context, ret.type, ret.defined_type, declarator_kind_flags);
        
        expect_token(context, TOKEN_closed_paren, "Expected a ')' ending parenthesized declarator.");
        
        if(context->should_exit_statement) return ret;
        
        context->token_at = saved_marker;
    }
    
    return ret;
}

struct pragma_pack_node{
    struct pragma_pack_node *next;
    
    struct string identifier;
    u64 value;
    struct token *token;
};

void parse_and_process_pragma_pack(struct context *context){
    
    expect_token(context, TOKEN_open_paren, "Expected a '(' after 'pragma pack'.");
    
    if(peek_token_eat(context, TOKEN_closed_paren)){
        context->pragma_alignment = 16; // Default on x64.
        return;
    }
    
    int have_value = 0;
    u64 value = 0;
    
    struct token *show_token = 0;
    
    enum pragma_pack_operation{
        PRAGMA_PACK_set,
        PRAGMA_PACK_push,
        PRAGMA_PACK_pop,
        PRAGMA_PACK_show,
    } operation = PRAGMA_PACK_set;
    
    struct string pack_identifier = zero_struct;
    
    int should_break = 0;
    while(!should_break){
        struct token *token = next_token(context);
        
        switch(token->type){
            case TOKEN_identifier:{
                struct string identifier = token->string;
                if(string_match(identifier, string("push"))){
                    operation = PRAGMA_PACK_push;
                    show_token = token;
                }else if(string_match(identifier, string("pop"))){
                    operation = PRAGMA_PACK_pop;
                }else if(string_match(identifier, string("show"))){
                    operation = PRAGMA_PACK_show;
                    show_token = token;
                }else{
                    pack_identifier = identifier;
                }
            }break;
            
            case TOKEN_binary_literal:
            case TOKEN_hex_literal:
            case TOKEN_base10_literal:{
                struct parsed_integer parsed_integer;
                if(token->type == TOKEN_base10_literal){
                    parsed_integer = parse_base10_literal(context, token);
                }else if(token->type == TOKEN_hex_literal) {
                    parsed_integer = parse_hex_literal(context, token);
                }else{
                    parsed_integer = parse_binary_literal(context, token);
                }
                
                have_value = 1;
                value = parsed_integer.value;
            }break;
            
            default: should_break = 1; break;
        }
        
        if(!peek_token_eat(context, TOKEN_comma)) break;
    }
    
    expect_token(context, TOKEN_closed_paren, "Expected a ')' at then end of '#pragma pack(<arguments>'.");
    
    switch(operation){
        case PRAGMA_PACK_set:{
            if(have_value){
                context->pragma_alignment = value;
            }else{
                context->pragma_alignment = 16; // Default on x64.
            }
        }break;
        case PRAGMA_PACK_push:{
            struct pragma_pack_node *node = push_struct(&context->scratch, struct pragma_pack_node);
            node->value = context->pragma_alignment;
            node->identifier = pack_identifier;
            node->token = show_token;
            
            sll_push_front(context->pragma_pack_stack, node);
            
            if(have_value) context->pragma_alignment = value;
        }break;
        case PRAGMA_PACK_pop:{
            if(pack_identifier.size){
                int found = 0;
                for(struct pragma_pack_node *node = context->pragma_pack_stack.first; node; node = node->next){
                    if(string_match(node->identifier, pack_identifier)){
                        context->pragma_pack_stack.first = node->next;
                        context->pragma_alignment = node->value;
                        found = 1;
                        break;
                    }
                }
                
                if(!found){
                    // @cleanup: make this its own warning value.
                    report_warning(context, WARNING_pragma_pack_show, show_token, "Identifier '%.*s' is not on the pragma pack stack.", pack_identifier.size, pack_identifier.data);
                }
            }else{
                if(context->pragma_pack_stack.first){
                    struct pragma_pack_node *pop = context->pragma_pack_stack.first;
                    context->pragma_alignment = pop->value;
                    
                    sll_pop_front(context->pragma_pack_stack);
                }
            }
            
            if(have_value) context->pragma_alignment = value;
        }break;
        case PRAGMA_PACK_show:{
            char *here_is_the_current_stack = "";
            if(context->pragma_pack_stack.first) here_is_the_current_stack = " Here is the current stack:";
            
            begin_error_report(context);
            report_warning(context, WARNING_pragma_pack_show, show_token, "Current \"pragma pack(show)\"-value is %llu.%s", context->pragma_alignment, here_is_the_current_stack);
            int depth = 0;
            for(struct pragma_pack_node *node = context->pragma_pack_stack.first; node; node = node->next){
                char *identifier = "";
                if(node->identifier.size) identifier = ", identifier: ";
                report_warning(context, WARNING_pragma_pack_show, node->token, "[%d] Pack-alignment %llu%s%.*s.", depth++, node->value, identifier, node->identifier.size, node->identifier.data);
            }
            end_error_report(context);
        }break;
    }
}

