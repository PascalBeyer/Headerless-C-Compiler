
//////////////////////////////////////////////////////////////////////////
//                               Parser                                 //
//////////////////////////////////////////////////////////////////////////

// A simple C-parser, the main entry points are 'parse_declaration_list'
// and 'parse_imperative_scope'. Depending on whether you are parsing a
// global declaration or a function body.
// (Compilation stage one vs stage two).
// The main complications come from the fact that C is an old and weird
// language as well as the fact that everything has to be able to
// 'parser_sleep', whenever we encounter an identifier we don't know yet
// The parser is in some sense entirely "single pass", i.e. we build
// the abstract tree, type check it and do constant propagation at the
// same time.
// Other then that, its just a 'simple' recursively decent parser.
//                                                        -11.09.2020

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
func s64 get_unique_ast_serial(struct context *context){
    s64 ret = context->ast_serializer++;
    
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

func void set_resolved_type(struct ast *ast, struct ast_type *type, struct ast *named_type){
    assert(type);
    ast->resolved_type = type;
    ast->defined_type = named_type;
}

#define parser_ast_push(context, token, type) ((struct ast_##type *)_parser_ast_push(context, token, sizeof(struct ast_##type), alignof(struct ast_##type), AST_##type))

func void *_parser_ast_push(struct context *context, struct token *token, smm size, u32 align, enum ast_kind kind){
    assert(token->type != TOKEN_invalid);
    
    struct ast *ast = push_struct_(context->arena, size, align); // @note: No need to zero, 'arena' never has any non-zero bytes.
    
    ast->kind  = kind;
    ast->token = token;
    ast->s = get_unique_ast_serial(context);
    ast->byte_offset_in_function = -1;
    
    return ast;
}

func struct ast *invalid_ast(struct context *context){
    struct ast *invalid = _parser_ast_push(context, get_current_token_for_error_report(context), sizeof(struct ast), alignof(struct ast), AST_invalid);
    set_resolved_type(invalid, &globals.typedef_void, null);
    return invalid;
}

#define parser_type_push(context, token, type) (struct ast_##type *)_parser_type_push(context, token, sizeof(struct ast_##type), alignof(struct ast_##type), AST_##type)

func struct ast_type *_parser_type_push(struct context *context, struct token *token, smm size, u32 align, enum ast_kind kind){
    assert(token->type != TOKEN_invalid);
    
    struct ast_type *type = push_struct_(context->arena, size, align);  // @note: No need to zero, 'arena' never has any non-zero bytes.
    
    type->kind = kind;
    type->token = token;
    type->s = get_unique_ast_serial(context);
    
    return type;
}

#define parser_compound_type_push(context, token, type) (struct ast_compound_type *)_parser_type_push(context, token, sizeof(struct ast_compound_type), alignof(struct ast_compound_type), AST_##type);

func struct ast_type *parser_push_pointer_type(struct context *context, struct ast_type *pointer_to, struct ast *defined_type, struct token *token){
    // @cleanup: we could check here if its a basic type and then do some math to figure out the pointer
    // this is kinda complicated, as globals.basic_types is an array of pointers, not basic_types themself.
    // I think we had to iterate
    assert(pointer_to);
    struct ast_pointer_type *ptr = parser_type_push(context, token, pointer_type);
    ptr->pointer_to = pointer_to;
    ptr->pointer_to_defined_type = defined_type;
    ptr->base.size = 8;
    ptr->base.alignment = 8;
    return cast(struct ast_type *)ptr;
}

// @cleanup: why are these 'ast'_push_bla should they not be 'parser'_push for consistency?
func struct ast *ast_push_binary_expression(struct context *context, enum ast_kind kind, struct token *token, struct ast *lhs, struct ast *rhs){
    struct ast *ast = _parser_ast_push(context, token, sizeof(struct ast_binary_op), alignof(struct ast_binary_op), kind);
    struct ast_binary_op *op = cast(struct ast_binary_op *)ast;
    op->lhs = lhs;
    op->rhs = rhs;
    return cast(struct ast *)op;
}

func struct ast *ast_push_unary_expression(struct context *context, enum ast_kind kind, struct token *token, struct ast *operand){
    struct ast_unary_op *op = cast(struct ast_unary_op *)_parser_ast_push(context, token, sizeof(struct ast_unary_op), alignof(struct ast_unary_op), kind);
    op->operand = operand;
    return cast(struct ast*)op;
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

func struct ast *ast_push_s64_literal(struct context *context, struct token *token, s64 value){
    struct ast_integer_literal *lit = parser_ast_push(context, token, integer_literal);
    lit->_s64 = value;
    set_resolved_type(&lit->base, &globals.typedef_s64, null);
    return &lit->base;
}

func struct ast *ast_push_u64_literal(struct context *context, struct token *token, u64 value){
    struct ast_integer_literal *lit = parser_ast_push(context, token, integer_literal);
    lit->_u64 = value;
    set_resolved_type(&lit->base, &globals.typedef_u64, null);
    return &lit->base;
}


func struct ast *ast_push_s32_literal(struct context *context, struct token *token, s64 value){
    assert(value <= s32_max);
    assert(value >= s32_min);
    struct ast_integer_literal *lit = parser_ast_push(context, token, integer_literal);
    lit->_s32 = (s32)value;
    set_resolved_type(&lit->base, &globals.typedef_s32, null);
    return &lit->base;
}

func s32 integer_literal_as_s32(struct ast *lit){
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
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

func u32 integer_literal_as_u32(struct ast *lit){
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
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

func s64 integer_literal_as_s64(struct ast *lit){
    assert(lit->kind == AST_integer_literal);
    
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
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

func u64 integer_literal_as_u64(struct ast *lit){
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
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

static struct ast_type *defined_type_to_type(struct ast *defined_type){
    if(defined_type->kind == AST_typedef){
        return ((struct ast_declaration *)defined_type)->type;
    }else{
        return (struct ast_type *)defined_type;
    }
}

func struct ast *defined_type_for_binary_op(struct ast_binary_op *op){
    
    // 
    // The defined type for a binary op should be the smallest defined type,
    // which fits both of the arguments.
    // 
    
    struct ast *lhs = op->lhs->defined_type;
    struct ast *rhs = op->rhs->defined_type;
    
    if(lhs && rhs){
        struct ast_type *lhs_type = defined_type_to_type(lhs);
        struct ast_type *rhs_type = defined_type_to_type(rhs);
        
        return (rhs_type->size > lhs_type->size) ? rhs : lhs;
    }
    
    smm lhs_size = lhs ? defined_type_to_type(lhs)->size : op->lhs->resolved_type->size;
    smm rhs_size = rhs ? defined_type_to_type(rhs)->size : op->rhs->resolved_type->size;
    
    if(lhs && lhs_size >= rhs_size) return lhs;
    if(rhs && rhs_size >= lhs_size) return rhs;
    
    return null;
}

func struct ast *defined_type_for_promotion(struct ast *ast){
    // :retain_type_information_through_promotion
    // if it was promoted, we retain the information about the actual size in the defined_type
    struct ast *defined_type = ast->defined_type;
    if(!defined_type) defined_type = (struct ast *)ast->resolved_type;
    return defined_type;
}

//_____________________________________________________________________________________________________________________

func struct ast *push_cast(struct context *context, struct ast_type *cast_to, struct ast *cast_to_defined_type, struct ast *cast_what){
    
    struct ast *cast = null;
    
    if(!cast_what){
        // @sigh: for user casts (which are prefix expressions), we push a cast with a null 'cast_what', which
        //        we fill in later.
    }else if(cast_what->resolved_type == cast_to){
        // set the 'defined_type' we might have casted (float) to (f32)
        set_resolved_type(cast_what, cast_to, cast_to_defined_type);
        cast = cast_what;
    }else if(cast_what->kind == AST_float_literal){
        if(cast_to->kind == AST_integer_type){
            struct ast_float_literal *f = cast(struct ast_float_literal *)cast_what;
            struct ast_integer_literal *i = parser_ast_push(context, cast_what->token, integer_literal);
            if(type_is_signed(cast_to)){
                switch(cast_to->size){
                    case 1: i->_s8  = (s8) f->value; break;
                    case 2: i->_s16 = (s16)f->value; break;
                    case 4: i->_s32 = (s32)f->value; break;
                    case 8: i->_s64 = (s64)f->value; break;
                    invalid_default_case();
                }
            }else{
                if(f->value < 0.0){
                    report_error(context, cast_what->token, "Cast from negative a floating point number to an unsigned type is illegal.");
                    return cast_what;
                }
                
                if(cast_to == &globals.typedef_Bool){
                    i->_u8 = (_Bool)f->value;
                }else{
                    switch(cast_to->size){
                        case 1: i->_u8  = (u8) f->value; break;
                        case 2: i->_u16 = (u16)f->value; break;
                        case 4: i->_u32 = (u32)f->value; break;
                        case 8: i->_u64 = (u64)f->value; break;
                        invalid_default_case();
                    }
                }
            }
            
            set_resolved_type(&i->base, cast_to, cast_to_defined_type);
            cast = &i->base;
        }else if(cast_to->kind == AST_float_type){
            if(cast_to == &globals.typedef_f32){
                struct ast_float_literal *lit = cast(struct ast_float_literal *)cast_what;
                // :we_always_keep_double
                lit->value = (f32)lit->value; // cast it to f32 and then back up
            }
            
            set_resolved_type(cast_what, cast_to, cast_to_defined_type);
            cast = cast_what;
        }
    }else if(cast_what->kind == AST_integer_literal){
        struct ast_integer_literal *lit = cast(struct ast_integer_literal *)cast_what;
        if(cast_to == &globals.typedef_Bool){
            lit->_u8 = (_Bool)integer_literal_as_u64(cast_what);
            
            cast = &lit->base;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
        }else if(cast_to->kind == AST_integer_type){
            // @hmm: I trust my self, that I thought about this...
            if(type_is_signed(cast_to)){
                s64 value = integer_literal_as_s64(cast_what);
                lit->_s64 = value;
            }else{
                u64 value = integer_literal_as_u64(cast_what);
                lit->_u64 = value;
            }
            
            cast = &lit->base;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
        }else if(cast_to->kind == AST_float_type){
            struct ast_float_literal *f = parser_ast_push(context, cast_what->token, float_literal);
            f->base.token = lit->base.token;
            // :we_always_keep_double
            // @cleanup: this does not really work: I don't think casting to f64 and then to f32 is the same
            //                                      as casting to f32 to begin with, but not sure...
            if(type_is_signed(cast_what->resolved_type)){
                f->value = (f64)integer_literal_as_s64(cast_what);
            }else{
                f->value = (f64)integer_literal_as_u64(cast_what);
            }
            
            set_resolved_type(&f->base, cast_to, cast_to_defined_type);
            cast = &f->base;
        }else if(cast_to->kind == AST_pointer_type){
            // Extend the integer literal to be 64-bits.
            lit->_s64 = integer_literal_as_s64(cast_what);
            
            // Convert it to a pointer literal simply by setting its type.
            cast_what->kind = AST_pointer_literal;
            static_assert(sizeof(struct ast_pointer_literal) == sizeof(struct ast_integer_literal));
            
#if !defined(__PBC__) && !defined(__clang__)
            // @cleanup: Currently, there is a compiler bug, which makes this not compile.
            static_assert(offset_in_type(struct ast_pointer_literal, pointer) == offset_in_type(struct ast_integer_literal, _s64));
#endif
            
            cast = cast_what;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
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
                        report_warning(context, WARNING_compile_time_truncation, cast_what->token, "Integer %lld gets truncated when casted to bitfield of width %d (range is %lld to %lld).", value, bitfield->width, min_value, max_value);
                    }
                }else{
                    u64 value = integer_literal_as_u64(cast_what);
                    if(value > 0x7fffffffffffffff || (smm)value > max_value){
                        report_warning(context, WARNING_compile_time_truncation, cast_what->token, "Integer %llu gets truncated when casted to bitfield of width %d (range is %lld to %lld).", value, bitfield->width, min_value, max_value);
                    }
                }
            }else{
                u64 max_value = (1ull << bitfield->width) - 1;
                
                if(type_is_signed(cast_what->resolved_type)){
                    s64 value = integer_literal_as_u64(cast_what);
                    if(value < 0 || (u64)value > max_value){
                        report_warning(context, WARNING_compile_time_truncation, cast_what->token, "Integer %lld gets truncated when casted to bitfield of width %d (range is %llu to %llu).", value, bitfield->width, 0, max_value);
                    }
                }else{
                    u64 value = integer_literal_as_u64(cast_what);
                    if(value > max_value){
                        report_warning(context, WARNING_compile_time_truncation, cast_what->token, "Integer %llu gets truncated when casted to bitfield of width %d (range is %llu to %llu).", value, bitfield->width, 0, max_value);
                    }
                }
            }
            
            cast = cast_what;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
        }else{
            assert(cast_to->kind == AST_bitfield_type || cast_to->kind == AST_void_type);
        }
    }else if(cast_what->kind == AST_pointer_literal){
        if(cast_to == &globals.typedef_Bool){
            struct ast_integer_literal *lit = (struct ast_integer_literal *)cast_what;
            
            cast_what->kind = AST_integer_literal;
            lit->_u64 = (((struct ast_pointer_literal *)lit)->pointer != 0);
            
            cast = cast_what;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
        }else if(cast_to->kind == AST_integer_type){
            // the value in 'cast_what->pointer' gets truncated implicitly.
            cast_what->kind = AST_integer_literal;
            cast = cast_what;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
        }else if(cast_to->kind == AST_pointer_type){
            // just properage the type.
            cast = cast_what;
            set_resolved_type(cast, cast_to, cast_to_defined_type);
        }
    }else if(cast_to->kind == AST_float_type && cast_what->resolved_type == &globals.typedef_u64){
        report_warning(context, WARNING_casting_u64_to_float, cast_what->token, "Casting an unsigned 64bit number to a floating point type is slow on x64, as x64 only has builtin signed to float support.");
    }
    
    if(!cast){
        // no constant propagation
        struct token *token = get_current_token_for_error_report(context);
        if(cast_what) token = cast_what->token;
        
        cast = ast_push_unary_expression(context, AST_cast, token, cast_what);
        set_resolved_type(cast, cast_to, cast_to_defined_type);
    }
    
    assert(cast->resolved_type == cast_to && cast->defined_type == cast_to_defined_type);
    return cast;
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


func struct ast *maybe_insert_bitfield_cast(struct context *context, struct ast *ast){
    if(ast->resolved_type->kind == AST_bitfield_type){
        struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)ast->resolved_type;
        
        // 
        // "If an int can represent all values of the originaly type (as restricted by the width, 
        //  for a bit-field), the value is converted to an int; otherwise it is converted to an
        //  unsigned int." (This assumes all bitfields are either _Bool, signed int or unsigned int).
        // 
        
        if(bitfield->width < 32){
            ast = push_cast(context, &globals.typedef_s32, null, ast);
        }else if(bitfield->width == 32){
            if(type_is_signed(bitfield->base_type)){
                ast = push_cast(context, &globals.typedef_s32, null, ast);
            }else{
                ast = push_cast(context, &globals.typedef_u32, null, ast);
            }
        }else{
            ast = push_cast(context, bitfield->base_type, null, ast);
        }
    }
    return ast;
}

func void perform_integer_promotion_on_literal(struct ast_integer_literal *lit){
    if(lit->base.resolved_type->size >= 4) return;
    
    // sign/zero extend the value to the whole 64-bits.
    lit->_s64 = integer_literal_as_s64(&lit->base);
    
    // :retain_type_information_through_promotion
    struct ast *defined_type = lit->base.defined_type;
    if(!defined_type) defined_type = (struct ast *)lit->base.resolved_type;
    
    set_resolved_type(&lit->base, &globals.typedef_s32, defined_type);
}

func struct ast *maybe_insert_integer_promotion_cast(struct context *context, struct ast *ast){
    ast = maybe_insert_bitfield_cast(context, ast);
    
    struct ast_type *promoted_type = perform_integer_promotions(ast->resolved_type);
    if(ast->resolved_type != promoted_type){
        // :retain_type_information_through_promotion
        // if it was promoted, we retain the information about the actual size in the defined_type
        struct ast *defined_type = ast->defined_type;
        if(!defined_type) defined_type = (struct ast *)ast->resolved_type;
        return push_cast(context, promoted_type, defined_type, ast);
    }
    return ast;
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


func void maybe_insert_arithmetic_conversion_casts(struct context *context, struct ast_binary_op *op){
    
    op->lhs = maybe_insert_bitfield_cast(context, op->lhs);
    op->rhs = maybe_insert_bitfield_cast(context, op->rhs);
    
    if(!type_is_arithmetic(op->lhs->resolved_type) || !type_is_arithmetic(op->rhs->resolved_type)) return;
    
    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
    if(!promoted_type) return;
    
    if(op->lhs->resolved_type != promoted_type){
        // :retain_type_information_through_promotion
        // if it was promoted, we retain the information about the actual size in the defined_type
        struct ast *defined_type = op->lhs->defined_type;
        if(!defined_type) defined_type = (struct ast *)op->lhs->resolved_type;
        op->lhs = push_cast(context, promoted_type, defined_type, op->lhs);
    }
    
    if(op->rhs->resolved_type != promoted_type){
        struct ast *defined_type = op->rhs->defined_type;
        if(!defined_type) defined_type = (struct ast *)op->rhs->resolved_type;
        op->rhs = push_cast(context, promoted_type, defined_type, op->rhs);
    }
}

func void maybe_cast_literal_0_to_void_pointer(struct context *context, struct ast **_lhs, struct ast **_rhs){
    
    struct ast *lhs = *_lhs;
    struct ast *rhs = *_rhs;
    
    if(lhs->resolved_type->kind == AST_pointer_type && rhs->kind == AST_integer_literal){
        if(integer_literal_as_u64(rhs) == 0){
            *_rhs = push_cast(context, lhs->resolved_type, lhs->defined_type, rhs);
        }
        return;
    }
    
    if(rhs->resolved_type->kind == AST_pointer_type && lhs->kind == AST_integer_literal){
        if(integer_literal_as_u64(lhs) == 0){
            *_lhs = push_cast(context, rhs->resolved_type, rhs->defined_type, lhs);
        }
    }
}

func void maybe_insert_cast_from_void_pointer(struct context *context, struct ast **_lhs, struct ast **_rhs){
    struct ast *lhs = *_lhs;
    struct ast *rhs = *_rhs;
    
    if(lhs->resolved_type->kind != AST_pointer_type) return;
    if(rhs->resolved_type->kind != AST_pointer_type) return;
    
    struct ast_pointer_type *lhs_pointer = (struct ast_pointer_type *)lhs->resolved_type;
    struct ast_pointer_type *rhs_pointer = (struct ast_pointer_type *)rhs->resolved_type;
    
    if(lhs_pointer->pointer_to == rhs_pointer->pointer_to) return;
    
    if(lhs_pointer->pointer_to == &globals.typedef_void){
        *_lhs = push_cast(context, rhs->resolved_type, rhs->defined_type, lhs);
        return;
    }
    
    if(rhs_pointer->pointer_to == &globals.typedef_void){
        *_rhs = push_cast(context, lhs->resolved_type, lhs->defined_type, rhs);
        return;
    }
}

//_____________________________________________________________________________________________________________________

func struct ast_scope *parser_push_new_scope(struct context *context, struct token *token, enum scope_flags flags){
    struct ast_scope *scope = parser_ast_push(context, token, scope);
    scope->flags = flags;
    set_resolved_type(&scope->base, &globals.typedef_void, null);
    
    scope->parent = context->current_scope;
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
    struct ast *defined_type;
};

func void sleep_or_error_on_unresolved_type(struct context *context, struct ast_type *type){
    struct ast_unresolved_type *unresolved = cast(struct ast_unresolved_type *)type;
    if(globals.compile_stage < COMPILE_STAGE_parse_function){
        parser_sleep(context, unresolved->base.token, SLEEP_on_struct);
    }else{
        // :Error, used ?
        
        struct string type_prefix = type_prefix_for_unresolved_type(unresolved);
        
        report_error(context, unresolved->base.token, "Undeclared %.*s.", type_prefix.size, type_prefix.data);
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
        if(atoms_match(wanted_unresolved->base.token->atom, given_unresolved->base.token->atom)){
            return wanted;
        }else{
            return null;
        }
    }
    
    // 
    // If either of the types is unresolved, try to look it up.
    // If we failed to look it up, it cannot match the other type, as the other type is resolved.
    // 
    if(wanted->kind == AST_unresolved_type) wanted = lookup_unresolved_type(wanted);
    if(given->kind  == AST_unresolved_type) given  = lookup_unresolved_type(given);
    if(!wanted || !given) return null;
    
    struct ast_type *ret = wanted;
    
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
            if(atoms_match(wanted_unresolved->base.token->atom, given_unresolved->base.token->atom)){
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
        
        if(!atoms_match(wanted_compound->identifier, given_compound->identifier)) return null;
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
        if(!atoms_match(wanted_enum->identifier, given_enum->identifier)) return null;
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

func struct string report_type_mismatch__internal(struct context *context, char *prefix, struct ast_type *type, struct ast *defined_type){
    struct string ret = zero_struct;
    
    b32 handled = false;
    if(defined_type){
        if(defined_type->kind == AST_typedef){
            struct ast_declaration *decl = cast(struct ast_declaration *)defined_type;
            struct string lhs_string = push_type_string(context->arena, &context->scratch, decl->type);
            ret = push_format_string(&context->scratch, "%s '%.*s' (aka %.*s)", prefix, decl->identifier->amount, decl->identifier->data, lhs_string.amount, lhs_string.data);
            handled = true;
        }else if(defined_type->kind == AST_enum){
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

func void report_type_mismatch_error(struct context *context, struct ast_type *lhs, struct ast *lhs_defined_type, struct ast_type *rhs, struct ast *rhs_defined_type, struct token *location){
    struct string lhs_string = report_type_mismatch__internal(context, "Wanted", lhs, lhs_defined_type);
    struct string rhs_string = report_type_mismatch__internal(context, "given", rhs, rhs_defined_type);
    
    report_error(context, location, "%.*s %.*s.", lhs_string.amount, lhs_string.data, rhs_string.amount, rhs_string.data);
}


func void report_type_mismatch_warning(struct context *context, struct ast_type *lhs, struct ast *lhs_defined_type, struct ast_type *rhs, struct ast *rhs_defined_type, struct token *location){
    struct string lhs_string = report_type_mismatch__internal(context, "Wanted", lhs, lhs_defined_type);
    struct string rhs_string = report_type_mismatch__internal(context, "given", rhs, rhs_defined_type);
    
    report_warning(context, WARNING_arithmetic_type_mismatch, location, "%.*s %.*s.", lhs_string.amount, lhs_string.data, rhs_string.amount, rhs_string.data);
}

func b32 casts_implicitly_to_bool(struct ast *ast){
    assert(ast->resolved_type);
    
    if(&globals.typedef_void == ast->resolved_type){
        return false;
    }
    
    if(ast->resolved_type->kind == AST_struct || ast->resolved_type->kind == AST_union){
        return false;
    }
    
    if(ast->resolved_type->kind == AST_function_type){
        return false;
    }
    
    if(ast->resolved_type->kind == AST_array_type){
        return false; // we should have loaded the array. (also this does not make that much sense)
    }
    
    return true;
}

//_____________________________________________________________________________________________________________________
// 

enum type_qualifiers{
    QUALIFIER_const     = 0x1,
    QUALIFIER_volatile  = 0x2,
    QUALIFIER_restrict  = 0x4,
    QUALIFIER_unaligned = 0x8,
    
    QUALIFIER_ptr32     = 0x10,
    QUALIFIER_ptr64     = 0x20,
};

struct declarator_return{
    struct token *ident;
    struct ast_type *type;
    struct ast *defined_type;
};

func struct ast_declaration *push_declaration_for_declarator(struct context *context, struct declarator_return declarator){
    struct ast_declaration *decl = parser_ast_push(context, declarator.ident, declaration);
    decl->type         = declarator.type;
    decl->defined_type = declarator.defined_type;
    decl->identifier   = declarator.ident;
    decl->offset_on_stack = -1;
    set_resolved_type(&decl->base, &globals.typedef_void, null);
    
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
    // SPECIFIER_thread_local
    
    // 
    // __declspec storage-class specifiers.
    // 
    SPECIFIER_dllimport = 0x10,
    SPECIFIER_dllexport = 0x20,
    SPECIFIER_selectany = 0x40,
    
    SPECIFIER_packed    = 0x80,
    
    // 
    // function-specifiers
    // 
    SPECIFIER_inline     = 0x100,
    SPECIFIER_noreturn   = 0x200,
    SPECIFIER_inline_asm = 0x400,
    SPECIFIER_printlike  = 0x800,
    
};

struct declaration_specifiers{
    
    struct ast *defined_type_specifier;
    struct ast_type *type_specifier;
    u64 specifier_flags;
    
    u64 alignment;
};

// :forward_declarations :predeclarations
func struct declaration_specifiers parse_declaration_specifiers(struct context *context, struct ast_type *optional_type_specifier, struct ast *optional_defined_type_speicifer);
func struct ast *parse_expression(struct context *context, b32 should_skip_comma_expression);
func struct declaration_list parse_declaration_list(struct context *context, struct ast_type *optional_type, struct ast *optional_defined_type);
func struct declarator_return parse_declarator(struct context *context, struct ast_type *initial_type, struct ast *initial_defined_type, enum declarator_kind_flags declarator_kind_flags);

func struct ast *parse_declaration_list_in_imperative_scope(struct context *context, struct ast_type *optional_type, struct ast *optional_defined_type){
    assert(context->current_scope);
    
    struct declaration_list list = parse_declaration_list(context, optional_type, optional_defined_type);
    if(context->should_exit_statement) return &globals.empty_statement;
    
    // set 'decl->offset_on_stack' for all local declarations
    for(struct declaration_node *node = list.first; node; node = node->next){
        struct ast_declaration *decl = node->decl;
        if(decl->base.kind == AST_typedef) continue;
        if(decl->base.kind == AST_function) continue;
        
        if(!(decl->flags & DECLARATION_FLAGS_is_local_persist)){
            parser_emit_memory_location(context, decl);
        }
    }
    
    if(list.first && list.first == list.last){
        // we have one element, return it as is, no need for a list
        return &list.first->decl->base;
    }
    
    // push 'AST_declaration_list' for this 'declaration_list'
    
    struct token *token = get_current_token_for_error_report(context);
    if(list.first) token = list.first->decl->base.token;
    
    struct ast_declaration_list *decl_list = parser_ast_push(context, token, declaration_list);
    decl_list->list = list;
    
    set_resolved_type(&decl_list->base, &globals.typedef_void, null);
    return &decl_list->base;
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
    
    struct ast_unresolved_type *unresolved = parser_type_push(context, sleep_on, unresolved_type);
    unresolved->compilation_unit = context->current_compilation_unit;
    unresolved->base.size = -1; // hopefully we fuck up the system enough so that it asserts if we ever accidently use this
    unresolved->containing_scope = context->current_scope;
    unresolved->kind = kind;
    
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

func b32 check_for_basic_types__internal(struct context *context, struct ast *ast, u32 flags, char *prefix, struct token *token){
    struct ast_type *type = ast->resolved_type;
    
    b32 okay = false;
    if((flags & CHECK_integer) && (type->kind == AST_integer_type))  okay = true;
    if((flags & CHECK_integer) && (type->kind == AST_bitfield_type)) okay = true;
    if((flags & CHECK_pointer) && (type->kind == AST_pointer_type))  okay = true;
    if((flags & CHECK_pointer) && (type->kind == AST_array_type))    okay = true;
    if((flags & CHECK_float)   && (type->kind == AST_float_type))    okay = true;
    
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
func b32 check_unary_for_basic_types(struct context *context, struct ast *ast, u32 flags, struct token *location){
    return check_for_basic_types__internal(context, ast, flags, "Operand of unary", location);
}

func b32 check_binary_for_basic_types(struct context *context, struct ast_binary_op *op, u32 flags){
    b32 lhs = check_for_basic_types__internal(context, op->lhs, flags, "Left of", op->base.token);
    b32 rhs = check_for_basic_types__internal(context, op->rhs, flags, "Right of", op->base.token);
    return (lhs && rhs);
}

#if defined(__clang__)
#define __readeflags __builtin_ia32_readeflags_u64
#endif

func struct ast *push_dot_or_arrow(struct context *context, struct compound_member *found, struct ast *operand, enum ast_kind ast_kind, struct token *test){
    
    
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
    
    if(operand->kind == AST_pointer_literal){
        assert(ast_kind == AST_member_deref); // We cannot use '.' on a pointer.
        
        // 
        // Adjust the pointer to be a pointer literal pointing to the member.
        // 
        struct ast_pointer_literal *pointer = (struct ast_pointer_literal *)operand;
        pointer->pointer += found->offset_in_type;
        
        struct ast_type *pointer_type = parser_push_pointer_type(context, found->type, found->defined_type, test);
        set_resolved_type(&pointer->base, pointer_type, null);
        
        // 
        // Push a dereference of of the adjusted pointer-literal.
        // 
        struct ast *ret = ast_push_unary_expression(context, AST_unary_deref, test, operand);
        set_resolved_type(ret, found->type, found->defined_type);
        return ret;
    }else{
        struct ast_dot_or_arrow *op = (struct ast_dot_or_arrow *)_parser_ast_push(context, test, sizeof(struct ast_dot_or_arrow), alignof(struct ast_dot_or_arrow), ast_kind);
        op->lhs = operand;
        op->member = found;
        
        set_resolved_type(&op->base, found->type, found->defined_type);
        return &op->base;
    }
}

func struct compound_member *find_member_in_compound(struct ast_compound_type *compound, struct atom ident){
    
    for(u32 index = 0; index < compound->amount_of_members; index++){
        if(atoms_match(compound->members[index].name->atom, ident)){
            return &compound->members[index];
        }
    }
    
    // This can probably happen for empty compounds.
    return null;
}

func struct ast *handle_dot_or_arrow(struct context *context, struct ast_compound_type *compound, struct ast *operand, enum ast_kind ast_kind, struct token *test){
    
    char *error = "Expected an identifier following '->'.";
    if(ast_kind == AST_member){
        error = "Expected an identifier following '.'.";
    }
    
    struct token *member = expect_token(context, TOKEN_identifier, error);
    if(context->should_exit_statement) return operand;
    
    struct compound_member *found = find_member_in_compound(compound, member->atom);
    if(!found){
        char *struct_or_union = (compound->base.kind == AST_struct) ? "structure" : "union";
        
        begin_error_report(context);
        report_error(context, member, "Identifier is not a member of %s.", struct_or_union);
        report_error(context, compound->base.token, "... Here is the definition of the %s.", struct_or_union);
        end_error_report(context);
        return operand;
    }
    
    return push_dot_or_arrow(context, found, operand, ast_kind, test);
}


func b32 ast_stack_push(struct context *context, struct ast *ast){
    if(context->ast_stack_at + 1 > array_count(context->ast_stack)){
        struct token *token = ast->token;
        
        smm i = context->ast_stack_at - 1;
        while(token->type == TOKEN_invalid && i >= 0){
            token = context->ast_stack[i--]->token;
        }
        
        assert(token->type != TOKEN_invalid);
        report_error(context, token, "Expression nests to deep.");
        return false;
    }
    context->ast_stack[context->ast_stack_at++] = ast;
    return true;
}

func struct ast *ast_stack_pop(struct context *context){
    assert(context->ast_stack_at > 0);
    struct ast *ret = context->ast_stack[--context->ast_stack_at];
    debug_only(context->ast_stack[context->ast_stack_at] = null);
    return ret;
}

func struct ast *ast_stack_current(struct context *context){
    if(context->ast_stack_at > 0){
        return context->ast_stack[context->ast_stack_at - 1];
    }
    return &globals.empty_statement;
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
    struct ast *defined_type_specifier = null;
    
    struct token *token = get_current_token(context);
    switch(token->type){
        case TOKEN_identifier:{
            struct ast_declaration *ast_typedef = lookup_typedef(context, context->current_compilation_unit, token, /*silent*/true);
            
            // This was not a typedef, hence '(identifier' does not start a type-name.
            if(!ast_typedef) return ret;
            
            next_token(context);
            type_specifier = ast_typedef->type;
            defined_type_specifier = &ast_typedef->base;
        }break;
        
        
        // 
        // C23 allows storage class specifiers for compound literals. @incomplete:
        // 
        // case TOKEN_static: case TOKEN_register:
        
        
        // 
        // All tokens staring a type-name:
        // 
        case TOKEN_volatile: case TOKEN_const: case TOKEN_unaligned: case TOKEN_restrict:
        
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

func struct ast *maybe_load_address_for_array_or_function(struct context *context, struct ast *ast){
    
    if(ast->resolved_type->kind == AST_array_type){
        // @hack: constant propergate an edge case needed for 'FIELD_OFFSET(struct s, array[1])';
        //        or written out: '(smm)&(((struct s *)0)->array[1])'.
        //        here '((struct s *)0)->array' gets subscripted and  therefore we load the address of it.
        
        struct ast *handled = null;
        
        if(ast->kind == AST_unary_deref){
            struct ast_unary_op *deref = cast(struct ast_unary_op *)ast;
            if(deref->operand->kind == AST_pointer_literal){
                handled = deref->operand;
            }
        }
        
        struct ast_array_type *array = cast(struct ast_array_type *)ast->resolved_type;
        if(handled){
            ast = handled;
        }else{
            ast = ast_push_unary_expression(context, AST_implicit_address_conversion, ast->token, ast);
        }
        struct ast_type *pointer_type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type, ast->token);
        set_resolved_type(ast, pointer_type, null);
    }else if(ast->resolved_type->kind == AST_function_type){
        struct ast *address = ast_push_unary_expression(context, AST_implicit_address_conversion, ast->token, ast);
        set_resolved_type(address, parser_push_pointer_type(context, ast->resolved_type, null, ast->token), null);
        ast = address;
    }
    return ast;
}

func struct ast *handle_pointer_arithmetic(struct context *context, struct ast *binary_op){
    struct ast_binary_op *op = cast(struct ast_binary_op *)binary_op;
    
    // @cleanup: where do we check for void?
    
    // op should probably be either '+', '-', '+=' or '-=' but whatever
    
    struct token *token = op->base.token;
    struct ast *pointer = op->lhs;
    struct ast *integer = op->rhs;
    
    assert(integer->resolved_type->kind == AST_integer_type);
    assert(pointer->resolved_type->kind == AST_pointer_type);
    struct ast_pointer_type *pointer_type = (struct ast_pointer_type *)pointer->resolved_type;
    
    set_resolved_type(binary_op, &pointer_type->base, pointer->defined_type);
    
    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer_type->pointer_to)) return binary_op;
    struct ast_type *deref_type = pointer_type->pointer_to;
    
    if(deref_type == &globals.typedef_void){
        report_error(context, token, "Cannot use pointer arithmetic on pointer to 'void'.");
        return binary_op;
    }
    
    if(deref_type->kind == AST_function_type){
        report_error(context, token, "Cannot use pointer arithmetic on function-pointer.");
        return binary_op;
    }
    
    assert(deref_type->size >= 0);
    
    struct ast *ret;
    if(integer->kind == AST_integer_literal){
        struct ast_integer_literal *lit = cast(struct ast_integer_literal *)integer;
        
        if(pointer->kind == AST_pointer_literal){
            struct ast_pointer_literal *pointer_lit = cast(struct ast_pointer_literal *)pointer;
            if(binary_op->kind == AST_binary_plus){
                pointer_lit->pointer += integer_literal_as_s64(integer);
            }else{
                pointer_lit->pointer -= integer_literal_as_s64(integer);
            }
            ret = pointer;
        }else{
            // Upconvert to s64.
            lit->_s64 = integer_literal_as_s64(integer);
            
            if(lit->_s64 != 0){
                
                // @cleanup: Check for compile time overflow?
                if(type_is_signed(lit->base.resolved_type)){
                    lit->_s64 *= deref_type->size;
                }else{
                    lit->_u64 *= deref_type->size;
                }
                
                set_resolved_type(&lit->base, &globals.typedef_u64, null);
                op->rhs = &lit->base;
                ret = &op->base;
            }else{
                ret = pointer;
            }
        }
    }else{
        if(integer->resolved_type != &globals.typedef_s64){
            integer = push_cast(context, &globals.typedef_s64, null, integer);
        }
        
        if(deref_type->size != 1){
            struct ast *size_lit = ast_push_s64_literal(context, token, deref_type->size);
            struct ast *new_index = ast_push_binary_expression(context, AST_binary_times, token, integer, size_lit);
            // @cleanup: s64?
            set_resolved_type(new_index, &globals.typedef_u64, null);
            integer = new_index;
        }
        
        op->lhs = pointer;
        op->rhs = integer;
        ret = &op->base;
    }
    
    assert(ret->resolved_type == pointer->resolved_type);
    return ret;
}

func struct ast *push_nodes_for_subscript(struct context *context, struct ast *lhs, struct ast *index, struct token *token){
    struct ast_type *lhs_type = lhs->resolved_type;
    assert(lhs_type->kind == AST_array_type || lhs_type->kind == AST_pointer_type);
    
    enum ast_kind subscript_kind = AST_none;
    struct ast_type *dereferenced_resolved_type = null;
    struct ast      *dereferenced_defined_type  = null;
    
    if(lhs_type->kind == AST_pointer_type){
        subscript_kind = AST_pointer_subscript;
        
        struct ast_pointer_type *pointer = (struct ast_pointer_type *)lhs_type;
        dereferenced_resolved_type = pointer->pointer_to;
        dereferenced_defined_type  = pointer->pointer_to_defined_type;
    }else if(lhs_type->kind == AST_array_type){
        subscript_kind = AST_array_subscript;
        
        struct ast_array_type *array = (struct ast_array_type *)lhs_type;
        
        if(lhs->kind == AST_unary_deref && index->kind == AST_integer_literal){
            // Transform
            //    (*((int (*) [])0))[3]
            // Into
            //    *((int *) 12)
            
            struct ast_unary_op *deref = (struct ast_unary_op *)lhs;
            if(deref->operand->kind == AST_pointer_literal){
                struct ast_pointer_literal *literal = (struct ast_pointer_literal *)deref->operand;
                literal->pointer += array->element_type->size * integer_literal_as_u64(index);
                
                struct ast_type *pointer_type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type, token);
                
                set_resolved_type(&literal->base, pointer_type, null);
                set_resolved_type(lhs, array->element_type, array->element_type_defined_type);
                
                return lhs;
            }
        }
        
        dereferenced_resolved_type = array->element_type;
        dereferenced_defined_type  = array->element_type_defined_type;
    }else{
        invalid_code_path;
    }
    
    index = maybe_insert_bitfield_cast(context, index);
    index = push_cast(context, &globals.typedef_s64, NULL, index);
    
    struct ast_subscript *subscript = _parser_ast_push(context, token, sizeof(struct ast_subscript), alignof(struct ast_subscript), subscript_kind);
    subscript->index = index;
    subscript->lhs = lhs;
    
    set_resolved_type(&subscript->base, dereferenced_resolved_type, dereferenced_defined_type);
    
    return &subscript->base;
}

func struct ast *maybe_insert_implicit_assignment_cast_and_check_that_types_match(struct context *context, struct ast_type *lhs_type, struct ast *lhs_defined_type, struct ast *rhs, struct token *assignment){
    
    rhs = maybe_insert_bitfield_cast(context, rhs);
    
    if(lhs_type == rhs->resolved_type) return rhs;
    if(rhs->resolved_type == &globals.typedef_poison) return rhs;
    
    //@cleanup: should we spread here? we could corrupt a global
    if(lhs_type == &globals.typedef_poison) return rhs;
    
    struct ast_type *rhs_type = rhs->resolved_type;
    
    b32 should_report_warning = false;
    b32 should_push_cast = false;
    
    if(lhs_type == &globals.typedef_Bool){
        if(type_is_arithmetic(rhs_type) || rhs_type->kind == AST_pointer_type){
            // these are fine, never warn, but also never report a warning.
            should_push_cast = true;
        }
        
    }else if(lhs_type->kind == AST_bitfield_type && (rhs_type->kind == AST_integer_type || rhs_type->kind == AST_float_type)){
        // 
        // Assigning from a integer or float to a bitfield.
        // 
        
        struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)lhs_type;
        if(rhs_type != bitfield->base_type){
            // @cleanup: bitfield->defined_type
            rhs = push_cast(context, bitfield->base_type, null, rhs);
        }
        should_push_cast = true;
    }else if(lhs_type->kind == AST_pointer_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        
        // 'pointer' = 'integer'
        if(rhs->kind == AST_integer_literal){
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
            should_push_cast = true;
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
                should_push_cast = true;
            }
        }
    }else if(lhs_type->kind == AST_float_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        if(rhs->kind != AST_integer_literal){
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
            assert(false); // should not be here... Their sizes are equal, thus they should be equal.
        }
    }else if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        
        if(rhs->kind == AST_integer_literal && assignment->type != TOKEN_and_equals){
            b32 fits;
            if(type_is_signed(lhs_type)){
                s64 value = integer_literal_as_s64(rhs);
                switch(lhs_type->size){
                    case 1: fits = (min_s8 <= value && value <= max_s8); break;
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
            struct ast_type *type = rhs_type;
            // :retain_type_information_through_promotion
            if(rhs->defined_type){
                if(rhs->defined_type->kind == AST_integer_type){
                    type = (struct ast_type *)rhs->defined_type;
                }else if(rhs->defined_type->kind == AST_typedef){
                    struct ast_declaration *decl = cast(struct ast_declaration *)rhs->defined_type;
                    assert(decl->type->kind == AST_integer_type);
                    type = decl->type;
                }
            }
            assert(type->size <= rhs_type->size);
            
            // @incomplete: right now this is kinda annoying. maybe we should propagate min and max values...
            //              and then check these, i.e get rid of the 'retain_type_information_through_promotion'
            //              These min and max values could be logged as 'log'.
#if 0
            if(lhs_type->size > type->size){
                if(!type_is_signed(type)){
                    // these are always fine 's32' = 'u8'
                }else if(type_is_signed(lhs_type) == type_is_signed(type)){
                    // these are fine: 'u32' = 'u16' or '
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
#endif
        }
    }
    
    if(should_report_warning){
        report_type_mismatch_warning(context, lhs_type, lhs_defined_type, rhs->resolved_type, rhs->defined_type, assignment);
    }
    
    if(should_push_cast){
        rhs = push_cast(context, lhs_type, lhs_defined_type, rhs);
    }else{
        if(!types_are_equal(lhs_type, rhs->resolved_type)){
            report_type_mismatch_error(context, lhs_type, lhs_defined_type, rhs->resolved_type, rhs->defined_type, assignment);
        }
    }
    
    return rhs;
}

struct designator_node{
    struct designator_node *next;
    struct ast *lhs;
    smm member_at;
    smm array_at;
};

func struct ast *push_current_object_for_designator_node(struct context *context, struct designator_node *designator_node, struct token *site){
    
    struct ast *current_object = designator_node->lhs;
    if(current_object->resolved_type->kind == AST_array_type){
        struct ast *index = ast_push_s64_literal(context, site, designator_node->array_at);
        current_object = push_nodes_for_subscript(context, current_object, index, site);
    }else{
        struct ast_compound_type *compound = (struct ast_compound_type *)current_object->resolved_type;
        current_object = push_dot_or_arrow(context, &compound->members[designator_node->member_at], current_object, AST_member, site);
    }
    return current_object;
}

func struct ast_list parse_initializer_list(struct context *context, struct ast *ast, smm *out_trailing_initializer_size){
    
    struct token *initial_open_curly = next_token(context);
    assert(initial_open_curly->type == TOKEN_open_curly);
    
    struct ast_list ret = {0};
    
    if(ast->resolved_type->kind != AST_array_type && ast->resolved_type->kind != AST_struct && ast->resolved_type->kind != AST_union){
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
            ret = parse_initializer_list(context, ast, out_trailing_initializer_size);
            expect_token(context, TOKEN_closed_curly, "Expected '}'."); // :Error, but it does not matter, noones ever going to see this.
            return ret;
        }
        
        struct ast *initializer = null;
        if(peek_token(context, TOKEN_closed_curly)){
            // :Extension
            // 
            // This is an extension, we allow 'int a = {}' and '(int){}'.
            // GCC/Clang complain about empty scalar initializer, but I think its pretty obvious 
            // what you want here and makes some type-agnostic macros work.
            initializer = ast_push_s32_literal(context, get_current_token(context), 0);
        }else{
            // 
            // 'int a = {1, 2, 3}' only generates a warning in gcc and clang.
            // but the resulting value of a is '1'.
            // So we error on it.
            // 
            initializer = parse_expression(context, /*should_skip_comma_expression*/true);
        }
        
        peek_token_eat(context, TOKEN_comma); // Technically, an initializer list is allowed to have a trailing comma.
        
        expect_token(context, TOKEN_closed_curly, "More then one member in initializer-list for scalar type.");
        
        initializer = maybe_load_address_for_array_or_function(context, initializer);
        initializer = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, ast->resolved_type, ast->defined_type, initializer, initial_open_curly);
        
        struct ast *assignment = ast_push_binary_expression(context, AST_assignment, initial_open_curly, ast, initializer);
        
        set_resolved_type(assignment, ast->resolved_type, ast->defined_type);
        
        ast_list_append(&ret, context->arena, assignment);
        
        return ret;
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
    
    struct designator_node initial_node = {.lhs = ast};
    
    if(ast->resolved_type->kind == AST_struct || ast->resolved_type->kind == AST_union){
        struct ast_compound_type *ast_struct = cast(struct ast_compound_type *)ast->resolved_type;
        
        if(!ast_struct->amount_of_members){
            //
            // struct {} x = {}; is allowed, struct {} x = {10}; is not!
            //
            if(!peek_token(context, TOKEN_closed_curly)){
                begin_error_report(context);
                report_error(context, get_current_token_for_error_report(context), "Cannot initalize an empty structure with non-empty initializer-list.");
                report_error(context, ast_struct->base.token, "... Here is the definition of the empty structure.");
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
        struct ast *current_object = null;
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
                current_object = designator_stack.first->lhs;
                
                if(token->type == TOKEN_dot){
                    struct ast_type *type = current_object->resolved_type;
                    if(type->kind != AST_struct && type->kind != AST_union){
                        struct token *identifier = expect_token(context, TOKEN_identifier, "Expected an identifier after '.' in initializer list.");
                        
                        struct string type_string = push_type_string(context->arena, &context->scratch, type);
                        report_error(context, token, "Field designator '.%.*s' can only be used for structs or unions and not '%.*s'.", identifier->size, identifier->data, type_string.size, type_string.data);
                        goto error;
                    }
                    
                    struct ast_dot_or_arrow *dot = (struct ast_dot_or_arrow *)handle_dot_or_arrow(context, (struct ast_compound_type *)type, current_object, AST_member, token);
                    if(context->should_exit_statement) goto error;
                    
                    struct designator_node *new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->lhs = &dot->base;
                    sll_push_front(designator_stack, new_node);
                    
                }else if(token->type == TOKEN_open_index){
                    
                    if(current_object->resolved_type->kind != AST_array_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, current_object->resolved_type);
                        report_error(context, token, "Array designator can only be used for arrays and not '%.*s'.", type_string.size, type_string.data);
                        goto error;
                    }
                    
                    struct ast_array_type *array = (struct ast_array_type *)current_object->resolved_type;
                    
                    struct ast *index = parse_expression(context, /*should_skip_comma_epression*/false);
                    if(index->kind != AST_integer_literal){
                        report_error(context, index->token, "Expected a constant integer expression in array subscript.");
                        goto error;
                    }
                    
                    u64 value;
                    if(type_is_signed(index->resolved_type)){
                        s64 signed_value = integer_literal_as_s64(index);
                        if(signed_value < 0){
                            report_error(context, index->token, "Array subscript cannot be negative.");
                            goto error;
                        }
                        
                        value = (u64)signed_value;
                    }else{
                        value = integer_literal_as_u64(index);
                    }
                    
                    if(value >= 0x7fffffffffffffff){
                        report_error(context, index->token, "Array subscript is to big.");
                        goto error;
                    }
                    
                    expect_token(context, TOKEN_closed_index, "Expected ']' at the end of array designator.");
                    
                    if(!array->is_of_unknown_size && (smm)value >= array->amount_of_elements){
                        report_error(context, index->token, "Index '%llu' out of bounds. Array has only '%llu' elements.", value, array->amount_of_elements);
                        goto error;
                    }
                    
                    designator_stack.first->array_at = value;
                    
                    struct designator_node *new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->lhs = push_nodes_for_subscript(context, current_object, index, token);
                    
                    sll_push_front(designator_stack, new_node);
                }
            } while(peek_token(context, TOKEN_dot) || peek_token(context, TOKEN_open_index));
            
            site = expect_token(context, TOKEN_equals, "Expected an '=' after initializer designation.");
            
            // 
            // At this point, the new current object is the top of the designator stack.
            // This means that for the rest of the code, the 'designator_stack' is one too far.
            // Hence, we get the current object, and then pop it off the stack.
            // 
            current_object = designator_stack.first->lhs;
            sll_pop_front(designator_stack);
        }else{
            
            if(!designator_stack.first){
                // :too_many_initializers
                //
                // here we are in a case where we ran out of designators (meaning we initialized everything)
                // but then the initializer list did not end and also did not have a designator, to reset 
                // the current_object, therefore report an error!
                //
                struct ast_type *type = ast->resolved_type;
                
                if(type->kind == AST_struct || type->kind == AST_union){
                    struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
                    char *structure_or_union = (compound->base.kind == AST_struct) ? "structure" : "union";
                    
                    begin_error_report(context);
                    report_error(context, site, "Too many initializers for %s '%.*s'.", structure_or_union, compound->identifier.size, compound->identifier.data);
                    report_error(context, compound->base.token, "... Here was the %s defined.", structure_or_union);
                    end_error_report(context);
                }else{
                    assert(type->kind == AST_array_type);
                    report_error(context, site, "Too many initializers for array.");
                }
                goto error;
            }
            
            current_object = push_current_object_for_designator_node(context, designator_stack.first, site);
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
            struct ast_list sub_initializers = parse_initializer_list(context, current_object, &trailing_sub_initializer_size);
            
            // :trailing_arrays
            // 
            // This handles stuff like
            //     struct { int a; int trailling_array[]; } asd = {1, {1, 2, 3}};
            //
            trailing_initializer_size = max_of(trailing_initializer_size, trailing_sub_initializer_size);
            
            // 
            // Append the assignments to our list.
            // 
            sll_push_back_list(ret, sub_initializers);
            ret.count += sub_initializers.count;
        }else if(peek_token(context, TOKEN_embed)){
            // 
            // This is data specified by a #embed directive.
            // The actual data is specified by the 'string' from the token.
            // Currently, we are only allowing initializing 'char[]' and 'unsigned char[]'.
            // 
            struct token *embed = next_token(context);
            struct string file_data = embed->string;
            struct designator_node *designator_node = designator_stack.first;
            struct ast_array_type *array_type = (struct ast_array_type *)designator_node->lhs->resolved_type;
            
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
            
            struct ast_embed *ast_embed = parser_ast_push(context, embed, embed);
            
            struct ast *assignment = ast_push_binary_expression(context, AST_assignment, embed, current_object, &ast_embed->base);
            set_resolved_type(assignment, current_object->resolved_type, current_object->defined_type);
            ast_list_append(&ret, context->arena, assignment);
            
            // @note: We need to add -1 here, because the code below assumes it was not incremented.
            designator_node->array_at += (file_data.size - 1); 
        }else{
            struct ast *expression = parse_expression(context, /*skip_comma_expression*/true);
            
            while(true){
                
                if(current_object->resolved_type->kind == AST_array_type && expression->kind == AST_string_literal){
                    // 
                    // This is the case for:
                    //      struct { u8 array[8]; } asd = {"asd"};
                    // 
                    struct ast_array_type *wanted_array = (struct ast_array_type *)current_object->resolved_type;
                    struct ast_array_type *given_array = (struct ast_array_type *)expression->resolved_type;
                    
                    if(wanted_array->element_type->size == given_array->element_type->size){
                        
                        // @cleanup: arrays of unknown size?
                        if(wanted_array->amount_of_elements == (given_array->amount_of_elements - 1)){
                            // Special case for 
                            //    struct { u8 array[3]; } asd = {"asd"};
                            // which should work and just not write a zero terminator.
                            
                        }else if(wanted_array->amount_of_elements < given_array->amount_of_elements){
                            report_error(context, site, "Array initialized to string literal of size %lld, but the array size is only %lld.", given_array->amount_of_elements, wanted_array->amount_of_elements);
                            goto error;
                        }
                        
                        struct ast *assignment = ast_push_binary_expression(context, AST_assignment, expression->token, current_object, expression);
                        set_resolved_type(assignment, current_object->resolved_type, current_object->defined_type);
                        
                        ast_list_append(&ret, context->arena, assignment);
                        break;
                    }
                }
                
                if(types_are_equal(expression->resolved_type, current_object->resolved_type)){
                    //
                    // This checks for struct {v2 a;} a = { return_v2() };
                    //
                    
                    struct ast *assignment = ast_push_binary_expression(context, AST_assignment, expression->token, current_object, expression);
                    set_resolved_type(assignment, current_object->resolved_type, current_object->defined_type);
                    
                    ast_list_append(&ret, context->arena, assignment);
                    break;
                }
                
                struct designator_node *new_node = null;
                struct ast_type *type = current_object->resolved_type;
                
                if(type->kind == AST_struct || type->kind == AST_union){
                    struct ast_compound_type *compound = (struct ast_compound_type *)type;
                    
                    if(!compound->amount_of_members){
                        report_error(context, site, "Initializing empty structure or union requires explicit '{}'.");
                        goto error;
                    }
                    
                    // 
                    // Recurse into the sub struct or union.
                    // 
                    new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->lhs = current_object;
                    new_node->member_at = 0;
                }else if(type->kind == AST_array_type){
                    new_node = push_struct(&context->scratch, struct designator_node);
                    new_node->lhs = current_object;
                }else{
                    expression = maybe_load_address_for_array_or_function(context, expression);
                    expression = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, current_object->resolved_type, current_object->defined_type, expression, site);
                    
                    struct ast *assignment = ast_push_binary_expression(context, AST_assignment, expression->token, current_object, expression);
                    set_resolved_type(assignment, current_object->resolved_type, current_object->defined_type);
                    ast_list_append(&ret, context->arena, assignment);
                    
                    break;
                }
                
                sll_push_front(designator_stack, new_node);
                current_object = push_current_object_for_designator_node(context, designator_stack.first, site);
            }
        }
        
        while(true){
            struct designator_node *node = designator_stack.first;
            struct ast_type *type = node->lhs->resolved_type;
            
            if(type->kind == AST_struct){
                struct ast_compound_type *compound = (struct ast_compound_type *)type;
                struct compound_member *member = &compound->members[node->member_at];
                
                if(member->name == globals.invalid_identifier_token){
                    // :member_list_contains_both_linear_and_nested
                    // 
                    // Skip the linear members once we are done with the nested type.
                    struct ast_compound_type *nested_compound = (struct ast_compound_type *)member->type;
                    node->member_at += nested_compound->amount_of_members;
                }
                node->member_at += 1;
                
                if(node->member_at < compound->amount_of_members) break;
                
            }else if(type->kind == AST_union){
                // pop the union! Only ever initialize one member of the union.
                // The current object should be the next thing after the union.
            }else{
                struct ast_array_type *array = (struct ast_array_type *)type;
                
                node->array_at++;
                
                if(array->is_of_unknown_size){
                    
                    // :trailing_arrays
                    // 
                    // If we are initializing an array of unknown size, we are at the trailling 
                    // array member of the structure, otherwise the parsing code would have rejected
                    // the structure.
                    // 
                    
                    smm amount_of_elements = node->array_at;
                    smm element_size = array->element_type->size;
                    
                    smm array_size = amount_of_elements * element_size; // @cleanup: overflow
                    
                    trailing_initializer_size = max_of(trailing_initializer_size, array_size);
                    break;
                }
                if(node->array_at < array->amount_of_elements) break;
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
    return ret;
}


func struct ast *parse_initializer(struct context *context, struct ast_identifier *lhs, struct token *equals){
    
    // 
    // We call this function for
    //     1) int a = ?;
    //     2) int a = {?};
    //     3) (int){?}
    //     4) (struct s){?}
    //     5) struct s a = ?;
    //     6) struct s a = {?};
    // 
    struct ast *ret = null;
    
    struct ast_type *type = lhs->base.resolved_type;
    
    if(peek_token(context, TOKEN_open_curly)){
        // 
        // Parse an initializer like 'struct s asd = {1,2,3};' or '(struct s){1,2,3};'.
        // 
        
        struct ast_compound_literal *compound_literal = parser_ast_push(context, get_current_token(context), compound_literal);
        set_resolved_type(&compound_literal->base, lhs->base.resolved_type, lhs->base.defined_type);
        
        compound_literal->decl = lhs->decl;
        
        compound_literal->assignment_list = parse_initializer_list(context, &lhs->base, &compound_literal->trailing_array_size);
        ret = &compound_literal->base;
        
        if(context->should_exit_statement) return ret;
    }else{
        // 
        // Parse an initializer like:
        //     struct s a = <expr>
        //     int a = <expr>
        // 
        
        //
        // 'int a = 1, 2, 3;' Is not legal.
        //
        struct ast *expr = parse_expression(context, /*skip_comma_expression*/true);
        
        if(context->should_exit_statement) return expr;
        
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
            
            if(expr->kind != AST_string_literal){
                // :Error print the string literal thing only for integer arrays?
                report_error(context, equals, "Array can only be initialized by {initializer-list} or \"string literal\".");
                return expr;
            }
            
            // If type is an array of unknown size, patch in the size
            // we get in this for example for 'char asd[] = "asd";' or struct literals.
            struct ast_array_type *lhs_type = (struct ast_array_type *)type;
            struct ast_array_type *rhs_type = (struct ast_array_type *)expr->resolved_type;
            
            if(lhs_type->element_type->kind != AST_integer_type){
                report_error(context, equals, "Array initialized by string literal has to be of integer type.");
                return expr;
            }
            
            if(lhs_type->element_type->size != rhs_type->element_type->size){
                // :error
                report_error(context, equals, "Mismatching array element types.");
                return expr;
            }
            
            // @note: For string literals we can right now have signed unsigned mismatches.
            
            if(lhs_type->is_of_unknown_size){
                // 
                // This might be called recursively from 'parse_initializer_list'. // @cleanup: 'parse_initializer_list' does not call 'parse_initializer' anymore I think.
                // Hence, don't patch the array size here, as we would not want to patch it for
                // trailing arrays of structures.
                // 
            }else if(lhs_type->amount_of_elements == (rhs_type->amount_of_elements - 1)){
                // 
                // Here we are in the 'char a[3] = "asd"' case.
                // This should not be an error, we just don't append the zero to the array.
                // 
            }else if(lhs_type->amount_of_elements < rhs_type->amount_of_elements){
                report_error(context, equals, "Array initialized to string literal of size %lld, but the array size is only %lld.", rhs_type->amount_of_elements, lhs_type->amount_of_elements);
                return expr;
            }
            
        }else{
            expr = maybe_load_address_for_array_or_function(context, expr);
            expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, type, lhs->base.defined_type, expr, equals);
        }
        
        ret = ast_push_binary_expression(context, AST_assignment, equals, &lhs->base, expr);
        
        set_resolved_type(ret, lhs->base.resolved_type, lhs->base.defined_type);
    }
    
    return ret;
}

// @note: for compare_exressions we set the resolved_type afterwards
func struct ast *perform_float_operation(struct context *context, struct ast_binary_op *op){
    struct ast_float_literal *lhs = (struct ast_float_literal *)op->lhs;
    struct ast_float_literal *rhs = (struct ast_float_literal *)op->rhs;
    
    assert(lhs->base.kind == AST_float_literal && rhs->base.kind == AST_float_literal);
    // @cleanup: how to deal with float vs double
    
    s32 compare_value = -1;
    
    switch(op->base.kind){
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
    
    if(compare_value != -1){
        // "The result has type int".
        return ast_push_s32_literal(context, op->base.token, compare_value);
    }
    
    return &lhs->base;
}

// @note: for compare_exressions we set the resolved_type afterwards
func struct ast *perform_integer_operation(struct context *context, struct ast_binary_op *op){
    struct ast_integer_literal *lhs = cast(struct ast_integer_literal *)op->lhs;
    struct ast_integer_literal *rhs = cast(struct ast_integer_literal *)op->rhs;
    
    assert(lhs->base.kind == AST_integer_literal && rhs->base.kind == AST_integer_literal);
    
    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
    
#define __perform_integer_operation(type)\
type lhs_val = integer_literal_as_##type(&lhs->base); \
type rhs_val = integer_literal_as_##type(&rhs->base);\
switch(op->base.kind){\
    case AST_binary_times:   lhs->_##type = lhs_val * rhs_val; break;\
    case AST_binary_divide:{\
        if(rhs_val == 0){\
            report_error(context, op->base.token, "Integer division by 0.");\
            return &lhs->base;\
        }\
        lhs->_##type = lhs_val / rhs_val; \
    }break;\
    case AST_binary_mod:{\
        if(rhs_val == 0){\
            report_error(context, op->base.token, "Integer modulation by 0.");\
            return &lhs->base;\
        }\
        lhs->_##type = lhs_val % rhs_val; \
    }break;\
    case AST_binary_plus:    lhs->_##type = lhs_val + rhs_val; break;\
    case AST_binary_minus:   lhs->_##type = lhs_val - rhs_val; break;\
    \
    case AST_binary_logical_equals:   lhs->_u64 = (lhs_val == rhs_val); break;\
    case AST_binary_logical_unequals: lhs->_u64 = (lhs_val != rhs_val); break;\
    case AST_binary_bigger_equals:    lhs->_u64 = (lhs_val >= rhs_val); break;\
    case AST_binary_smaller_equals:   lhs->_u64 = (lhs_val <= rhs_val); break;\
    case AST_binary_bigger:           lhs->_u64 = (lhs_val > rhs_val);  break;\
    case AST_binary_smaller:          lhs->_u64 = (lhs_val < rhs_val);  break;\
    invalid_default_case();\
}\

    
    
    if(promoted_type->size == 4){
        if(promoted_type == &globals.typedef_u32){
            __perform_integer_operation(u32);
        }else{
            assert(promoted_type == &globals.typedef_s32);
            __perform_integer_operation(s32);
        }
    }else{
        assert(promoted_type->size == 8);
        if(promoted_type == &globals.typedef_u64){
            __perform_integer_operation(u64);
        }else{
            assert(promoted_type == &globals.typedef_s64);
            __perform_integer_operation(s64);
        }
    }
    
#undef __perform_integer_operation
    
    set_resolved_type(&lhs->base, promoted_type, null);
    return &lhs->base;
}


func b32 check_types_for_increment_or_decrement(struct context *context, struct ast *operand, char *token){
    
    // @cleanup: if we canonicalize pointers this could be globals.void_pointer
    if(operand->resolved_type->kind == AST_pointer_type){
        struct ast_pointer_type *pointer = (struct ast_pointer_type *)operand->resolved_type;
        if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return true;
        
        if(pointer->pointer_to == &globals.typedef_void){
            report_error(context, operand->token, "Operand of '%s' cannot be a void-pointer.", token);
            return true;
        }
        
        if(pointer->pointer_to->kind == AST_function_type){
            report_error(context, operand->token, "Operand of '%s' cannot be a function-pointer.", token);
            return true;
        }
    }
    
    if(!context->in_lhs_expression){
        report_error(context, operand->token, "Operand of '%s' must be an L-value.", token);
        return true;
    }
    
    if(operand->resolved_type->kind == AST_function_type){
        report_error(context, operand->token, "Operand of '%s' cannot be a function.", token);
        return true;
    }
    
    if(operand->resolved_type->kind == AST_array_type){
        report_error(context, operand->token, "Operand of '%s' cannot be an array.", token);
        return true;
    }
    
    return false;
}

// pushes a declaration without a name with type 'type' and 'defined_type', this declaration is not initialized, and the declaration 
// is not reachable. It then returns an identifier that references this declaration.
// This is used for struct literal kind of situations.
func struct ast_identifier *push_unnamed_declaration(struct context *context, struct ast_type *type, struct ast *defined_type, struct token *site){
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
    
    struct ast_identifier *ident = parser_ast_push(context, declarator.ident, identifier);
    ident->decl = decl;
    set_resolved_type(&ident->base, decl->type, decl->defined_type);
    
    return ident;
}

func void get_pretty_print_string_for_type(struct context *context, struct string_list *format_list, struct ast_list *arguments, struct ast *value, s32 depth_or_minus_one, struct string field_width, struct string precision, struct string type_specifiers){
    struct ast_type *type = value->resolved_type;
    
    switch(type->kind){
        case AST_union:
        case AST_struct:{
            struct ast_compound_type *compound = (struct ast_compound_type *)type;
            
            // @cleanup: Check the 'defined_type'?
            string_list_postfix(format_list, &context->scratch, type->kind == AST_struct ? string("(struct ") : string("(union "));
            string_list_postfix(format_list, &context->scratch, compound->identifier.string);
            string_list_postfix(format_list, &context->scratch, string(")"));
            
            if(depth_or_minus_one == -1){
                string_list_postfix(format_list, &context->scratch, string("{"));
            }else{
                string_list_postfix(format_list, &context->scratch, string("{\n"));
            }
            
            struct string spaces = zero_struct;
            if(depth_or_minus_one != -1){
                spaces = push_format_string(&context->scratch, "%*s", (depth_or_minus_one + 1) * 4, "");
            }
            
            for(u32 member_index = 0; member_index < compound->amount_of_members; member_index++){
                struct compound_member *member = &compound->members[member_index];
                if(member->name == globals.invalid_identifier_token) continue;
                
                if(depth_or_minus_one != -1){
                    string_list_postfix(format_list, &context->scratch, spaces);
                }
                
                string_list_postfix(format_list, &context->scratch, string("."));
                string_list_postfix(format_list, &context->scratch, member->name->string);
                string_list_postfix(format_list, &context->scratch, string(" = "));
                
                struct ast *value_to_recurse = push_dot_or_arrow(context, member, value, AST_member, value->token);
                if(depth_or_minus_one == -1){
                    get_pretty_print_string_for_type(context, format_list, arguments, value_to_recurse, -1, field_width, precision, type_specifiers);
                    if(member_index + 1 < compound->amount_of_members){
                        string_list_postfix(format_list, &context->scratch, string(", "));
                    }
                }else{
                    get_pretty_print_string_for_type(context, format_list, arguments, value_to_recurse, depth_or_minus_one + 1, field_width, precision, type_specifiers);
                    string_list_postfix(format_list, &context->scratch, string(",\n"));
                }
            }
            
            if(depth_or_minus_one != -1){
                string_list_postfix(format_list, &context->scratch, create_string(spaces.data, spaces.size - 4));
            }
            string_list_postfix(format_list, &context->scratch, string("}"));
        }break;
        
        case AST_array_type:{
            struct ast_array_type *array = cast(struct ast_array_type *)type;
            
            int is_string = 0;
            if(array->element_type == &globals.typedef_s8){
                for(u32 index = 0; index < type_specifiers.size; index++){
                    if(type_specifiers.data[index] == 's') is_string = 1;
                }
            }
            
            if(is_string){
                char *dot = precision.size ? "." : "";
                struct string format_string = push_format_string(&context->scratch, "%%%.*s%s%.*ss", field_width.size, field_width.data, dot, precision.size, precision.data);
                string_list_postfix(format_list, &context->scratch, format_string);
                ast_list_append(arguments, context->arena, value);
            }else{
                string_list_postfix(format_list, &context->scratch, string("{"));
                for(int i = 0; i < array->amount_of_elements; i++){
                    struct ast *index = ast_push_s32_literal(context, value->token, i);
                    
                    struct ast *value_to_recurse = push_nodes_for_subscript(context, value, index, value->token);
                    
                    // @cleanup: for now we don't specify the depth here... maybe we should
                    get_pretty_print_string_for_type(context, format_list, arguments, value_to_recurse, -1, field_width, precision, type_specifiers);
                    if(i + 1 != array->amount_of_elements){
                        string_list_postfix(format_list, &context->scratch, string(", "));
                    }
                }
                string_list_postfix(format_list, &context->scratch, string("}"));
            }
        }break;
        case AST_enum:{
            // @cleanup:
            string_list_postfix(format_list, &context->scratch, string("enum(%d)"));
            ast_list_append(arguments, context->arena, value);
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
            
            ast_list_append(arguments, context->arena, value);
        }break;
        case AST_bitfield_type:
        case AST_integer_type:{
            
            // for va_args: "the integer promotions are performed on each argument" 
            value = maybe_insert_integer_promotion_cast(context, value);
            
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
            ast_list_append(arguments, context->arena, value);
        }break;
        case AST_float_type:{
            
            char *dot = precision.size ? "." : "";
            char *postfix = type->size == 4 ? "f" : "";
            
            struct string format_string = push_format_string(&context->scratch, "%%%.*s%s%.*sf%s", field_width.size, field_width.data, dot, precision.size, precision.data, postfix);
            
            string_list_postfix(format_list, &context->scratch, format_string);
            
            if(type->size == 4){
                // for va_args: "arguments that have type float are promoted to double"
                // :retain_type_information_through_promotion
                value = push_cast(context, &globals.typedef_f64, cast(struct ast *)value->resolved_type, value);
            }
            
            ast_list_append(arguments, context->arena, value);
        }break;
        invalid_default_case();
    }
}

struct ast *maybe_insert_implicit_nodes_for_varargs_argument(struct context *context, struct ast *expr){
    expr = maybe_load_address_for_array_or_function(context, expr);
    
    if(expr->resolved_type->kind == AST_integer_type || expr->resolved_type->kind == AST_bitfield_type){
        // "The integer promotions are performed on each argument."
        expr = maybe_insert_integer_promotion_cast(context, expr);
    }
    
    if(expr->resolved_type == &globals.typedef_f32){
        // "Arguments that have type float are promoted to double."
        // :retain_type_information_through_promotion
        expr = push_cast(context, &globals.typedef_f64, (struct ast *)expr->resolved_type, expr);
    }
    
    return expr;
}

void printlike__infer_format_string_and_arguments_for_argument(struct context *context, struct ast *argument, struct string_list *pretty_print_list, struct ast_list *pretty_print_argument_list, struct string flags, struct string field_width, struct string precision, struct string type_specifiers){
    
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
        
        //
        // Save the struct to the compound in a temporary declaration, so we do not evaluate 'argument' more than once.
        // @cleanup: Why are we doing this by value?
        //           Maybe this is one answer: We cannot always take the address of 'argument', e.g.: 
        //           
        //               struct small_struct{ u64 value; };
        //               struct small_struct returns_small_struct(){ return (struct small_struct){0}; }
        //               print("{}", returns_small_struct());
        //
        struct ast_identifier *struct_identifier = push_unnamed_declaration(context, argument->resolved_type, null, argument->token);
        struct ast *assign = ast_push_binary_expression(context, AST_assignment, argument->token, &struct_identifier->base, argument);
        set_resolved_type(assign, argument->resolved_type, argument->defined_type);
        
        if(argument->resolved_type->flags & TYPE_FLAG_ends_in_array_of_unknown_size){
            // :compounds_with_trailing_array
            parser_emit_memory_location(context, struct_identifier->decl);
        }
        
        
        struct ast_list_node *argument_before_our_first = pretty_print_argument_list->last;
        
        //
        // Actually pretty print the argument to string.
        //
        get_pretty_print_string_for_type(context, pretty_print_list, pretty_print_argument_list, &struct_identifier->base, depth_or_minus_one, field_width, precision, type_specifiers);
        
        struct ast_list_node *our_first_argument = argument_before_our_first ? argument_before_our_first->next : pretty_print_argument_list->first;
        
        if(our_first_argument){
            struct ast *expr = maybe_insert_implicit_nodes_for_varargs_argument(context, our_first_argument->value);
            
            struct ast *comma = ast_push_binary_expression(context, AST_comma_expression, argument->token, assign, expr);
            set_resolved_type(comma, expr->resolved_type, expr->defined_type);
            our_first_argument->value = comma;
        }
    }else{
        // No need to push a declaration for these, they are just read once anyway, 
        // also no need to specify a depth.
        get_pretty_print_string_for_type(context, pretty_print_list, pretty_print_argument_list, argument, -1, field_width, precision, type_specifiers);
    }
}

struct ast *check_call_to_printlike_function(struct context *context, struct ast_function_call *call, struct ast *operand){
    
    struct ast_function_type *printlike_function_type = (struct ast_function_type *)call->identifier_expression->resolved_type;
    assert(printlike_function_type->base.kind == AST_function_type);
    
    //
    // Parse the arguments, without inserting any promotions.
    //
    if(!peek_token_eat(context, TOKEN_closed_paren)){
        do{
            struct ast *expr = parse_expression(context, true);
            if(context->should_exit_statement) return expr;
            
            if(expr->resolved_type == &globals.typedef_void){
                report_error(context, expr->token, "Expression of type void cannot be used as function argument.");
            }
            
            ast_list_append(&call->call_arguments, context->arena, expr);
        }while(peek_token_eat(context, TOKEN_comma));
        expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of parameter list.");
    }
    
    // @cleanup: allow -1 to allow 'print()' just printint a newline?
    if(printlike_function_type->argument_list.count > call->call_arguments.count){
        report_error(context, call->base.token, "Too few arguments to function.");
        return operand;
    }
    
    //
    // Get the varargs array, by skipping the fixed arguments.
    //
    struct ast_list_node *last_fixed_argument = null;
    
    struct ast_list variable_arguments = call->call_arguments;
    for(u32 argument_index = 0; argument_index < printlike_function_type->argument_list.count - 1; argument_index++){
        
        if(argument_index == printlike_function_type->argument_list.count - 2){
            last_fixed_argument = variable_arguments.first;
        }
        
        variable_arguments.count -= 1;
        variable_arguments.first = variable_arguments.first->next;
    }
    
    //
    // Build the fixed arguments, these are the arguments before the 'format-string' argument.
    //
    struct ast_list fixed_arguments = zero_struct;
    
    if(last_fixed_argument){
        last_fixed_argument->next = null;
        fixed_arguments.first = call->call_arguments.first;
        fixed_arguments.last  = last_fixed_argument;
        fixed_arguments.count = printlike_function_type->argument_list.count - 1;
    }
    
    //
    // Insert promotions casts for the 'fixed_arguments'.
    //
    for(struct ast_list_node *argument_node = fixed_arguments.first, *parameter_node = printlike_function_type->argument_list.first; argument_node; argument_node = argument_node->next, parameter_node = parameter_node->next){
        struct ast_declaration *decl = (struct ast_declaration *)parameter_node->value; 
        struct ast *expr = argument_node->value;
        
        if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) return expr;
        
        expr = maybe_load_address_for_array_or_function(context, expr);
        argument_node->value = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, decl->type, decl->defined_type, expr, expr->token);
    }
    
    
    // 
    // The first argument in the 'variable_arguments' is the format string, or if not present
    // the format string should be either {} or {:#}.
    // For large structures, more than 3 members we use the {:#}.
    // For small structures we use {}. If there is more than one argument the format string should be always {}.
    //
    
    struct ast_list_node *format_string_node = format_string_node = null;
    struct ast_string_literal *format_string_literal = null;
    if(variable_arguments.first->value->kind == AST_string_literal){
        //
        // We found the argument. Remove it from the list.
        //
        format_string_literal = (struct ast_string_literal *)variable_arguments.first->value;
        format_string_node = variable_arguments.first;
        
        variable_arguments.first = variable_arguments.first->next;
        variable_arguments.count -= 1;
    }
    
    struct string format_string = zero_struct;
    
    if(format_string_literal){
        format_string = format_string_literal->value;
    }else{
        //
        // We don't have a string literal. Build one based on the arguments. @cleanup: warn on 'char *' pointer?
        //
        
        //
        // @cleanup: add names if they are easy to add.
        //
        
        if(variable_arguments.count == 1){
            
            struct ast *argument = variable_arguments.first->value;
            if(argument->resolved_type->kind == AST_struct || argument->resolved_type->kind == AST_union){
                struct ast_compound_type *compound = (struct ast_compound_type *)argument->resolved_type;
                
                if(compound->amount_of_members > 3){
                    format_string = string("{:#}\n");
                }
            }
            
            if(!format_string.data){
                format_string = string("{}\n");
            }
            
            if(argument->kind == AST_identifier){
                format_string = push_format_string(context->arena, "%.*s = %.*s", argument->token->size, argument->token->data, format_string.size, format_string.data);
            }
        }else{
            struct string_list format_string_list = zero_struct;
            
            for(struct ast_list_node *argument_node = variable_arguments.first; argument_node; argument_node = argument_node->next){
                string_list_postfix(&format_string_list, &context->scratch, string("{} "));
            }
            
            format_string = string_list_flatten(format_string_list, &context->scratch);
            
            //
            // Replace the last " " with a newline.
            //
            assert(format_string.size);
            format_string.data[format_string.size-1] = '\n';
        }
    }
    
    struct ast_list_node *argument_node = variable_arguments.first;
    
    //
    // The arguments and the format string we will fill in.
    //
    struct string_list pretty_print_list          = zero_struct;
    struct ast_list    pretty_print_argument_list = zero_struct;
    
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
                // '%%' inserts one '%' into the string.
                //
                string_eat_front(&format_string, 2); // eat both '%%'
                start.size += 1;                     // keep one of the '%'
                string_list_postfix(&pretty_print_list, &context->scratch, start);
                continue;
            }else{
                //
                // Copy every thing that happened before the argument to the _pretty print format string_
                //
                string_list_postfix(&pretty_print_list, &context->scratch, start);
            }
            
            struct string it = format_string;
            
            string_eat_front(&it, 1); // eat the '%'
            
            struct string flags = string_eat_characters_front(&it, "+- #0");
            struct string field_width = zero_struct;
            struct string precision   = zero_struct;
            
            //
            // parse field width
            //
            if(it.size && it.data[0] == '*'){
                field_width = string_eat_front(&it, 1);
                
                //
                // @cleanup: these arguments have to be preserved
                //
                
                if(!argument_node) goto too_few_args_for_print;
                
                if(argument_node->value->resolved_type->kind != AST_integer_type){
                    struct string type_string = push_type_string(context->arena, &context->scratch, argument_node->value->resolved_type);
                    report_error(context,  argument_node->value->token, "Field width specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                    return operand;
                }
                
                struct ast_list_node *next = argument_node->next;
                sll_push_back(pretty_print_argument_list, argument_node);
                pretty_print_argument_list.count++;
                argument_node = next;
            }else{
                field_width = string_eat_characters_front(&it, "0123456789");
            }
            
            //
            // parse precision
            //
            if(it.size && it.data[0] == '.'){
                string_eat_front(&it, 1); // eat the '.'
                
                if(it.size && it.data[0] == '*'){
                    precision = string_eat_front(&it, 1);
                    
                    //
                    // @cleanup: these arguments have to be preserved
                    //
                    
                    if(!argument_node) goto too_few_args_for_print;
                    
                    if(argument_node->value->resolved_type->kind != AST_integer_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, argument_node->value->resolved_type);
                        report_error(context,  argument_node->value->token, "Precision specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                        return operand;
                    }
                    
                    struct ast_list_node *next = argument_node->next;
                    sll_push_back(pretty_print_argument_list, argument_node);
                    pretty_print_argument_list.count++;
                    argument_node = next;
                }else{
                    precision = string_eat_characters_front(&it, "0123456789");
                }    
            }
            
            //
            // parse length modifiers
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
                report_error(context, call->base.token, "Missing format specifier after '%.*s' in format string.", format_string.size, format_string.data);
                return operand;
            }
            
            u8 conversion_specifier = it.data[0];
            
            string_eat_front(&it, 1);
            
            struct string format_specifier = {
                .data = format_string.data,
                .size = it.data - format_string.data,
            };
            
            // we are done with parsing _commit_ the format string. 
            // This is sort of stupid, we should probably get rid of 'it'
            format_string = it;
            
            if(!argument_node) goto too_few_args_for_print;
            
            if(conversion_specifier == '?'){
                //
                // Special format specifier '?'. Infer the type!
                //
                
                struct string type_specifiers = {0};
                
                struct ast *argument = argument_node->value;
                argument_node = argument_node->next;
                
                if(length_modifier){
                    report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Length specifiers (hh, h, l, ll etc.) do not apply to format specifier '?'.");
                }
                
                printlike__infer_format_string_and_arguments_for_argument(context, argument, &pretty_print_list, &pretty_print_argument_list, flags, field_width, precision, type_specifiers);
            }else{
                //
                // Classic c format specifiers. Append the format specifier to the pretty print list as the user put it there.
                // Append the argument to the argument list.
                // Check the format string for possible errors.
                //
                string_list_postfix(&pretty_print_list, &context->scratch, format_specifier);
                
                struct ast_list_node *next = argument_node->next;
                struct ast *argument = argument_node->value;
                
                sll_push_back(pretty_print_argument_list, argument_node);
                pretty_print_argument_list.count += 1;
                
                argument_node = next;
                
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
                
                struct ast *defined_type = argument->defined_type;
                struct ast_type *unpromoted_type = argument->resolved_type;
                if(conversion_specifier == 'n'){
                    if(argument->resolved_type->kind != AST_pointer_type){
                        report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%s%c' expects a pointer.", (char *)&length_modifier, conversion_specifier);
                    }else{
                        struct ast_pointer_type *pointer = (struct ast_pointer_type *)argument->resolved_type;
                        defined_type    = pointer->pointer_to_defined_type;
                        unpromoted_type = pointer->pointer_to;
                        
                        conversion_specifier = 'd';
                    }
                }
                
                // :retain_type_information_through_promotion
                if(defined_type){
                    struct ast_type *original = unpromoted_type;
                    
                    if(defined_type->kind == AST_typedef){
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
                        
                        if(length_modifier == 'j' || length_modifier == 'z' || length_modifier == 't'){
                            
                            char *should_warn = null;
                            
                            struct string type_string = string("");
                            
                            if(defined_type && defined_type->kind == AST_typedef){
                                struct ast_declaration *ast_typedef = (struct ast_declaration *)defined_type;
                                type_string = ast_typedef->identifier->string;
                            }
                            
                            if(length_modifier == 'j'){
                                if((conversion_specifier == 'd' || conversion_specifier == 'i')){
                                    if(!string_match(type_string, string("intmax_t"))) should_warn = "intmax_t";
                                }else if((conversion_specifier == 'u')){
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
                                report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%s%c' expects %s.", (char *)&length_modifier, conversion_specifier, should_warn);
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
                                
                                if(expected_type != unpromoted_type){
                                    struct string expected_type_string = push_type_string(context->arena, &context->scratch, expected_type);
                                    struct string type_string          = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                    report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%s%c' expects %.*s, but the argument is of type '%.*s'.", (char *)&length_modifier, conversion_specifier, expected_type_string.size, expected_type_string.data, type_string.size, type_string.data);
                                }
                            }else if(conversion_specifier == 'u'){
                                struct ast_type *expected_type = &globals.typedef_u32;
                                if(length_modifier == 'hh') expected_type = &globals.typedef_u8;
                                if(length_modifier == 'h')  expected_type = &globals.typedef_u16;
                                if(length_modifier == 'l')  expected_type = &globals.typedef_u32;
                                if(length_modifier == 'll') expected_type = &globals.typedef_u64;
                                
                                if(expected_type != unpromoted_type){
                                    struct string expected_type_string = push_type_string(context->arena, &context->scratch, expected_type);
                                    struct string type_string          = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                    report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%s%c' expects %.*s, but the argument is of type '%.*s'.", (char *)&length_modifier, conversion_specifier, expected_type_string.size, expected_type_string.data, type_string.size, type_string.data);
                                }
                            }else{
                                //
                                // allow either signed an unsigned for 'o' 'x' 'X'.
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
                                
                                if(expected_signed_type != unpromoted_type && expected_unsigned_type != unpromoted_type){
                                    struct string signed_type_string   = push_type_string(context->arena, &context->scratch, expected_signed_type);
                                    struct string type_string          = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                    report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%s%c' expects signed or unsigned %.*s, but the argument is of type '%.*s'.", (char *)&length_modifier, conversion_specifier, signed_type_string.size, signed_type_string.data, type_string.size, type_string.data);
                                }
                            }
                        }else{
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                        }
                    }break;
                    
                    case 'c':{
                        if(length_modifier == 0){
                            if(unpromoted_type != &globals.typedef_u8 && unpromoted_type != &globals.typedef_s8){
                                struct string type_string = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%c' expects signed or unsigned character, but the argument is of type '%.*s'.", conversion_specifier, type_string.size, type_string.data);
                            }
                        }else if(length_modifier == 'l'){
                            if(unpromoted_type != &globals.typedef_u16 && unpromoted_type != &globals.typedef_s16){
                                struct string type_string = push_type_string(context->arena, &context->scratch, unpromoted_type);
                                report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%c' expects signed or unsigned character, but the argument is of type '%.*s'.", conversion_specifier, type_string.size, type_string.data);
                            }
                        }else{
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                        }
                    }break;
                    
                    case 'p':{
                        if(length_modifier != 0){
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                        }
                        
                        if(argument->resolved_type->kind != AST_pointer_type){
                            struct string type_string = push_type_string(context->arena, &context->scratch, argument->resolved_type);
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%%p' requires a pointer, but the argument is of type '%.*s'.", type_string.size, type_string.data);
                        }
                    }break;
                    
                    case 'e': case 'E':
                    case 'a': case 'A':
                    case 'g': case 'G':
                    case 'f': case 'F':{
                        
                        if(length_modifier != 0 && length_modifier != 'l' && length_modifier != 'L'){
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                        }
                        
                        if(argument->resolved_type->kind != AST_float_type){
                            struct string type_string = push_type_string(context->arena, &context->scratch, argument->resolved_type);
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%%f' requires a float, but the argument is of type '%.*s'.", type_string.size, type_string.data);
                        }
                    }break;
                    
                    case 's':{
                        struct ast_type *element_type = null;
                        if(argument->resolved_type->kind == AST_pointer_type){
                            struct ast_pointer_type *pointer = (struct ast_pointer_type *)argument->resolved_type;
                            element_type = pointer->pointer_to;
                        }else if(argument->resolved_type->kind == AST_array_type){
                            struct ast_array_type *array = (struct ast_array_type *)argument->resolved_type;
                            element_type = array->element_type;
                        }
                        
                        b32 should_warn = true;
                        if(length_modifier == 'l'){
                            if(element_type == &globals.typedef_s16 || element_type == &globals.typedef_u16) should_warn = false;
                        }else if(length_modifier == 0){
                            if(element_type == &globals.typedef_s8 || element_type == &globals.typedef_u8) should_warn = false;
                        }else{
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token ,"Format length specifier '%s' does not apply to conversion specifier %c (%s%c is not a correct format string syntax).", (char *)&length_modifier, conversion_specifier, (char *)&length_modifier, conversion_specifier);
                        }
                        
                        if(should_warn){
                            struct string type_string = push_type_string(context->arena, &context->scratch, argument->resolved_type);
                            report_warning(context, WARNING_incorrect_format_specifier, argument->token, "Format specifier '%%s' requires 'char *', but the argument is of type '%.*s'.", type_string.size, type_string.data);
                        }
                    }break;
                    
                    default:{
                        report_warning(context, WARNING_unknown_format_specifier, call->base.token, "Unknown format specifier %c.", conversion_specifier);
                    }break;
                }
            }
        }else{
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
                // parse field width
                //
                if(format_string.size && format_string.data[0] == '*'){
                    field_width = string_eat_front(&format_string, 1);
                    this_is_probably_supposed_to_be_a_rust_format_string |= 1;
                    
                    //
                    // @cleanup: these arguments have to be preserved
                    //
                    
                    if(!argument_node) goto too_few_args_for_print;
                    
                    if(argument_node->value->resolved_type->kind != AST_integer_type){
                        struct string type_string = push_type_string(context->arena, &context->scratch, argument_node->value->resolved_type);
                        report_error(context,  argument_node->value->token, "Field width specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                        return operand;
                    }
                    
                    struct ast_list_node *next = argument_node->next;
                    sll_push_back(pretty_print_argument_list, argument_node);
                    pretty_print_argument_list.count++;
                    argument_node = next;
                }else{
                    field_width = string_eat_characters_front(&format_string, "0123456789");
                }
                
                //
                // parse precision
                //
                if(format_string.size && format_string.data[0] == '.'){
                    string_eat_front(&format_string, 1); // eat the '.'
                    
                    if(format_string.size && format_string.data[0] == '*'){
                        this_is_probably_supposed_to_be_a_rust_format_string |= 1;
                        precision = string_eat_front(&format_string, 1);
                        
                        //
                        // @cleanup: these arguments have to be preserved
                        //
                        
                        if(!argument_node) goto too_few_args_for_print;
                        
                        if(argument_node->value->resolved_type->kind != AST_integer_type){
                            struct string type_string = push_type_string(context->arena, &context->scratch, argument_node->value->resolved_type);
                            report_error(context,  argument_node->value->token, "Precision specifier '*' requires an integer argument. Given argument is of type '%.*s'.", type_string.size, type_string.data);
                            return operand;
                        }
                        
                        struct ast_list_node *next = argument_node->next;
                        sll_push_back(pretty_print_argument_list, argument_node);
                        pretty_print_argument_list.count++;
                        argument_node = next;
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
                    
                    assert(format_string_literal);
                    report_error(context, format_string_literal->base.token, "Incorrectly formatted format string '%.*s%s' for __declspec(printlike) procedure.", format_specifier.size, format_specifier.data, dotdotdot);
                    return operand;
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
            
            string_eat_front(&format_string, 1); // eat the '}'.
            
            //
            // We got a Rust style format specifier!
            // Copy every thing that happened before the argument to the _pretty print format string_.
            //
            
            string_list_postfix(&pretty_print_list, &context->scratch, start);
            
            struct string format_specifier = {
                .data = start.data + start.size,
                .size = (format_string.data + format_string.size) - start.data + start.size,
            };
            
            struct ast *argument = null;
            if(identifier.size){
                //
                // We found the identifier.
                //
                struct ast_declaration *declaration = lookup_declaration(context, context->current_compilation_unit, atom_for_string(identifier));
                
                if(!declaration && (string_match(identifier, string("s")) || string_match(identifier, string("x")))){
                    // Allow '{s}', '{x}'.
                    type_specifiers = identifier;
                }else{
                    if(!declaration){
                        report_error(context, format_string_literal->base.token, "Identifier in format specifier '%.*s', is undeclared.", format_specifier.size, format_specifier.data);
                        return operand;
                    }
                    
                    assert(format_string_literal);
                    struct ast_identifier *ident = parser_ast_push(context, format_string_literal->base.token, identifier);
                    ident->decl = declaration;
                    
                    if(have_equals){
                        struct string identifier_equals = push_format_string(&context->scratch, "%.*s = ", identifier.size, identifier.data);
                        string_list_postfix(&pretty_print_list, &context->scratch, identifier_equals);
                    }
                    
                    set_resolved_type(&ident->base, declaration->type, null);
                    
                    argument = &ident->base;
                }
            }
            
            if(!argument){
                if(!argument_node){
                    too_few_args_for_print:
                    report_error(context, call->base.token, "Less arguments for __declspec(printlike)-function, than indicated by the format string.");
                    return operand;
                }
                
                argument = argument_node->value;
                argument_node = argument_node->next;
            }
            
            //
            // @cleanup: validate 'type_specifiers' there should be at most one for floats, one for integers and one 's'.
            //
            
            printlike__infer_format_string_and_arguments_for_argument(context, argument, &pretty_print_list, &pretty_print_argument_list, flags, field_width, precision, type_specifiers);
        }
    }
    
    //
    // Error if we have more arguments than the format string specified.
    //
    if(argument_node){
        report_error(context, argument_node->value->token, "__declspec(printlike) function specifies more arguments than expected based on the format string.");
        return operand;
    }
    
    //
    // Build the new call argument list, it should be 
    //    1) Fixed arguments
    //    2) Format string literal
    //    3) Pretty print argument list
    //
    struct ast_list new_call_arguments = fixed_arguments;
    
    struct string new_format_string = string_list_flatten(pretty_print_list, context->arena);
    if(format_string_literal){
        format_string_node->value = maybe_load_address_for_array_or_function(context, &format_string_literal->base);
        sll_push_back(new_call_arguments, format_string_node);
        new_call_arguments.count++;
    }else{
        format_string_literal = parser_ast_push(context, call->base.token, string_literal);
        
        struct ast_array_type *type = parser_type_push(context, call->base.token, array_type);
        type->amount_of_elements = (new_format_string.size + 1); // plus one for the zero_terminator
        type->element_type = &globals.typedef_s8;
        type->base.size = type->amount_of_elements * sizeof(s8);
        type->base.alignment = 1;
        
        set_resolved_type(&format_string_literal->base, &type->base, null);
        
        struct ast *implicit_address_conversion = maybe_load_address_for_array_or_function(context, &format_string_literal->base);
        ast_list_append(&new_call_arguments, context->arena, implicit_address_conversion);
    }
    
    // 
    // @cleanup: How should this work with other kind of string literals.
    // 
    format_string_literal->value = new_format_string;
    format_string_literal->string_kind = STRING_KIND_utf8;
    
    //
    // Promote the 'pretty_print_argument_list' as you would _normal_ varargs.
    //
    for(struct ast_list_node *new_argument = pretty_print_argument_list.first; new_argument; new_argument = new_argument->next){
        new_argument->value = maybe_insert_implicit_nodes_for_varargs_argument(context, new_argument->value);
    }
    
    sll_push_back_list(new_call_arguments, pretty_print_argument_list);
    new_call_arguments.count += pretty_print_argument_list.count;
    
#if 0
    smm actual_argument_count = 0;
    for(struct ast_list_node *node = new_call_arguments.first; node; node = node->next){
        actual_argument_count++;
        
        struct ast *expr = node->value;
        struct ast_type *type = expr->resolved_type;
        
        struct string type_string = push_type_string(&context->scratch, &context->scratch, type);
        print("    [%d] %.*s\n", actual_argument_count, type_string.size, type_string.data);
    }
    assert(actual_argument_count == new_call_arguments.count);
#endif
    
    call->call_arguments = new_call_arguments;
    return null;
}

func struct ast *check_intrinsic_function_call(struct context *context, struct ast_function_call *call, struct ast *operand){
    struct intrinsic_info *intrinsic_info = lookup_intrinsic(operand->token->atom);
    switch(intrinsic_info->kind){
        case INTRINSIC_KIND_va_start:{
            if(call->call_arguments.count != 2){
                report_error(context, call->base.token, "Call to intrinsic function '__va_start' must have exactly two arguments.");
                return operand;
            }
            
            if(!context->current_function || !(context->current_function->type->flags & FUNCTION_TYPE_FLAGS_is_varargs)){
                report_error(context, call->base.token, "Intrinsic function '__va_start' can only used in a varargs function.");
                return operand;
            }
            
            int should_error = 0;
            
            struct ast *format = call->call_arguments.last->value;
            
            // @cleanup: ignore casts... these might have been inserted by the argument promotions.
            while(format->kind == AST_cast){
                format = ((struct ast_unary_op *)format)->operand;
            }
            
            if(format->kind != AST_identifier){
                should_error = 1;
            }else{
                struct ast_identifier *ident = (struct ast_identifier *)format;
                
                struct ast_declaration *last_argument = (struct ast_declaration *)context->current_function->type->argument_list.last->value;
                
                if(ident->decl != last_argument){
                    should_error = 1;
                }
            }
            
            if(should_error){
                report_error(context, call->base.token, "Second argument to intrinsic function '__va_start' must be the last named argument of the varargs function.");
                return operand;
            }
            
            call->call_arguments.last->value = format;
        }break;
        default:{
            // do nothing, if an intrinsic does not have a necessity for special call handling.
        }break;
    }
    return null;
}

// :compound_assignments
// This routine transforms 'a op= b' into '((unnamed) = &a,  *(unnamed) = *(unnamed) op b)'
func struct ast_binary_op *punt_compound_assignment_with_unnamed_declaration(struct context *context, struct ast_binary_op *assignment, enum ast_kind AST_binary_op){
    // @cleanup: we could reuse 'assignment'
    
    struct token *site = assignment->base.token;
    
    struct ast *lhs = assignment->lhs;
    struct ast *rhs = assignment->rhs;
    
    struct ast *address = ast_push_unary_expression(context, AST_unary_address, site, lhs);
    struct ast_type *pointer_type = parser_push_pointer_type(context, lhs->resolved_type, lhs->defined_type, site);
    set_resolved_type(address, pointer_type, null);
    
    struct ast_identifier *unnamed = push_unnamed_declaration(context, pointer_type, null, site);
    struct ast *address_assignment = ast_push_binary_expression(context, AST_assignment, site, &unnamed->base, address);
    set_resolved_type(address_assignment, pointer_type, null);
    
    struct ast *deref = ast_push_unary_expression(context, AST_unary_deref, site, &unnamed->base);
    set_resolved_type(deref, lhs->resolved_type, lhs->defined_type);
    
    struct ast_binary_op *op = (struct ast_binary_op *)ast_push_binary_expression(context, AST_binary_op, site, deref, rhs);
    maybe_insert_arithmetic_conversion_casts(context, op);
    assert(op->lhs->resolved_type == op->rhs->resolved_type);
    set_resolved_type(&op->base, op->lhs->resolved_type, op->lhs->defined_type);
    
    rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, lhs->resolved_type, lhs->defined_type, &op->base, site);
    
    // @WARNING: we reuse 'deref' here, I don't think this should be a problem, but if there is weird behaviour
    //           this might be the culprit
    struct ast *value_assignment = ast_push_binary_expression(context, AST_assignment, site, deref, rhs);
    set_resolved_type(value_assignment, lhs->resolved_type, lhs->defined_type);
    
    struct ast *comma = ast_push_binary_expression(context, AST_comma_expression, site, address_assignment, value_assignment);
    set_resolved_type(comma, lhs->resolved_type, lhs->defined_type);
    
    return (struct ast_binary_op *)comma;
}

// @cleanup: Eventually, we should implement this ourselves.
__declspec(dllimport) double strtod(const char *str, char **endptr);

func struct ast *parse_expression(struct context *context, b32 should_skip_comma_expression){
    
    if(maybe_report_error_for_stack_exhaustion(context, get_current_token_for_error_report(context), "Expression nests to deep.")){
        return invalid_ast(context);
    }
    
    // we push an invalid ast onto the stack, to 'shield' the ast stack, when calling 'parse_expression' recursively. We assert that this works in two ways here.
    
    smm ast_stack_before = context->ast_stack_at;
    b32 did_push = false;
    if(ast_stack_before){ // we only have to push if we are not the first thing
        did_push = ast_stack_push(context, &globals.guard_ast);
    }
    
    restart:;
    
    if(context->should_exit_statement) return invalid_ast(context);
    
    
    struct ast *operand = null;
    
    switch(get_current_token(context)->type){
        
        // 
        // :prefix_expression parse_prefix_expression
        // 
        
        // @note: Integer promotion get's handled in the second part.
        
        case TOKEN_increment:   ast_stack_push(context, ast_push_unary_expression(context, AST_unary_preinc,      next_token(context), null)); goto restart;
        case TOKEN_decrement:   ast_stack_push(context, ast_push_unary_expression(context, AST_unary_predec,      next_token(context), null)); goto restart;
        case TOKEN_logical_not: ast_stack_push(context, ast_push_unary_expression(context, AST_unary_logical_not, next_token(context), null)); goto restart;
        case TOKEN_bitwise_not: ast_stack_push(context, ast_push_unary_expression(context, AST_unary_bitwise_not, next_token(context), null)); goto restart;
        case TOKEN_times:       ast_stack_push(context, ast_push_unary_expression(context, AST_unary_deref,       next_token(context), null)); goto restart;
        case TOKEN_minus:       ast_stack_push(context, ast_push_unary_expression(context, AST_unary_minus,       next_token(context), null)); goto restart;
        case TOKEN_plus:        ast_stack_push(context, ast_push_unary_expression(context, AST_unary_plus,        next_token(context), null)); goto restart;
        case TOKEN_and:         ast_stack_push(context, ast_push_unary_expression(context, AST_unary_address,     next_token(context), null)); goto restart;
        
        // :sizeof/alignof threading bug
        case TOKEN_sizeof:  ast_stack_push(context, ast_push_unary_expression(context, AST_sizeof,  next_token(context), null)); goto restart;
        case TOKEN_alignof: ast_stack_push(context, ast_push_unary_expression(context, AST_alignof, next_token(context), null)); goto restart;
        
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
                
                if(ast_stack_current(context)->kind == AST_alignof || ast_stack_current(context)->kind == AST_sizeof){
                    struct ast *size_or_alignof = ast_stack_pop(context);
                    b32 is_alignof = (size_or_alignof->kind == AST_alignof);
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
                    
                    
                    // "The [..] type (an unsigned integer type) is 'size_t', defined in <stddef.h>."
                    operand = ast_push_u64_literal(context, size_or_alignof->token, is_alignof ? type->alignment : type->size);
                    if(is_alignof){
                        expect_token(context, TOKEN_closed_paren, "Expected ')' after 'alignof(type'.");
                    }else{
                        expect_token(context, TOKEN_closed_paren, "Expected ')' after 'sizeof(type'.");
                    }
                    context->in_lhs_expression = false;
                    
                    goto skip_primary_expression_because_we_got_a_sizeof_or_align_of_with_a_type;
                }
                
                expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of a cast.");
                if(peek_token(context, TOKEN_open_curly)){
                    // 
                    // :struct_literal :parse_struct_literal :parse_struct_or_array_literal :compound_literal :parse_compund_literal
                    // 
                    
                    if(type_to_cast_to.type->kind == AST_function_type){
                        report_error(context, open_paren, "Cast to function is illegal."); // @cleanup: Should we allow this as weird version of lambas?
                        return invalid_ast(context);
                    }
                    
                    struct ast_identifier *ident = push_unnamed_declaration(context, type_to_cast_to.type, type_to_cast_to.defined_type, get_current_token(context));
                    
                    struct ast_compound_literal *compound_literal = parser_ast_push(context, get_current_token(context), compound_literal);
                    set_resolved_type(&compound_literal->base, type_to_cast_to.type, type_to_cast_to.defined_type);
                    compound_literal->decl = ident->decl;
                    compound_literal->decl->assign_expr = &compound_literal->base;
                    
                    compound_literal->assignment_list = parse_initializer_list(context, &ident->base, &compound_literal->trailing_array_size);
                    if(context->should_exit_statement) return &ident->base;
                    
                    if(type_is_array_of_unknown_size(type_to_cast_to.type)){
                        struct ast_array_type *array_of_unknown_size = (struct ast_array_type *)type_to_cast_to.type;
                        smm array_length = 0;
                        if(compound_literal->trailing_array_size){
                            array_length = compound_literal->trailing_array_size/array_of_unknown_size->element_type->size;
                        }
                        
                        // 
                        // Create a new array type, with the newly discovered bounds.
                        struct ast_array_type *array_type = parser_type_push(context, compound_literal->base.token, array_type); // @cleanup: Better token.
                        array_type->element_type              = array_of_unknown_size->element_type;
                        array_type->element_type_defined_type = array_of_unknown_size->element_type_defined_type;
                        
                        patch_array_size(context, array_type, array_length, compound_literal->base.token);
                        
                        // Set the new type.
                        ident->decl->type = &array_type->base;
                        
                        // Reset the trailing array size, to not overallocate later on.
                        compound_literal->trailing_array_size = 0;
                    }
                    
                    if(type_to_cast_to.type->flags & TYPE_FLAG_ends_in_array_of_unknown_size){
                        // :compounds_with_trailing_array
                        parser_emit_memory_location(context, ident->decl);
                    }
                    
                    operand = &compound_literal->base;
                    
                    // struct literals should be assignable? msvc does that.
                    context->in_lhs_expression = true;
                    goto skip_primary_expression_because_we_got_a_struct_literal;
                }else{
                    if(type_to_cast_to.type->kind == AST_union){
                        report_error(context, open_paren, "Cast to union is illegal.");
                        return invalid_ast(context);
                    }
                    if(type_to_cast_to.type->kind == AST_struct){
                        report_error(context, open_paren, "Cast to struct is illegal.");
                        return invalid_ast(context);
                    }
                    if(type_to_cast_to.type->kind == AST_array_type){
                        report_error(context, open_paren, "Cast to array is illegal.");
                        return invalid_ast(context);
                    }
                    if(type_to_cast_to.type->kind == AST_function_type){
                        report_error(context, open_paren, "Cast to function is illegal.");
                        return invalid_ast(context);
                    }
                }
                
                ast_stack_push(context, push_cast(context, type_to_cast_to.type, type_to_cast_to.defined_type, null));
                goto restart;
            }else{
                
                // 
                // Parenthesized expression.
                // 
                
                struct ast *expr = parse_expression(context, false);
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
            struct ast_integer_literal *lit = parser_ast_push(context, lit_token, integer_literal);
            
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
            struct ast *defined_type = null;
            
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
                        defined_type = (struct ast *)&globals.typedef_s8;
                        value = *(s8 *)escaped.string.data;
                    }else if(escaped.string.size == 2){
                        type = &globals.typedef_s32;
                        defined_type = (struct ast *)&globals.typedef_s16;
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
                    
                    // @note: Not supprisingly, this one is unsigned.
                    type = &globals.typedef_u32;
                    value = *(u32 *)escaped.string.data;
                }break;
                invalid_default_case();
            }
            
            lit->_u64 = value;
            
            set_resolved_type(&lit->base, type, defined_type);
            operand = &lit->base;
            context->in_lhs_expression = false;
        }break;
        
#define explicit_number_kind_case(type)\
case NUMBER_KIND_##type:{ \
    if(type##_min > (smm)val || type##_max < (smm)val){ \
        report_warning(context, WARNING_compile_time_truncation, lit_token, "Literal is prefixed '%s', but does not fit.", #type); \
    } \
    lit->_##type = (type)val; \
    set_resolved_type(&lit->base, &globals.typedef_##type, null);\
}break
        
        
        case TOKEN_base10_literal:{
            struct token *lit_token = next_token(context);
            
            if(lit_token->size == 1 && lit_token->data[0] == '0'){
                // @note: Fast path for 0.
                operand = ast_push_s32_literal(context, lit_token, 0);
                context->in_lhs_expression = false;
                break;
            }
            
            struct ast_integer_literal *lit = parser_ast_push(context, lit_token, integer_literal);
            struct parsed_integer parsed_integer = parse_base10_literal(context, lit_token);
            u64 val = parsed_integer.value;
            
            // "The type of an integer constant is the first of the corresponding list
            //  in which its value can be represented."
            
            switch(parsed_integer.number_kind){
                case NUMBER_KIND_invalid:{
                    assert(context->should_exit_statement);
                    set_resolved_type(&lit->base, &globals.typedef_s32, null);
                    return &lit->base;
                }break;
                
                explicit_number_kind_case(s8);
                explicit_number_kind_case(s16);
                explicit_number_kind_case(s32);
                explicit_number_kind_case(s64);
                explicit_number_kind_case(u8);
                explicit_number_kind_case(u16);
                explicit_number_kind_case(u32);
                explicit_number_kind_case(u64);
                
                case NUMBER_KIND_long: // @cleanup: if we do long vs int think about this
                case NUMBER_KIND_int:{
                    if(val <= max_s32){
                        lit->_s32 = (s32)val;
                        set_resolved_type(&lit->base, &globals.typedef_s32, null);
                        break;
                    }
                } // fallthrough
                case NUMBER_KIND_long_long:{
                    if(val >= max_s64){
                        //18446744073709551615;
                        // @cleanup: warning here
                    }
                    lit->_s64 = (s64)val;
                    set_resolved_type(&lit->base, &globals.typedef_s64, null);
                }break;
                case NUMBER_KIND_unsigned_long: // @cleanup: long vs int
                case NUMBER_KIND_unsigned:{
                    if(val <= max_u32){
                        lit->_u32 = (u32)val;
                        set_resolved_type(&lit->base, &globals.typedef_u32, null);
                    }else{
                        lit->_u64 = (u64)val;
                        set_resolved_type(&lit->base, &globals.typedef_u64, null);
                    }
                } break;
                case NUMBER_KIND_unsigned_long_long:{
                    lit->_u64 = val;
                    set_resolved_type(&lit->base, &globals.typedef_u64, null);
                }break;
                invalid_default_case();
            }
            
            operand = &lit->base;
            context->in_lhs_expression = false;
        }break;
        case TOKEN_binary_literal:
        case TOKEN_hex_literal:{
            struct token *lit_token = next_token(context);
            struct ast_integer_literal *lit = parser_ast_push(context, lit_token, integer_literal);
            
            struct parsed_integer parsed_integer;
            if(lit_token->type == TOKEN_hex_literal){
                parsed_integer = parse_hex_literal(context, lit_token);
            }else{
                parsed_integer = parse_binary_literal(context, lit_token);
            }
            u64 val = parsed_integer.value;
            
            switch(parsed_integer.number_kind){
                case NUMBER_KIND_invalid:{
                    assert(context->should_exit_statement);
                    set_resolved_type(&lit->base, &globals.typedef_s32, null);
                    return &lit->base;
                }break;
                
                explicit_number_kind_case(s8);
                explicit_number_kind_case(s16);
                explicit_number_kind_case(s32);
                explicit_number_kind_case(s64);
                explicit_number_kind_case(u8);
                explicit_number_kind_case(u16);
                explicit_number_kind_case(u32);
                explicit_number_kind_case(u64);
                
                case NUMBER_KIND_s128: case NUMBER_KIND_u128:{
                    report_error(context, lit_token, "128-bit literals are not supported.");
                    return &lit->base;
                }
                
#undef explicit_number_kind_case
                
                case NUMBER_KIND_long:
                case NUMBER_KIND_int:{
                    if(val <= max_s32){
                        lit->_s32 = (s32)val;
                        set_resolved_type(&lit->base, &globals.typedef_s32, null);
                    }else if(val <= max_u32){
                        lit->_u32 = (u32)val;
                        set_resolved_type(&lit->base, &globals.typedef_u32, null);
                    }else if(val <= max_s64){
                        lit->_s64 = (s64)val;
                        set_resolved_type(&lit->base, &globals.typedef_s64, null);
                    }else{
                        lit->_u64 = (u64)val;
                        set_resolved_type(&lit->base, &globals.typedef_u64, null);
                    }
                }break;
                case NUMBER_KIND_long_long:{
                    if(val <= max_s64){
                        lit->_s64 = (s64)val;
                        set_resolved_type(&lit->base, &globals.typedef_s64, null);
                    }else{
                        lit->_u64 = (u64)val;
                        set_resolved_type(&lit->base, &globals.typedef_u64, null);
                    }
                }break;
                case NUMBER_KIND_unsigned_long:
                case NUMBER_KIND_unsigned:{
                    if(val <= max_u32){
                        lit->_u32 = (u32)val;
                        set_resolved_type(&lit->base, &globals.typedef_u32, null);
                    }else{
                        lit->_u64 = val;
                        set_resolved_type(&lit->base, &globals.typedef_u64, null);
                    }
                }break;
                case NUMBER_KIND_unsigned_long_long:{
                    lit->_u64 = val;
                    set_resolved_type(&lit->base, &globals.typedef_u64, null);
                }break;
                invalid_default_case();
            }
            operand = &lit->base;
            context->in_lhs_expression = false;
        }break;
        case TOKEN_string_literal:{
            struct ast_string_literal *lit = parser_ast_push(context, get_current_token(context), string_literal);
            
            enum   string_kind composed_string_kind    = STRING_KIND_invalid;
            struct string      composed_string_literal = zero_struct;
            
            {
                // 
                // We have to concatinate adjacent string literals.
                // First we figure out the 'string_kind' and all the strings.
                // 
                
                struct string *string_base = push_uninitialized_data(&context->scratch, struct string, 0);
                smm total_string_size = 0;
                
                for(struct token *string_literal = next_token(context); string_literal; string_literal = peek_token_eat(context, TOKEN_string_literal)){
                    
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
                    
                    *push_uninitialized_struct(&context->scratch, struct string) = string;
                    total_string_size += string.size;
                }
                
                smm amount_of_strings = push_uninitialized_data(&context->scratch, struct string, 0) - string_base;
                
                if(amount_of_strings == 1){
                    composed_string_literal = string_base[0];
                }else{
                    composed_string_literal.size = total_string_size;
                    composed_string_literal.data = push_uninitialized_data(context->arena, u8, total_string_size);
                    
                    for(smm string_index = 0, string_at = 0; string_index < amount_of_strings; string_index++){
                        struct string string = string_base[string_index];
                        
                        memcpy(composed_string_literal.data + string_at, string.data, string.size);
                        string_at += string.size;
                    }
                }
            }
            
            // @cleanup: How do we handle tokens here. It feels like the escaping code should 
            //           build a very specific "token" for the escape, instead of us specifying it here.
            //           Hence, for now I am going to ignore the issue and report the error at the first string literal.
            struct string string = escape_and_convert_string(context, lit->base.token, composed_string_literal, composed_string_kind);
            
            struct ast_type *element_type = null;
            switch(composed_string_kind){
                case STRING_KIND_utf8:  element_type = &globals.typedef_s8;  break;
                case STRING_KIND_utf16: element_type = &globals.typedef_u16; break;
                case STRING_KIND_utf32: element_type = &globals.typedef_u32; break;
                invalid_default_case();
            }
            
            lit->value = string;
            lit->string_kind = composed_string_kind;
            
            struct ast_array_type *type = parser_type_push(context, lit->base.token, array_type);
            type->amount_of_elements = (string.size/element_type->size + 1); // plus one for the zero_terminator
            type->element_type = element_type;
            type->base.size = type->amount_of_elements * element_type->size;
            type->base.alignment = element_type->alignment;
            
            set_resolved_type(&lit->base, &type->base, null);
            
            operand = &lit->base;
            if(context->should_exit_statement) return operand;
            
            // :strings_are_lhs_expressions
            // string literals are actually lhs expression (you can take the address of them),
            // but you cannot assign to them, as they are arrays.
            context->in_lhs_expression = true;
        }break;
        
        case TOKEN_float_hex_literal:
        case TOKEN_float_literal:{
            struct token *float_token = next_token(context);
            struct ast_float_literal *f = parser_ast_push(context, float_token, float_literal);
            
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
                set_resolved_type(&f->base, &globals.typedef_f32, null);
                return &f->base;
            }
            
            struct string value = create_string(string.data, string.size - suffix.size);
            char *cvalue = push_cstring_from_string(&context->scratch, value);
            char *endptr;
            double val = strtod(cvalue, &endptr);
            
            // @cleanup: once we parse these ourselves we can make sure of this, but for now, we might allow invalid
            //           suffixes and thats fine.
            // @cleanup: handle strtod errors
            // assert(endptr == cvalue + value.size);
            
            set_resolved_type(&f->base, is_32_bit ? &globals.typedef_f32 : &globals.typedef_f64, null);
            f->value = val;
            
            operand = &f->base;
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
            
            struct ast_identifier *ident = parser_ast_push(context, token, identifier);
            
            struct ast_declaration *lookup = lookup_declaration(context, context->current_compilation_unit, token->atom);
            
            set_resolved_type(&ident->base, &globals.typedef_void, null); // has to be something for error
            
            if(!lookup){
                if(globals.compile_stage < COMPILE_STAGE_parse_function){
                    parser_sleep(context, token, SLEEP_on_decl);
                }else{
                    report_error(context, token, "Undeclared identifier.");
                }
                set_resolved_type(&ident->base, &globals.typedef_poison, null);
                return &ident->base;
            }else if(lookup->base.kind != AST_declaration && lookup->base.kind != AST_function){
                assert(lookup->base.kind == AST_typedef);
                
                if(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries){
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
                return &ident->base;
            }
            
            lookup->times_referenced++; // @cleanup: this should be atomic
            
            if(context->current_scope){
                if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &lookup->type)){
                    return &ident->base;
                }
            }
            
            if(lookup->base.kind == AST_function){
                struct ast_function *function = (struct ast_function *)lookup;
                
                struct function_node *node = push_struct(context->arena, struct function_node);
                node->function = function;
                if(context->current_function){
                    sll_push_front(context->current_function->called_functions, node);
                }else{
                    assert(!context->current_scope); // we are at global scope. Queue this one to be a
                    // sort of entry point to the program
                    // @speed: this should probably not be done atomically, but rather just loaded from the
                    //         main thread when we are done with phase 2.
                    //         So we could just keep these lists local and then collect them.
                    add_potential_entry_point(node);
                }
                
                
                if(!function->token_that_referenced_this_function){
                    function->token_that_referenced_this_function = ident->base.token;
                }else{
                    // @cleanup: I feel like there should be a better solution.
                    if(!globals.file_table.data[ident->base.token->file_index]->is_system_include && globals.file_table.data[function->token_that_referenced_this_function->file_index]->is_system_include){
                        function->token_that_referenced_this_function = ident->base.token;
                    }
                }
            }
            
            if(lookup->flags & DECLARATION_FLAGS_is_enum_member){
                assert(lookup->assign_expr->kind == AST_integer_literal);
                // @sigh
                struct ast_integer_literal *source = cast(struct ast_integer_literal *)lookup->assign_expr;
                struct ast_integer_literal *lit = parser_ast_push(context, token, integer_literal);
                lit->base.token = ident->base.token;
                // @note: right now this only does enums
                lit->_s32 = source->_s32;
                
                operand = &lit->base;
                set_resolved_type(operand, source->base.resolved_type, source->base.defined_type);
                context->in_lhs_expression = false;
            }else{
                ident->decl = lookup;
                
                set_resolved_type(&ident->base, lookup->type, lookup->defined_type);
                operand = &ident->base;
                
                context->in_lhs_expression = true;
            }
        }break;
        
        default:{
            report_syntax_error(context, get_current_token_for_error_report(context), "Unexpected token in expression.");
            return &globals.empty_statement;
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
                if(operand->resolved_type == &globals.typedef_Bool){
                    // :Error 
                    // @cleanup: C allows these.
                    report_error(context, test, "Operand of type '_Bool' cannot be incremented.");
                    return operand;
                }
                
                struct ast *inc = ast_push_unary_expression(context, AST_unary_postinc, test, operand);
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer, test)) return operand;
                if(check_types_for_increment_or_decrement(context, operand, "++")) return operand;
                
                set_resolved_type(inc, operand->resolved_type, operand->defined_type);
                operand = inc;
                
                context->in_lhs_expression = false;
            }break;
            case TOKEN_decrement:{
                if(operand->resolved_type == &globals.typedef_Bool){
                    // :Error
                    // @cleanup: C allows these.
                    report_error(context, test, "Operand of type '_Bool' cannot be decremented.");
                    return operand;
                }
                
                struct ast *dec = ast_push_unary_expression(context, AST_unary_postdec, test, operand);
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer, test)) return operand;
                if(check_types_for_increment_or_decrement(context, operand, "--")) return operand;
                
                set_resolved_type(dec, operand->resolved_type, operand->defined_type);
                operand = dec;
                
                context->in_lhs_expression = false;
            }break;
            case TOKEN_open_index:{
                
                struct ast *index = parse_expression(context, false);
                expect_token(context, TOKEN_closed_index, "Expected ']' at the end of array subscript.");
                if(context->should_exit_statement) return operand;
                
                
                if(operand->resolved_type->kind == AST_integer_type){
                    //
                    // This is a @hack to work around some cursed stuff c is supporting.
                    // As 'a[b]' is technically just the same as '*(a + b)' expressions like
                    // '1[argv]' are legal and evaluate to the same as 'argv[1]'.
                    //                                                            -20.04.2023
                    // 
                    struct ast *temp = operand;
                    operand = index;
                    index = temp;
                }
                
                if(!check_unary_for_basic_types(context, index, CHECK_integer, test)) return operand;
                
                if(operand->resolved_type->kind == AST_struct){
                    
                    // :hlc_extension
                    // for structs that contain a field '.data' and a field '.size' map
                    //     struct s { char *data; u64 size } s;
                    //     s[index];
                    // to
                    //     (struct s *<s> = &s, u64 <i> = index, <i> >= <s>->size ? panic() : 0, <s>->data)[<i>]
                    
                    struct ast_compound_type *compound = (struct ast_compound_type *)operand->resolved_type; 
                    
                    // struct s *<s> = &s
                    struct ast_type *s_ident_type = parser_push_pointer_type(context, &compound->base, null, test);
                    
                    operand = ast_push_unary_expression(context, AST_unary_address, test, operand);
                    set_resolved_type(operand, s_ident_type, null);
                    
                    struct ast_identifier *s_ident = push_unnamed_declaration(context, s_ident_type, null, test);
                    
                    struct ast *s_assign = ast_push_binary_expression(context, AST_assignment, operand->token, &s_ident->base, operand);
                    set_resolved_type(s_assign, s_ident_type, null);
                    
                    // <s>->data and <s>->size
                    struct compound_member *data_member = find_member_in_compound(compound, globals.keyword_data);
                    struct compound_member *size_member = find_member_in_compound(compound, globals.keyword_size);
                    if(!data_member){
                        // :error
                        report_error(context, test, "Left hand side of [] is of struct type but does not contain a '.data' member.");
                        return operand;
                    }
                    
                    if(!size_member){
                        // :error
                        report_error(context, test, "Left hand side of [] is of struct type but does not contain a '.size' member.");
                        return operand;
                    }
                    
                    if(data_member->type->kind != AST_pointer_type){
                        report_error(context, test, "Struct subscript has a '.data' member, but it is not a pointer.");
                        return operand;
                    }
                    
                    if(size_member->type != &globals.typedef_u64 && size_member->type != &globals.typedef_s64){
                        report_error(context, test, "Struct subscript has a '.size' member, but it is not a 64-bit integer type.");
                        return operand;
                    }
                    
                    struct ast *data = push_dot_or_arrow(context, data_member, &s_ident->base, AST_member_deref, test);
                    struct ast *size = push_dot_or_arrow(context, size_member, &s_ident->base, AST_member_deref, test);
                    
                    // maybe cast the index to the index type
                    index = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, size->resolved_type, size->defined_type, index, test);
                    
                    // u64 <i> = index
                    struct ast_identifier *i_ident = push_unnamed_declaration(context, size->resolved_type, size->defined_type, test);
                    struct ast *i_assign = ast_push_binary_expression(context, AST_assignment, operand->token, &i_ident->base, index);
                    set_resolved_type(i_assign, size->resolved_type, size->defined_type);
                    
                    // <i> >= <s>->size ? panic() : 0
                    struct ast_conditional_expression *cond = parser_ast_push(context, test, conditional_expression);
                    cond->condition = ast_push_binary_expression(context, AST_binary_bigger_equals, test, &i_ident->base, size);
                    set_resolved_type(cond->condition, &globals.typedef_s32, null);
                    cond->if_true  = &(parser_ast_push(context, test, panic))->base;
                    set_resolved_type(cond->if_true, &globals.typedef_s32, null);
                    cond->if_false = ast_push_s32_literal(context, test, 0);
                    set_resolved_type(&cond->base, &globals.typedef_s32, 0);
                    
                    // glue them together using comma expressions
                    operand = ast_push_binary_expression(context, AST_comma_expression, test, s_assign, i_assign);
                    set_resolved_type(operand, data->resolved_type, data->defined_type);
                    
                    operand = ast_push_binary_expression(context, AST_comma_expression, test, operand, &cond->base);
                    set_resolved_type(operand, &globals.typedef_s32, null);
                    
                    operand = ast_push_binary_expression(context, AST_comma_expression, test, operand, data);
                    set_resolved_type(operand, data->resolved_type, data->defined_type);
                    
                    index = &i_ident->base;
                }
                
                if(operand->resolved_type->kind != AST_pointer_type && operand->resolved_type->kind != AST_array_type){
                    report_error(context, test, "Left hand side of [] needs to be of pointer or array type.");
                    return operand;
                }
                
                if(operand->resolved_type->kind == AST_pointer_type){
                    struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)operand->resolved_type;
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, test, "Cannot subscript a void-pointer");
                        return operand;
                    }
                    
                    if(pointer->pointer_to->kind == AST_function_type){
                        report_error(context, test, "Cannot subscript a function-pointer.");
                        return operand;
                    }
                    
                    // :unresolved_types
                    //
                    // If the type we are pointing to is unresolved, like
                    //     struct unresolved *pointer;
                    // We have to resolve it, or sleep on it/error on it.
                    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return operand;
                }
                
                
                //
                // @cleanup: warning or error if the lhs is an array and the index is statically out of bounds?
                //
                
                operand = push_nodes_for_subscript(context, operand, index, test);
                context->in_lhs_expression = true;
            }break;
            case TOKEN_dot:{
                if(globals.allow_dot_as_arrow && operand->resolved_type->kind == AST_pointer_type){
                    goto treat_dot_as_arrow_because_allow_dot_as_arrow_was_set_and_we_got_a_pointer_type_for_a_dot;
                }
                
                if(operand->resolved_type->kind != AST_struct && operand->resolved_type->kind != AST_union){
                    report_error(context, test, "Left of '.' needs to be of struct or union type.");
                    return operand;
                }
                
                struct ast_compound_type *compound = cast(struct ast_compound_type *)operand->resolved_type;
                struct ast *op = handle_dot_or_arrow(context, compound, operand, AST_member, test);
                operand = cast(struct ast *)op;
                
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
                
                operand = maybe_load_address_for_array_or_function(context, operand); // Apperantly, you can use '->' on arrays.
                
                struct ast_type *type = operand->resolved_type;
                
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
                struct ast *op = handle_dot_or_arrow(context, compound, operand, AST_member_deref, test);
                
                operand = cast(struct ast *)op;
                
                context->in_lhs_expression = true;
            }break;
            case TOKEN_open_paren:{
                struct ast_function_call *call = parser_ast_push(context, test, function_call);
                
                if(operand->kind == AST_unary_deref && operand->resolved_type->kind == AST_function_type){
                    // 
                    // Convert '(*function_pointer)()' to 'function_pointer()'.
                    // 
                    operand = ((struct ast_unary_op *)operand)->operand;
                }
                
                if(operand->kind == AST_identifier) call->base.token = operand->token;
                
                call->identifier_expression = operand;
                
                struct ast_type *type = operand->resolved_type;
                if(type->kind == AST_pointer_type){
                    type = ((struct ast_pointer_type *)type)->pointer_to;
                }
                
                if(type->kind != AST_function_type){
                    report_error(context, operand->token, "Calling a non-procedure.");
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
                
                set_resolved_type(&call->base, function_type->return_type, function_type->return_type_defined_type);
                operand = &call->base;
                context->in_lhs_expression = false;
                
                if(function_type->flags & FUNCTION_TYPE_FLAGS_is_printlike){
                    //
                    // For a printlike function we do the parameter parsing ourselves.
                    // This is because 
                    //      1) we want to be able to see through promotions and stuff.
                    //         This could also be accomplished by searching the syntax tree, 
                    //         but that is sometimes annoying.
                    //      2) we want to be able to not have a 'format' argument.
                    //         In this case we will _infer_ a 'format' argument.
                    //
                    
                    struct ast *error = check_call_to_printlike_function(context, call, operand);
                    if(error) return error;
                    
                    break;
                }
                
                struct ast_list_node* function_argument_iterator = function_type->argument_list.first;
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    do{
                        struct ast *expr = parse_expression(context, true);
                        if(context->should_exit_statement) return expr;
                        
                        if(function_argument_iterator){
                            
                            
                            assert(function_argument_iterator->value->kind == AST_declaration);
                            struct ast_declaration *decl = cast(struct ast_declaration *)function_argument_iterator->value;
                            
                            if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) return expr;
                            
                            // "the arguments are implicitly converted, as if by assignment"
                            expr = maybe_load_address_for_array_or_function(context, expr);
                            expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, decl->type, decl->defined_type, expr, expr->token);
                            
                            function_argument_iterator = function_argument_iterator->next;
                        }else{
                            if(!(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs)){
                                // :Error
                                report_error(context, call->base.token, "Too many arguments to function.");
                                return operand;
                            }else{
                                
                                if(expr->resolved_type == &globals.typedef_void){
                                    report_error(context, expr->token, "Expression of type void cannot be used as function argument.");
                                }
                                
                                expr = maybe_insert_implicit_nodes_for_varargs_argument(context, expr);
                            }
                        }
                        
                        ast_list_append(&call->call_arguments, context->arena, expr);
                        
                    }while(peek_token_eat(context, TOKEN_comma));
                    expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of parameter list.");
                }
                
                if(function_type->argument_list.count > call->call_arguments.count){
                    // :Error
                    report_error(context, call->base.token, "Too few arguments to function.");
                    return operand;
                }
                
                if(function_type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic){
                    struct ast *identifier = call->identifier_expression;
                    assert(identifier->kind == AST_identifier); // not sure @cleanup
                    struct ast *error = check_intrinsic_function_call(context, call, identifier);
                    if(error) return error; // not sure
                }
                
                context->in_lhs_expression = false;
            }break;
            default:{
                prev_token(context);
                do_continue = false;
            }break;
        }
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
        
        struct ast *ast = ast_stack_current(context);
        if(ast->kind == AST_invalid) break; // We are at the guard ast.
        
        enum precedence ast_precedence   = ast_to_precedence[ast->kind];
        enum precedence token_precedence = token_to_precedence[binary_expression->type];
        
        if(ast_precedence == token_precedence && (ast_precedence == PRECEDENCE_assignment || ast_precedence == PRECEDENCE_ternary)){
            // right-associative, on equality break.
            break;   
        }
        
        // If the next token has lower precedence, wait for that one to finish, before finishing this one.
        if(token_precedence && ast_precedence > token_precedence) break;
        
        switch(ast->kind){
            
            // :prefix_expression parse_prefix_expression
            // 
            // Precedence 2: prefix expressions, Right-to-Left Associative
            // 
            //    ++operand, --operand, +operand, -operand
            //    !operand, ~operand, (type)operand, *operand
            //    &operand, sizeof operand, _Alignof operand
            //    
            case AST_unary_predec:
            case AST_unary_preinc:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                // @note: no need to integer promote
                if(operand->resolved_type == &globals.typedef_Bool){
                    // :Error
                    char *operation_string = (prefix->kind == AST_unary_preinc) ? "incremented" : "decremented";
                    report_error(context, prefix->token, "Operand of type _Bool cannot be %s.", operation_string);
                    return operand;
                }
                
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer, op->base.token)) return operand;
                char *token_string = (prefix->kind == AST_unary_preinc) ? "++" : "--";
                if(check_types_for_increment_or_decrement(context, operand, token_string)) return operand;
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            case AST_sizeof:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                if(operand->resolved_type->kind == AST_function_type){
                    report_error(context, op->base.token, "Expression of function type has undefined size.");
                    return operand;
                }
                
                if(operand->resolved_type == &globals.typedef_void){
                    report_error(context, op->base.token, "Expression of type void has undefined size.");
                    return operand;
                }
                
                if(type_is_array_of_unknown_size(operand->resolved_type)){
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
                    report_error(context, op->base.token, "'sizeof' cannot be applied to array of unknown size.");
                    return operand;
                }
                
                prefix = ast_push_u64_literal(context, prefix->token, operand->resolved_type->size);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            case AST_alignof:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                if(operand->resolved_type->kind == AST_function_type){
                    report_error(context, op->base.token, "Expression of function type has undefined alignment.");
                    return operand;
                }
                if(operand->resolved_type == &globals.typedef_void){
                    report_error(context, op->base.token, "Expression of type void has undefined alignment.");
                    return operand;
                }
                
                prefix = ast_push_u64_literal(context, prefix->token, operand->resolved_type->alignment);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            case AST_unary_bitwise_not:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = (struct ast_integer_literal *)operand;
                    
                    // "The result of the ~ is the bitwise complement of its (promoted) operand."
                    perform_integer_promotion_on_literal(lit);
                    
                    lit->_u64 = ~lit->_u64; // singed does not matter and should stay the same
                    prefix = &lit->base;
                    break;
                }
                
                if(!check_unary_for_basic_types(context, operand, CHECK_integer, op->base.token)) return operand;
                operand = maybe_insert_integer_promotion_cast(context, operand);
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            case AST_unary_logical_not:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                operand = maybe_load_address_for_array_or_function(context, operand);
                
                if(!check_unary_for_basic_types(context, operand, CHECK_basic, op->base.token)) return operand;
                
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)operand;
                    lit->_u64 = !integer_literal_as_u64(&lit->base);
                    
                    // "The result has type int."
                    set_resolved_type(&lit->base, &globals.typedef_s32, null);
                    
                    prefix = &lit->base;
                    break;
                }
                
                if(operand->resolved_type->kind == AST_integer_type || operand->resolved_type->kind == AST_bitfield_type){
                    operand = maybe_insert_integer_promotion_cast(context, operand);
                }
                
                set_resolved_type(&op->base, &globals.typedef_s32, null);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            case AST_unary_plus:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = (struct ast_integer_literal *)operand;
                    
                    // "The result of the unary operator + is the value of its (promoted) operand."
                    perform_integer_promotion_on_literal(lit);
                    
                    prefix = operand; // nothing to do here.
                    break;
                }
                
                // @cleanup: CHECK_basic?
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_float, op->base.token)) return operand;
                
                if(operand->resolved_type->kind == AST_integer_type || operand->resolved_type->kind == AST_bitfield_type){
                    operand = maybe_insert_integer_promotion_cast(context, operand);
                }
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            case AST_unary_minus:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)operand;
                    
                    // "The result of the unary operator - is the negative of its (promoted) operand."
                    perform_integer_promotion_on_literal(lit);
                    
                    // @cleanup: warn here unsigned stays unsigned
                    if(!type_is_signed(operand->resolved_type)){
                        report_warning(context, WARNING_unsigned_negation, operand->token, "Negation of an unsigned number is still unsigned.");
                    }
                    
                    // @note: Negation is the same of signed and unsigned values.
                    switch(lit->base.resolved_type->size){
                        case 1: lit->_s8  = -lit->_s8;  break;
                        case 2: lit->_s16 = -lit->_s16; break;
                        case 4: lit->_s32 = -lit->_s32; break;
                        case 8: lit->_s64 = -lit->_s64; break;
                        invalid_default_case();
                    }
                    
                    prefix = &lit->base;
                    break;
                }else if(operand->kind == AST_float_literal){
                    struct ast_float_literal *lit = cast(struct ast_float_literal *)operand;
                    lit->value = -lit->value;
                    prefix = &lit->base;
                    break;
                }
                
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_float, op->base.token)) return operand;
                
                if(operand->resolved_type->kind == AST_integer_type || operand->resolved_type->kind == AST_bitfield_type){
                    operand = maybe_insert_integer_promotion_cast(context, operand);
                    
                    if(!type_is_signed(operand->resolved_type)){
                        report_warning(context, WARNING_unsigned_negation, operand->token, "Negation of an unsigned number is still unsigned.");
                    }
                }
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            case AST_unary_deref:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                operand = maybe_load_address_for_array_or_function(context, operand);
                
                if(operand->resolved_type->kind != AST_pointer_type){
                    report_error(context, prefix->token, "Invalid dereference.");
                    return operand;
                }
                
                struct ast *handled = null;
                if(operand->kind == AST_unary_address){
                    // const_prop '*&a' to 'a'
                    struct ast_unary_op *address = cast(struct ast_unary_op *)operand;
                    assert(address->base.resolved_type->kind == AST_pointer_type);
                    handled = address->operand;
                    
                }
                
                if(handled){
                    prefix = handled;
                }else{
                    struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)operand->resolved_type;
                    
                    if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &pointer->pointer_to)) return operand;
                    
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, op->base.token, "Dereferencing a void-pointer.");
                        return operand;
                    }
                    
                    set_resolved_type(&op->base, pointer->pointer_to, pointer->pointer_to_defined_type);
                }
                context->in_lhs_expression = true;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            case AST_unary_address:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                if(!context->in_lhs_expression){
                    report_error(context, op->base.token, "Operand of unary '&' must be a lhs expression.");
                    return operand;
                }
                
                if(operand->kind == AST_unary_deref){
                    // @note: const prop '&*a' to 'a'
                    struct ast_unary_op *deref = cast(struct ast_unary_op *)operand;
                    prefix = deref->operand;
                    assert(deref->operand->resolved_type->kind == AST_pointer_type);
                }else{
                    struct ast_type *ptr = parser_push_pointer_type(context, operand->resolved_type, operand->defined_type, operand->token);
                    set_resolved_type(&op->base, ptr, null);
                }
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            case AST_cast:{
                struct ast *prefix = ast_stack_pop(context);
                struct ast_unary_op *op = (struct ast_unary_op *)prefix;
                
                // :cast_expression
                assert(op->base.resolved_type); // should have been set by the parsing
                
                // @cleanup: is this correct?
                operand = maybe_load_address_for_array_or_function(context, operand);
                operand = maybe_insert_bitfield_cast(context, operand);
                
                if(operand->resolved_type == &globals.typedef_void){
                    if(op->base.resolved_type != &globals.typedef_void){
                        report_error(context, operand->token, "cast of 'void' to 'non-void'.");
                        return operand;
                    }
                }else if(operand->resolved_type->kind == AST_integer_type){
                    // @note: these are fine, can cast to everything
                }else if(operand->resolved_type->kind == AST_float_type){
                    if(op->base.resolved_type->kind == AST_pointer_type){
                        report_error(context, operand->token, "Cast from float to pointer is illegal.");
                        return operand;
                    }
                }else if(operand->resolved_type->kind == AST_pointer_type){
                    if(op->base.resolved_type->kind == AST_float_type){
                        report_error(context, operand->token, "Cast from pointer to float is illegal.");
                        return operand;
                    }
                }else{
                    if(op->base.resolved_type != &globals.typedef_void){
                        struct string type_string = push_type_string(context->arena, &context->scratch, operand->resolved_type);
                        report_error(context, operand->token, "Cannot cast operand of type '%.*s' to something other than 'void'.", type_string.length, type_string.data);
                        return operand;
                    }
                }
                
                // @cleanup: @leak: we push another cast, because this applies constant propagations
                operand = push_cast(context, op->base.resolved_type, op->base.defined_type, operand);
                
                prefix = operand; // 'constant' properage the previous cast.
                context->in_lhs_expression = false;
                
                op->operand = operand;
                operand = prefix; // sets in op->operand in the next iteration
            }break;
            
            // :multiplicative_expression parse_multiplicative_expression
            // 
            // Precedence 3: Multiplicative expression, Left-to-Right Associative
            //    
            //    lhs * rhs, lhs / rhs, lhs % rhs
            //    
            case AST_binary_times: case AST_binary_divide: case AST_binary_mod:{
                
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                if(op->lhs->kind == AST_integer_literal && operand->kind == AST_integer_literal){
                    operand = perform_integer_operation(context, op);
                }else{
                    op->rhs = operand;
                    if(!check_binary_for_basic_types(context, op, CHECK_integer | CHECK_float)) return operand;
                    maybe_insert_arithmetic_conversion_casts(context, op);
                    
                    // @note: only checking the rhs works, because of the arithmetic conversion casts.
                    if(op->base.kind == AST_binary_mod && op->rhs->resolved_type->kind == AST_float_type){
                        report_error(context, op->base.token, "Operator '%%' is illegal for floats.");
                        return operand;
                    }
                    
                    if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                        operand = perform_float_operation(context, op);
                    }else{
                        if(op->lhs->resolved_type != op->rhs->resolved_type){
                            report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                            return operand;
                        }
                        set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
                        operand = &op->base;
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
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(op->lhs->kind == AST_integer_literal && operand->kind == AST_integer_literal){
                    operand = perform_integer_operation(context, op);
                }else{
                    // @cleanup: I think this check should be later for only arithmetic types
                    if(!check_binary_for_basic_types(context, op, CHECK_basic)) return operand;
                    op->lhs = maybe_load_address_for_array_or_function(context, op->lhs);
                    op->rhs = maybe_load_address_for_array_or_function(context, op->rhs);
                    
                    struct ast *handled = null;
                    // @cleanup: interger + pointer is legal, integer - pointer is not.
                    if(op->lhs->resolved_type->kind == AST_pointer_type && op->rhs->resolved_type->kind == AST_integer_type){
                        // 'pointer + integer', 'pointer - integer',
                        handled = handle_pointer_arithmetic(context, &op->base);
                    }else if(op->base.kind == AST_binary_minus){
                        // 'pointer - pointer'
                        
                        if(op->lhs->resolved_type->kind == AST_pointer_type && op->rhs->resolved_type->kind == AST_pointer_type){
                            if(!types_are_equal(op->lhs->resolved_type, op->rhs->resolved_type)){
                                report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                                return operand;
                            }
                            set_resolved_type(&op->base, &globals.typedef_s64, null);
                            
                            struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)op->lhs->resolved_type;
                            struct ast *size = ast_push_s64_literal(context, op->base.token, pointer->pointer_to->size);
                            handled = ast_push_binary_expression(context, AST_binary_divide, op->base.token, &op->base, size);
                            set_resolved_type(handled, &globals.typedef_s64, null);
                        }
                    }else if(op->lhs->resolved_type->kind == AST_integer_type && op->rhs->resolved_type->kind == AST_pointer_type){
                        // swap the arguments, as this is what 'handle_pointer_arithmetic' expects
                        struct ast *temp = op->rhs;
                        op->rhs = op->lhs;
                        op->lhs = temp;
                        handled = handle_pointer_arithmetic(context, &op->base);
                    }
                    
                    if(!handled){
                        maybe_insert_arithmetic_conversion_casts(context, op);
                        
                        if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                            handled = perform_float_operation(context, op);
                        }else{
                            if(op->lhs->resolved_type != op->rhs->resolved_type){
                                report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                                return operand;
                            }else{
                                set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
                                handled = &op->base;
                            }
                        }
                    }
                    operand = handled;
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
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(!check_binary_for_basic_types(context, op, CHECK_integer)) return operand;
                
                // "The integer promotions are performed on each of the operands."
                op->lhs = maybe_insert_integer_promotion_cast(context, op->lhs);
                op->rhs = maybe_insert_integer_promotion_cast(context, op->rhs);
                
                if(op->rhs->kind == AST_integer_literal){
                    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)op->rhs;
                    
                    if(type_is_signed(literal->base.resolved_type)){
                        b32 should_error = false;
                        switch(literal->base.resolved_type->size){
                            case 1: should_error = (literal->_s8  < 0); break;
                            case 2: should_error = (literal->_s16 < 0); break;
                            case 4: should_error = (literal->_s32 < 0); break;
                            case 8: should_error = (literal->_s64 < 0); break;
                            invalid_default_case();
                        }
                        if(should_error){
                            report_error(context, literal->base.token, "Cannot shift by a negative amount.");
                            return operand;
                        }
                    }
                    
                    if(op->lhs->kind == AST_integer_literal){
                        struct ast_integer_literal *lhs_lit = cast(struct ast_integer_literal *)op->lhs;
                        struct ast_integer_literal *rhs_lit = literal;
                        
                        // @note: I think these shifts are implicitly arithmetic when they have to be
                        
                        struct ast_type *lhs_type = lhs_lit->base.resolved_type;
                        
                        if(op->base.kind == AST_binary_left_shift){
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
                        
                        // "The type of the result is that of the promoted left operand."
                        lhs_lit->base.resolved_type = lhs_type;
                        
                        operand = &lhs_lit->base;
                        goto shift_expression_end;
                    }
                    
                    u64 value = integer_literal_as_u64(&literal->base);
                    
                    // @cleanup: overflow on the right hand side?
                    // we only have to check the u8 part as that is the only thing that gets used.
                    if(value > (u64)op->lhs->resolved_type->size * 8){
                        report_error(context, op->base.token, "Shift by '%d' but lhs has only '%d' bits.", value, op->lhs->resolved_type->size * 8);
                        return operand;
                    }
                }
                
                // @hmm: we require op->rhs->size == 1...
                op->rhs = push_cast(context, &globals.typedef_u8, null, op->rhs);
                
                // "the type of the result is that of the promoted left operand"
                set_resolved_type(&op->base, op->lhs->resolved_type, op->lhs->defined_type);
                operand = &op->base;
                context->in_lhs_expression = false;
                shift_expression_end:;
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
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
                    operand = perform_integer_operation(context, op);
                    // "The result has type int."
                    set_resolved_type(operand, &globals.typedef_s32, null);
                    break;
                }
                
                op->lhs = maybe_load_address_for_array_or_function(context, op->lhs);
                op->rhs = maybe_load_address_for_array_or_function(context, op->rhs);
                
                if(!check_binary_for_basic_types(context, op, CHECK_basic)) return operand;
                
                maybe_cast_literal_0_to_void_pointer(context, &op->lhs, &op->rhs); // @cleanup: this seems sus to me, but all compilers seem to do this.
                maybe_insert_arithmetic_conversion_casts(context, op);
                
                if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                    // @note: perform_float_operation returns an 'AST_integer_literal' in this case
                    operand = perform_float_operation(context, op);
                    break;
                }
                
                struct ast_type *match = types_are_equal(op->lhs->resolved_type, op->rhs->resolved_type);
                if(!match){
                    report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                    return operand;
                }
                
                set_resolved_type(&op->base, &globals.typedef_s32, null);
                operand = &op->base;
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
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
                    operand = perform_integer_operation(context, op);
                    // "The result has type int."
                    set_resolved_type(operand, &globals.typedef_s32, null);
                    break;
                }
                
                op->lhs = maybe_load_address_for_array_or_function(context, op->lhs);
                op->rhs = maybe_load_address_for_array_or_function(context, op->rhs);
                
                if(!check_binary_for_basic_types(context, op, CHECK_basic)) return operand;
                
                maybe_insert_cast_from_void_pointer(context, &op->lhs, &op->rhs);
                maybe_cast_literal_0_to_void_pointer(context, &op->lhs, &op->rhs);
                maybe_insert_arithmetic_conversion_casts(context, op);
                
                if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                    // @note: perform_float_operation returns an 'AST_integer_literal' in this case
                    operand = perform_float_operation(context, op);
                    break;
                }
                
                struct ast_type *match = types_are_equal(op->lhs->resolved_type, op->rhs->resolved_type);
                if(!match){
                    if(op->lhs->resolved_type->kind == AST_pointer_type && op->rhs->resolved_type->kind == AST_pointer_type){
                        report_type_mismatch_warning(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                    }else{
                        report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                        return operand;
                    }
                }
                
                set_resolved_type(&op->base, &globals.typedef_s32, null);
                operand = &op->base;
                context->in_lhs_expression = false;
            }break;
            
            // :bitwise_and_expression parse_bitwise_and_expression
            // 
            // Precedence 8: Bitwise AND, Left-to-Right Associative
            // 
            //    lhs & rhs
            //    
            case AST_binary_and:{
                
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(op->rhs->kind == AST_integer_literal && op->lhs->kind == AST_integer_literal){
                    // @cleanup: why do we not use perform_integer_operation?
                    
                    u64 lhs = integer_literal_as_u64(op->lhs);
                    u64 rhs = integer_literal_as_u64(op->rhs);
                    
                    lhs &= rhs;
                    
                    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
                    
                    set_resolved_type(op->lhs, promoted_type, null);
                    ((struct ast_integer_literal *)op->lhs)->_u64 = lhs;
                    operand = op->lhs;
                }else{
                    if(!check_binary_for_basic_types(context, op, CHECK_integer)) return operand;
                    maybe_insert_arithmetic_conversion_casts(context, op);
                    
                    if(op->lhs->resolved_type != op->rhs->resolved_type){
                        report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                        return operand;
                    }
                    set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
                    operand = &op->base;
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
                
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(op->rhs->kind == AST_integer_literal && op->lhs->kind == AST_integer_literal){
                    // @cleanup: why do we not use perform_integer_operation?
                    u64 lhs = integer_literal_as_u64(op->lhs);
                    u64 rhs = integer_literal_as_u64(op->rhs);
                    
                    lhs ^= rhs;
                    
                    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
                    
                    set_resolved_type(op->lhs, promoted_type, null);
                    ((struct ast_integer_literal *)op->lhs)->_u64 = lhs;
                    operand = op->lhs;
                }else{
                    if(!check_binary_for_basic_types(context, op, CHECK_integer)) return operand;
                    maybe_insert_arithmetic_conversion_casts(context, op);
                    
                    if(op->lhs->resolved_type != op->rhs->resolved_type){
                        report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                        return operand;
                    }
                    set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
                    operand = &op->base;
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
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                
                if(op->rhs->kind == AST_integer_literal && op->lhs->kind == AST_integer_literal){
                    // @cleanup: why do we not use perform_integer_operation?
                    u64 lhs = integer_literal_as_u64(op->lhs);
                    u64 rhs = integer_literal_as_u64(op->rhs);
                    
                    lhs |= rhs;
                    
                    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
                    
                    set_resolved_type(op->lhs, promoted_type, null);
                    ((struct ast_integer_literal *)op->lhs)->_u64 = lhs;
                    operand = op->lhs;
                }else{
                    if(!check_binary_for_basic_types(context, op, CHECK_integer)) return operand;
                    maybe_insert_arithmetic_conversion_casts(context, op);
                    
                    if(op->lhs->resolved_type != op->rhs->resolved_type){
                        report_type_mismatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                        return operand;
                    }
                    set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
                    operand = &op->base;
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
                operand = maybe_load_address_for_array_or_function(context, operand);
                
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                op->lhs = op->lhs;
                if(!casts_implicitly_to_bool(operand)){
                    // :Error pick a way to say this an stick to it
                    report_error(context, op->base.token, "Right of '&&' has to be convertible to _Bool.");
                    return operand;
                }
                
                if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)op->lhs;
                    if(integer_literal_as_u64(op->lhs) && integer_literal_as_u64(op->rhs)){
                        lit->_u64 = 1;
                    }else{
                        lit->_u64 = 0;
                    }
                    set_resolved_type(&lit->base, &globals.typedef_s32, null);
                    operand = &lit->base;
                }else{
                    set_resolved_type(&op->base, &globals.typedef_s32, null);
                    operand = &op->base;
                }
                context->in_lhs_expression = false;
            }break;
            
            // :logical_or_expression parse_logical_or_expression
            // 
            // Precedence 12: Logical OR, Left-to-Right Associative
            // 
            //    lhs || rhs
            //    
            case AST_logical_or:{
                operand = maybe_load_address_for_array_or_function(context, operand);
                
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                op->rhs = operand;
                op->lhs = op->lhs;
                if(!casts_implicitly_to_bool(operand)){
                    report_error(context, op->base.token, "Right of '||' has to be convertible to _Bool.");
                    return operand;
                }
                
                if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)op->lhs;
                    if(integer_literal_as_u64(op->lhs) || integer_literal_as_u64(op->rhs)){
                        lit->_u64 = 1;
                    }else{
                        lit->_u64 = 0;
                    }
                    set_resolved_type(&lit->base, &globals.typedef_s32, null);
                    operand = &lit->base;
                }else{
                    set_resolved_type(&op->base, &globals.typedef_s32, null);
                    operand = &op->base;
                }
                context->in_lhs_expression = false;
            }break;
            
            // :conditional_expression parse_conditional_expression
            // 
            // Precedence 13: Ternary operator, Right-to-Left Associative (the middle operator is parsed as if it was in parenthesis)
            // 
            //    lhs ? true : false
            //    
            case AST_conditional_expression:{
                struct ast_conditional_expression *cond = cast(struct ast_conditional_expression *)ast_stack_pop(context);
                
                // @cleanup: this has a lot of code that any binary expression also has e.g.
                //              'maybe_cast_literal_0_to_void_pointer'
                //              'maybe_insert_arithmetic_conversion_casts'
                //           maybe we could make it compatible with binary ops, to use these functions
                
                cond->if_false = maybe_load_address_for_array_or_function(context, operand);
                cond->if_false = maybe_insert_bitfield_cast(context, cond->if_false);
                // "one of the following shall hold":
                
                // "- both are arithmetic types"
                if(type_is_arithmetic(cond->if_true->resolved_type) && type_is_arithmetic(cond->if_false->resolved_type)){
                    struct ast_type *arithmetic_type = perform_usual_arithmetic_conversions(cond->if_true->resolved_type, cond->if_false->resolved_type);
                    
                    if(cond->if_true->resolved_type != arithmetic_type){
                        cond->if_true = push_cast(context, arithmetic_type, defined_type_for_promotion(cond->if_true), cond->if_true);
                    }
                    if(cond->if_false->resolved_type != arithmetic_type){
                        cond->if_false = push_cast(context, arithmetic_type, defined_type_for_promotion(cond->if_false), cond->if_false);
                    }
                    
                }else{
                    
                    // "- one is a pointer to an object and the other is a pointer to void"
                    maybe_insert_cast_from_void_pointer(context, &cond->if_true, &cond->if_false);
                    
                    // "- one operand is a pointer and the other is a null pointer constant"
                    maybe_cast_literal_0_to_void_pointer(context, &cond->if_true, &cond->if_false);
                    
                    // "- both operands have the same union or structure type"
                    // "- both operands have void type"
                    // "- both operands are pointers to compatible types"
                    if(!types_are_equal(cond->if_false->resolved_type, cond->if_true->resolved_type)){
                        report_type_mismatch_error(context, cond->if_true->resolved_type, cond->if_true->defined_type, cond->if_false->resolved_type, cond->if_false->defined_type, cond->base.token);
                        return operand;
                    }
                }
                
                // @cleanup: how should these defined_types propagate in this case?
                struct ast *defined_type = cond->if_true->defined_type ? cond->if_true->defined_type : cond->if_false->defined_type;
                set_resolved_type(&cond->base, cond->if_true->resolved_type, defined_type);
                
                // Constant propagation.
                if(cond->condition->kind == AST_integer_literal){
                    operand = integer_literal_as_u64(cond->condition) ? cond->if_true : cond->if_false;
                }else if(cond->condition->kind == AST_pointer_literal){
                    struct ast_pointer_literal *pointer_literal = (struct ast_pointer_literal *)cond->condition;
                    operand = pointer_literal->pointer ? cond->if_true : cond->if_false;
                }else if(cond->condition->kind == AST_float_literal){
                    struct ast_float_literal *float_literal = (struct ast_float_literal *)cond->condition;
                    operand = float_literal->value ? cond->if_true : cond->if_false;
                }else{
                    operand = &cond->base;
                }
                context->in_lhs_expression = false;
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
                struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                assignment->rhs = maybe_load_address_for_array_or_function(context, operand);
                assignment->rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, assignment->lhs->resolved_type, assignment->lhs->defined_type, assignment->rhs, assignment->base.token);
                set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                
                operand = &assignment->base;
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
            
            // For this problem we have the hack/solution 'punt_compound_assignment_with_unnamed_declaration',
            // which transforms the assignment 'a op= b' into '(unnamed = &a, *unnamed = *unnamed op b)'
            // with assignment cast and arithmetic conversions applied.
            // This is probably pretty slow and produces terrible code, so only use it if you have to!
            //                                                                                   -19.09.2021
            
            case AST_plus_assignment:
            case AST_minus_assignment:{
                struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                assignment->rhs = maybe_load_address_for_array_or_function(context, operand);
                
                if(!check_binary_for_basic_types(context, assignment, CHECK_basic)) return operand;
                
                if(assignment->rhs->resolved_type->kind == AST_pointer_type){
                    // the rhs should never be a pointer
                    report_error(context, assignment->base.token, "Right of '%s' cannot be a pointer.", assignment->base.kind == AST_plus_assignment ? "+=" : "-=");
                    return operand;
                }
                
                struct ast_type *lhs_type = assignment->lhs->resolved_type;
                struct ast_type *rhs_type = assignment->rhs->resolved_type;
                
                if(lhs_type->kind == AST_pointer_type){
                    if(rhs_type->kind == AST_integer_type){
                        struct ast *handled = handle_pointer_arithmetic(context, &assignment->base);
                        
                        if(handled != &assignment->base){
                            //
                            // This can happen if we get something like '(u8 *)0 + 1337'
                            // which gets transformed into '(u8 *)1337'
                            //
                            assignment = (struct ast_binary_op *)handled; // @sigh: scarry cast
                        }
                    }else{
                        report_error(context, assignment->base.token, "Left is a pointer type and the right is not an integer.");
                        return operand;
                    }
                }else{
                    b32 punt = false;
                    
                    if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_float_type){
                        // punt on 'integer += float' and 'integer -= float'.
                        // this is necessary as for example 'int i = 0x4000000; i += 1.0f;' yields 'i == 0x4000000'.
                        punt = true;
                    }else if(assignment->lhs->resolved_type == &globals.typedef_f32 && assignment->rhs->resolved_type == &globals.typedef_f64){
                        // punt on 'float += double', this is necessary for example
                        //     double var_00 = -1183312983.13;
                        //     float  var_08 =  -282911132.13f;
                        //     var_08 -= var_00;
                        // @cleanup: search for a better example!
                        punt = true;
                    }
                    
                    if(assignment->lhs->resolved_type == &globals.typedef_Bool || (assignment->lhs->resolved_type->kind == AST_bitfield_type)) punt = 1;
                    
                    if(punt){
                        enum ast_kind kind = assignment->base.kind == AST_plus_assignment ? AST_binary_plus : AST_binary_minus;
                        assignment = punt_compound_assignment_with_unnamed_declaration(context, assignment, kind);
                    }else{
                        assignment->rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, assignment->lhs->resolved_type, assignment->lhs->defined_type, assignment->rhs, assignment->base.token);
                        set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                    }
                }
                
                operand = &assignment->base;
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_modulo_assignment:{
                struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                assignment->rhs = maybe_load_address_for_array_or_function(context, operand);
                
                if(!check_binary_for_basic_types(context, assignment, CHECK_integer)) return operand;
                
                // Punt if the right hand side is bigger than the lhs, because we could have situation like
                //     char a = 1; int b = 0x100; a %= b; 
                // in which case 'b' cant be truncated to char, or it will crash.
                b32 punt = (assignment->lhs->resolved_type->size < assignment->rhs->resolved_type->size);
                
                if(assignment->rhs->kind == AST_integer_literal){
                    u64 value = integer_literal_as_u64(assignment->rhs);
                    if(value == 0){
                        report_error(context, assignment->base.token, "Compile time '%=' by 0.");
                        return operand;
                    }
                    
                    if(value < (1ull << assignment->lhs->resolved_type->size)){
                        // @cleanup: you could see how we could report an error here instead of punting in some cases
                        //           but this hits the common case of % by some small integer (like 10).
                        punt = false;
                    }
                }
                
                if(assignment->lhs->resolved_type == &globals.typedef_Bool || (assignment->lhs->resolved_type->kind == AST_bitfield_type)) punt = 1;
                
                if(punt){
                    assignment = punt_compound_assignment_with_unnamed_declaration(context, assignment, AST_binary_mod);
                }else{
                    assignment->rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, assignment->lhs->resolved_type, assignment->lhs->defined_type, assignment->rhs, assignment->base.token);
                    set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                }
                
                operand = &assignment->base;
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_and_assignment:
            case AST_or_assignment:
            case AST_xor_assignment:{
                struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                assignment->rhs = operand;
                
                set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                
                if(!check_binary_for_basic_types(context, assignment, CHECK_integer)) return operand;
                
                if(assignment->lhs->resolved_type == &globals.typedef_Bool || (assignment->lhs->resolved_type->kind == AST_bitfield_type)){
                    enum ast_kind punt_kind = 0;
                    if(assignment->base.kind == AST_and_assignment) punt_kind = AST_binary_and;
                    if(assignment->base.kind == AST_or_assignment)  punt_kind = AST_binary_or;
                    if(assignment->base.kind == AST_xor_assignment) punt_kind = AST_binary_xor;
                    
                    assignment = punt_compound_assignment_with_unnamed_declaration(context, assignment, punt_kind);
                }else{
                    // These are always fine, we never have to punt.
                    assignment->rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, assignment->lhs->resolved_type, assignment->lhs->defined_type, assignment->rhs, assignment->base.token);
                }
                
                operand = &assignment->base;
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_divide_assignment:
            case AST_times_assignment:{
                struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                assignment->rhs = maybe_load_address_for_array_or_function(context, operand);
                if(!check_binary_for_basic_types(context, assignment, CHECK_integer|CHECK_float)) return operand;
                
                struct ast_type *lhs_type = assignment->lhs->resolved_type;
                struct ast_type *rhs_type = assignment->rhs->resolved_type;
                
                int punt = false;
                if(lhs_type->kind == AST_integer_type && rhs_type->kind == AST_float_type) punt = true;
                
                //
                // Punt if the lhs type is smaller than the rhs.
                // This is prevents the truncation of the rhs, when in reality the rhs should be promoted.
                // 
                if(lhs_type->size < rhs_type->size) punt = true;
                
                if(assignment->rhs->kind == AST_integer_literal){
                    // 
                    // @copy and paste from above.
                    // 
                    
                    u64 value = integer_literal_as_u64(assignment->rhs);
                    if(value == 0){
                        if(assignment->base.kind == AST_divide_assignment){
                            report_error(context, assignment->base.token, "Compile time '/=' by 0.");
                            return operand;
                        }else{
                            report_warning(context, WARNING_compile_time_multiplication_by_zero, assignment->base.token, "Compile time '*=' by 0.");
                        }
                    }
                    
                    if(value < (1ull << assignment->lhs->resolved_type->size)){
                        // @cleanup: you could see how we could report an error here instead of punting in some cases
                        //           but this hits the common case of % by some small integer (like 10).
                        punt = false;
                    }
                }
                
                if(assignment->lhs->resolved_type == &globals.typedef_Bool || (assignment->lhs->resolved_type->kind == AST_bitfield_type)) punt = 1;
                
                if(punt){
                    enum ast_kind kind = (assignment->base.kind == AST_divide_assignment) ? AST_binary_divide : AST_binary_times;
                    assignment = punt_compound_assignment_with_unnamed_declaration(context, assignment, kind);
                }else{
                    assignment->rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, assignment->lhs->resolved_type, assignment->lhs->defined_type, assignment->rhs, assignment->base.token);
                    set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                }
                
                operand = &assignment->base;
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_left_shift_assignment:
            case AST_right_shift_assignment:{
                struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                assignment->rhs = maybe_load_address_for_array_or_function(context, operand);
                if(!check_binary_for_basic_types(context, assignment, CHECK_integer)) return operand;
                
                if(assignment->lhs->resolved_type == &globals.typedef_Bool || (assignment->lhs->resolved_type->kind == AST_bitfield_type)){
                    enum ast_kind kind = (assignment->base.kind == AST_left_shift_assignment) ? AST_binary_left_shift : AST_binary_right_shift;
                    assignment = punt_compound_assignment_with_unnamed_declaration(context, assignment, kind);
                }else{
                    assignment->rhs = push_cast(context, &globals.typedef_u8, null, assignment->rhs);
                    set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                }
                
                operand = &assignment->base;
                context->in_lhs_expression = false; // Does not really matter they get scoped the other way.
            }break;
            
            case AST_comma_expression:{
                assert(!should_skip_comma_expression);
                struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
                
                op->rhs = operand;
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                operand = &op->base;
                context->in_lhs_expression = false; // @hmm: should we allow (3, a) = 3; probably not
            }break;
            
            invalid_default_case();
        }
    }
    
    switch(binary_expression->type){
        
        // Precedence 3
        case TOKEN_times: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_times, binary_expression, operand, null));  goto restart;
        case TOKEN_slash: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_divide, binary_expression, operand, null)); goto restart;
        case TOKEN_mod:   ast_stack_push(context, ast_push_binary_expression(context, AST_binary_mod, binary_expression, operand, null));    goto restart;
        
        // Precedence 4
        case TOKEN_plus:  ast_stack_push(context, ast_push_binary_expression(context, AST_binary_plus, binary_expression, operand, null));  goto restart;
        case TOKEN_minus: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_minus, binary_expression, operand, null)); goto restart;
        
        // Precedence 5
        case TOKEN_left_shift:  ast_stack_push(context, ast_push_binary_expression(context, AST_binary_left_shift, binary_expression, operand, null));   goto restart;
        case TOKEN_right_shift: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_right_shift, binary_expression, operand, null)); goto restart;
        
        // Precedence 6
        case TOKEN_bigger_equals:  ast_stack_push(context, ast_push_binary_expression(context, AST_binary_bigger_equals, binary_expression, operand, null)); goto restart;
        case TOKEN_smaller_equals: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_smaller_equals, binary_expression, operand, null)); goto restart;
        case TOKEN_bigger:  ast_stack_push(context, ast_push_binary_expression(context, AST_binary_bigger, binary_expression, operand, null)); goto restart;
        case TOKEN_smaller: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_smaller, binary_expression, operand, null)); goto restart;
        
        // Precedence 7
        case TOKEN_logical_equals:   ast_stack_push(context, ast_push_binary_expression(context, AST_binary_logical_equals,   binary_expression, operand, null)); goto restart;
        case TOKEN_logical_unequals: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_logical_unequals, binary_expression, operand, null)); goto restart;
        
        // Precedence 8
        case TOKEN_and: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_and, binary_expression, operand, null)); goto restart;
        
        // Precedence 9
        case TOKEN_xor: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_xor, binary_expression, operand, null)); goto restart;
        
        // Precedence 10
        case TOKEN_or: ast_stack_push(context, ast_push_binary_expression(context, AST_binary_or, binary_expression, operand, null)); goto restart;
        
        // Precedence 11
        case TOKEN_logical_and:{
            operand = maybe_load_address_for_array_or_function(context, operand);
            if(!casts_implicitly_to_bool(operand)){
                report_error(context, next_token(context), "Left of '&&' has to be convertible to _Bool.");
                return operand;
            }
            
            struct ast *ast = ast_push_binary_expression(context, AST_logical_and, binary_expression, operand, null);
            ast_stack_push(context, ast);
            goto restart;
        }break;
        
        // Precedence 12
        case TOKEN_logical_or:{
            operand = maybe_load_address_for_array_or_function(context, operand);
            
            if(!casts_implicitly_to_bool(operand)){
                report_error(context, next_token(context), "Left of '||' has to be convertible to _Bool.");
                return operand;
            }
            struct ast *ast = ast_push_binary_expression(context, AST_logical_or, binary_expression, operand, null);
            ast_stack_push(context, ast);
            goto restart;
        }break;
        
        // Precedence 13
        case TOKEN_question_mark:{
            // conditional_expression = logical_or_expression ? expression : conditional_expression;
            
            struct token *question_mark = binary_expression;
            struct ast_conditional_expression *cond = parser_ast_push(context, question_mark, conditional_expression);
            
            operand = maybe_load_address_for_array_or_function(context, operand);
            
            if(!casts_implicitly_to_bool(operand)){
                report_error(context, cond->base.token, "Left hand side of '?' has no implicit conversion to bool.");
                return operand;
            }
            
            cond->condition = operand;
            struct ast *if_true = parse_expression(context, false);
            cond->if_true = maybe_load_address_for_array_or_function(context, if_true);
            cond->if_true = maybe_insert_bitfield_cast(context, cond->if_true);
            expect_token(context, TOKEN_colon, "Expected ':' in conditional expression.");
            if(context->should_exit_statement) return operand;
            
            ast_stack_push(context, &cond->base);
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
            struct ast *assign;
            
            switch(binary_expression->type){
                case TOKEN_equals:{
                    assign = ast_push_binary_expression(context, AST_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_and_equals:{
                    assign = ast_push_binary_expression(context, AST_and_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_or_equals:{
                    assign = ast_push_binary_expression(context, AST_or_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_xor_equals:{
                    assign = ast_push_binary_expression(context, AST_xor_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_plus_equals:{
                    assign = ast_push_binary_expression(context, AST_plus_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_minus_equals:{
                    assign = ast_push_binary_expression(context, AST_minus_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_left_shift_equals:{
                    assign = ast_push_binary_expression(context, AST_left_shift_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_right_shift_equals:{
                    assign = ast_push_binary_expression(context, AST_right_shift_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_div_equals:{
                    assign = ast_push_binary_expression(context, AST_divide_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_mod_equals:{
                    assign = ast_push_binary_expression(context, AST_modulo_assignment, binary_expression, operand, null);
                }break;
                case TOKEN_times_equals:{
                    assign = ast_push_binary_expression(context, AST_times_assignment, binary_expression, operand, null);
                }break;
                default: assign = null; break;
            };
            
            if(assign){
                if(!context->in_lhs_expression){
                    report_error(context, assign->token, "Left is not assignable.");
                    return operand;
                }
                
                if(operand->resolved_type->kind == AST_array_type){
                    report_error(context, assign->token, "Cannot assign to an array.");
                    return operand;
                }
                
                if(operand->resolved_type->kind == AST_function_type){
                    report_error(context, assign->token, "Cannot assign to a function.");
                    return operand;
                }
                
                if(operand->resolved_type == &globals.typedef_Bool && assign->kind != AST_assignment && assign->kind != AST_xor_assignment && assign->kind != AST_or_assignment && assign->kind != AST_and_assignment){
                    report_error(context, assign->token, "Only the operators '=', '|=', '&=' and '^=' are supported, when the left is of type _Bool.");
                    return operand;
                }
                
                if(operand->resolved_type == &globals.typedef_void){
                    report_error(context, assign->token, "Cannot assign to something of type 'void'.");
                    return operand;
                }
                
                ast_stack_push(context, assign);
                goto restart;
            }
        }break;
        
        // Precedence 15
        case TOKEN_comma:{
            if(should_skip_comma_expression){
                prev_token(context);
                break;
            }
            
            struct ast *ast = ast_push_binary_expression(context, AST_comma_expression, binary_expression, operand, null);
            ast_stack_push(context, ast);
            goto restart;
        }break;
        
        // The 'binary_expression' token was not part of this expression.
        default: prev_token(context); break;
    }
    
    
    // Remove the artificial invalid ast we put on in the beginning.
    if(did_push){
        struct ast *guard_ast = ast_stack_pop(context);
        assert(guard_ast->kind == AST_invalid);
    }
    
    // @note: This should never fire as we want to return early if there is an error deeper in the stack.
    assert(ast_stack_before == context->ast_stack_at);
    
    return operand;
}

func void validate_intrinsic(struct context *context, struct intrinsic_info *info, struct ast_function *function){
    struct ast_function_type *type = function->type;
    (void)info;
    
    //
    // @cleanup: currently the only intrinsic function is 'va_start'...
    //
    
    for_ast_list(type->argument_list){
        assert(it->value->kind == AST_declaration);
        struct ast_declaration *decl = (struct ast_declaration *)it->value;
        
        if(!maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) return;
    }
    
    if(type->flags & FUNCTION_TYPE_FLAGS_is_varargs){
        if((info->kind != INTRINSIC_KIND_va_start)){
            report_error(context, function->base.token, "Intrinsic function cannot be declared varargs.");
            return;
        }
    }
    
#define arg_decl_to_type(a) (((struct ast_declaration *)(a))->type)
    
    // Check that the function matches what we expect.
    switch(info->kind){
        case INTRINSIC_KIND_va_start:{
            if(type->return_type != &globals.typedef_void      ||
                    type->argument_list.count != 1                  ||
                    !(type->flags & FUNCTION_TYPE_FLAGS_is_varargs) ||
                    (arg_decl_to_type(type->argument_list.first->value)->kind != AST_pointer_type)){
                report_error(context, function->base.token,
                        "Declaration of intrinsic function must match 'void __va_start(va_list* , ...)'.");
            }
        }break;
        
        invalid_default_case();
    }
    
#undef arg_decl_to_type
}

func void check_and_set_declaration_specifier_flag(struct context *context, struct declaration_specifiers *specifiers, enum declaration_specifier_flag flag, struct token *site, char *error_name){
    if(specifiers->specifier_flags & flag){
        report_warning(context, WARNING_double_specifier, site, "Double '%s'.", error_name);
    }else{
        specifiers->specifier_flags |= flag;
    }
}

func struct declaration_specifiers parse_declaration_specifiers(struct context *context, struct ast_type *optional_type_specifier, struct ast *optional_defined_type_specifier){
    
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
                report_warning(context, WARNING_atomic_ignored, token, "'_Atomic' is ignored for now.");
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
                    struct ast *const_expr = parse_expression(context, /*should_skip_comma_expression*/false);
                    
                    if(const_expr->kind != AST_integer_literal){
                        report_error(context, const_expr->token, "Argument of '_Alignas' must be a type name or a constant expression.");
                    }else{
                        value = integer_literal_as_u64(const_expr);
                    }
                }
                
                // "An alignment specification of zero has no effect."
                if(value != 0){
                    
                    if(!is_power_of_two(value)){
                        report_error(context, token, "Alignment must be a power of two.");
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
                struct token *directive = expect_token(context, TOKEN_identifier, "Expected a directive after '__declspec'.");
                b32 skip = false;
                
                struct atom directive_string = directive->atom;
                
                if(atoms_match(directive_string, globals.keyword_dllimport)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_dllimport, directive, "__declspec(dllimport)");
                }else if(atoms_match(directive_string, globals.keyword_dllexport)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_dllexport, directive, "__declspec(dllexport)");
                }else if(atoms_match(directive_string, globals.keyword_noreturn)){
                    check_and_set_declaration_specifier_flag(context, &specifiers, SPECIFIER_noreturn, directive, "__declspec(noreturn)");
                }else if(atoms_match(directive_string, globals.keyword_align)){
                    expect_token(context, TOKEN_open_paren, "Expected '(' to follow 'align'.");
                    
                    struct ast *const_expr = parse_expression(context, /*should_skip_comma_expression*/false);
                    
                    if(const_expr->kind != AST_integer_literal){
                        report_error(context, directive, "Expected a constant expression in '__declspec(align(_))'.");
                    }else{
                        u64 value = integer_literal_as_u64(const_expr);
                        
                        //
                        // struct __declspec(align(0x100)) asd;
                        //
                        // @note: the difference between '__declspec(align(#)) struct asd {int asd; } asd;'
                        //        and                    'struct __declspec(align(#)) asd {int asd; } asd;'
                        // is whether all instances of 'struct asd' are #-aligned or just 'asd'.
                        //
                        
                        if(!is_power_of_two(value)){
                            report_error(context, directive, "Alignment must be a power of two.");
                        }
                        
                        if(specifiers.alignment){
                            // @cleanup: warn here!
                            // report_warning(context, WARNING_alignment_specified_more_then_once, "Alignment");
                            specifiers.alignment = max_of(specifiers.alignment, value);
                        }else{
                            specifiers.alignment = value;
                        }
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
                while(peek_token(context, TOKEN_declspec)){
                    next_token(context);
                    struct token *open = expect_token(context, TOKEN_open_paren, "Expected '(' to follow '__declspec'.");
                    
                    struct token *declspec = expect_token(context, TOKEN_identifier, "Expected an identifier after '__declspec'.");
                    b32 unsupported = false;
                    
                    if(atoms_match(declspec->atom, globals.keyword_align)){
                        expect_token(context, TOKEN_open_paren, "Expected '(' to follow 'align'.");
                        
                        struct ast *const_expr = parse_expression(context, /*should_skip_comma_expression*/false);
                        
                        if(const_expr->kind != AST_integer_literal){
                            report_error(context, declspec, "Expected a constant expression in '__declspec(align(_))'.");
                        }else{
                            u64 value = integer_literal_as_u64(const_expr);
                            
                            //
                            // struct __declspec(align(0x100)) asd;
                            //
                            // @note: the difference between '__declspec(align(#)) struct asd {int asd; } asd;'
                            //        and                    'struct __declspec(align(#)) asd {int asd; } asd;'
                            // is whether all instances of 'struct asd' are #-aligned or just 'asd'.
                            //
                            
                            if(!is_power_of_two(value)){
                                report_error(context, declspec, "Alignment must be a power of two.");
                            }
                            
                            if(declspec_alignment != 1){
                                // @cleanup: warning.
                                declspec_alignment = max_of(declspec_alignment, value);
                            }else{
                                declspec_alignment = value;
                            }
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
                        compound = parser_compound_type_push(context, name ? name : token, union);
                    }else{
                        compound = parser_compound_type_push(context, name ? name : token, struct);
                    }
                    
                    if(name) compound->base.token = name;
                    
                    if(is_intrin_type) compound->base.flags |= TYPE_FLAG_is_intrin_type;
                    
                    compound->identifier = name ? name->atom : globals.unnamed_tag;
                    
                    smm size = 0;
                    smm alignment = declspec_alignment;
                    
                    struct ast_type *current_bitfield_type = null;
                    s64 current_bitfield_index = 0;
                    
                    struct token *had_array_of_unknown_size = null;
                    
                    while(!peek_token_eat(context, TOKEN_closed_curly)){
                        
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
                        struct ast *lhs_defined_type = struct_specifiers.defined_type_specifier;
                        
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
                                
                                struct ast *constant = parse_expression(context, true);
                                if(constant->kind != AST_integer_literal){
                                    report_error(context, constant->token, "Bitfield width must be constant.");
                                    constant = ast_push_s32_literal(context, constant->token, 0);
                                }
                                
                                s64 width = integer_literal_as_s64(constant);
                                
                                if(width < 0){
                                    report_error(context, constant->token, "Bitfield width must be non-negative.");
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
                                    report_error(context, constant->token, "Bitfield width for type '%.*s' must be '<= %d', but is '%lld'.", type_string.size, type_string.data, (declarator.type->size * 8), width);
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
                                
                                struct ast_bitfield_type *bitfield = parser_type_push(context, colon, bitfield_type);
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
                            if(is_packed) decl_alignment = 1; // If the struct is __declspec(packed), overwrite the _natural_ alignment.
                            if(struct_specifiers.alignment) decl_alignment = struct_specifiers.alignment;
                            
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
                                    
                                    register_compound_member(context, compound, ident, declarator.type, offset_in_type);
                                }else{
                                    
                                    // :member_list_contains_both_linear_and_nested
                                    // 
                                    // First register the "anonymous" nested member, 
                                    // then each of the members of the substructure.
                                    register_compound_member(context, compound, declarator.ident, declarator.type, offset_in_type);
                                    
                                    struct ast_compound_type *nested_compound = (struct ast_compound_type *)declarator.type;
                                    
                                    for(u32 member_index = 0; member_index < nested_compound->amount_of_members; member_index++){
                                        struct compound_member *member = &nested_compound->members[member_index];
                                        
                                        if(member->name != globals.invalid_identifier_token){
                                            if(find_member_in_compound(compound, member->name->atom)){
                                                report_error(context, ident, "Redeclaration of member '%.*s'.", member->name->size, member->name->data);
                                                break;
                                            }
                                        }
                                        
                                        register_compound_member(context, compound, member->name, member->type, offset_in_type + member->offset_in_type);
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
                            register_compound_type(context, &compound->base, compound->base.token);
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
                    struct ast_compound_type *ast_enum = parser_compound_type_push(context, name ? name : token, enum);
                    
                    // "each enumerated type should be compatible with a signed or unsinged integer type. Implementation
                    //  defined but should be able to hold all of the members" -> we are lazy: int
                    ast_enum->base.size = 4;
                    ast_enum->base.alignment = 4;
                    ast_enum->identifier = name ? name->atom : globals.unnamed_enum;
                    
                    if(!context->sleeping_ident && name) context->sleeping_ident = name;
                    
                    s64 current_value = 0;
                    do{
                        if(peek_token_eat(context, TOKEN_closed_curly)) goto enum_skip_curly;
                        
                        struct token *ident = expect_token(context, TOKEN_identifier, "Expected an identifier in enum declaration.");
                        
                        struct ast *literal;
                        if(peek_token_eat(context, TOKEN_equals)){
                            struct ast *const_expr = parse_expression(context, true);
                            
                            if(const_expr->kind != AST_integer_literal){
                                report_error(context, const_expr->token, "Expression needs to resolve to constant integer.");
                                const_expr = ast_push_s32_literal(context, const_expr->token, 0);
                            }
                            
                            literal = const_expr;
                            
                            current_value = integer_literal_as_s64(const_expr);
                        }else{
                            literal = ast_push_s32_literal(context, ident, (s32)current_value);
                        }
                        
                        // "an identifier declared as an enumeration constant has type int".
                        set_resolved_type(literal, &globals.typedef_s32, cast(struct ast *)ast_enum);
                        
                        // @note: allow to u32_max, as enums are often used as flag-constants and it just does not matter
                        //        whether its s32 or u32.
                        
                        if(s32_min <= current_value && current_value <= s32_max){
                            // we are fine!
                        }else if(s32_max < current_value && current_value <= u32_max){
                            
                            // @cleanup: report a warning?
                            current_value = (s32)current_value;
                        }else{
                            report_error(context, token, "Enum members are ints, this member is outside of the range of the enum. Member is %lld range is [%d, %d].", current_value, s32_min, u32_max);
                        }
                        
                        //
                        // the c-spec says: enum members are ints, and the enum is of a size that can hold all enum members
                        struct ast_declaration *decl = parser_ast_push(context, ident, declaration);
                        decl->identifier = ident;
                        decl->flags |= DECLARATION_FLAGS_is_enum_member;
                        decl->type = &ast_enum->base;
                        decl->assign_expr = literal;
                        decl->offset_on_stack = -1;
                        decl->compilation_unit = context->current_compilation_unit;
                        
                        decl = parser_register_declaration(context, decl);
                        
                        register_compound_member(context, ast_enum, ident, &ast_enum->base, current_value);
                        
                        current_value++;
                    }while(peek_token_eat(context, TOKEN_comma));
                    
                    expect_token(context, TOKEN_closed_curly, "Expected '}' ending enum declaration or ',' to keep going.");
                    
                    enum_skip_curly:;
                    
                    if(name){
                        ast_enum->compilation_unit = context->current_compilation_unit;
                        register_compound_type(context, &ast_enum->base, ast_enum->base.token);
                    }
                    
                    specifiers.defined_type_specifier = (struct ast *)&ast_enum->base;
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
                        
                        specifiers.defined_type_specifier = (struct ast *)lookup;
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
                    
                    specifiers.defined_type_specifier = &ast_typedef->base;
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
            
            case C_TYPE_void: specifiers.type_specifier =  &globals.typedef_void; break;
            
            case C_TYPE_Bool: specifiers.type_specifier =  &globals.typedef_Bool; break;
            
            case C_TYPE_unsigned: specifiers.type_specifier =  &globals.typedef_u32; break;
            case C_TYPE_signed:   specifiers.type_specifier =  &globals.typedef_s32; break;
            case C_TYPE_char:     specifiers.type_specifier =  &globals.typedef_s8; break;
            case C_TYPE_short:    specifiers.type_specifier =  &globals.typedef_s16; break;
            case C_TYPE_int:      specifiers.type_specifier =  &globals.typedef_s32; break;
            case C_TYPE_long:     specifiers.type_specifier =  &globals.typedef_s32; break;
            
            case (C_TYPE_short | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_s16; break;
            case (C_TYPE_long  | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_s32; break;
            case (C_TYPE_long  | C_TYPE_long_long): specifiers.type_specifier =  &globals.typedef_s64; break;
            case (C_TYPE_long  | C_TYPE_long_long | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_s64; break;
            
            case (C_TYPE_unsigned | C_TYPE_char):  specifiers.type_specifier =  &globals.typedef_u8; break;
            case (C_TYPE_unsigned | C_TYPE_short): specifiers.type_specifier =  &globals.typedef_u16; break;
            case (C_TYPE_unsigned | C_TYPE_int):   specifiers.type_specifier =  &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_long):  specifiers.type_specifier =  &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_long | C_TYPE_long_long): specifiers.type_specifier =  &globals.typedef_u64; break;
            
            case (C_TYPE_unsigned | C_TYPE_short | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_u16; break;
            case (C_TYPE_unsigned | C_TYPE_long  | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_long | C_TYPE_long_long | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_u64; break;
            
            case (C_TYPE_signed | C_TYPE_char):  specifiers.type_specifier =  &globals.typedef_s8; break;
            case (C_TYPE_signed | C_TYPE_short): specifiers.type_specifier =  &globals.typedef_s16; break;
            case (C_TYPE_signed | C_TYPE_int):   specifiers.type_specifier =  &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_long):  specifiers.type_specifier =  &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_long | C_TYPE_long_long): specifiers.type_specifier =  &globals.typedef_s64; break;
            
            case (C_TYPE_signed | C_TYPE_short | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_s16; break;
            case (C_TYPE_signed | C_TYPE_long  | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_long | C_TYPE_long_long | C_TYPE_int): specifiers.type_specifier =  &globals.typedef_s64; break;
            
            case C_TYPE_int8:  specifiers.type_specifier =  &globals.typedef_s8; break;
            case C_TYPE_int16: specifiers.type_specifier =  &globals.typedef_s16; break;
            case C_TYPE_int32: specifiers.type_specifier =  &globals.typedef_s32; break;
            case C_TYPE_int64: specifiers.type_specifier =  &globals.typedef_s64; break;
            
            case (C_TYPE_signed | C_TYPE_int8):  specifiers.type_specifier =  &globals.typedef_s8; break;
            case (C_TYPE_signed | C_TYPE_int16): specifiers.type_specifier =  &globals.typedef_s16; break;
            case (C_TYPE_signed | C_TYPE_int32): specifiers.type_specifier =  &globals.typedef_s32; break;
            case (C_TYPE_signed | C_TYPE_int64): specifiers.type_specifier =  &globals.typedef_s64; break;
            
            case (C_TYPE_unsigned | C_TYPE_int8):  specifiers.type_specifier =  &globals.typedef_u8; break;
            case (C_TYPE_unsigned | C_TYPE_int16): specifiers.type_specifier =  &globals.typedef_u16; break;
            case (C_TYPE_unsigned | C_TYPE_int32): specifiers.type_specifier =  &globals.typedef_u32; break;
            case (C_TYPE_unsigned | C_TYPE_int64): specifiers.type_specifier =  &globals.typedef_u64; break;
            
            case C_TYPE_float:    specifiers.type_specifier =  &globals.typedef_f32; break;
            case C_TYPE_double:   specifiers.type_specifier =  &globals.typedef_f64; break;
            case (C_TYPE_long | C_TYPE_double): specifiers.type_specifier =  &globals.typedef_f64; break;
            
            default:{
                prev_token(context); // This token is valid, as we just now got a c type.
                report_error(context, next_token(context), "Invalid combination of base types.");
                specifiers.type_specifier =  &globals.typedef_poison;
            }break;
        }
    }
    
    if((specifiers.specifier_flags & SPECIFIER_inline) && !(specifiers.specifier_flags & SPECIFIER_extern)){
        //
        // If not explicitly extern, inline implies static.
        //
        specifiers.specifier_flags |= SPECIFIER_static;
    }
    
    return specifiers;
}


func struct declaration_list parse_declaration_list(struct context *context, struct ast_type *type_specifier, struct ast *defined_type_specifier){
    
    // @cleanup: explain this again
    b32 should_set_sleeping_ident = !context->sleeping_ident;
    
    struct declaration_list ret = zero_struct;
    
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
        
        if(declarator.type->kind == AST_unresolved_type){
            // :unresolved_types.
            // The only unresolved types that are allowed are either pointers
            // or 'AST_unresolved_type' in function arguments or typedefs.
            //                                                    -09.10.2020
            if(specifiers.specifier_flags & SPECIFIER_typedef){
                // This is fine allow typedefs to :unresolved_types.
            }else{
                sleep_or_error_on_unresolved_type(context, declarator.type);
                goto end;
            }
        }
        
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
                report_error(context, declarator.ident, "This 'extern' variable was never defined.");
                goto end;
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
            parser_register_declaration_in_local_scope_without_checking_for_redeclarations(context, context->current_scope, decl);
            
        }else if(!(specifiers.specifier_flags & SPECIFIER_typedef) && declarator.type->kind == AST_function_type){
            
            // 
            // This is a function declaration or definition at global or local scope.
            // 
            
            struct ast_function *function = parser_ast_push(context, declarator.ident, function);
            function->type = cast(struct ast_function_type *)declarator.type;
            function->identifier = declarator.ident;
            function->memory_location = null;
            function->offset_in_text_section = -1; // Set atomically when done emitting.
            function->compilation_unit = context->current_compilation_unit;
            
            set_resolved_type(&function->base, &globals.typedef_void, null);
            
            if(!context->current_scope) function->as_decl.flags |= DECLARATION_FLAGS_is_global;
            
            if(specifiers.specifier_flags & SPECIFIER_static)    function->as_decl.flags |= DECLARATION_FLAGS_is_static;
            if(specifiers.specifier_flags & SPECIFIER_selectany) function->as_decl.flags |= DECLARATION_FLAGS_is_selectany;
            
            if(specifiers.specifier_flags & SPECIFIER_inline_asm){
                
                // @cleanup: dllexport and such
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_varargs){
                    report_error(context, function->base.token, "A function declared '__declspec(inline_asm)' cannot be varargs.\n");
                }
                
                function->type->flags |= FUNCTION_TYPE_FLAGS_is_inline_asm;
            }
            
            // Check if this function is intrinsic.
            struct intrinsic_info *function_is_intrinsic = lookup_intrinsic(function->identifier->atom);
            if(function_is_intrinsic){
                function->type->flags |= FUNCTION_TYPE_FLAGS_is_intrinsic;
                
                validate_intrinsic(context, function_is_intrinsic, function);
                if(context->should_exit_statement) goto end;
            }
            
            if(specifiers.specifier_flags & SPECIFIER_dllexport){
                if(specifiers.specifier_flags & SPECIFIER_static){
                    // :error
                    report_error(context, function->base.token, "Cannot export static function.");
                    goto end;
                }
                if(function_is_intrinsic){
                    // :error
                    report_error(context, function->base.token, "Cannot export intrinsic function.");
                    goto end;
                }
                function->as_decl.flags |= DECLARATION_FLAGS_is_dllexport;
                
                //
                // Exported functions are potential entry points!
                //
                struct function_node *node = push_struct(context->arena, struct function_node);
                node->function = function;
                add_potential_entry_point(node);
            }
            
            if(specifiers.alignment > 0){
                report_warning(context, WARNING_function_alignment, function->base.token, "'__declspec(align(_))' is ignored for functions.");
            }
            
            if(specifiers.specifier_flags & SPECIFIER_dllimport){
                if(specifiers.specifier_flags & SPECIFIER_static){
                    report_error(context, function->base.token, "Cannot '__declspec(dllimport)' a static function.");
                    goto end;
                }
                
                if(function_is_intrinsic){
                    report_error(context, function->base.token, "Intrinsic function cannot be declared '__declspec(dllimport)'.");
                    goto end;
                }
                
                // We check that this declaration is actually contained in some dll only for the ones 
                // we actually use. This is done in 'explain.c'.
                function->as_decl.flags |= DECLARATION_FLAGS_is_dllimport;
            }
            
            if(specifiers.specifier_flags & SPECIFIER_printlike){
                //
                // Validate that this 'printlike' function is varargs and has a format string as the 
                // last named argument.
                //
                struct ast_function_type *function_type = function->type;
                
                if(!(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs)){
                    report_error(context, function->base.token, "'__declspec(printlike)' function needs to be varargs.");
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
                    report_error(context, function->base.token, "'__declspec(printlike)' function needs to have an argument of type 'char *' as its last named argument.");
                    goto end;
                }
                
                function_type->flags |= FUNCTION_TYPE_FLAGS_is_printlike;
            }
            
            struct ast_scope *scope = null;
            if(peek_token(context, TOKEN_open_curly)){
                if(function_is_intrinsic){
                    report_error(context, get_current_token(context), "Intrinsic function cannot be defined.");
                    goto end;
                }
                
                if(specifiers.specifier_flags & SPECIFIER_dllimport){
                    report_error(context, function->base.token, "Cannot define a function that is declared '__declspec(dllimport)'.");
                    goto end;
                }
                
                if(!sll_is_empty(ret)){
                    // :Error
                    report_error(context, function->base.token, "Cannot define a function in a compound declaration list.");
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
                    work_queue_push_work(context, &globals.work_queue_stage_two, WORK_parse_function_body, parse_work);
                    
                    function->as_decl.flags |= DECLARATION_FLAGS_is_local_persist;
                    function->scope = &scope->base;
                    ast_list_append(&context->local_functions, context->arena, &function->base);
                }
                
                should_break = true; // disallow int a(){}, b;
            }
            
            // @note: After this, 'decl' might not be 'function', 
            //        because there might have been a prior declaration.
            decl = parser_register_declaration(context, &function->as_decl);
            if(context->should_sleep) goto end; // @cleanup: should_sleep?
            
            if(!context->current_scope && scope){
                // This is a global declaration and we define it!
                
                parser_register_definition(context, decl, &scope->base, declarator.type);
                assert(get_current_token(context)->type == TOKEN_open_curly);
                
                if(specifiers.specifier_flags & SPECIFIER_dllexport){
                    struct function_node *function_node = push_struct(context->arena, struct function_node);
                    function_node->function = function;
                    add_potential_entry_point(function_node);
                }
            }
            
        }else{
            
            if(!(specifiers.specifier_flags & SPECIFIER_typedef) && declarator.type == &globals.typedef_void){
                report_error(context, declarator.ident, "Cannot declare a variable of type void.");
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
            if(specifiers.specifier_flags & SPECIFIER_typedef) decl->base.kind = AST_typedef;
            
            if(specifiers.alignment > 0) decl->overwrite_alignment = specifiers.alignment;
            
            if(specifiers.specifier_flags & SPECIFIER_static){ // @note: flag before registering
                if(context->current_scope){
                    ast_list_append(&context->current_function->static_variables, context->arena, &decl->base);
                    decl->flags |= DECLARATION_FLAGS_is_local_persist;
                }else{
                    decl->flags |= DECLARATION_FLAGS_is_static;
                }
            }
            
            if(specifiers.specifier_flags & SPECIFIER_extern)    decl->flags |= DECLARATION_FLAGS_is_extern;
            if(specifiers.specifier_flags & SPECIFIER_selectany) decl->flags |= DECLARATION_FLAGS_is_selectany;
            
            if(specifiers.specifier_flags & SPECIFIER_dllimport) decl->flags |= DECLARATION_FLAGS_is_dllimport;
            if(specifiers.specifier_flags & SPECIFIER_dllexport) decl->flags |= DECLARATION_FLAGS_is_dllexport;
            
            if(!context->current_scope) decl->flags |= DECLARATION_FLAGS_is_global;
            
            decl->compilation_unit = context->current_compilation_unit;
            
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
            decl = parser_register_declaration(context, decl);
            if(context->should_exit_statement) goto end;
            
            if(peek_token(context, TOKEN_equals)){
                struct token *equals = next_token(context);
                
                if(specifiers.specifier_flags & SPECIFIER_typedef){
                    // :Error
                    report_error(context, equals, "Cannot initialize a typedef.");
                    goto end;
                }
                
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
                
                struct ast_identifier *ident = parser_ast_push(context, declarator.ident, identifier);
                ident->decl = decl;
                set_resolved_type(&ident->base, decl->type, decl->defined_type);
                
                struct ast *rhs = parse_initializer(context, ident, equals);
                if(context->should_exit_statement) goto end;
                
                struct ast_type *type = decl->type;
                
                if(type_is_array_of_unknown_size(type)){
                    
                    smm array_length = 0;
                    if(rhs->kind == AST_compound_literal){
                        struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)rhs;
                        struct ast_array_type *array = (struct ast_array_type *)type;
                        if(compound_literal->trailing_array_size){
                            array_length = compound_literal->trailing_array_size/array->element_type->size;
                        }
                        
                        // Reset the trailing array size, to not overallocate later on.
                        compound_literal->trailing_array_size = 0;
                    }else{
                        assert(rhs->kind == AST_assignment);
                        struct ast_binary_op *assignment = (struct ast_binary_op *)rhs;
                        
                        assert(assignment->rhs->resolved_type->kind == AST_array_type);
                        struct ast_array_type *array = (struct ast_array_type *)assignment->rhs->resolved_type;
                        array_length = array->amount_of_elements;
                    }
                    
                    struct ast_array_type *array_of_unknown_size = (struct ast_array_type *)type;
                    
                    // 
                    // Create a new array type, with the newly discovered bounds.
                    struct ast_array_type *array_type = parser_type_push(context, decl->identifier, array_type);
                    array_type->element_type              = array_of_unknown_size->element_type;
                    array_type->element_type_defined_type = array_of_unknown_size->element_type_defined_type;
                    
                    patch_array_size(context, array_type, array_length, equals);
                    
                    type = &array_type->base;
                    
                    // We control the 'ident', so just patch in the new type.
                    set_resolved_type(&ident->base, type, decl->defined_type);
                }
                
                if(!context->current_scope){
                    // If we are at global scope, submit that this is actually a definition.
                    parser_register_definition(context, decl, rhs, type);
                }else{
                    // If we are at local scope, we control this declaration, thus we can just plug in
                    // its expression and type slot.
                    decl->assign_expr = rhs;
                    decl->type = type;
                }
                
                if(context->should_exit_statement) goto end;
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

void maybe_report_warning_for_assignment_in_condition(struct context *context, struct ast *condition){
    if(condition->kind == AST_assignment){
        report_warning(context, WARNING_assignment_in_condition, condition->token, "Assignment withing condition, did you mean '=='?");
    }
}

func struct ast *parse_imperative_scope(struct context *context);

void parse_static_assert(struct context *context, struct token *static_assert_token){
    
    expect_token(context, TOKEN_open_paren, "Expected a '(' after _Static_assert.");
    struct ast *constant_expression = parse_expression(context, /*should_skip_comma_expression*/true);
    
    struct string string_literal = {0};
    
    if(peek_token_eat(context, TOKEN_comma)){
        struct ast *string_literal_expression = parse_expression(context, /*should_skip_comma_expression*/true);
        if(string_literal_expression->kind != AST_string_literal){
            report_error(context, string_literal_expression->token, "Second argument to _Static_assert needs to be a string literal.");
        }else{
            struct ast_string_literal *ast_string_literal = (struct ast_string_literal *)string_literal_expression;
            string_literal = ast_string_literal->value;
        }
    }
    
    if(!context->error){
        if(constant_expression->kind != AST_integer_literal){
            report_error(context, constant_expression->token, "First argument to '_Static_assert' is not constant.");
        }else{
            s64 value = integer_literal_as_s64(constant_expression);
            
            if(value == 0){
                if(string_literal.data && string_literal.size){
                    // @cleanup: wide characters?
                    report_error(context, static_assert_token, "%.*s", string_literal.size, string_literal.data);
                }else{
                    report_error(context, static_assert_token, "_Static_assert fired.");
                }
            }
        }
    }
    
    expect_token(context, TOKEN_closed_paren, "Expected a ')' at the end of _Static_assert.");
}

func struct ast *parse_statement(struct context *context){
    
    struct ast *ret = null;
    if(maybe_report_error_for_stack_exhaustion(context, get_current_token_for_error_report(context), "Scopeing nests to deep.")) return ret;
    
    b32 needs_semicolon = true;
    
    struct token *initial_token = next_token(context); // Returns the current token.
    switch(initial_token->type){
        case TOKEN_semicolon:{
            // The token has to be correct, so we can set the right line number.
            struct ast *empty_statement = _parser_ast_push(context, initial_token, sizeof(struct ast), alignof(struct ast), AST_empty_statement);
            set_resolved_type(empty_statement, &globals.typedef_void, null);
            ret = empty_statement;
            needs_semicolon = false;
        }break;
        case TOKEN_if:{
            struct ast_if *ast_if = parser_ast_push(context, initial_token, if);
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'if'.");
            
            ast_if->condition = parse_expression(context, false);
            maybe_report_warning_for_assignment_in_condition(context, ast_if->condition);
            
            ast_if->condition = maybe_load_address_for_array_or_function(context, ast_if->condition);
            if(!casts_implicitly_to_bool(ast_if->condition)){
                // :Error
                report_error(context, ast_if->condition->token, "'if' condition has to cast to bool.");
                return &ast_if->base;
            }
            expect_token(context, TOKEN_closed_paren, "Expected ')' ending 'if' condition.");
            
            ast_if->statement = parse_statement(context);
            needs_semicolon = false;
            
            if(peek_token_eat(context, TOKEN_else)){
                ast_if->else_statement = parse_statement(context);
            }
            
            set_resolved_type(&ast_if->base, &globals.typedef_void, null);
            ret = &ast_if->base;
        }break;
        case TOKEN_for:{
            // @note: the 'comma seperated lists' you see in for loops are actually just the comma operator
            
            struct ast_for *ast_for = parser_ast_push(context, initial_token, for);
            expect_token(context, TOKEN_open_paren, "Expected '(' after 'for'.");
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            ast_for->scope_for_decl = scope;
            {
                if(!peek_token_eat(context, TOKEN_semicolon)){
                    // @hack: @cleanup: this should be either a declaration or an expression, for now any statement is valid....
                    ast_for->decl = parse_statement(context);
                }
                
                if(peek_token(context, TOKEN_semicolon)){
                    ast_for->condition = ast_push_s32_literal(context, next_token(context), 1); // desugars to true
                }else{
                    ast_for->condition = parse_expression(context, false);
                    maybe_report_warning_for_assignment_in_condition(context, ast_for->condition);
                    
                    ast_for->condition = maybe_load_address_for_array_or_function(context, ast_for->condition);
                    if(!casts_implicitly_to_bool(ast_for->condition)){
                        // :Error
                        report_error(context, ast_for->condition->token, "'for' condition has to cast to bool.");
                        parser_scope_pop(context, scope);
                        return &ast_for->base;
                    }
                    expect_token(context, TOKEN_semicolon, "Expected ';' after 'for'-condition.");
                }
                
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    ast_for->increment = parse_expression(context, false);
                    expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of 'for'.");
                }
                
                ast_for->body = parse_statement(context);
            }
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
            set_resolved_type(&ast_for->base, &globals.typedef_void, null);
            ret = &ast_for->base;
        }break;
        case TOKEN_while:{
            // @note: We desugar 'while' to 'for(;condition;)'.
            struct ast_for *ast_while = parser_ast_push(context, initial_token, for);
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'while'.");
            
            ast_while->condition = parse_expression(context, false);
            maybe_report_warning_for_assignment_in_condition(context, ast_while->condition);
            
            ast_while->condition = maybe_load_address_for_array_or_function(context, ast_while->condition);
            if(!casts_implicitly_to_bool(ast_while->condition)){
                report_error(context, ast_while->condition->token, "'while' condition has to cast to bool.");
                return &ast_while->base;
            }
            expect_token(context, TOKEN_closed_paren, "Expected ')' at the end of 'while'.");
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            ast_while->scope_for_decl = scope;
            ast_while->body = parse_statement(context);
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
            set_resolved_type(&ast_while->base, &globals.typedef_void, null);
            ret = &ast_while->base;
        }break;
        case TOKEN_do:{
            // :AST_do_while
            struct ast_for *do_while = parser_ast_push(context, initial_token, for);
            do_while->base.kind = AST_do_while;
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            do_while->scope_for_decl = scope;
            do_while->body = parse_statement(context);
            parser_scope_pop(context, scope);
            
            expect_token(context, TOKEN_while, "Missing 'while' in do-while statement.");
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'while'.");
            
            do_while->condition = parse_expression(context, false);
            maybe_report_warning_for_assignment_in_condition(context, do_while->condition);
            
            do_while->condition = maybe_load_address_for_array_or_function(context, do_while->condition);            
            if(!casts_implicitly_to_bool(do_while->condition)){
                report_error(context, do_while->condition->token, "'while' condition has to cast to bool.");
                return &do_while->base;
            }
            expect_token(context, TOKEN_closed_paren, "Expected ')' ending 'while'-condition.");
            
            set_resolved_type(&do_while->base, &globals.typedef_void, null);
            ret = &do_while->base;
        }break;
        case TOKEN_break:{
            struct ast_break *ast_break = parser_ast_push(context, initial_token, break);
            
            struct ast_scope *scope = context->current_scope;
            for(; scope; scope = scope->parent){
                if(scope->flags & SCOPE_FLAG_can_break) break;
            }
            
            if(!scope){
                report_syntax_error(context, initial_token, "'break' is not inside of a breakable scope.");
                return &ast_break->base;
            }
            ast_break->scope_to_break = scope;
            
            set_resolved_type(&ast_break->base, &globals.typedef_void, null);
            ret = &ast_break->base;
        }break;
        case TOKEN_continue:{
            struct ast_continue *ast_continue = parser_ast_push(context, initial_token, continue);
            
            struct ast_scope *scope = context->current_scope;
            for(; scope; scope = scope->parent){
                if(scope->flags & SCOPE_FLAG_can_continue) break;
            }
            
            if(!scope){
                report_syntax_error(context, initial_token, "'continue' is not inside of a continue-able scope.");
                return &globals.empty_statement;
            }
            ast_continue->scope_to_continue = scope;
            set_resolved_type(&ast_continue->base, &globals.typedef_void, null);
            ret = &ast_continue->base;
        }break;
        case TOKEN_switch:{
            struct ast_switch *ast_switch = parser_ast_push(context, initial_token, switch);
            expect_token(context, TOKEN_open_paren, "Expected '(' following 'switch'.");
            ast_switch->switch_on = parse_expression(context, false);
            ast_switch->switch_on = maybe_insert_bitfield_cast(context, ast_switch->switch_on);
            
            if(ast_switch->switch_on->resolved_type->kind != AST_integer_type){
                report_error(context, ast_switch->base.token, "Can only switch on integers.");
                return &ast_switch->base;
            }
            
            // "The integer promotions are performed on the controlling expression"
            ast_switch->switch_on = maybe_insert_integer_promotion_cast(context, ast_switch->switch_on);
            
            expect_token(context, TOKEN_closed_paren, "Expected ')' ending 'switch'-condition.");
            
            struct ast_switch *previous_switch = context->current_switch;
            context->current_switch = ast_switch;
            
            struct ast_scope *scope = parser_push_new_scope(context, get_current_token_for_error_report(context), SCOPE_FLAG_can_break);
            ast_switch->statement = parse_statement(context);
            parser_scope_pop(context, scope);
            
            context->current_switch = previous_switch;
            
            set_resolved_type(&ast_switch->base, &globals.typedef_void, null);
            needs_semicolon = false;
            ret = &ast_switch->base;
        }break;
        case TOKEN_case:{
            if(!context->current_switch){
                report_syntax_error(context, initial_token, "'case' is only allowed within 'switch'-statement.");
                return &globals.empty_statement;
            }
            
            struct ast *const_expr = parse_expression(context, false);
            if(const_expr->kind != AST_integer_literal){
                report_error(context, const_expr->token, "Operand of 'case' has to be constant.");
                return const_expr;
            }
            
            struct ast *switch_on = context->current_switch->switch_on;
            struct ast *promoted_const_expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, switch_on->resolved_type, switch_on->defined_type, const_expr, const_expr->token);
            assert(promoted_const_expr->kind == AST_integer_literal);
            assert(promoted_const_expr->resolved_type == context->current_switch->switch_on->resolved_type);
            
            u64 value = integer_literal_as_u64(promoted_const_expr);
            
            for(struct ast_list_node *node = context->current_switch->case_list.first; node; node = node->next){
                struct ast_case *ast_case = (struct ast_case *)node->value;
                if(ast_case->value == value){
                    begin_error_report(context);
                    if(type_is_signed(switch_on->resolved_type)){
                        report_error(context, initial_token, "Case '%lld' already handled.", value);
                    }else{
                        report_error(context, initial_token, "Case '%llu' already.", value);
                    }
                    report_error(context, ast_case->base.token, "... Here is the previous case.");
                    end_error_report(context);
                }
            }
            
            struct ast_case *ast_case = parser_ast_push(context, initial_token, case);
            ast_case->value = value;
            expect_token(context, TOKEN_colon, "Expected ':' after 'case'-label.");
            needs_semicolon = false;
            
            ast_list_append(&context->current_switch->case_list, context->arena, &ast_case->base);
            
            set_resolved_type(&ast_case->base, &globals.typedef_void, null);
            
            if(!peek_token(context, TOKEN_closed_curly)) ast_case->statement = parse_statement(context);
            
            ret = &ast_case->base;
        }break;
        case TOKEN_default:{
            if(!context->current_switch){
                report_syntax_error(context, initial_token, "'default' is only allowed within 'switch'-statement.");
                return &globals.empty_statement;
            }
            
            if(context->current_switch->default_case){
                begin_error_report(context);
                report_syntax_error(context, initial_token, "More than one 'default'-case in switch.");
                report_syntax_error(context, context->current_switch->default_case->base.token, "... Here was the previous 'default'-case.");
                end_error_report(context);
            }
            
            struct ast_case *ast_case = parser_ast_push(context, initial_token, case);
            expect_token(context, TOKEN_colon, "Expected ':' after 'default'.");
            needs_semicolon = false;
            set_resolved_type(&ast_case->base, &globals.typedef_void, null);
            context->current_switch->default_case = ast_case;
            
            if(!peek_token(context, TOKEN_closed_curly))ast_case->statement = parse_statement(context);
            
            ret = &ast_case->base;
        }break;
        case TOKEN_else:{
            report_syntax_error(context, initial_token, "'else' without matching 'if'.");
            return &globals.empty_statement;
        }break;
        case TOKEN_return:{
            struct ast_return *ast_return = parser_ast_push(context, initial_token, return);
            struct ast_function_type *current_function_type = context->current_function->type;
            if(peek_token(context, TOKEN_semicolon)){
                if(current_function_type->return_type != &globals.typedef_void){
                    struct string type_string = push_type_string(context->arena, &context->scratch, current_function_type->return_type);
                    report_error(context, get_current_token(context), "Expected an expression after 'return' in function with return type '%s'.", type_string.data);
                    return &globals.empty_statement;
                }
            }else{
                // @note: I used to check here that a void-function does not return a value.
                //        gcc and clang allow returning a void expression from a void function.
                //        This might be useful for future compatibility I guess...?
                //        Lets support it for now!
                
                ast_return->expr = parse_expression(context, false);
                ast_return->expr = maybe_load_address_for_array_or_function(context, ast_return->expr);
                ast_return->expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, current_function_type->return_type, current_function_type->return_type_defined_type, ast_return->expr, ast_return->base.token);
            }
            
            set_resolved_type(&ast_return->base, &globals.typedef_void, null);
            ret = &ast_return->base;
        }break;
        case TOKEN_open_curly:{
            struct ast_scope *scope = parser_push_new_scope(context, initial_token, SCOPE_FLAG_none);
            ret = cast(struct ast *)parse_imperative_scope(context);
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
        }break;
        // case TOKEN_closed_curly:{
        //      This should now be done by parse_imperative_block, before 'if(1)}' was a valid statement.
        //      We can still get here tho, for the exact reason, but it should be a syntax error.
        // }break;
        case TOKEN_goto:{
            struct ast_goto *ast_goto = parser_ast_push(context, initial_token, goto);
            struct token *ident = expect_token(context, TOKEN_identifier, "Missing identifier after 'goto'.");
            ast_goto->ident = ident->atom;
            ast_list_append(&context->current_function->goto_list, context->arena, &ast_goto->base);
            
            set_resolved_type(&ast_goto->base, &globals.typedef_void, null);
            ret = &ast_goto->base;
        }break;
        case TOKEN_identifier:{
            if(peek_token_eat(context, TOKEN_colon)){
                
                struct atom ident = initial_token->atom;
                needs_semicolon = false;
                
                for_ast_list(context->current_function->label_list){
                    struct ast_label *label = cast(struct ast_label *)it->value;
                    if(atoms_match(label->ident, ident)){
                        begin_error_report(context);
                        report_error(context, initial_token, "Redefinition of label '%.*s'.", ident.amount, ident.data);
                        report_error(context, label->base.token, "... Here is the previous definition.");
                        end_error_report(context);
                        return &label->base;
                    }
                }
                
                struct ast_label *label = parser_ast_push(context, initial_token, label);
                label->ident = ident;
                ast_list_append(&context->current_function->label_list, context->arena, &label->base);
                
                set_resolved_type(&label->base, &globals.typedef_void, null);
                
                if(!peek_token(context, TOKEN_closed_curly)) label->statement = parse_statement(context);
                
                ret = &label->base;
                break;
            }
            
            struct ast_declaration *lookup = lookup_typedef(context, context->current_compilation_unit, initial_token, /*silent*/true);
            if(lookup){
                ret = parse_declaration_list_in_imperative_scope(context, lookup->type, /*defined_type*/&lookup->base);
                break;
            }
            
            prev_token(context);
            ret = parse_expression(context, false);
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
            ret = parse_declaration_list_in_imperative_scope(context, null, null);
            
            // @note: A function declaration (which is not a definition) does need a semicolon.
            if(ret->kind == AST_function) needs_semicolon = false;
        }break;
        
        case TOKEN_asm:{
            struct ast_asm_block *asm_block = parser_ast_push(context, initial_token, asm_block);
            set_resolved_type(&asm_block->base, &globals.typedef_void, null);
            
            expect_token(context, TOKEN_open_curly, "Expected '{' after '__asm__'.");
            
            parse_asm_block(context, asm_block);
            
            needs_semicolon = false;
            ret = &asm_block->base;
        }break;
        
        case TOKEN_static_assert:{
            parse_static_assert(context, initial_token);
            
            struct ast *empty_statement = _parser_ast_push(context, initial_token, sizeof(struct ast), alignof(struct ast), AST_empty_statement);
            set_resolved_type(empty_statement, &globals.typedef_void, null);
            ret = empty_statement;
        }break;
        
        default:{
            // This should not be assert(false): take for example +value; that is a valid statement.
            prev_token(context);
            
            // If it is nothing else; it's gotta be an assignment or expression.
            ret = parse_expression(context, false);
        }break;
        
        case TOKEN_invalid:{
            assert(context->error);
            return null;
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
    
    assert(ret);
    return ret;
}

// @note: We assume the TOKEN_open_curly was already consumed.
func struct ast *parse_imperative_scope(struct context *context){
    
    struct ast_scope *scope = context->current_scope;
    assert(scope);
    
    // Keep parsing inputs even after an error has occurred!
    while(true){
        // Statements only occur at function scope, so there should not be any sleeping.
        assert(!context->should_sleep);
        
        if(peek_token_eat(context, TOKEN_closed_curly) || peek_token(context, TOKEN_invalid)) break;
        
        struct ast *statement = parse_statement(context);
        
        if(statement->kind == AST_assignment){
            // @cleanup: compound assignments?
            struct ast_binary_op *assign = (struct ast_binary_op *)statement;
            struct ast *at = assign->lhs;
            
            int should_break = false;
            while(!should_break){
                switch(at->kind){
                    case AST_identifier:{
                        struct ast_identifier *ident = (struct ast_identifier *)at;
                        ident->decl->times_written++;
                        
                        should_break = true;
                    }break;
                    
                    // case AST_member_deref:
                    case AST_member:{
                        struct ast_dot_or_arrow *dot_or_arrow = (struct ast_dot_or_arrow *)at;
                        
                        at = dot_or_arrow->lhs;
                    }break;
                    default:{
                        should_break = true;
                    }break;
                }
            }
        }
        
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
            ast_list_append(&scope->statement_list, context->arena, statement);
        }
    }
    
    for(u32 table_index = 0; table_index < scope->current_max_amount_of_declarations; table_index++){
        
        struct ast_declaration *decl = scope->declarations[table_index];
        if(!decl) continue;
        
        if(decl->base.kind == AST_declaration){
            if(decl->times_referenced == 0){
                report_warning(context, WARNING_unused_local_variable, decl->base.token, "Local variable is never used.");
            }else if(decl->times_referenced == decl->times_written){
                report_warning(context, WARNING_local_variable_only_ever_written, decl->base.token, "Local variable is never read, only written.");
            }
        }
    }
    
    return cast(struct ast *)scope;
}

func struct declarator_return parse_declarator(struct context* context, struct ast_type *_initial_type, struct ast *_initial_defined_type, enum declarator_kind_flags declarator_kind_flags){
    
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
    
    while(peek_token(context, TOKEN_times)){
        struct token *pointer = next_token(context);
        
        ret.type = parser_push_pointer_type(context, ret.type, ret.defined_type, pointer);
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
        }else if(declarator_kind_flags == DECLARATOR_type_name){
            // We only want an abstract-declarator.
            // We can check, the next token for '*', '(' and '['.
            if(peek_token(context, TOKEN_times) || peek_token(context, TOKEN_open_paren) || peek_token(context, TOKEN_open_index)){
                have_nested_declarator = true;
            }
            
            // @cleanup: Not sure how to handle these...
            if(peek_token(context, TOKEN_cdecl) || peek_token(context, TOKEN_stdcall)){
                have_nested_declarator = true;
            }
        }else{
            assert(declarator_kind_flags == (DECLARATOR_type_name | DECLARATOR_identifier));
            // We want either, this is the difficult case.
            // @cleanup: For now we assume it is a nested declarator.
            have_nested_declarator = true;
        }
        
        if(have_nested_declarator){
            if(!skip_until_tokens_are_balanced(context, open_paren, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '('")){
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
        //    int(int)
        // 
        
        if(!(declarator_kind_flags & DECLARATOR_type_name)){
            // :Error
            report_syntax_error(context, get_current_token_for_error_report(context), "Expected an identifier or '(' in a direct declarator.");
            return ret;
        }
        
        if(!peek_token(context, TOKEN_open_index) && !peek_token(context, TOKEN_open_paren)){
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
            
            struct ast_function_type *function = parser_type_push(context, open_paren, function_type);
            
            
            // @cleanup: This hack is not correct, complilers also handle 'int function(typedef_to_void)' the same.
            if(peek_token_eat(context, TOKEN_void)){
                if(peek_token(context, TOKEN_closed_paren)){
                    // this is fine we will not pass the if below
                }else{
                    // if it is only (void) then we treat it as if there are no arguments
                    prev_token(context);
                }
            }
            
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
                        declarator.type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type, declarator.ident);
                        if(!declarator.defined_type){
                            declarator.defined_type = cast(struct ast *)array;
                        }
                    }else if(declarator.type->kind == AST_function_type){
                        // 
                        // "A declaration of a parameter as 'function returning type' shall be adjusted to
                        //  'pointer to function returning type'."
                        //  
                        declarator.type = parser_push_pointer_type(context, declarator.type, null, declarator.ident);
                    }
                    
                    struct ast_declaration *decl = push_declaration_for_declarator(context, declarator);
                    
                    ast_list_append(&function->argument_list, context->arena, &decl->base);
                    
                    if(decl->assign_expr){ // @cleanup: This can never happen right? we should 'peek_token(context, TOKEN_equals)' if anything.
                        report_error(context, prev_token(context), "Assignment in function declaration. Default arguments are invalid.");
                        return ret;
                    }
                    
                }while(peek_token_eat(context, TOKEN_comma));
                
                expect_token(context, TOKEN_closed_paren, "Expected ')' ending function declarator.");
                if(context->should_exit_statement) return ret;
            }
            
            struct post_type_node *post_type_node = push_struct(&context->scratch, struct post_type_node);
            
            post_type_node->type = &function->base;
            post_type_node->next = post_types;
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
            
            struct ast_array_type *array_type = parser_type_push(context, open_index, array_type);
            
            if(peek_token_eat(context, TOKEN_closed_index)){ // u32 arr[];
                array_size_zero_also_means_array_of_unknown_size:;
                
                array_type->amount_of_elements = 0;
                array_type->is_of_unknown_size = true;
                array_type->base.size = 0;
                array_type->base.flags |= TYPE_FLAG_ends_in_array_of_unknown_size;
            }else{
                struct ast *index_expression = parse_expression(context, false);
                if(index_expression->kind != AST_integer_literal){
                    report_error(context, array_type->base.token, "Array subscript is not constant.");
                    return ret;
                }
                expect_token(context, TOKEN_closed_index, "Expected ']' ending array declarator.");
                
                s64 value = integer_literal_as_s64(index_expression);
                
                if(value == 0){
                    goto array_size_zero_also_means_array_of_unknown_size;
                }
                
                if(value <= 0){
                    report_error(context, array_type->base.token, "Array size must be positive.");
                    return ret;
                }
                
                array_type->amount_of_elements = value;
                
                if(context->should_exit_statement) return ret;
            }
            
            struct post_type_node *post_type_node = push_struct(&context->scratch, struct post_type_node);
            
            post_type_node->type = &array_type->base;
            post_type_node->next = post_types;
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
        
        if(post_type->kind == AST_function_type){
            struct ast_function_type *function_type = (struct ast_function_type *)post_type;
            // 
            // We have to set the return type.
            // 
            
            if(ret.type->kind == AST_function_type){
                // :Error
                report_error(context, function_type->base.token, "Function returning function is illegal. Return a function pointer instead.");
                return ret;
            }else if(ret.type->kind == AST_array_type){
                // :Error
                report_error(context, function_type->base.token, "Function returning array is illegal. You can wrap the array inside of a struct.");
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
                report_error(context, array_type->base.token, "Array of type void is illegal.");
                return ret;
            }
            
            if(ret.type->kind == AST_function_type){
                report_error(context, array_type->base.token, "Array of functions is illegal, please use function pointers.");
                return ret;
            }
            
            if(type_is_array_of_unknown_size(ret.type)){
                // @note: only the first array in the chain can be 'unknown_size'.
                //        Meaning 'char a[][2]' is legal, 'char a[2][]' is not.
                
                // :Error find a better word for 'outer'.
                report_error(context, array_type->base.token, "Only the most outer array can be of unknown size. ('char a[][2]' is legal, 'char a[2][]' is not).");
                return ret;
            }
            
            array_type->element_type = ret.type;
            array_type->element_type_defined_type = ret.defined_type;
            
            if(!array_type->is_of_unknown_size){
                patch_array_size(context, array_type, array_type->amount_of_elements, array_type->base.token);
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

