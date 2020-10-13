

//////////////////////////////////////////////////////////////////////////
//                               Parser                                 //
//////////////////////////////////////////////////////////////////////////

// A simple C-parser, the main entry points are 'parse_declaration_list'
// and 'parse_imperative_scope'. Depending on wheter you are parsing a 
// global declaration or a function body. 
// (Compilation stage one vs stage two).
// The main complications come from the fact that C is an old an weird
// language as well as the fact that everything has to be able to 
// 'parser_sleep', whenever we encounter an identifier we dont know yet
// The parser is in some sense entirely "single pass", i.e. we build
// the abstract tree, type check it and do constant propagation at the
// same time.
// Other then that, its just a 'simple' recursively decent parser. 
//                                                        -11.09.2020


// unique_serial
// unique()
// :unique
func s64 get_unique_ast_serial(struct context *context){
    s64 ret = context->ast_serializer++;
    
#if defined(_Debug)
    //if(ret == 4342){
    //if(ret == 4271){
    if(ret == 4339){
        //os_debug_break();
    }
#endif
    
    return ret;
}

func void _parser_sleep(struct context *context, struct token *sleep_on, u32 line, enum sleep_purpose purpose){
    if(context->should_sleep) return;
    assert(sleep_on);
    
    context->should_sleep = true;
    context->sleep_on = sleep_on;
    context->sleep_purpose = purpose;
    debug_only(context->sleep_line = line;);
}
#define parser_sleep(context, sleep_on, purpose) _parser_sleep(context, sleep_on, __LINE__, purpose)


////////////////////////////////////////////////////////////////////////////////////////////////////////////

func void set_resolved_type(struct ast *ast, struct ast_type *type, struct ast *named_type){
    ast->resolved_type = type;
    ast->defined_type = named_type;
}


func void parser_init_ast(struct context *context, struct ast *ast, enum ast_kind kind, struct token *token){
    ast->kind         = kind;
    ast->s            = get_unique_ast_serial(context);
    ast->token        = token;
    ast->resolved_type = null;
    ast->byte_offset_in_function = -1;
}

#define parser_ast_push(context, type) (struct ast_##type *)_parser_ast_push(context, get_current_token(context),\
sizeof(struct ast_##type), alignof(struct ast_##type), AST_##type)
func void *_parser_ast_push(struct context *context, struct token *token, smm size, u32 align, enum ast_kind kind){
    struct ast *ast = push_struct_(context->arena, size, align);
    memset(ast, 0, size);
    parser_init_ast(context, ast, kind, token);
    return ast;
}

#define parser_type_push(context, type) (struct ast_##type *)_parser_type_push(context, get_current_token(context),\
sizeof(struct ast_##type), alignof(struct ast_##type), AST_##type)
func struct ast_type *_parser_type_push(struct context *context, struct token *token, smm size, u32 align, enum ast_kind kind){
    struct ast_type *type = push_struct_(context->arena, size, align);
    memset(type, 0, size);
    type->kind = kind;
    type->token = token;
    type->s = get_unique_ast_serial(context);
    return type;
}

#define parser_compound_type_push(context, type) (struct ast_compound_type *)_parser_type_push(context, get_current_token(context), sizeof(struct ast_compound_type), alignof(struct ast_compound_type), AST_##type);

func struct ast_type *parser_push_pointer_type(struct context *context, struct ast_type *pointer_to, struct ast *defined_type){
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

// @cleanup: why are these 'ast'_push_bla should they not be 'parser'_push for consistency?
func struct ast *ast_push_binary_expression(struct context *context, enum ast_kind kind, struct token *token,
                                            struct ast *lhs, struct ast *rhs){
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

func struct ast *ast_push_s64_literal(struct context *context, s64 value){
    struct ast_integer_literal *lit = parser_ast_push(context, integer_literal);
    lit->_s64 = value;
    set_resolved_type(&lit->base, &globals.typedef_s64, null);
    return &lit->base;
}

func struct ast *ast_push_s32_literal(struct context *context, s64 value){
    assert(value <= s32_max);
    assert(value >= s32_min);
    struct ast_integer_literal *lit = parser_ast_push(context, integer_literal);
    lit->_s32 = (s32)value;
    set_resolved_type(&lit->base, &globals.typedef_s32, null);
    return &lit->base;
}

func s32 integer_literal_as_s32(struct ast *lit){
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
    assert(type->kind == AST_integer_type);
    s32 value;
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

func u32 integer_literal_as_u32(struct ast *lit){
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
    assert(type->kind == AST_integer_type);
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
    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)lit;
    struct ast_type *type = literal->base.resolved_type;
    assert(type->kind == AST_integer_type);
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
    assert(type->kind == AST_integer_type);
    u64 value;
    if(type_is_signed(type)){
        switch(type->size){
            case 1: value = (u64)literal->_s8; break;
            case 2: value = (u64)literal->_s16; break;
            case 4: value = (u64)literal->_s32; break;
            case 8: value = (u64)literal->_s64; break;
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

func struct ast *defined_type_for_binary_op(struct ast_binary_op *op){
    if(!op->lhs->defined_type) return op->rhs->defined_type;
    if(!op->rhs->defined_type) return op->lhs->defined_type;
    
    struct ast *lhs = op->lhs->defined_type;
    struct ast_type *lhs_type = null;
    
    if(lhs->kind == AST_typedef){
        lhs_type = (cast(struct ast_declaration *)lhs)->type;
    }else{
        lhs_type = cast(struct ast_type *)lhs;
    }
    
    struct ast *rhs = op->rhs->defined_type;
    struct ast_type *rhs_type = null;
    if(rhs->kind == AST_typedef){
        rhs_type = (cast(struct ast_declaration *)rhs)->type;
    }else{
        rhs_type = cast(struct ast_type *)rhs;
    }
    
    if(rhs_type->size > lhs_type->size) return lhs;
    return rhs;
}

func struct ast *defined_type_for_promotion(struct ast *ast){
    // :retain_type_information_through_promotion
    // if it was promoted, we retain the information about the actual size in the defined_type
    struct ast *defined_type = ast->defined_type;
    if(!defined_type) defined_type = (struct ast *)ast->resolved_type;
    return defined_type;
}

///////////////////////////////////////////////////////////////////////////////

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
            struct ast_integer_literal *i = parser_ast_push(context, integer_literal);
            if(type_is_signed(cast_to)){
                switch(cast_to->size){
                    case 1: i->_s8  = (s8) f->value;
                    case 2: i->_s16 = (s16)f->value;
                    case 4: i->_s32 = (s32)f->value;
                    case 8: i->_s64 = (s64)f->value;
                    invalid_default_case();
                }
            }else{
                if(f->value < 0.0){
                    report_error(context, cast_what->token, "Cast from negative a floating point number to an  unsigned type is illegal.");
                    return cast_what;
                }
                switch(cast_to->size){
                    case 1: i->_u8  = (u8) f->value;
                    case 2: i->_u16 = (u16)f->value;
                    case 4: i->_u32 = (u32)f->value;
                    case 8: i->_u64 = (u64)f->value;
                    invalid_default_case();
                }
            }
            
            set_resolved_type(&i->base, cast_to, cast_to_defined_type);
            cast = &i->base;
        }else{
            // :we_always_keep_double
            assert(cast_to->kind == AST_float_type);
            set_resolved_type(cast_what, cast_to, cast_to_defined_type);
            if(cast_to == &globals.typedef_f32){
                struct ast_float_literal *lit = cast(struct ast_float_literal *)cast_what;
                lit->value = (f32)lit->value; // cast it to f32 and then back up
            }
            
            cast = cast_what;
        }
    }else if(cast_what->kind == AST_integer_literal){
        struct ast_integer_literal *lit = cast(struct ast_integer_literal *)cast_what;
        if(cast_to->kind == AST_integer_type){
            //@hmm: I trust my self, that I thought about this...
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
            struct ast_float_literal *f = parser_ast_push(context, float_literal);
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
        }else{
            assert(cast_to->kind == AST_pointer_type); // @cleanup: should we have 'pointer_literal'?
        }
    }
    
    if(!cast){
        // no constant propagation
        cast = ast_push_unary_expression(context, AST_cast, get_current_token(context), cast_what);
        set_resolved_type(cast, cast_to, cast_to_defined_type);
    }
    
    assert(cast->resolved_type == cast_to && cast->defined_type == cast_to_defined_type);
    return cast;
}

///////////////////////////////////////////////////////////////////////////////

func struct ast_type *perform_integer_promotions(struct ast_type *type){
    if(type == &globals.typedef_s8)  return &globals.typedef_s32;
    if(type == &globals.typedef_u8)  return &globals.typedef_s32;
    if(type == &globals.typedef_s16) return &globals.typedef_s32;
    if(type == &globals.typedef_u16) return &globals.typedef_s32;
    
    // "all other types are unchanged"
    return type;
}

func struct ast *maybe_insert_integer_promotion_cast(struct context *context, struct ast *ast){
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

func void maybe_cast_literal_0_to_void_pointer(struct context *context, struct ast_binary_op *op){
    if(op->lhs->resolved_type->kind == AST_pointer_type && op->rhs->kind == AST_integer_literal){
        if(integer_literal_as_u64(op->rhs) == 0){
            op->rhs = push_cast(context, globals.typedef_void_pointer, null, op->rhs);
        }
    }
    
    if(op->rhs->resolved_type->kind == AST_pointer_type && op->lhs->kind == AST_integer_literal){
        if(integer_literal_as_u64(op->lhs) == 0){
            op->lhs = push_cast(context, globals.typedef_void_pointer, null, op->lhs);
        }
    }
}


/////////////////////////////////////////

struct type_info_return{
    struct ast_type *type;
    struct ast *defined_type; // either 'AST_enum' or 'AST_typedef' or 'null'
};

// @cleanup: we dont seem to alter the global structure..., maybe that fine
// :unresolved_types
func struct type_info_return resolve_unresolved_type_or_sleep(struct context *context, struct ast_type *type){
    struct type_info_return ret = zero_struct;
    
    assert(type->kind == AST_unresolved_type);
    struct ast_unresolved_type *unresolved = cast(struct ast_unresolved_type *)type;
    struct token *type_name = unresolved->sleeping_on;
    assert(type_name);
    
    b32 found_something = false;
    
    if(unresolved->unresolved_sleep_purpose == SLEEP_on_decl){
        struct ast_declaration *decl = lookup_typedef(context, type_name, false);
        if(decl){
            ret.type = decl->type;
            ret.defined_type = &decl->base;
            found_something = true;
        }
    }else{
        assert(unresolved->unresolved_sleep_purpose == SLEEP_on_struct);
        struct ast_type *lookup = lookup_compound_type(context, type_name->value);
        
        if(lookup){
            if(lookup->kind == AST_enum){
                ret.type = &globals.typedef_s32;
                ret.defined_type = (struct ast *)lookup;
            }else{
                ret.type = lookup;
            }
            found_something = true;
        }
    }
    
    if(!found_something){
        parser_sleep(context, type_name, unresolved->unresolved_sleep_purpose);
    }
    
    return ret;
}

// @note: these functions path the types
// :unresolved_types
func b32 maybe_resolve_pointer_to_unresolved_type_or_sleep(struct context *context, struct ast_pointer_type *pointer){
    if(pointer->pointer_to->kind == AST_unresolved_type){
        struct type_info_return resolved = resolve_unresolved_type_or_sleep(context, pointer->pointer_to);
        if(resolved.type){
            
            // @cleanup: threading: do we have to write these two as one?
            pointer->pointer_to = resolved.type;
            pointer->pointer_to_defined_type = resolved.defined_type;
        }else{
            return true; // sleep
        }
    }
    return false;
}

// :unresolved_types
func b32 maybe_resolve_declaration_of_unresolved_type_or_sleep(struct context *context, struct ast_declaration *decl){
    if(decl->type->kind == AST_unresolved_type){
        struct type_info_return resolved = resolve_unresolved_type_or_sleep(context, decl->type);
        if(resolved.type){
            decl->type = resolved.type;
            decl->defined_type = resolved.defined_type;
        }else{
            return true;
        }
    }
    return false;
}

func struct ast_type *types_are_equal(struct context *context, struct ast_type *wanted, struct ast_type *given){
    if(wanted == given) return wanted;
    struct ast_type *ret = wanted;
    
    if(given->kind == AST_pointer_type && wanted->kind == AST_pointer_type){
        struct ast_pointer_type *wanted_pointer = (struct ast_pointer_type *)wanted;
        if(wanted_pointer->pointer_to == &globals.typedef_void){
            return given;
        }
        struct ast_pointer_type *given_pointer = (struct ast_pointer_type *)given;
        if(given_pointer->pointer_to == &globals.typedef_void){
            return wanted;
        }
    }
    
    if(wanted->kind == AST_pointer_type && given->kind == AST_pointer_type){
        struct ast_pointer_type *wanted_pointer = cast(struct ast_pointer_type *)wanted;
        struct ast_pointer_type *given_pointer  = cast(struct ast_pointer_type *)given;
        
        while(wanted_pointer->pointer_to->kind == AST_pointer_type && given_pointer->pointer_to->kind == AST_pointer_type){
            wanted_pointer = cast(struct ast_pointer_type *)wanted_pointer->pointer_to;
            given_pointer  = cast(struct ast_pointer_type *)given_pointer->pointer_to;
        }
        
        // if they are both unresolved types, but the types are the same, they match
        if(wanted_pointer->pointer_to->kind == AST_unresolved_type && given_pointer->pointer_to->kind == AST_unresolved_type){
            struct ast_unresolved_type *wanted_unresolved = cast(struct ast_unresolved_type *)wanted_pointer->pointer_to;
            struct ast_unresolved_type *given_unresolved = cast(struct ast_unresolved_type *)given_pointer->pointer_to;
            if(wanted_unresolved->sleeping_on->value == given_unresolved->sleeping_on->value &&
               wanted_unresolved->unresolved_sleep_purpose == given_unresolved->unresolved_sleep_purpose){
                return wanted;
            }
        }
        
        // @note: these patch the pointer, if they find an unresolved type
        if(maybe_resolve_pointer_to_unresolved_type_or_sleep(context, wanted_pointer)) return null;
        if(maybe_resolve_pointer_to_unresolved_type_or_sleep(context, given_pointer))  return null;
        
        wanted = wanted_pointer->pointer_to;
        given = given_pointer->pointer_to;
    }
    
    assert(wanted && given);
    
    if(wanted->kind != given->kind) return null;
    
    if(wanted->kind == AST_function_type){
        struct ast_function_type *wanted_function = cast(struct ast_function_type *)wanted;
        struct ast_function_type *given_function  = cast(struct ast_function_type *)given;
        
        if(wanted_function->argument_list.count != given_function->argument_list.count) return null;
        
        if(!types_are_equal(context, wanted_function->return_type, given_function->return_type)){
            return null;
        }
        
        struct ast_list_node *given_it = given_function->argument_list.first;
        for_ast_list(wanted_function->argument_list){
            struct ast_declaration *wanted_decl = cast(struct ast_declaration *)it->value;
            struct ast_declaration *given_decl = cast(struct ast_declaration *)given_it->value;
            
            // @note: these patch the pointer, if they find an unresolved type
            if(maybe_resolve_declaration_of_unresolved_type_or_sleep(context, wanted_decl)) return null;
            if(maybe_resolve_declaration_of_unresolved_type_or_sleep(context, given_decl)) return null;
            
            assert(given_decl->base.kind == AST_declaration);
            
            if(!types_are_equal(context, wanted_decl->type, given_decl->type)) return null;
            given_it = given_it->next;
        }
        assert(given_it == null);
        return ret;
    }
    
    if(wanted->kind == AST_array_type){
        struct ast_array_type *wanted_array = cast(struct ast_array_type *)wanted;
        struct ast_array_type *given_array  = cast(struct ast_array_type *)given;
        
        if(wanted_array->amount_of_elements != given_array->amount_of_elements) return null;
        
        if(!types_are_equal(context, wanted_array->element_type, given_array->element_type)){
            return null;
        }
        return ret;
    }
    
    if(wanted->kind == AST_struct || wanted->kind == AST_union){
        struct ast_compound_type *wanted_compound = cast(struct ast_compound_type *)wanted;
        struct ast_compound_type *given_compound  = cast(struct ast_compound_type *)given;
        if(wanted_compound->identifier != given_compound->identifier) return null;
        if(wanted_compound->declarations.count != given_compound->declarations.count) return null;
        
        struct ast_list_node *given_it = given_compound->declarations.first;
        for_ast_list(wanted_compound->declarations){
            struct ast_declaration *wanted_decl = cast(struct ast_declaration *)it->value;
            struct ast_declaration *given_decl = cast(struct ast_declaration *)given_it->value;
            
            if(wanted_decl->identifier != given_decl->identifier) return null;
            if(!types_are_equal(context, wanted_decl->type, given_decl->type)) return null;
            
            given_it = given_it->next;
        }
        assert(given_it == null);
        return ret;
    }
    
    if(wanted->kind == AST_enum){
        struct ast_compound_type *wanted_enum = cast(struct ast_compound_type *)wanted;
        struct ast_compound_type *given_enum  = cast(struct ast_compound_type *)given;
        if(wanted_enum->identifier != given_enum->identifier) return null;
        if(given_enum->declarations.count != wanted_enum->declarations.count) return null;
        
        struct ast_list_node *given_it = given_enum->declarations.first;
        for_ast_list(wanted_enum->declarations){
            struct ast_declaration *wanted_decl = cast(struct ast_declaration *)it->value;
            struct ast_declaration *given_decl = cast(struct ast_declaration *)given_it->value;
            
            if(wanted_decl->identifier != given_decl->identifier) return null;
            struct ast_integer_literal *wanted_lit = cast(struct ast_integer_literal *)wanted_decl->assign_expr;
            struct ast_integer_literal *given_lit = cast(struct ast_integer_literal *)given_decl->assign_expr;
            
            // @note: enums are of type int, so we only have to check int here
            if(wanted_lit->_s32 != given_lit->_s32) return null;
            
            given_it = given_it->next;
        }
        assert(given_it == null);
        return ret;
    }
    
    // they match iff they are equal in the end. (this is the case for example for int * == int *)
    return (wanted == given) ? ret : null;
}

func struct string report_type_missmatch__internal(struct context *context, char *prefix, struct ast_type *type, struct ast *defined_type){
    struct string ret = zero_struct;
    
    b32 handled = false;
    if(defined_type){
        if(defined_type->kind == AST_typedef){
            struct ast_declaration *decl = cast(struct ast_declaration *)defined_type;
            struct string lhs_string = push_type_string(context->arena, &context->scratch, decl->type);
            ret = push_format_string(&context->scratch, "%s '%.*s' (aka %.*s)", prefix, decl->identifier->amount, decl->identifier->data, lhs_string.amount, lhs_string.data);
            handled = true;
        }else if(defined_type->kind == AST_enum){
            struct string lhs_string = push_type_string(&context->scratch, context->arena, cast(struct ast_type *)defined_type);
            ret = push_format_string(&context->scratch, "%s '%.*s'", prefix, lhs_string.amount, lhs_string.data);
            handled = true;
        }
    }
    
    if(!handled){
        struct string lhs_string = push_type_string(&context->scratch, context->arena, type);
        ret = push_format_string(&context->scratch, "%s '%.*s'", prefix, lhs_string.amount, lhs_string.data);
    }
    return ret;
}

func struct ast_type *report_type_missmatch_error(struct context *context, struct ast_type *lhs, struct ast *lhs_defined_type, struct ast_type *rhs, struct ast *rhs_defined_type, struct token *location){
    struct string lhs_string = report_type_missmatch__internal(context, "wanted", lhs, lhs_defined_type);
    struct string rhs_string = report_type_missmatch__internal(context, "given", rhs, rhs_defined_type);
    
    report_error(context, location, "%.*s %.*s.", lhs_string.amount, lhs_string.data, rhs_string.amount, rhs_string.data);
    
    return &globals.typedef_void;
}


func struct ast_type *report_type_missmatch_warning(struct context *context, struct ast_type *lhs, struct ast *lhs_defined_type, struct ast_type *rhs, struct ast *rhs_defined_type, struct token *location){
    struct string lhs_string = report_type_missmatch__internal(context, "wanted", lhs, lhs_defined_type);
    struct string rhs_string = report_type_missmatch__internal(context, "given", rhs, rhs_defined_type);
    
    report_warning(context, location, "%.*s %.*s.", lhs_string.amount, lhs_string.data, rhs_string.amount, rhs_string.data);
    
    return &globals.typedef_void;
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
    
    return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////

#define DECLARATOR_is_cdecl    0x1
#define DECLARATOR_is_stdcall  0x2
#define DECLARATOR_is_const    0x4
#define DECLARATOR_is_volatile 0x8

struct declarator_return{
    struct token *ident;
    struct ast_type *type;
    struct ast *defined_type;
    u32 declarator_flags;
};

func struct ast_declaration *push_declaration_for_declarator(struct context *context, struct declarator_return declarator){
    struct ast_declaration *decl = parser_ast_push(context, declaration);
    decl->type = declarator.type;
    decl->defined_type = declarator.defined_type;
    decl->base.token = declarator.ident;
    decl->offset_on_stack = -1;
    set_resolved_type(&decl->base, &globals.typedef_void, null);
    
    if(context->should_sleep) return decl; // we deref declarator.ident... not sure
    decl->identifier = declarator.ident->value;
    
    return decl;
}


enum abstract_flags{
    ABSTRACT_no_identifier    = 0x1,
    ABSTRACT_allow_identifier = 0x2,
};

// :forward_declarations
func struct ast_type *parse_type_specifier(struct context *context, struct ast **defined_type);
func struct ast *parse_initializer(struct context *context, struct ast *ast, struct token *equals);
func struct ast *parse_expression(struct context *context, b32 should_skip_comma_expression);
func struct declaration_list parse_declaration_list(struct context *context, struct ast_type *optional_type, struct ast *optional_defined_type);
func struct declarator_return parse_declarator(struct context *context, struct ast_type *initial_type, struct ast *initial_defined_type, enum abstract_flags abstract_flags);

func struct ast *parse_declaration_in_imperative_scope(struct context *context, struct ast_type *optional_type, struct ast *optional_defined_type){
    assert(context->current_scope);
    
    struct declaration_list list = parse_declaration_list(context, optional_type, optional_defined_type);
    if(context->should_sleep) return &context->empty_statement;
    
    // set 'decl->offset_on_stack' for all local declarations
    for(struct declaration_node *node = list.first; node; node = node->next){
        struct ast_declaration *decl = node->decl;
        if(!(decl->flags & DECLARATION_FLAGS_is_local_persist)){
            decl->offset_on_stack = parser_emit_memory_location(context, decl->type->size, decl->type->alignment, decl->base.token);
        }
        
    }
    
    if(list.first && list.first == list.last){
        // we have one element, return it as is, no need for a list
        return &list.first->decl->base;
    }
    
    // push 'AST_declaration_list' for this 'declaration_list'
    struct ast_declaration_list *decl_list = parser_ast_push(context, declaration_list);
    decl_list->list = list;
    
    set_resolved_type(&decl_list->base, &globals.typedef_void, null);
    return &decl_list->base;
}


func struct ast_type *push_unresolved_type(struct context *context, struct token *sleep_on, enum sleep_purpose sleep_purpose, char *type_prefix){
    
    // :unresolved_types
    // @cleanup: write something here about what unresolved_types are, how they arise, how we deal with them and
    //           some of the special cases
    
    struct ast_unresolved_type *unresolved = parser_type_push(context, unresolved_type);
    unresolved->unresolved_sleep_purpose = sleep_purpose;
    unresolved->sleeping_on = sleep_on;
    unresolved->base.size = -1; // hopefully we fuckup the system enough so that it asserts if we ever accedentally use this
    unresolved->type_prefix = type_prefix;
    return &unresolved->base;
}

////////////////
// expessions //
////////////////

enum CHECK_flag{
    CHECK_none    = 0,
    CHECK_integer = 0x1,
    CHECK_float   = 0x2,
    CHECK_pointer = 0x4,
    //CHECK_array   = 0x8,
    
    // @cleanup: CHECK_scalar, CHECK_arithmetic and so on
    
    CHECK_basic   = CHECK_integer | CHECK_float | CHECK_pointer,
};

func b32 check_for_basic_types__internal(struct context *context, struct ast *ast, u32 flags, char *prefix){
    b32 okay = false;
    if((flags & CHECK_integer) && (ast->resolved_type->kind == AST_integer_type)) okay = true;
    if((flags & CHECK_pointer) && (ast->resolved_type->kind == AST_pointer_type)) okay = true;
    if((flags & CHECK_pointer) && (ast->resolved_type->kind == AST_array_type))   okay = true;
    if((flags & CHECK_float)   && (ast->resolved_type->kind == AST_float_type))   okay = true;
    
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
        
        struct string s = token_to_string(context, *ast->token);
        report_error(context, ast->token, "%s '%.*s' has to be of %s type.", prefix, s.amount, s.data, msg);
        return false;
    }
    return true;
}

// @hmm name..., this does not take the unary but the operand... this is differant from check_binary
func b32 check_unary_for_basic_types(struct context *context, struct ast *ast, u32 flags){
    return check_for_basic_types__internal(context, ast, flags, "Operand of unary");
}


func b32 check_binary_for_basic_types(struct context *context, struct ast_binary_op *op, u32 flags){
    b32 lhs = check_for_basic_types__internal(context, op->lhs, flags, "Lhs of");
    b32 rhs = check_for_basic_types__internal(context, op->rhs, flags, "Rhs of");
    return (lhs && rhs);
}


#if defined(__clang__)
#define __readeflags __builtin_ia32_readeflags_u64
#endif

func struct ast_declaration *find_declaration_in_compound(struct ast_compound_type *compound, unique_string ident){
    struct ast_declaration *found = null;
    for_ast_list(compound->declarations){
        assert(it->value->kind == AST_declaration);
        struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
        if(decl->identifier == ident){
            found = decl;
            break;
        }
    }
    return found;
}

func struct ast *handle_dot_or_arrow__internal(struct context *context, struct ast_declaration *found, struct ast *operand, enum ast_kind ast_kind, struct token *test){
    
    struct ast_dot_or_arrow *op = push_struct(context->arena, struct ast_dot_or_arrow);
    op->lhs = operand;
    op->member_decl = found;
    parser_init_ast(context, &op->base, ast_kind, test);
    
    // :PointerInStruct :unresolved_types
    // if there is an unresolved pointer in a struct i.e pointer->pointer_to == null
    // then the type name is the token of pointer and we try to patch the pointer or go to sleep
    
    // Update: we now have a sleep purpose, which means this storing the name in the token is not enough
    //         so we store the purpose as well... we could hack this even worse, by putting the hash in the name
    //         but im above that right now  -6.11.19
    
    // Update: This turned out to be really volatile, so now there is AST_unresolved_type at the end of the
    //         pointer chain. -07.03.2020
    
    // Update: I realized that in a lot of cases we can/have to ignore unresolved types and only try to look
    //         them up / sleep if we have to actually know them. In the case of 'a->b' we have to know the
    //         we know 'a' has a resolved type, we have to check 'b', but we don't need to go the pointer 
    //         chain all the way down. This means that this function becomes a lot more trivial. 
    //         The work is essentially done in the call to'maybe_resolve_pointer_to_unresolved_type_or_sleep'
    //         when parsing '->'    -06.09.2020
    
    
    set_resolved_type(&op->base, found->type, found->defined_type);
    return &op->base;
}

func struct ast *handle_dot_or_arrow(struct context *context, struct ast_compound_type *compound, struct ast *operand, enum ast_kind ast_kind, struct token *test){
    struct token *member = expect_token(context, TOKEN_identifier);
    struct ast_declaration *found = find_declaration_in_compound(compound, member->value);
    if(!found){
        // :Error
        report_error(context, member, "Identifier is not a member of struct.");
        return operand;
    }
    return handle_dot_or_arrow__internal(context, found, operand, ast_kind, test);
}

func b32 ast_stack_push(struct context *context, struct ast *ast){
    if(context->ast_stack_at + 1 > array_count(context->ast_stack)){
        report_error(context, ast->token, "expression nests to deep.");
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
    return &context->empty_statement;
}

static struct type_info_return maybe_parse_type_for_cast_or_sizeof(struct context *context){
    
    struct token *token = get_current_token(context);
    struct type_info_return ret = zero_struct;
    switch(token->type){
        case TOKEN_volatile: case TOKEN_const:
        case TOKEN_void:
        case TOKEN_signed: case TOKEN_unsigned:

        case TOKEN_Bool:
        case TOKEN_char:
        case TOKEN_short:
        case TOKEN_int:
        case TOKEN_long:
        
        case TOKEN_int8:
        case TOKEN_int16:
        case TOKEN_int32:
        case TOKEN_int64:
        
        case TOKEN_float:
        case TOKEN_double:
        case TOKEN_struct:
        
        case TOKEN_union:
        case TOKEN_enum:{
            ret.type = parse_type_specifier(context, &ret.defined_type);
        }break;
        case TOKEN_identifier:{
            struct ast_declaration *decl = lookup_typedef(context, token, true);
            if(decl){
                ret.type = decl->type;
                ret.defined_type = &decl->base;
                next_token(context);
            }else{
                return ret;
            }
        }break;
        default:{
            return ret;
        }break;
    }
    
    struct declarator_return declarator = parse_declarator(context, ret.type, ret.defined_type, ABSTRACT_no_identifier);
    
    if(declarator.type->kind == AST_unresolved_type){
        return resolve_unresolved_type_or_sleep(context, declarator.type);
    }
    
    ret.type = declarator.type;
    ret.defined_type = declarator.defined_type;
    
    return ret;
}

func struct ast *maybe_load_address_for_array_or_function(struct context *context, struct ast *ast){
    if(ast->resolved_type->kind == AST_array_type){
        struct ast_array_type *array = cast(struct ast_array_type *)ast->resolved_type;
        ast = ast_push_unary_expression(context, AST_unary_address, ast->token, ast);
        set_resolved_type(ast, parser_push_pointer_type(context, array->element_type, array->element_type_defined_type), null);
    }else if(ast->resolved_type->kind == AST_function_type){
        struct ast *address = ast_push_unary_expression(context, AST_unary_address, ast->token, ast);
        set_resolved_type(address, parser_push_pointer_type(context, ast->resolved_type, null), null);
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
    
    assert(pointer->resolved_type->kind == AST_pointer_type);
    struct ast_pointer_type *pointer_type = (struct ast_pointer_type *)pointer->resolved_type;
    
    
    if(maybe_resolve_pointer_to_unresolved_type_or_sleep(context, pointer_type)) return pointer;
    struct ast_type *deref_type = pointer_type->pointer_to;
    assert(deref_type->size > 0);
    
    struct ast *ret;
    if(integer->kind == AST_integer_literal){
        struct ast_integer_literal *lit = cast(struct ast_integer_literal *)integer;
        // @cleanup: upconvert lit here to be s64
        
        // @cleanup: how to handle this with respect to overflow? and signdness
        if(lit->_s64 != 0){
            lit->_s64 *= deref_type->size;
            set_resolved_type(&lit->base, &globals.typedef_u64, null);
            op->rhs = &lit->base;
            ret = &op->base;
        }else{
            ret = pointer;
        }
    }else{
        assert(integer->resolved_type->kind == AST_integer_type);
        if(integer->resolved_type != &globals.typedef_s64){
            integer = push_cast(context, &globals.typedef_s64, null, integer);
        }
        
        if(deref_type->size != 1){
            struct ast *size_lit = ast_push_s64_literal(context, deref_type->size);
            struct ast *new_index = ast_push_binary_expression(context, AST_binary_times, token, integer, size_lit);
            // @cleanup: s64?
            set_resolved_type(new_index, &globals.typedef_u64, null);
            integer = new_index;
        }
        
        op->lhs = pointer;
        op->rhs = integer;
        ret = &op->base;
    }
    set_resolved_type(ret, pointer->resolved_type, pointer->defined_type);
    return ret;
}

func struct ast *push_nodes_for_subscript(struct context *context, struct ast *lhs, struct ast *index, struct token *token){
    // for an array: '*(&array + sizeof(deref_type) * (s64)index)'
    // for a pointer: '*(pointer + sizeof(deref_type) * (s64)index)'
    
    lhs = maybe_load_address_for_array_or_function(context, lhs);
    struct ast *binary_op = ast_push_binary_expression(context, AST_binary_plus, token, lhs, index);
    struct ast *to_deref = handle_pointer_arithmetic(context, binary_op);
    
    assert(to_deref->resolved_type->kind == AST_pointer_type);
    struct ast *ret = ast_push_unary_expression(context, AST_unary_deref, token, to_deref);
    struct ast_pointer_type *pointer = ((struct ast_pointer_type *)to_deref->resolved_type);
    
    assert(pointer->pointer_to->kind != AST_unresolved_type);
    set_resolved_type(ret, pointer->pointer_to, pointer->pointer_to_defined_type);
    
    return ret;
}

// @cleanup: oof name
func void parse_initializer_and_push_it_to_an_as_list(struct context *context, struct ast_struct_or_array_literal *ret, struct ast *current_ast, struct token *token){
    
    struct ast *initializer = parse_initializer(context, current_ast, token);
    if(context->should_sleep) return;
    if(initializer->kind == AST_struct_or_array_literal){
        struct ast_struct_or_array_literal *lit = cast(struct ast_struct_or_array_literal *)initializer;
        sll_push_back_list(ret->assignment_list, lit->assignment_list);
        ret->assignment_list.count += lit->assignment_list.count;
    }else{
        assert(initializer->kind == AST_assignment);
        ast_list_append(&ret->assignment_list, context->arena, initializer);
    }
}

func struct ast *parse_struct_or_array_literal(struct context *context, struct ast *ast){
    // @note: array and struct litterals are handled the same
    struct ast_struct_or_array_literal *ret = parser_ast_push(context, struct_or_array_literal);
    set_resolved_type(&ret->base, ast->resolved_type, ast->defined_type);
    expect_token(context, TOKEN_open_curly);
    
    // :struct_literal_location
    // this is super @ugly: we need the declaration, to clear the memory prior to assigning to it.
    // If we recurse in this function we do not need to clear the memory and then this comparison
    // will not be true.
    if(ast->kind == AST_identifier){
        struct ast_identifier *ident = cast(struct ast_identifier *)ast;
        ret->decl = ident->decl;
    }
    
    
    smm array_at = 0;
    struct ast_list_node *member_at = null;
    if(ast->resolved_type->kind == AST_struct || ast->resolved_type->kind == AST_union){
        struct ast_compound_type *ast_struct = cast(struct ast_compound_type *)ast->resolved_type;
        member_at = ast_struct->declarations.first;
    }else{
        assert(ast->resolved_type->kind == AST_array_type);
    }
    
    b32 has_had_dot = false;
    
    do{
        if(peek_token(context, TOKEN_closed_curly)) break;
        struct ast_type *current_type = ast->resolved_type;
        struct ast *current_ast = ast;
        
        b32 in_lhs_expr = false;
        while(!context->should_sleep && in_current_token_array(context)){
            if(peek_token(context, TOKEN_dot)){
                if(current_type->kind != AST_struct && current_type->kind != AST_union){
                    struct string string = push_type_string(context->arena, &context->scratch, current_type);
                    report_error(context, get_current_token(context), "assigned type '%.*s' is not of struct type.", string.amount, string.data);
                    return &ret->base;
                }
                
                struct ast_compound_type *compound = cast(struct ast_compound_type *)current_type;
                current_ast = handle_dot_or_arrow(context, compound, current_ast, AST_member, next_token(context));
                current_type = current_ast->resolved_type;
                in_lhs_expr = true;
                has_had_dot = true;
                
            }else if(peek_token(context, TOKEN_open_index)){
                if(current_type->kind != AST_array_type){
                    struct string type_string = push_type_string(context->arena, &context->scratch, current_type);
                    report_error(context, get_current_token(context), "assigned type '%.*s' is not of array type.", type_string.amount, type_string.data);
                    return &ret->base;
                }
                struct ast_array_type *array = cast(struct ast_array_type *)current_type;
                
                struct token *open_index = next_token(context);
                
                struct ast *index = parse_expression(context, false);
                if(index->resolved_type->kind != AST_integer_type){
                    report_error(context, index->token, "array subscript must be of integer type.");
                    return &ret->base;
                }
                expect_token(context, TOKEN_closed_index);
                
                current_ast = push_nodes_for_subscript(context, current_ast, index, open_index);
                current_type = array->element_type;
                
                in_lhs_expr = true;
                has_had_dot = true;
            }else if(peek_token(context, TOKEN_equals)){
                if(!in_lhs_expr){
                    // @cleanup: this seems unreachable...
                    // :Error
                    report_error(context, get_current_token(context), "unexpected.");
                    return &ret->base;
                }
                parse_initializer_and_push_it_to_an_as_list(context, ret, current_ast, next_token(context));
                break;
            }else{
                if(in_lhs_expr || has_had_dot){
                    // :Error
                    report_error(context, get_current_token(context), "unexpected.");
                    return &ret->base;
                }
                
                if(current_type->kind == AST_struct ||current_type->kind == AST_union){
                    if(!member_at){
                        report_error(context, get_current_token(context), "to many initializers.");
                        return &ret->base;
                    }
                    
                    struct ast_declaration *decl = cast(struct ast_declaration*)member_at->value;
                    
                    struct ast *lhs = handle_dot_or_arrow__internal(context, decl, ast, AST_member, get_current_token(context));
                    
                    parse_initializer_and_push_it_to_an_as_list(context, ret, lhs, get_current_token(context));
                    
                    member_at = member_at->next;
                    break;
                }else if(current_type->kind == AST_array_type){
                    struct ast_array_type *array_type = cast(struct ast_array_type *)current_type;
                    
                    if(array_type->is_of_unknown_size){
                        array_type->amount_of_elements = max_of(array_at + 1, array_type->amount_of_elements);
                    }
                    
                    if(array_at >= array_type->amount_of_elements){
                        report_error(context, get_current_token(context), "to many initilizers for array.");
                        return &ret->base;
                    }
                    struct ast *index = ast_push_s64_literal(context, array_at);
                    struct ast *subscript = push_nodes_for_subscript(context, current_ast, index, get_current_token(context));
                    
                    parse_initializer_and_push_it_to_an_as_list(context, ret, subscript, get_current_token(context));
                    array_at++;
                    break;
                }
                not_implemented;
            }
        }
    }while(peek_token_eat(context, TOKEN_comma));
    
    expect_token(context, TOKEN_closed_curly);
    
    return &ret->base;
}

func struct ast *maybe_insert_implicit_assignment_cast_and_check_that_types_match(struct context *context, struct ast_type *lhs_type, struct ast *lhs_defined_type, struct ast *rhs, struct token *assignment){
    if(lhs_type == rhs->resolved_type) return rhs;
    
    struct ast_type *rhs_type = rhs->resolved_type;
    
    b32 should_report_warning  = false;
    b32 should_push_cast = false;
    
    if(lhs_type->kind == AST_pointer_type && rhs_type->kind == AST_integer_type){
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
        
        while(wanted_pointer->pointer_to->kind == AST_pointer_type && given_pointer->pointer_to->kind == AST_pointer_type){
            wanted_pointer = cast(struct ast_pointer_type *)wanted_pointer->pointer_to;
            given_pointer  = cast(struct ast_pointer_type *)given_pointer->pointer_to;
        }
        
        // @note: we put in the pointers here to not have to handle 'void'
        if(!types_are_equal(context, &wanted_pointer->base, &given_pointer->base)){
            should_report_warning  = true;
            should_push_cast = true;
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
    }else if(rhs_type->kind == AST_integer_type && rhs_type->kind == AST_integer_type){
        should_push_cast = true;
        
        if(rhs->kind == AST_integer_literal){
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
                report_warning(context, assignment, "Compile time truncation of integer.");
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
                    // either size missmatch or signdedness missmatch, as the types are unequal
                    should_report_warning  = true;    
                }
            }
#endif
        }
    }
    
    if(should_report_warning){
        report_type_missmatch_warning(context, lhs_type, lhs_defined_type, rhs->resolved_type, rhs->defined_type, assignment);
    }
    
    if(should_push_cast){
        // if both sides are either arithmetic or pointer_types, we push a cast.
        rhs = push_cast(context, lhs_type, lhs_defined_type, rhs);
    }else{
        if(!types_are_equal(context, lhs_type, rhs->resolved_type)){
            report_type_missmatch_error(context, lhs_type, lhs_defined_type, rhs->resolved_type, rhs->defined_type, assignment);
        }
    }
    
    return rhs;
}


// ast is the lhs of the assignment, we need it to build assignments
// @cleanup: rename ast and factor out the common code
func struct ast *parse_initializer(struct context *context, struct ast *ast, struct token *equals){
    struct ast_type *type = ast->resolved_type;
    
    // @note: I did not want the think about the array patching after an error, so we early out
    struct ast *ret;
    if(peek_token(context, TOKEN_open_curly) && (type->kind == AST_struct || type->kind == AST_array_type || type->kind == AST_union)){
        ret = parse_struct_or_array_literal(context, ast);
        if(context->should_sleep) return ret;
        
        // if type is an array of unknown size, patch in the size
        if(type->kind == AST_array_type && ret->resolved_type->kind == AST_array_type){
            struct ast_array_type *lhs_type = cast(struct ast_array_type *)type;
            struct ast_array_type *rhs_type = cast(struct ast_array_type *)ret->resolved_type;
            if(lhs_type->is_of_unknown_size){
                lhs_type->is_of_unknown_size = false;
                lhs_type->amount_of_elements = rhs_type->amount_of_elements;
                lhs_type->base.size = lhs_type->amount_of_elements * lhs_type->element_type->size;
            }
        }
        
#if 0
        ret = ast_push_binary_expression(parser, AST_assignment, equals, ast, ret);
        set_resolved_type(ret, ast->resolved_type);
#endif
        
    }else{
        // @note: this is either the assignment for 'u32 a = 123;' or the implicit assignment for
        //        a struct literal like (int){123}, or 'u32 a = {123};', i guess
        struct token *got_curly = peek_token_eat(context, TOKEN_open_curly);
        struct ast *expr = null;
        if(got_curly && peek_token(context, TOKEN_closed_curly)){
            expr = ast_push_s32_literal(context, 0);
        }
        if(!expr) expr = parse_expression(context, true);
        if(context->should_sleep) return expr;
        
        
        // if type is an array of unknown size, patch in the size
        // we get in this for example for 'char asd[] = "asd";' or struct literals
        if(type->kind == AST_array_type && expr->resolved_type->kind == AST_array_type){
            struct ast_array_type *lhs_type = cast(struct ast_array_type *)type;
            struct ast_array_type *rhs_type = cast(struct ast_array_type *)expr->resolved_type;
            if(lhs_type->is_of_unknown_size){
                lhs_type->is_of_unknown_size = false;
                
                lhs_type->amount_of_elements = rhs_type->amount_of_elements;
                lhs_type->base.size = lhs_type->amount_of_elements * lhs_type->element_type->size;
            }
        }else{
            expr = maybe_load_address_for_array_or_function(context, expr);
        }
        
        expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, type, ast->defined_type, expr, equals);
        
        ret = ast_push_binary_expression(context, AST_assignment, equals, ast, expr);
        
        set_resolved_type(ret, ast->resolved_type, ast->defined_type);
        if(got_curly) expect_token(context, TOKEN_closed_curly);
    }
    
    return ret;
}

// @note: for compare_exressions we set the resolved_type afterwards
func struct ast *perform_float_operation(struct ast_binary_op *op){
    struct ast_float_literal *lhs = (struct ast_float_literal *)op->lhs;
    struct ast_float_literal *rhs = (struct ast_float_literal *)op->rhs;
    
    assert(lhs->base.kind == AST_float_literal && rhs->base.kind == AST_float_literal);
    // @cleanup: how to deal with float vs double
    switch(op->base.kind){
        case AST_binary_times:            lhs->value *= rhs->value; break;
        case AST_binary_divide:           lhs->value /= rhs->value; break;
        case AST_binary_plus:             lhs->value += rhs->value; break;
        case AST_binary_minus:            lhs->value -= rhs->value; break;
        case AST_binary_logical_equals:   lhs->value = (f64)(lhs->value == rhs->value); break;
        case AST_binary_logical_unequals: lhs->value = (f64)(lhs->value != rhs->value); break;
        case AST_binary_bigger_equals:    lhs->value = (f64)(lhs->value >= rhs->value); break;
        case AST_binary_smaller_equals:   lhs->value = (f64)(lhs->value <= rhs->value); break;
        case AST_binary_bigger:           lhs->value = (f64)(lhs->value >  rhs->value); break;
        case AST_binary_smaller:          lhs->value = (f64)(lhs->value <  rhs->value); break;
    }
    
    return &lhs->base;
}

// @note: for compare_exressions we set the resolved_type afterwards
func struct ast *perform_integer_operation(struct ast_binary_op *op){
    struct ast_integer_literal *lhs = cast(struct ast_integer_literal *)op->lhs;
    struct ast_integer_literal *rhs = cast(struct ast_integer_literal *)op->rhs;
    
    assert(lhs->base.kind == AST_integer_literal && rhs->base.kind == AST_integer_literal);
    
    struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
    
    // @cleanup: / % should check for 0
    if(promoted_type->size == 4){
        if(promoted_type == &globals.typedef_u32){
            u32 lhs_val = integer_literal_as_u32(&lhs->base);
            u32 rhs_val = integer_literal_as_u32(&rhs->base);
            switch(op->base.kind){
                case AST_binary_times:   lhs->_u32 = lhs_val * rhs_val; break;
                case AST_binary_divide:  lhs->_u32 = lhs_val / rhs_val; break;
                case AST_binary_mod:     lhs->_u32 = lhs_val % rhs_val; break;
                case AST_binary_plus:    lhs->_u32 = lhs_val + rhs_val; break;
                case AST_binary_minus:   lhs->_u32 = lhs_val - rhs_val; break;
                
                case AST_binary_logical_equals:   lhs->_s32 = (lhs_val == rhs_val); break;
                case AST_binary_logical_unequals: lhs->_s32 = (lhs_val != rhs_val); break;
                case AST_binary_bigger_equals:    lhs->_s32 = (lhs_val >= rhs_val); break;
                case AST_binary_smaller_equals:   lhs->_s32 = (lhs_val <= rhs_val); break;
                case AST_binary_bigger:           lhs->_s32 = (lhs_val > rhs_val);  break;
                case AST_binary_smaller:          lhs->_s32 = (lhs_val < rhs_val);  break;
                invalid_default_case();
            }
        }else{
            assert(promoted_type == &globals.typedef_s32);
            s32 lhs_val = integer_literal_as_s32(&lhs->base);
            s32 rhs_val = integer_literal_as_s32(&rhs->base);
            switch(op->base.kind){
                case AST_binary_times:   lhs->_s32 = lhs_val * rhs_val; break;
                case AST_binary_divide:  lhs->_s32 = lhs_val / rhs_val; break;
                case AST_binary_mod:     lhs->_s32 = lhs_val % rhs_val; break;
                case AST_binary_plus:    lhs->_s32 = lhs_val + rhs_val; break;
                case AST_binary_minus:   lhs->_s32 = lhs_val - rhs_val; break;
                
                case AST_binary_logical_equals:   lhs->_s32 = (lhs_val == rhs_val); break;
                case AST_binary_logical_unequals: lhs->_s32 = (lhs_val != rhs_val); break;
                case AST_binary_bigger_equals:    lhs->_s32 = (lhs_val >= rhs_val); break;
                case AST_binary_smaller_equals:   lhs->_s32 = (lhs_val <= rhs_val); break;
                case AST_binary_bigger:           lhs->_s32 = (lhs_val > rhs_val);  break;
                case AST_binary_smaller:          lhs->_s32 = (lhs_val < rhs_val);  break;
                invalid_default_case();
            }
        }
    }else{
        assert(promoted_type->size == 8);
        if(promoted_type == &globals.typedef_u64){
            u64 lhs_val = integer_literal_as_u64(&lhs->base);
            u64 rhs_val = integer_literal_as_u64(&rhs->base);
            switch(op->base.kind){
                case AST_binary_times:   lhs->_u64 = lhs_val * rhs_val; break;
                case AST_binary_divide:  lhs->_u64 = lhs_val / rhs_val; break;
                case AST_binary_mod:     lhs->_u64 = lhs_val % rhs_val; break;
                case AST_binary_plus:    lhs->_u64 = lhs_val + rhs_val; break;
                case AST_binary_minus:   lhs->_u64 = lhs_val - rhs_val; break;
                
                case AST_binary_logical_equals:   lhs->_s32 = (lhs_val == rhs_val); break;
                case AST_binary_logical_unequals: lhs->_s32 = (lhs_val != rhs_val); break;
                case AST_binary_bigger_equals:    lhs->_s32 = (lhs_val >= rhs_val); break;
                case AST_binary_smaller_equals:   lhs->_s32 = (lhs_val <= rhs_val); break;
                case AST_binary_bigger:           lhs->_s32 = (lhs_val > rhs_val);  break;
                case AST_binary_smaller:          lhs->_s32 = (lhs_val < rhs_val);  break;
                invalid_default_case();
            }
        }else{
            assert(promoted_type == &globals.typedef_s64);
            s64 lhs_val = integer_literal_as_s64(&lhs->base);
            s64 rhs_val = integer_literal_as_s64(&rhs->base);
            switch(op->base.kind){
                case AST_binary_times:   lhs->_s64 = lhs_val * rhs_val; break;
                case AST_binary_divide:  lhs->_s64 = lhs_val / rhs_val; break;
                case AST_binary_mod:     lhs->_s64 = lhs_val % rhs_val; break;
                case AST_binary_plus:    lhs->_s64 = lhs_val + rhs_val; break;
                case AST_binary_minus:   lhs->_s64 = lhs_val - rhs_val; break;
                
                case AST_binary_logical_equals:   lhs->_s32 = (lhs_val == rhs_val); break;
                case AST_binary_logical_unequals: lhs->_s32 = (lhs_val != rhs_val); break;
                case AST_binary_bigger_equals:    lhs->_s32 = (lhs_val >= rhs_val); break;
                case AST_binary_smaller_equals:   lhs->_s32 = (lhs_val <= rhs_val); break;
                case AST_binary_bigger:           lhs->_s32 = (lhs_val > rhs_val);  break;
                case AST_binary_smaller:          lhs->_s32 = (lhs_val < rhs_val);  break;
                invalid_default_case();
            }
        }
    }
    
    set_resolved_type(&lhs->base, promoted_type, null);
    return &lhs->base;
}

func struct ast *parse_expression(struct context *context, b32 should_skip_comma_expression){
    // we push an invalid ast onto the stack, to 'shield' the ast stack, when calling 'parse_expression' recursively. We assert that this works in two ways here.
    
    smm ast_stack_before = context->ast_stack_at;
    b32 did_push = false;
    if(ast_stack_before){ // we only have to push if we are not the first thing
        did_push = ast_stack_push(context, &context->invalid_ast);
    }
    
    restart:;
    struct ast *operand = null;
    
    // :prefix_expression parse_prefix_expression
    smm amount_of_prefixes = 0;
    while(true){
        struct ast *current_prefix = null;
        struct token *test = next_token(context);
        switch(test->type){
            case TOKEN_open_paren:{
                // :cast_expression parse_cast_expression
                struct type_info_return type_to_cast_to = maybe_parse_type_for_cast_or_sizeof(context);
                if(context->should_sleep) return &context->empty_statement;
                if(type_to_cast_to.type){
                    expect_token(context, TOKEN_closed_paren);
                    if(peek_token(context, TOKEN_open_curly)){
                        // :struct_literal parse_struct_literal
                        
                        // @hack:
                        struct declarator_return declarator = {
                            .ident = context->invalid_identifier,
                            .type  =  type_to_cast_to.type,
                            .defined_type = type_to_cast_to.defined_type,
                        };
                        struct ast_declaration *decl = push_declaration_for_declarator(context, declarator);
                        // if we are at global scope, we just ignore this!
                        decl->offset_on_stack = parser_emit_memory_location(context, type_to_cast_to.type->size, type_to_cast_to.type->alignment, get_current_token(context));
                        
                        struct ast_identifier *ident = parser_ast_push(context, identifier);
                        ident->base.token = get_current_token(context);
                        ident->decl = decl;
                        set_resolved_type(&ident->base, decl->type, decl->defined_type);
                        
                        
                        // @note: this is either a 'AST_assignment' or a 'AST_struct_or_array_literal'
                        //        which essentially is a list of 'AST_assignment's
                        struct ast *initializer = parse_initializer(context, &ident->base, get_current_token(context));
                        if(context->should_sleep) return initializer;
                        
                        operand = initializer;
                        set_resolved_type(operand, type_to_cast_to.type, type_to_cast_to.defined_type);
                        
                        // struct literals should be assignable? msvc does that.
                        context->in_lhs_expression = true;
                        goto skip_primary_expression_because_we_got_a_struct_literal;
                    }else{
                        if(type_to_cast_to.type->kind == AST_union){
                            report_error(context, test, "cast to union is illegal.");
                            return &context->empty_statement;
                        }
                        if(type_to_cast_to.type->kind == AST_struct){
                            report_error(context, test, "cast to struct is illegal.");
                            return &context->empty_statement;
                        }
                        if(type_to_cast_to.type->kind == AST_array_type){
                            report_error(context, test, "cast to array is illegal.");
                            return &context->empty_statement;
                        }
                    }
                    
                    current_prefix = push_cast(context, type_to_cast_to.type, type_to_cast_to.defined_type, null);
                }else{
                    prev_token(context);
                }
            }break;
            // @note: integer promotion get's handled in the second part
            case TOKEN_increment:{
                current_prefix = ast_push_unary_expression(context, AST_unary_preinc, test, null); break;
            }break;
            case TOKEN_decrement:{
                current_prefix = ast_push_unary_expression(context, AST_unary_predec, test, null); break;
            }break;
            case TOKEN_logical_not:{
                current_prefix = ast_push_unary_expression(context, AST_logical_not, test, null); break;
            }break;
            case TOKEN_bitwise_not:{
                current_prefix = ast_push_unary_expression(context, AST_unary_bitwise_not, test, null); break;
            }break;
            case TOKEN_times:{
                current_prefix = ast_push_unary_expression(context, AST_unary_deref, test, null); break;
            }break;
            case TOKEN_minus:{
                current_prefix = ast_push_unary_expression(context, AST_unary_minus, test, null); break;
            }break;
            case TOKEN_plus:{
                current_prefix = ast_push_unary_expression(context, AST_unary_plus, test, null); break;
            }break;
            case TOKEN_and:{
                current_prefix = ast_push_unary_expression(context, AST_unary_address, test, null); break;
            }break;
            case TOKEN_sizeof:{
                expect_token(context, TOKEN_open_paren);
                struct ast_type *type = maybe_parse_type_for_cast_or_sizeof(context).type;
                if(context->should_sleep) return &context->empty_statement;
                if(type){
                    operand = ast_push_s64_literal(context, type->size);
                    
                    expect_token(context, TOKEN_closed_paren);
                    context->in_lhs_expression = false;
                    goto skip_primary_expression_because_we_got_a_sizeof_or_align_of_with_a_type;
                }
                current_prefix = ast_push_unary_expression(context, AST_sizeof, test, null);
            }break;
            case TOKEN_alignof:{
                expect_token(context, TOKEN_open_paren);
                struct ast_type *type = maybe_parse_type_for_cast_or_sizeof(context).type;
                if(context->should_sleep) return &context->empty_statement;
                if(type){
                    operand = ast_push_s64_literal(context, type->alignment);
                    expect_token(context, TOKEN_closed_paren);
                    context->in_lhs_expression = false;
                    goto skip_primary_expression_because_we_got_a_sizeof_or_align_of_with_a_type;
                }
                current_prefix = ast_push_unary_expression(context, AST_alignof, test, null);
            }break;
            default: prev_token(context); break;
        }
        if(current_prefix){
            if(ast_stack_push(context, current_prefix)){
                amount_of_prefixes++;
            }
        }else{
            break;
        }
    }
    
    
    // :primary_expression parse_primary_expression
    switch(get_current_token(context)->type){
        case TOKEN_character_literal:{
            struct ast_integer_literal *lit = parser_ast_push(context, integer_literal);
            struct token *lit_token = next_token(context);
            lit->_s32 = (s32)lit_token->number;
            // @celanup: multybyte sequences
            set_resolved_type(&lit->base, &globals.typedef_s32, null);
            operand = &lit->base;
            context->in_lhs_expression = false;
        }break;
        
        // "the type of an integer constant is the first of the corresponding list in which its value can be represented"
        case TOKEN_base10_literal:{
            struct ast_integer_literal *lit = parser_ast_push(context, integer_literal);
            struct token *lit_token = next_token(context);
            u64 val = lit_token->number;
            
            switch(lit_token->number_kind){
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
            }
            
            operand = &lit->base;
            context->in_lhs_expression = false;
        }break;
        case TOKEN_hex_literal:{
            struct ast_integer_literal *lit = parser_ast_push(context, integer_literal);
            struct token *lit_token = next_token(context);
            u64 val = lit_token->number;
            
            switch(lit_token->number_kind){
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
            }
            operand = &lit->base;
            context->in_lhs_expression = false;
        }break;
        case TOKEN_string_literal:{
            struct ast_list list = zero_struct;
            
            // concatinate adjacent string literals
            u64 size = 0;
            while(peek_token(context, TOKEN_string_literal)){
                struct ast_string_literal *lit = parser_ast_push(context, string_literal);
                ast_list_append(&list, &context->scratch, &lit->base);
                lit->value = next_token(context)->value;
                size += lit->value->size;
            }
            
            struct ast_string_literal *lit = parser_ast_push(context, string_literal);
            lit->base.token = list.first->value->token;
            
            if(list.count == 0){
                lit->value = list.first->value->token->value;
            }else{
                // concatinate all the string in list
                u8 *data = push_data(context->arena, u8, size + 1);
                u8 *data_it = data;
                for_ast_list(list){
                    struct ast_string_literal *lit_at = cast(struct ast_string_literal *)it->value;
                    memcpy(data_it, lit_at->value->data, lit_at->value->size);
                    data_it += lit_at->value->size;
                }
                data[size] = 0;
                
                // :string_literal_location this needs to be interned right now, as we iterate over the
                // string_table in coff_writer
                lit->value = string_table_insert(&globals.string_table, create_string(data, size), context->arena);
            }
            
            struct ast_array_type *type = parser_type_push(context, array_type);
            type->amount_of_elements = lit->value->size + 1; // plus one for the zero_terminator
            type->element_type = &globals.typedef_s8;
            type->base.size = type->amount_of_elements * 1;
            type->base.alignment = 1;
            
            set_resolved_type(&lit->base, &type->base, null);
            
            operand = &lit->base;
            
            // :strings_are_lhs_expressions
            // string literals are actually lhs expression (you can take the address of them),
            // but you cannot assign to them, as they are arrays.
            context->in_lhs_expression = true;
        }break;
        case TOKEN_float_literal:{
            // @note: float literals get loaded rip-relative, so the ast has to be alive forever.
            struct ast_float_literal *f = parser_ast_push(context, float_literal);
            
            next_token(context);
            b32 is_32_bit = f->base.token->number_kind == NUMBER_KIND_float32;
            set_resolved_type(&f->base, is_32_bit ? &globals.typedef_f32 : &globals.typedef_f64, null);
            
            f->value = f->base.token->_f64;
            
            operand = &f->base;
            context->in_lhs_expression = false;
        }break;
        case TOKEN___FUNCTION__:{
            struct ast_string_literal *lit = parser_ast_push(context, string_literal);
            next_token(context);
            
            lit->value = string_table_insert(&globals.string_table, *context->current_function->identifier, context->arena);
            
            struct ast_array_type *type = parser_type_push(context, array_type);
            type->amount_of_elements = lit->value->size + 1; // plus one for the zero_terminator
            type->element_type = &globals.typedef_s8;
            type->base.size = type->amount_of_elements * 1;
            type->base.alignment = 1;
            
            // @cleanup: this should be an array
            set_resolved_type(&lit->base, &type->base, null);
            
            operand = &lit->base;
            
            // :strings_are_lhs_expressions
            context->in_lhs_expression = true;
        }break;
        case TOKEN_identifier:{
            struct ast_identifier *ident = parser_ast_push(context, identifier);
            next_token(context);
            struct ast_declaration *lookup = lookup_declaration(context, ident->base.token->value);
            if(!lookup){
                set_resolved_type(&ident->base, &globals.typedef_void, null); // has to be something
                parser_sleep(context, ident->base.token, SLEEP_on_decl);
                operand = &ident->base;
                return operand;
            }else if(lookup->base.kind != AST_declaration && lookup->base.kind != AST_function){
                set_resolved_type(&ident->base, &globals.typedef_void, null); // has to be something
                // :Error
                report_error(context, ident->base.token, "Unexpected token in scope.");
                return &ident->base;
            }
            
            if(!(lookup->flags & DECLARATION_FLAGS_is_referanced)){
                // @note: only write to the declaration if it was not set, to avoid thrashing....
                //        maybe we need something here to avoid optimization
                
                lookup->flags |= DECLARATION_FLAGS_is_referanced;
            }
            
            if(lookup->flags & DECLARATION_FLAGS_is_enum_member){
                assert(lookup->assign_expr->kind == AST_integer_literal);
                // @sigh
                struct ast_integer_literal *source = cast(struct ast_integer_literal *)lookup->assign_expr;
                struct ast_integer_literal *lit = parser_ast_push(context, integer_literal);
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
                
                // @note: for function types and array types we check type->is_constant
                context->in_lhs_expression = true;
            }
        }break;
        case TOKEN_open_paren:{
            next_token(context);
            
            struct ast *expr = parse_expression(context, false);
            if(context->should_sleep) return expr;
            
            expect_token(context, TOKEN_closed_paren);
            operand = expr;
            
            // context->in_lhs_expression is set the right way.
        }break;
        default:{
            report_error(context, get_current_token(context), "unexpected token in expression");
            return &context->empty_statement;
        }break;
    }
    
    skip_primary_expression_because_we_got_a_sizeof_or_align_of_with_a_type:;
    skip_primary_expression_because_we_got_a_struct_literal:;
    
    // :postfix_expression parse_postfix_expression
    b32 do_continue = true;
    while(do_continue){
        struct token *test = next_token(context);
        switch(test->type){
            case TOKEN_increment:{
                struct ast *inc = ast_push_unary_expression(context, AST_unary_postinc, test, operand);
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer)) return operand;
                if(!context->in_lhs_expression){
                    report_error(context, inc->token, "Operand of '++' must be an L-value.");
                    return operand;
                }
                if(operand->resolved_type->kind == AST_function_type ||
                   operand->resolved_type->kind == AST_array_type){
                    report_error(context, inc->token, "Operand of '++' must be non-constant type.");
                    return operand;
                }
                
                if(operand->resolved_type->kind == AST_pointer_type){ // @cleanup: if we canonicalize pointers this could be globals.void_pointer
                    struct ast_pointer_type *pointer = (struct ast_pointer_type *)operand->resolved_type;
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, inc->token, "Cannot increment a void-pointer.");
                        return operand;
                    }
                }
                
                set_resolved_type(inc, operand->resolved_type, operand->defined_type);
                operand = inc;
                
                context->in_lhs_expression = false;
            }break;
            case TOKEN_decrement:{
                struct ast *dec = ast_push_unary_expression(context, AST_unary_postdec, test, operand);
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer)) return operand;
                if(!context->in_lhs_expression){
                    report_error(context, dec->token, "Operand of '--' must be an L-value.");
                    return operand;
                }
                if(operand->resolved_type->kind == AST_function_type ||
                   operand->resolved_type->kind == AST_array_type){
                    report_error(context, dec->token, "Operand of '--' must be of non-constant type.");
                    return operand;
                }
                
                
                if(operand->resolved_type->kind == AST_pointer_type){ // @cleanup: if we canonicalize pointers this could be globals.void_pointer
                    struct ast_pointer_type *pointer = (struct ast_pointer_type *)operand->resolved_type;
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, dec->token, "Cannot decrement a void-pointer.");
                            return operand;
                    }
                }
                
                
                set_resolved_type(dec, operand->resolved_type, operand->defined_type);
                operand = dec;
                
                context->in_lhs_expression = false;
            }break;
            case TOKEN_open_index:{
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
                }
                
                struct ast *expr = parse_expression(context, false);
                if(context->should_sleep) return operand;
                expect_token(context, TOKEN_closed_index);
                if(!check_unary_for_basic_types(context, expr, CHECK_integer)) return operand;
                
                operand = push_nodes_for_subscript(context, operand, expr, test);
                
                context->in_lhs_expression = true;
            }break;
            case TOKEN_dot:{
                if(operand->resolved_type->kind != AST_struct && operand->resolved_type->kind != AST_union){
                    report_error(context, test, "Left of '.' needs to be of struct or union type.");
                    return operand;
                }
                struct ast_compound_type *compound = cast(struct ast_compound_type *)operand->resolved_type;
                struct ast *op = handle_dot_or_arrow(context, compound, operand, AST_member, test);
                if(op)operand = cast(struct ast *)op;
                
                context->in_lhs_expression = true;
            }break;
            case TOKEN_arrow:{
                struct ast_type *type = operand->resolved_type;
                if(!type){
                    assert(context->should_sleep);
                    return operand;
                }
                if(type->kind != AST_pointer_type){
                    report_error(context, test, "Left of '->' needs to be of pointer to struct or union type.");
                    return operand;
                }
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)type;
                // :unresolved_type
                if(maybe_resolve_pointer_to_unresolved_type_or_sleep(context, pointer)) return operand;
                
                if(pointer->pointer_to->kind != AST_struct && pointer->pointer_to->kind != AST_union){
                    report_error(context, test, "Left of '->' needs to be of pointer to struct or union type.");
                    return operand;
                }
                struct ast_compound_type *compound = cast(struct ast_compound_type *)pointer->pointer_to;
                struct ast *op = handle_dot_or_arrow(context, compound, operand, AST_member_deref, test);
                
                if(op)operand = cast(struct ast *)op;
                
                context->in_lhs_expression = true;
            }break;
            case TOKEN_open_paren:{
                struct ast_function_call *call = parser_ast_push(context, function_call);
                call->base.token = test;
                if(operand->kind == AST_identifier) call->base.token = operand->token;
                call->identifier_expression = operand;
                
                struct ast_type *type = operand->resolved_type;
                if(type->kind == AST_pointer_type){
                    struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)type;
                    type = pointer->pointer_to;
                }
                
                if(type->kind != AST_function_type){
                    report_error(context, operand->token, "Calling a non-procedure.");
                    return operand;
                }
                
                struct ast_function_type *function_type = cast(struct ast_function_type *)type;
                
                struct ast_list_node* function_argument_iterator = function_type->argument_list.first;
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    do{
                        struct ast *expr = parse_expression(context, true);
                        expr = maybe_load_address_for_array_or_function(context, expr);
                        if(context->should_sleep) return expr;
                        
                        if(function_argument_iterator){
                            assert(function_argument_iterator->value->kind == AST_declaration);
                            struct ast_declaration *decl = cast(struct ast_declaration *)function_argument_iterator->value;
                            // "the arguments are implicitly converted, as if by assignment"
                            expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, decl->type, decl->defined_type, expr, expr->token);

                            function_argument_iterator = function_argument_iterator->next;
                        }else{
                            if(!(function_type->flags & FUNCTION_TYPE_FLAGS_is_varargs)){
                                // :Error
                                report_error(context, call->base.token, "To many arguments to function.");
                                return operand;
                            }else{
                                if(expr->resolved_type->kind == AST_integer_type){
                                    // "the integer promotions are performed on each argument"
                                    expr = maybe_insert_integer_promotion_cast(context, expr);
                                }
                                
                                if(expr->resolved_type == &globals.typedef_f32){
                                    // "arguments that have type float are promoted to double"
                                    // :retain_type_information_through_promotion
                                    expr = push_cast(context, &globals.typedef_f64, cast(struct ast *)expr->resolved_type, expr);
                                }
                            }
                        }
                        
                        ast_list_append(&call->call_arguments, context->arena, expr);
                        
                    }while(peek_token_eat(context, TOKEN_comma));
                    expect_token(context, TOKEN_closed_paren);
                }
                
                if(function_type->argument_list.count > call->call_arguments.count){
                    // :Error
                    report_error(context, call->base.token, "To few arguments to function.");
                    return operand;
                }
                
                set_resolved_type(&call->base, function_type->return_type, function_type->return_type_defined_type);
                
                operand = &call->base;
                
                context->in_lhs_expression = false;
            }break;
            default:{
                prev_token(context);
                do_continue = false;
            }break;
        }
    }
    
    // :prefix_expression parse_prefix_expression
    while(amount_of_prefixes--){
        struct ast *prefix = ast_stack_pop(context);
        
        struct ast_unary_op *op = cast(struct ast_unary_op *)prefix;
        //op->operand = operand; set this at the end, because someone could change it. (e.g type promotion)
        
        switch(prefix->kind){
            case AST_unary_preinc:{
                // @note: no need to integer promote
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer)) return operand;
                if(!context->in_lhs_expression){
                    report_error(context, prefix->token, "Operand of '++' must be an L-value.");
                    return operand;
                }
                if(operand->resolved_type->kind == AST_function_type ||
                   operand->resolved_type->kind == AST_array_type){
                    report_error(context, operand->token, "Operand of '++' must be non-constant type.");
                    return operand;
                }
                
                
                if(operand->resolved_type->kind == AST_pointer_type){ // @cleanup: if we canonicalize pointers this could be globals.void_pointer
                    struct ast_pointer_type *pointer = (struct ast_pointer_type *)operand->resolved_type;
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, op->base.token, "Cannot increment a void-pointer.");
                        return operand;
                    }
                }
                
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
            }break;
            case AST_unary_predec:{
                // @note: no need to integer promote
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer)) return operand;
                if(!context->in_lhs_expression){
                    report_error(context, prefix->token, "Operand of '--' must be an L-value.");
                    return operand;
                }
                if(operand->resolved_type->kind == AST_function_type ||
                   operand->resolved_type->kind == AST_array_type){
                    // :error
                    report_error(context, operand->token, "Operand of '--' must be of non-constant type.");
                    return operand;
                }
                
                if(operand->resolved_type->kind == AST_pointer_type){ // @cleanup: if we canonicalize pointers this could be globals.void_pointer
                    struct ast_pointer_type *pointer = (struct ast_pointer_type *)operand->resolved_type;
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, op->base.token, "Cannot decrement a void-pointer.");
                        return operand;
                    }
                }
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
            }break;
            case AST_unary_bitwise_not:{
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)operand;
                    lit->_u64 = ~lit->_u64; // singed does not matter and should stay the same
                    prefix = &lit->base;
                    break;
                }
                
                if(!check_unary_for_basic_types(context, operand, CHECK_integer)) return operand;
                operand = maybe_insert_integer_promotion_cast(context, operand);
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
            }break;
            case AST_sizeof:{
                if(operand->resolved_type->kind == AST_function_type){
                    report_error(context, op->base.token, "Function types have undefined size.");
                    return operand;
                }
                
                prefix = ast_push_s64_literal(context, operand->resolved_type->size);
                expect_token(context, TOKEN_closed_paren);
                
                context->in_lhs_expression = false;
            }break;
            case AST_alignof:{
                if(operand->resolved_type->kind == AST_function_type){
                    report_error(context, op->base.token, "Function types have undefined alignment.");
                    return operand;
                }
                
                prefix = ast_push_s64_literal(context, operand->resolved_type->alignment);
                expect_token(context, TOKEN_closed_paren);
                
                context->in_lhs_expression = false;
            }break;
            case AST_logical_not:{
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer)) return operand;
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)operand;
                    lit->_s32 = !lit->_u64;
                    prefix = &lit->base;
                    set_resolved_type(&op->base, &globals.typedef_s32, null);
                    break;
                }
                
                if(operand->resolved_type->kind == AST_integer_type){
                    operand = maybe_insert_integer_promotion_cast(context, operand);
                }
                
                set_resolved_type(&op->base, &globals.typedef_s32, null);
                
                context->in_lhs_expression = false;
            }break;
            case AST_unary_plus:{
                if(operand->kind == AST_integer_literal){
                    prefix = operand; // nothing to do here.
                    break;
                }
                
                // @cleanup: CHECK_basic?
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_pointer)) return operand;
                if(operand->resolved_type->kind == AST_integer_type){
                    operand = maybe_insert_integer_promotion_cast(context, operand);
                }
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
            }break;
            case AST_unary_minus:{
                if(operand->kind == AST_integer_literal){
                    struct ast_integer_literal *lit = cast(struct ast_integer_literal *)operand;
                    // @cleanup: warn here unsigned stays unsigned
                    if(!type_is_signed(operand->resolved_type)){
                        report_warning(context, operand->token, "Negation of unsigned is still unsigned.");
                    }
                    
                    // this only flipps the top bit so signdedness does not matter
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
                
                if(!check_unary_for_basic_types(context, operand, CHECK_integer | CHECK_float)) return operand;
                
                if(operand->resolved_type->kind == AST_integer_type){
                    operand = maybe_insert_integer_promotion_cast(context, operand);
                    
                    if(!type_is_signed(operand->resolved_type)){
                        report_warning(context, operand->token, "Negation of unsigned is still unsigned.");
                    }
                }
                
                set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
                context->in_lhs_expression = false;
            }break;
            case AST_unary_deref:{
                if(operand->resolved_type->kind != AST_pointer_type && operand->resolved_type->kind != AST_array_type){
                    report_error(context, prefix->token, "Dereferancing a non-pointer, non-array type.");
                    return operand;
                }
                
                // @cleanup: is this the place todo the *********f thing?
                
                if(operand->resolved_type->kind == AST_pointer_type){
                    struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)operand->resolved_type;
                    if(maybe_resolve_pointer_to_unresolved_type_or_sleep(context, pointer)) return operand;
                    
                    if(pointer->pointer_to == &globals.typedef_void){
                        report_error(context, op->base.token, "Dereferancing a void-pointer.");
                        return operand;
                    }
                    
                    set_resolved_type(&op->base, pointer->pointer_to, pointer->pointer_to_defined_type);
                }else{
                    // is this maybe_load_address_for_array? -> yes, but we need the array type...
                    struct ast_array_type *array = cast(struct ast_array_type *)operand->resolved_type;
                    operand = ast_push_unary_expression(context, AST_unary_address, operand->token, operand);
                    set_resolved_type(operand, parser_push_pointer_type(context, array->element_type, array->element_type_defined_type), null);
                    set_resolved_type(&op->base, array->element_type, array->element_type_defined_type);
                }
                context->in_lhs_expression = true;
            }break;
            case AST_unary_address:{
                if(!context->in_lhs_expression){
                    report_error(context, op->base.token, "Operand of unary '&' must be a lhs expression.");
                    return operand;
                }
                
                struct ast_type *ptr = parser_push_pointer_type(context, operand->resolved_type, operand->defined_type);
                set_resolved_type(&op->base, ptr, null);
                context->in_lhs_expression = false;
            }break;
            case AST_cast:{
                // :cast_expression
                assert(op->base.resolved_type); // should have been set by the parsing
                
                if(op->base.resolved_type->kind == AST_integer_type && operand->kind == AST_integer_literal){
                    u64 value = integer_literal_as_u64(operand);
                    struct ast_integer_literal *literal = cast(struct ast_integer_literal *)operand;
                    struct ast_type *type = op->base.resolved_type;
                    if(type_is_signed(type)){
                        switch(type->size){
                            case 1: literal->_s8  = (s8) value; break;
                            case 2: literal->_s16 = (s16)value; break;
                            case 4: literal->_s32 = (s32)value; break;
                            case 8: literal->_s64 = (s64)value; break;
                            invalid_default_case(value = 0);
                        }
                    }else{
                        switch(type->size){
                            case 1: literal->_u8  = (u8) value; break;
                            case 2: literal->_u16 = (u16)value; break;
                            case 4: literal->_u32 = (u32)value; break;
                            case 8: literal->_u64 = (u64)value; break;
                            invalid_default_case(value = 0);
                        }
                    }
                    set_resolved_type(&literal->base, type, op->base.defined_type);
                    prefix = &literal->base;
                    break;
                }
                
                // @incomplete: all other constant casts (float->float, float->int, int->float)
                
                
                // @cleanup: is this correct?
                operand = maybe_load_address_for_array_or_function(context, operand);
                
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
                        report_error(context, operand->token, "Cannot cast operand of type '%.*s' to something other then 'void'.", type_string.length, type_string.data);
                        return operand;
                    }
                }
                
                context->in_lhs_expression = false;
            }break;
            invalid_default_case(return prefix);
        }
        op->operand = operand;
        operand = prefix; // sets in op->operand in the next iteration
    }
    
    // @note whether we parse 1 << 3 << 2 as (1 << 3) << 2 or as 1 << (3 << 2) depends on wheter this is here or after the (kind != AST_none) check
    
    // :shift_expression parse_shift_expression
    if((ast_stack_current(context)->kind == AST_binary_left_shift) || (ast_stack_current(context)->kind == AST_binary_right_shift)){
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        op->rhs = operand;
        
        if(!check_binary_for_basic_types(context, op, CHECK_integer)) return operand;
        
        // "the integer promotions are performed on each of the operands"
        op->lhs = maybe_insert_integer_promotion_cast(context, op->lhs);
        op->rhs = maybe_insert_integer_promotion_cast(context, op->rhs);
        
        if(op->rhs->kind == AST_integer_literal){ // @cleanup: this is now to general...
            struct ast_integer_literal *literal = cast(struct ast_integer_literal *)op->rhs;
            
            if(type_is_signed(literal->base.resolved_type)){
                b32 should_warn = false;
                switch(literal->base.resolved_type->size){
                    case 1: should_warn = (literal->_s8  < 0); break;
                    case 2: should_warn = (literal->_s16 < 0); break;
                    case 4: should_warn = (literal->_s32 < 0); break;
                    case 8: should_warn = (literal->_s64 < 0); break;
                    invalid_default_case();
                }
                // @cleanup: this is only a warning undefinded behavior
                if(should_warn) {
                    report_error(context, literal->base.token, "Cannot shift by a negative amount.");
                    return operand;
                }
            }
            
            if(op->lhs->kind == AST_integer_literal){
                struct ast_integer_literal *lhs_lit = cast(struct ast_integer_literal *)op->lhs;
                struct ast_integer_literal *rhs_lit = literal;
                // @note: I think these shifts are implicitly arithmetic when they have to be
                
                // "integer promotions are performed"
                struct ast_type *lhs_type = perform_integer_promotions(lhs_lit->base.resolved_type);
                //struct ast_type *rhs_type = perform_integer_promotions(rhs_lit->base.resolved_type);
                
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
                // "the type of the result is that of the promoted left operand"
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
    }
    
    if(peek_token(context, TOKEN_left_shift) || peek_token(context, TOKEN_right_shift)){
        struct token *token = next_token(context);
        enum ast_kind kind = (token->type == TOKEN_left_shift) ? AST_binary_left_shift : AST_binary_right_shift;
        ast_stack_push(context, ast_push_binary_expression(context, kind, token, operand, null));
        goto restart;
    }
    
    // :bitwise_expression parse_bitwise_expression
    if((ast_stack_current(context)->kind == AST_binary_and) || (ast_stack_current(context)->kind == AST_binary_or) ||
       (ast_stack_current(context)->kind == AST_binary_xor)){
        
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        op->rhs = operand;
        
        if(op->rhs->kind == AST_integer_literal && op->lhs->kind == AST_integer_literal){
            u64 lhs = integer_literal_as_u64(op->lhs);
            u64 rhs = integer_literal_as_u64(op->rhs);
            
            // dont care about size of sign for these.
            switch(op->base.kind){
                case AST_binary_and: lhs &= rhs; break;
                case AST_binary_or:  lhs |= rhs; break;
                case AST_binary_xor: lhs ^= rhs; break;
                invalid_default_case();
            }
            
            struct ast_type *promoted_type = perform_usual_arithmetic_conversions(op->lhs->resolved_type, op->rhs->resolved_type);
            
            set_resolved_type(op->lhs, promoted_type, null);
            ((struct ast_integer_literal *)op->lhs)->_u64 = lhs;
            operand = op->lhs;
        }else{
            if(!check_binary_for_basic_types(context, op, CHECK_integer)) return operand;
            maybe_insert_arithmetic_conversion_casts(context, op);
            
            if(op->lhs->resolved_type != op->rhs->resolved_type){
                report_type_missmatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                return operand;
            }
            set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
            operand = &op->base;
            context->in_lhs_expression = false;
        }
    }
    
    
    if(peek_token(context, TOKEN_and)){
        struct ast *ast = ast_push_binary_expression(context, AST_binary_and, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }else if(peek_token(context, TOKEN_or)){
        struct ast *ast = ast_push_binary_expression(context, AST_binary_or, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }else if(peek_token(context, TOKEN_xor)){
        struct ast *ast = ast_push_binary_expression(context, AST_binary_xor, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }
    
    // :multiplicative_expression parse_multiplicative_expression
    if(ast_stack_current(context)->kind == AST_binary_times || 
       ast_stack_current(context)->kind == AST_binary_divide ||
       ast_stack_current(context)->kind == AST_binary_mod){
        
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        op->rhs = operand;
        if(op->lhs->kind == AST_integer_literal && operand->kind == AST_integer_literal){
            operand = perform_integer_operation(op);
        }else{
            op->rhs = operand;
            if(!check_binary_for_basic_types(context, op, CHECK_integer | CHECK_float)) return operand;
            maybe_insert_arithmetic_conversion_casts(context, op);
            
            if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                operand = perform_float_operation(op);
            }else{
                if(op->lhs->resolved_type != op->rhs->resolved_type){
                    report_type_missmatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                    return operand;
                }
                set_resolved_type(&op->base, op->lhs->resolved_type, defined_type_for_binary_op(op));
                operand = &op->base;
            }
        }
        context->in_lhs_expression = false;
    }
    
    if(peek_token(context, TOKEN_times)){
        struct ast *ast = ast_push_binary_expression(context, AST_binary_times, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }else if(peek_token(context, TOKEN_slash)){
        struct ast *ast = ast_push_binary_expression(context, AST_binary_divide, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }else if(peek_token(context, TOKEN_mod)){
        struct ast *ast = ast_push_binary_expression(context, AST_binary_mod, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }
    
    // :additive_expression parse_additive_expression
    if(ast_stack_current(context)->kind == AST_binary_plus ||
       ast_stack_current(context)->kind == AST_binary_minus){
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        op->rhs = operand;
        
        if(op->lhs->kind == AST_integer_literal && operand->kind == AST_integer_literal){
            operand = perform_integer_operation(op);
        }else{
            // @cleanup: I think this check should be later for only arithmetic types
            if(!check_binary_for_basic_types(context, op, CHECK_basic)) return operand;
            op->lhs = maybe_load_address_for_array_or_function(context, op->lhs);
            op->rhs = maybe_load_address_for_array_or_function(context, op->rhs);
            
            struct ast *handled = null;
            // @cleanup: interger + pointer is legal, integer - pointer is not.
            if(op->lhs->resolved_type->kind == AST_pointer_type &&
               op->rhs->resolved_type->kind == AST_integer_type){
                // 'pointer + integer', 'pointer - integer', 
                handled = handle_pointer_arithmetic(context, &op->base);
            }else if(op->base.kind == AST_binary_minus){
                if(op->lhs->resolved_type->kind == AST_pointer_type &&
                   op->rhs->resolved_type->kind == AST_pointer_type){
                    // 'pointer - pointer'
                    if(!types_are_equal(context, op->lhs->resolved_type, op->rhs->resolved_type)){
                        report_type_missmatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                        return operand;
                    }
                    set_resolved_type(&op->base, &globals.typedef_s64, null);
                    
                    struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)op->lhs->resolved_type;
                    struct ast *size = ast_push_s64_literal(context, pointer->pointer_to->size);
                    handled = ast_push_binary_expression(context, AST_binary_divide, op->base.token, &op->base, size);
                    set_resolved_type(handled, &globals.typedef_s64, null);
                }
            }
            
            if(!handled){
                maybe_insert_arithmetic_conversion_casts(context, op);
                
                if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                    handled = perform_float_operation(op);
                }else{   
                    if(op->lhs->resolved_type != op->rhs->resolved_type){
                        report_type_missmatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
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
    }
    
    if(peek_token(context, TOKEN_plus)){
        struct ast *op = ast_push_binary_expression(context, AST_binary_plus, next_token(context), operand, null);
        ast_stack_push(context, op);
        goto restart;
    }else if(peek_token(context, TOKEN_minus)){
        struct ast *op = ast_push_binary_expression(context, AST_binary_minus, next_token(context), operand, null);
        ast_stack_push(context, op);
        goto restart;
    }
    
    // :compare_expression parse_compare_expression
    switch(ast_stack_current(context)->kind){
        case AST_binary_logical_equals:
        case AST_binary_logical_unequals:
        case AST_binary_bigger_equals:
        case AST_binary_smaller_equals:
        case AST_binary_bigger:
        case AST_binary_smaller:{
            struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
            op->rhs = operand;
            
            if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
                operand = perform_integer_operation(op);
                set_resolved_type(operand, &globals.typedef_s32, null);
                break;
            }
            
            if(!check_binary_for_basic_types(context, op, CHECK_basic)) return operand;
            
            maybe_cast_literal_0_to_void_pointer(context, op);
            maybe_insert_arithmetic_conversion_casts(context, op);
            
            if(op->lhs->kind == AST_float_literal && op->rhs->kind == AST_float_literal){
                operand = perform_float_operation(op);
                set_resolved_type(operand, &globals.typedef_s32, null);
                break;
            }
            
            struct ast_type *match = types_are_equal(context, op->lhs->resolved_type, op->rhs->resolved_type);
            if(!match){
                report_type_missmatch_error(context, op->lhs->resolved_type, op->lhs->defined_type, op->rhs->resolved_type, op->rhs->defined_type, op->base.token);
                return operand;
            }
            
            set_resolved_type(&op->base, &globals.typedef_s32, null);
            operand = &op->base;
            context->in_lhs_expression = false;
        }break;
        default: break;
    }
    
    {
        struct token *test = next_token(context);
        enum ast_kind kind = AST_none;
        switch(test->type){
            case TOKEN_logical_equals:   kind = AST_binary_logical_equals;   break;
            case TOKEN_logical_unequals: kind = AST_binary_logical_unequals; break;
            case TOKEN_bigger_equals:    kind = AST_binary_bigger_equals;    break;
            case TOKEN_smaller_equals:   kind = AST_binary_smaller_equals;   break;
            case TOKEN_bigger:           kind = AST_binary_bigger;           break;
            case TOKEN_smaller:          kind = AST_binary_smaller;          break;
            default: prev_token(context);                                     break;
        }
        
        if(kind != AST_none){
            struct ast *ast = ast_push_binary_expression(context, kind, test, operand, null);
            ast_stack_push(context, ast);
            goto restart;
        }
    }
    
    // :conditional_expression parse_conditional_expression
    if(ast_stack_current(context)->kind == AST_conditional_expression){
        struct ast_conditional_expression *cond = cast(struct ast_conditional_expression *)ast_stack_pop(context);
        
        // @cleanup: this has a lot of code that any binary expression also has e.g.
        //              'maybe_cast_literal_0_to_void_pointer'
        //              'maybe_insert_arithmetic_conversion_casts'
        //           maybe we could make it compatible with binary ops, to use these functions
        if(!cond->if_true){
            cond->if_true = maybe_load_address_for_array_or_function(context, operand);
            expect_token(context, TOKEN_colon);
            ast_stack_push(context, &cond->base);
            goto restart;
        }
        cond->if_false = maybe_load_address_for_array_or_function(context, operand);
        
        // "one of the following shall hold"
        
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
            // "- both operands have the same union or structure type"
            // "- both operands have void type"
            // "- both operands are pointers to compatable types"
            // "- one operand is a pointer and the other is a null pointer constant"
            if(cond->if_false->resolved_type->kind == AST_pointer_type && cond->if_true->kind == AST_integer_literal){
                if(integer_literal_as_u64(cond->if_true) == 0){
                    cond->if_true = push_cast(context, globals.typedef_void_pointer, null, cond->if_true);
                }
            }
            
            if(cond->if_true->resolved_type->kind == AST_pointer_type && cond->if_false->kind == AST_integer_literal){
                if(integer_literal_as_u64(cond->if_false) == 0){
                    cond->if_false = push_cast(context, globals.typedef_void_pointer, null, cond->if_false);
                }
            }
            
            // "- one is a pointer to an object and te other is a pointer to void"
            if(!types_are_equal(context, cond->if_false->resolved_type, cond->if_true->resolved_type)){
                report_type_missmatch_error(context, cond->if_true->resolved_type, cond->if_true->defined_type, cond->if_false->resolved_type, cond->if_false->defined_type, cond->base.token);
                return operand;
            }
        }
        
        
        
        // @cleanup: how should these defined_types propagate in this case?
        struct ast *defined_type = cond->if_true->defined_type ? cond->if_true->defined_type : cond->if_false->defined_type;
        set_resolved_type(&cond->base, cond->if_true->resolved_type, defined_type);
        
        // constant propagation
        if(cond->condition->kind == AST_integer_literal){
            if(integer_literal_as_u64(cond->condition)){
                operand = cond->if_true;
            }else{
                operand = cond->if_false;
            }
        }else{
            operand = &cond->base;
        }
        context->in_lhs_expression = false;
    }
    
    if(peek_token(context, TOKEN_question_mark)){
        struct ast_conditional_expression *cond = parser_ast_push(context, conditional_expression);
        next_token(context);
        if(!casts_implicitly_to_bool(operand)){
            report_error(context, cond->base.token, "Left hand side of '?' has no implicit conversion to bool.");
            return operand;
        }
        cond->condition = operand;
        ast_stack_push(context, &cond->base);
        goto restart;
    }
    
    // :logical_and_expression parse_logical_and_expression
    if(ast_stack_current(context)->kind == AST_logical_and){
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        op->rhs = operand;
        if(!casts_implicitly_to_bool(operand)){
            // :Error pick a way to say this an stick to it
            report_error(context, op->base.token, "Right of '&&' has to be castable to bool.");
            return operand;
        }
        
        if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
            struct ast_integer_literal *lit = cast(struct ast_integer_literal *)op->lhs;
            if(integer_literal_as_u64(op->lhs) && integer_literal_as_u64(op->rhs)){
                lit->_s32 = 1;
            }else{
                lit->_s32 = 0;
            }
            set_resolved_type(&lit->base, &globals.typedef_s32, null);
            operand = &lit->base;
        }else{
            set_resolved_type(&op->base, &globals.typedef_s32, null);
            operand = &op->base;
        }
        context->in_lhs_expression = false;
    }
    
    if(peek_token(context, TOKEN_logical_and)){
        if(!casts_implicitly_to_bool(operand)){
            report_error(context, next_token(context), "Left of '&&' has to be castable to bool.");
            return operand;
        }
        struct ast *ast = ast_push_binary_expression(context, AST_logical_and, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }
    
    // :logical_or_expression parse_logical_or_expression
    if(ast_stack_current(context)->kind == AST_logical_or){
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        op->rhs = operand;
        if(!casts_implicitly_to_bool(operand)){
            report_error(context, op->base.token, "right of '||' has to be castable to bool.");
            return operand;
        }
        
        if(op->lhs->kind == AST_integer_literal && op->rhs->kind == AST_integer_literal){
            struct ast_integer_literal *lit = cast(struct ast_integer_literal *)op->lhs;
            if(integer_literal_as_u64(op->lhs) || integer_literal_as_u64(op->rhs)){
                lit->_s32 = 1;
            }else{
                lit->_s32 = 0;
            }
            set_resolved_type(&lit->base, &globals.typedef_s32, null);
            operand = &lit->base;
        }else{
            set_resolved_type(&op->base, &globals.typedef_s32, null);
            operand = &op->base;
        }
        context->in_lhs_expression = false;
    }
    
    if(peek_token(context, TOKEN_logical_or)){
        if(!casts_implicitly_to_bool(operand)){
            report_error(context, next_token(context), "left of '||' has to be castable to bool.");
            return operand;
        }
        struct ast *ast = ast_push_binary_expression(context, AST_logical_or, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }
    
    
    {        
        struct ast *assign = null;
        // :assign_expression parse_assign_expression :assignment_expression parse_assignment_expression
        // :compound_assignment
        switch(get_current_token(context)->type){
            case TOKEN_equals:{
                assign = ast_push_binary_expression(context, AST_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_and_equals:{
                assign = ast_push_binary_expression(context, AST_and_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_or_equals:{
                assign = ast_push_binary_expression(context, AST_or_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_xor_equals:{
                assign = ast_push_binary_expression(context, AST_xor_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_plus_equals:{
                assign = ast_push_binary_expression(context, AST_plus_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_minus_equals:{
                assign = ast_push_binary_expression(context, AST_minus_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_left_shift_equals:{
                assign = ast_push_binary_expression(context, AST_left_shift_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_right_shift_equals:{
                assign = ast_push_binary_expression(context, AST_right_shift_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_div_equals:{
                assign = ast_push_binary_expression(context, AST_divide_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_mod_equals:{
                assign = ast_push_binary_expression(context, AST_modulo_assignment, next_token(context), operand, null);
            }break;
            case TOKEN_times_equals:{
                assign = ast_push_binary_expression(context, AST_times_assignment, next_token(context), operand, null);
            }break;
        };
        
        if(assign){
            if(!context->in_lhs_expression){
                report_error(context, assign->token, "Lhs is not assignable.");
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
            
            if(operand->resolved_type == &globals.typedef_void){
                report_error(context, assign->token, "Cannot assign to something of type 'void'.");
                return operand;
            }
            
            ast_stack_push(context, assign);
            goto restart;
        }
        
        while(true){
            b32 should_break = false;
            switch(ast_stack_current(context)->kind){
                case AST_assignment:
                case AST_plus_assignment:
                case AST_minus_assignment:
                case AST_and_assignment:
                case AST_or_assignment:
                case AST_xor_assignment:
                case AST_divide_assignment:
                case AST_modulo_assignment:
                case AST_times_assignment:
                case AST_left_shift_assignment:
                case AST_right_shift_assignment:{
                    struct ast_binary_op *assignment = cast(struct ast_binary_op *)ast_stack_pop(context);
                    assignment->rhs = maybe_load_address_for_array_or_function(context, operand);
                    
                    switch(assignment->base.kind){
                        case AST_assignment: break;
                        
                        case AST_minus_assignment:
                        case AST_plus_assignment:
                        if(!check_binary_for_basic_types(context, assignment, CHECK_basic)) return operand;
                        break;
                        
                        case AST_left_shift_assignment:
                        case AST_right_shift_assignment:
                        case AST_modulo_assignment:
                        case AST_and_assignment:
                        case AST_or_assignment:
                        case AST_xor_assignment:
                        if(!check_binary_for_basic_types(context, assignment, CHECK_integer)) return operand;
                        break;
                        
                        case AST_times_assignment:
                        case AST_divide_assignment:
                        if(!check_binary_for_basic_types(context, assignment, CHECK_integer|CHECK_float)) return operand;
                        break;
                        invalid_default_case();
                    }
                    
                    // @cleanup: should one be able to add pointers of the same type?
                    
                    struct ast *handled = null;
                    if(assignment->base.kind == AST_left_shift_assignment || assignment->base.kind == AST_right_shift_assignment){
                        assignment->rhs = push_cast(context, &globals.typedef_u8, null, assignment->rhs);
                        handled = &assignment->base;
                    }
                    
                    if(assignment->base.kind == AST_plus_assignment || assignment->base.kind == AST_minus_assignment){
                        //if(assignment->base.token->line == 130) os_debug_break();
                        
                        if(assignment->lhs->resolved_type->kind == AST_pointer_type && assignment->rhs->resolved_type->kind == AST_integer_type){
                            handled = handle_pointer_arithmetic(context, &assignment->base);
                        }
                    }
                    
                    if(!handled){
                        assignment->rhs = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, assignment->lhs->resolved_type, assignment->lhs->defined_type, assignment->rhs, assignment->base.token);
                        
                        handled = &assignment->base;
                    }
                    
                    set_resolved_type(&assignment->base, assignment->lhs->resolved_type, assignment->lhs->defined_type);
                    operand = handled;
                    context->in_lhs_expression = false; // doesn't really matter they get scoped the other way
                }break;
                default: should_break = true; break;
            }
            if(should_break) break;
        }
    }
    
    if(!should_skip_comma_expression && ast_stack_current(context)->kind == AST_comma_expression){
        struct ast_binary_op *op = cast(struct ast_binary_op *)ast_stack_pop(context);
        
        op->rhs = operand;
        set_resolved_type(&op->base, operand->resolved_type, operand->defined_type);
        operand = &op->base;
        context->in_lhs_expression = false; // @hmm: should we allow (3, a) = 3; probably not
    }
    
    
    if(!should_skip_comma_expression && peek_token(context, TOKEN_comma)){
        struct ast *ast = ast_push_binary_expression(context, AST_comma_expression, next_token(context), operand, null);
        ast_stack_push(context, ast);
        goto restart;
    }
    
    // remove the artifical invalid ast we put on in the beginning
    if(did_push){
        struct ast *invalid_ast = ast_stack_pop(context);
        assert(invalid_ast->kind == AST_invalid);
    }
    
    // @note: this should never fire as we want to return early if there is an error deeper in the stack
    assert(ast_stack_before == context->ast_stack_at);
    
    return operand;
}


// @sigh: defined_type is an _out_ variable
func struct ast_type *parse_type_specifier(struct context *context, struct ast **out_defined_type){
    while(get_current_token(context)->type == TOKEN_const || get_current_token(context)->type == TOKEN_volatile){
        next_token(context);
    }
    
    { // :basic_type parse_basic_type
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
        }c_type = 0;
        
        b32 should_loop = true;
        while(should_loop){
            switch(next_token(context)->type){
                case TOKEN_void:  c_type |= C_TYPE_void;  break;
                case TOKEN_char:  c_type |= C_TYPE_char;  break;
                case TOKEN_short: c_type |= C_TYPE_short; break;
                case TOKEN_int:   c_type |= C_TYPE_int;   break;
                case TOKEN_long:{
                    if(c_type & C_TYPE_long){
                        c_type |= C_TYPE_long_long;
                    }else{
                        c_type |= C_TYPE_long;
                    }
                }break;
                case TOKEN_int8:     c_type |= C_TYPE_int8;     break;
                case TOKEN_int16:    c_type |= C_TYPE_int16;    break;
                case TOKEN_int32:    c_type |= C_TYPE_int32;    break;
                case TOKEN_int64:    c_type |= C_TYPE_int64;    break;
                case TOKEN_float:    c_type |= C_TYPE_float;    break;
                case TOKEN_double:   c_type |= C_TYPE_double;   break;
                case TOKEN_unsigned: c_type |= C_TYPE_unsigned; break;
                case TOKEN_signed:   c_type |= C_TYPE_signed;   break;
                case TOKEN_Bool:     c_type |= C_TYPE_Bool;     break;
                default: {
                    should_loop = false;
                    prev_token(context);
                }break;
            }
        }
        
        switch(c_type){
            case C_TYPE_none: break;
            
            case C_TYPE_void:     return &globals.typedef_void;
            
            case C_TYPE_unsigned: return &globals.typedef_u32;
            case C_TYPE_signed:   return &globals.typedef_s32;
            case C_TYPE_char:     return &globals.typedef_s8;
            case C_TYPE_short:    return &globals.typedef_s16;
            case C_TYPE_int:      return &globals.typedef_s32;
            case C_TYPE_long:     return &globals.typedef_s32;
            
            case (C_TYPE_short | C_TYPE_int): return &globals.typedef_s16;
            case (C_TYPE_long  | C_TYPE_int): return &globals.typedef_s32;
            case (C_TYPE_long  | C_TYPE_long_long): return &globals.typedef_s64;
            case (C_TYPE_long  | C_TYPE_long_long | C_TYPE_int): return &globals.typedef_s64;
            
            case (C_TYPE_unsigned | C_TYPE_char):  return &globals.typedef_u8;
            case (C_TYPE_unsigned | C_TYPE_short): return &globals.typedef_u16;
            case (C_TYPE_unsigned | C_TYPE_int):   return &globals.typedef_u32;
            case (C_TYPE_unsigned | C_TYPE_long):  return &globals.typedef_u32;
            case (C_TYPE_unsigned | C_TYPE_long | C_TYPE_long_long): return &globals.typedef_u64;
            
            
            case (C_TYPE_unsigned | C_TYPE_short | C_TYPE_int): return &globals.typedef_u16;
            case (C_TYPE_unsigned | C_TYPE_long  | C_TYPE_int): return &globals.typedef_u32;
            case (C_TYPE_unsigned | C_TYPE_long | C_TYPE_long_long | C_TYPE_int): return &globals.typedef_u64;
            
            case (C_TYPE_signed | C_TYPE_char):  return &globals.typedef_s8;
            case (C_TYPE_signed | C_TYPE_short): return &globals.typedef_s16;
            case (C_TYPE_signed | C_TYPE_int):   return &globals.typedef_s32;
            case (C_TYPE_signed | C_TYPE_long):  return &globals.typedef_s32;
            case (C_TYPE_signed | C_TYPE_long | C_TYPE_long_long): return &globals.typedef_s64;
            
            case (C_TYPE_signed | C_TYPE_short | C_TYPE_int): return &globals.typedef_s16;
            case (C_TYPE_signed | C_TYPE_long  | C_TYPE_int): return &globals.typedef_s32;
            case (C_TYPE_signed | C_TYPE_long | C_TYPE_long_long | C_TYPE_int): return &globals.typedef_s64;
            
            case C_TYPE_int8:  return &globals.typedef_s8;
            case C_TYPE_int16: return &globals.typedef_s16;
            case C_TYPE_int32: return &globals.typedef_s32;
            case C_TYPE_int64: return &globals.typedef_s64;
            
            case (C_TYPE_signed | C_TYPE_int8):  return &globals.typedef_s8;
            case (C_TYPE_signed | C_TYPE_int16): return &globals.typedef_s16;
            case (C_TYPE_signed | C_TYPE_int32): return &globals.typedef_s32;
            case (C_TYPE_signed | C_TYPE_int64): return &globals.typedef_s64;
            
            case (C_TYPE_unsigned | C_TYPE_int8):  return &globals.typedef_u8;
            case (C_TYPE_unsigned | C_TYPE_int16): return &globals.typedef_u16;
            case (C_TYPE_unsigned | C_TYPE_int32): return &globals.typedef_u32;
            case (C_TYPE_unsigned | C_TYPE_int64): return &globals.typedef_u64;
            
            case C_TYPE_float:    return &globals.typedef_f32;
            case C_TYPE_double:   return &globals.typedef_f64;
            case (C_TYPE_long | C_TYPE_double): return &globals.typedef_f64;
            
            
            case C_TYPE_Bool:{
                // @hack: @incomplete: what should we do about _Bool, I really dislike it, but maybe I should implement it...
                //not_implemented;
                return &globals.typedef_u8;
            }
            default:{
                report_error(context, get_current_token(context), "Invalid combination of base types.");
                return &globals.typedef_void;
            }break;
        }
    }
    
    struct token *token = next_token(context);
    // :struct :parse_struct
    if(token->type == TOKEN_struct || token->type == TOKEN_union){
        b32 is_union = (token->type == TOKEN_union);
        
        u64 declspec_alignment = 0;
        
        while(peek_token(context, TOKEN_identifier) && 
              get_current_token(context)->value == globals.keyword_declspec){
            next_token(context);
            expect_token(context, TOKEN_open_paren);
            
            struct token *declspec = expect_token(context, TOKEN_identifier);
            b32 unsupported = false;
            
            if(declspec->value == globals.keyword_align){
                expect_token(context, TOKEN_open_paren);
                // @hmm: I gues one could allow character or octal literals... I think I'm gonna change that 
                //       system, when I try to optimize the tokenizer. -10.09.2020
                if(peek_token(context, TOKEN_base10_literal) || peek_token(context, TOKEN_hex_literal)){
                    //struct __declspec(align(0x100)) asd;
                    u64 value = next_token(context)->number;
                    if(!is_power_of_two(value)){
                        report_error(context, declspec, "Alignment must be a power of two.");
                        return &globals.typedef_void;
                    }
                    declspec_alignment = value;
                }else{
                    report_error(context, get_current_token(context), "");
                }
                expect_token(context, TOKEN_closed_paren);
            }else{
                unsupported = true;
            }
            
            if(!unsupported){
                expect_token(context, TOKEN_closed_paren);
            }
            
            if(unsupported){
                report_warning(context, declspec, "Unsupported declspec ignored.");
                if(!skip_until_tokens_are_ballanced(context, TOKEN_open_paren, TOKEN_closed_paren)){
                    return &globals.typedef_void;
                }
            }
        }
        struct token *name = peek_token_eat(context, TOKEN_identifier);
        if(peek_token_eat(context, TOKEN_open_curly)){
            if(!context->sleeping_ident && name) context->sleeping_ident = name;
            
            //@ugh manual
            struct ast_compound_type *compound;
            if(is_union){
                compound = parser_compound_type_push(context, union);
            }else{
                compound = parser_compound_type_push(context, struct);
            }
            
            if(name) compound->base.token = name;
            
            compound->identifier = name ? name->value : globals.unnamed_tag;
            
            smm size = 0;
            smm alignment = declspec_alignment;
            
            // @cleanup: is the below comment still true?
            // if we sleep on some identifier we cannot break out of the loop because, outside we want to know whether it has a declarator or a ';'. but if we break out current token is whatever.
            b32 do_not_know_size_yet = false;
            
            // return out early if we should sleep, this prevents any 'context->should_sleep = false'
            // situations.
            if(context->should_sleep) return &compound->base;
            while(!peek_token_eat(context, TOKEN_closed_curly)){
                struct ast *lhs_defined_type = null;
                struct ast_type *lhs_type = parse_type_specifier(context, &lhs_defined_type);
                
                // :PointerInStruct
                // @note: we have to allow 'parser->should_sleep' here, because of 
                //           struct asd { struct asd *asd; };
                //        Here we cannot sleep on 'struct asd', as we are 'struct asd' and thus it would never
                //        wake up. Instead we rely on 'lhs_type' to be 'AST_unresolved_type'.
                if(context->error) return &compound->base;
                assert(!context->should_sleep || lhs_type->kind == AST_unresolved_type);
                
                // check for anonymous struct or union
                if(peek_token_eat(context, TOKEN_semicolon)){
                    if(lhs_type->kind == AST_struct || lhs_type->kind == AST_union){
                        struct ast_compound_type *other_compound = cast(struct ast_compound_type *)lhs_type;
                        if(other_compound->identifier == globals.unnamed_tag){
                            // @ugly @hack @speed we should just append the list and not copy it.
                            // @note: right now that would not work, as I think
                            // struct asd{
                            //    struct asd2;
                            //}
                            // appends all the members of asd2 here... (which actually is sort of neat)
                            size = align_up(size, lhs_type->alignment);
                            for_ast_list(other_compound->declarations){
                                ast_list_append(&compound->declarations, context->arena, it->value);
                                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                                if(!is_union){
                                    decl->offset_in_type += size;
                                }
                            }
                            
                            if(is_union){
                                size = max_of(lhs_type->size, size);
                            }else{
                                size += lhs_type->size;
                            }
                            alignment = max_of(lhs_type->alignment, alignment);
                            
                            continue;
                        }
                    }
                    
                    // We got 'u32;' inside of a struct.
                    // :Error
                    report_error(context, lhs_type->token, "Expected an indentifier.");
                    return &compound->base;
                }
                
                do{
                    
                    struct declarator_return declarator = parse_declarator(context, lhs_type, lhs_defined_type, 0);
                    if(declarator.type->kind == AST_unresolved_type){
                        struct type_info_return resolved = resolve_unresolved_type_or_sleep(context, declarator.type);
                        if(resolved.type){
                            declarator.type = resolved.type;
                            declarator.defined_type = resolved.defined_type;
                        }
                    }
                    if(context->should_sleep){
                        if(context->error) return &compound->base;
                        if(declarator.type->kind == AST_pointer_type){
                            // @robustness: this feels a bit volitile, because we are setting
                            //              'parser_should_sleep' manually
                            context->should_sleep = false;
                        }else{
                            // if we should sleep and lhs_type is not a pointer then we do not know the size yet
                            // in which case we have to sleep
                            do_not_know_size_yet = true;
                            break;
                        }
                    }
                    struct ast_declaration *decl = push_declaration_for_declarator(context, declarator);
                    
                    if(is_union){
                        decl->offset_in_type = 0;
                        size = max_of(decl->type->size, size);
                    }else{
                        size = align_up(size, decl->type->alignment);
                        decl->offset_in_type = size;
                        size += decl->type->size;
                    }
                    alignment = max_of(decl->type->alignment, alignment);
                    
                    ast_list_append(&compound->declarations, context->arena, &decl->base);
                }while(peek_token_eat(context, TOKEN_comma));
                
                if(do_not_know_size_yet) break;
                expect_token(context, TOKEN_semicolon);
            }
            
            if(size <= 0){
                report_error(context, compound->base.token, "Struct has to be of positive size.");
                return &compound->base;
            }
            
            if(do_not_know_size_yet){
                compound->base.size = -1;
                compound->base.alignment = -1;
                context->should_sleep = true;
            }else{
                compound->base.size = size;
                compound->base.alignment = alignment;
                if(name) register_compound_type(context, &compound->base, compound->identifier);
            }
            
            return &compound->base;
        }else{ // no open curly
            char *union_or_struct = (is_union) ? "union" : "struct";
            
            if(!name){
                report_error(context, get_current_token(context), "Expected an identifier or '{' after '%s'.", union_or_struct);
                return &globals.typedef_void;
            }
            struct ast_type *lookup = lookup_compound_type(context, name->value);
            
            if(!lookup){
                return push_unresolved_type(context, name, SLEEP_on_struct, union_or_struct);
            }else if(lookup->kind != AST_struct && lookup->kind != AST_union){
                report_error(context, name, "Expected a %s name after '%s'.", union_or_struct, union_or_struct);
                return &globals.typedef_void;
            }
            return lookup;
        }
    }
    
    // :enum :parse_enum :ast_enum
    if(token->type == TOKEN_enum){
        struct ast_compound_type *ast_enum = parser_compound_type_push(context, enum);
        
        // "each enumerated type should be compatible with a signed or unsinged integer type. Implementation
        //  defined but should be able to hold all of the members" -> we are lazy: int
        ast_enum->base.size = 4;
        ast_enum->base.alignment = 4;
        struct token *name = peek_token_eat(context, TOKEN_identifier);
        ast_enum->identifier = name ? name->value : globals.unnamed_enum;
        
        if(peek_token_eat(context, TOKEN_open_curly)){
            if(!context->sleeping_ident && name) context->sleeping_ident = name;
            
            s64 current_value = 0;
            do{
                if(peek_token_eat(context, TOKEN_closed_curly)) goto enum_skip_curly;
                
                struct ast_declaration *decl = parser_ast_push(context, declaration);
                struct token *ident = expect_token(context, TOKEN_identifier);
                decl->identifier = ident->value;
                decl->flags |= DECLARATION_FLAGS_is_enum_member;
                
                struct ast *lit;
                if(peek_token_eat(context, TOKEN_equals)){
                    struct ast *const_expr = parse_expression(context, true);
                    
                    if(const_expr->kind != AST_integer_literal){
                        report_error(context, const_expr->token, "Expression needs to resolve to constant integer.");
                        return &ast_enum->base;
                    }
                    
                    lit = const_expr;
                    
                    current_value = integer_literal_as_s64(const_expr);
                }else{
                    lit = ast_push_s32_literal(context, current_value);
                }
                current_value++;
                
                //
                // the c-spec says: enum members are ints, and the enum is of a size that can hold all enum members
                decl->type = &ast_enum->base;
                
                // "an identifier declared as an enumeration constant has type int".
                set_resolved_type(lit, &globals.typedef_s32, cast(struct ast *)ast_enum);
                decl->assign_expr = lit;
                decl->offset_on_stack = -1;
                
                parser_register_declaration(context, decl);
                
                ast_list_append(&ast_enum->declarations, context->arena, &decl->base);
            }while(peek_token_eat(context, TOKEN_comma));
            expect_token(context, TOKEN_closed_curly);
            
            enum_skip_curly:;
            
            if(name) register_compound_type(context, &ast_enum->base, ast_enum->identifier);
            
            *out_defined_type = cast(struct ast *)&ast_enum->base;
            return &globals.typedef_s32;
        }else{ // no open curly
            if(!name){
                report_error(context, get_current_token(context), "Expected an identifier or '{' after 'enum'.");
                return &globals.typedef_void;
            }
            struct ast_type *lookup = lookup_compound_type(context, name->value);
            
            if(!lookup){
                return push_unresolved_type(context, name, SLEEP_on_struct, "enum");
            }else if(lookup->kind != AST_enum){
                report_error(context, name, "Expected an enum name after 'enum'.");
                return &globals.typedef_void;
            }
            *out_defined_type = cast(struct ast *)lookup;
            return &globals.typedef_s32;
        }
    }
    
    if(token->type == TOKEN_identifier){
        struct ast_declaration *decl = lookup_typedef(context, token, false);
        if(decl){
            *out_defined_type = &decl->base;
            return decl->type; // success
        }else{
            return push_unresolved_type(context, token, SLEEP_on_decl, "");
        }
    }
    
    report_error(context, token, "Unexpected token where a type should be.");
    
    return &globals.typedef_void; // @hmm poisen?
}


func void validate_intrinsic(struct context *context, struct intrinsic_info *info, struct ast_function *function){
    struct ast_function_type *type = function->type;
#define arg_decl_to_type(a) (((struct ast_declaration *)(a))->type)
    
    if(type->flags & FUNCTION_TYPE_FLAGS_is_varargs){
        if(info->kind != INTRINSIC_KIND_va_start){
            report_error(context, function->base.token, "Intrinsic function cannot be declared varargs.");
            return;
        }
    }
    
    // check that the function matches what we expect.
    switch(info->kind){
        case INTRINSIC_KIND_set_scalar_double:{
            if(type->return_type->size != 16){
                report_error(context, function->base.token, "Return value of intrinsic function must be of size 16.");
            }
            
            if(type->argument_list.count != 1){
                report_error(context, function->base.token, "Intrinsic function must have exactly one argument.");
                break;
            }
            if(arg_decl_to_type(type->argument_list.first->value) != &globals.typedef_f64){
                report_error(context, function->base.token, "Argument of intrinsic function must be of type 'double'.");
            }
        }break;
        case INTRINSIC_KIND_packed_double_op:
        case INTRINSIC_KIND_scalar_double_op:{
            if(type->return_type->size != 16){
                report_error(context, function->base.token, "Return value of intrinsic function must be of size 16.");
            }
            
            if(type->argument_list.count != 2){
                report_error(context, function->base.token, "Intrinsic function must have exactly two arguments.");
            }
            
            if(((struct ast_declaration *)type->argument_list.first->value)->type->size != 16){
                report_error(context, function->base.token, "First argument of intrinsic function must be of size 16.");
            }
            
            if(((struct ast_declaration *)type->argument_list.last->value)->type->size != 16){
                report_error(context, function->base.token, "Second argument of intrinsic function must be of size 16.");
            }
        }break;
        case INTRINSIC_KIND_va_start:{
            if(type->return_type != &globals.typedef_void      || 
               type->argument_list.count != 1                      || 
               !(type->flags & FUNCTION_TYPE_FLAGS_is_varargs) ||
               ((struct ast_declaration *)type->argument_list.first->value)->type->kind != AST_pointer_type){
                report_error(context, function->base.token, "Declaration of intrinsic function must match 'void __va_start(va_list* , ...)'");
            }
        }break;
        case INTRINSIC_KIND_rdtsc:{
            if(type->argument_list.count || type->return_type != &globals.typedef_u64){
                report_error(context, function->base.token, "Declaration of intrinsic function must match 'unsigned __int64 __rdtsc(void)'.");
            }
        }break;
        case INTRINSIC_KIND_pause:{
            if(type->argument_list.count || type->return_type != &globals.typedef_void){
                report_error(context, function->base.token, "Declaration of intrinsic function must match 'void _mm_pause(void)'.");
            }
        }break;
        case INTRINSIC_KIND_InterlockedCompareExchange64:{
            if(type->argument_list.count != 3){
                report_error(context, function->base.token, "Intrinsic function must have exactly three arguments.");
            }
            struct ast_type *dest      = arg_decl_to_type(type->argument_list.first->value);
            struct ast_type *value     = arg_decl_to_type(type->argument_list.first->next->value);
            struct ast_type *comperand = arg_decl_to_type(type->argument_list.last->value);
            if(dest->kind != AST_pointer_type || 
               ((struct ast_pointer_type *)dest)->pointer_to != &globals.typedef_s64 ||
               value != &globals.typedef_s64 || comperand != &globals.typedef_s64){
                report_error(context, function->base.token, "Intrinsic function must match '__int64 _InterlockedCompareExchange64(__int64 **destination, __int64 exchange, __int64 comperand)'.");
            }
        }break;
        case INTRINSIC_KIND_InterlockedCompareExchange128:{
            if(type->argument_list.count != 4){
                report_error(context, function->base.token, "Intrinsic function must have exactly three arguments.");
            }
            struct ast_type *dest       = arg_decl_to_type(type->argument_list.first->value);
            struct ast_type *value_low  = arg_decl_to_type(type->argument_list.first->next->value);
            struct ast_type *value_high = arg_decl_to_type(type->argument_list.first->next->next->value);
            struct ast_type *comperand  = arg_decl_to_type(type->argument_list.last->value);
            if(dest->kind != AST_pointer_type || 
               ((struct ast_pointer_type *)dest)->pointer_to != &globals.typedef_s64 ||
               value_low != &globals.typedef_s64 || value_high != &globals.typedef_s64 || 
               comperand->kind != AST_pointer_type || 
               ((struct ast_pointer_type *)comperand)->pointer_to != &globals.typedef_s64){
                report_error(context, function->base.token, "Intrinsic function must match '__int64 _InterlockedCompareExchange64(__int64 *destination, __int64 exchange_high, __int64 exchange_low, __int64 *comperand)'.");
            }
        }break;
        case INTRINSIC_KIND_interlocked_inc_dec_64:{
            if(type->argument_list.count != 1){
                report_error(context, function->base.token, "Intrinsic function must have exactly one argument.");
                break;
            }
            struct ast_type *arg = arg_decl_to_type(type->argument_list.first->value);
            if(arg->kind != AST_pointer_type || ((struct ast_pointer_type *)arg)->pointer_to != &globals.typedef_s64 || type->return_type != &globals.typedef_s64){
                report_error(context, function->base.token, "Intrinsic function must match '__int64 %.*s(__int64 *)'.", function->identifier->size, function->identifier->data);
            }
        }break;
        case INTRINSIC_KIND_interlocked_fetch_op_64:{
            if(type->argument_list.count != 2){
                report_error(context, function->base.token, "Intrinsic function must have exactly two argument.");
                break;
            }
            struct ast_type *dest = arg_decl_to_type(type->argument_list.first->value);
            struct ast_type *addend = arg_decl_to_type(type->argument_list.last->value);
            
            if(type->return_type != &globals.typedef_s64 || addend != &globals.typedef_s64 ||
               dest->kind != AST_pointer_type || ((struct ast_pointer_type *)dest)->pointer_to != &globals.typedef_s64){
                report_error(context, function->base.token, "Intrinsic function must match '__int64 %.*s(__int 64 *dest, __int64 addend))", function->identifier->size, function->identifier->data);
            }
        }break;        
        invalid_default_case();
    }   
#undef arg_decl_to_type
}

func struct declaration_list parse_declaration_list(struct context *context, struct ast_type *lhs_type, struct ast *lhs_defined_type){
    
    b32 should_set_sleeping_ident = !context->sleeping_ident;
    
    // @incomplete: we ignore these for now.
    // @incomplete: non function dllimports
    // storage class specifier (static, extern, auto, typedef)
    // type qualifier (const, restrict, volitile, atomic)
    // alignment qualifier (alignas)
    // function specifiers (_Noreturn, inline)
    
    b32 is_typedef = false;
    b32 is_dllimport = false;
    b32 is_extern = false;
    b32 is_static = false;
    b32 is_inline = false; // @incomplete: currently ignored
    
    if(!lhs_type){
        while(true){ // __declspec
            
            assert(!lhs_defined_type);
            if(peek_token_eat(context, TOKEN_typedef)){
                if(is_typedef) report_error(context, prev_token(context), "Double typedef.");
                is_typedef = true;
                continue;
            }else if(peek_token_eat(context, TOKEN_extern)){
                if(is_extern) report_error(context, prev_token(context), "Double extern.");
                is_extern = true;
                continue;
            }else if(peek_token_eat(context, TOKEN_static)){
                if(is_static) report_error(context, prev_token(context), "Double static.");
                is_static = true;
                continue;
            }else if(peek_token_eat(context, TOKEN___inline) || peek_token_eat(context, TOKEN_inline) || peek_token_eat(context, TOKEN___forceinline)){ // @note: '__inline' is an MSVC thing, because C89 does not have 'inline'.
                if(is_inline) report_error(context, prev_token(context), "Double inline.");
                is_inline = true;
                continue;
            }
            
            
            struct token *token = get_current_token(context);
            if(token->value == globals.keyword_declspec){
                assert(token->type == TOKEN_identifier);
                next_token(context);
                expect_token(context, TOKEN_open_paren);
                token = expect_token(context, TOKEN_identifier);
                if(token->value == globals.keyword_dllimport){
                    is_dllimport = true;
                    expect_token(context, TOKEN_closed_paren);
                }else /*if(token->value == globals.keyword_align){
                    expect_token(context, TOKEN_open_paren);
                    // @incomplete:
                    expect_token(context, TOKEN_base10_literal); // @cleanup: @hacky @incomplete:
                    expect_token(context, TOKEN_closed_paren);
                    expect_token(context, TOKEN_closed_paren);
                }else if(token->value == globals.keyword_noreturn){
                    // @incomplete:
                    expect_token(context, TOKEN_closed_paren);
                }else*/{
                    report_warning(context, token, "Unsupported declspec ignored.");
                    skip_until_tokens_are_ballanced(context, TOKEN_open_paren, TOKEN_closed_paren);
                }
            }else{
                break;
            }
        }
        
        lhs_type = parse_type_specifier(context, &lhs_defined_type); // might not be the actuall type e.g. int (*a)
    }
    
    struct declaration_list ret = zero_struct;
    if(context->should_sleep) goto end;
    
    ret.type_specifier_defined_type = lhs_defined_type;
    ret.type_specifier = lhs_type;
    if(peek_token(context, TOKEN_semicolon)) goto end; // @cleanup: the 'does not declare anything' check should be here.
    
    do{
        struct declarator_return declarator = parse_declarator(context, lhs_type, lhs_defined_type, 0);
        if(should_set_sleeping_ident) context->sleeping_ident = declarator.ident;
        if(context->should_sleep){
            // @cleanup: set waiting ident?
            goto end;
        }
        
        // :unresolved_types
        if(declarator.type->kind == AST_unresolved_type){
            struct type_info_return resolved = resolve_unresolved_type_or_sleep(context, declarator.type);
            if(resolved.type){
                declarator.type = resolved.type;
                declarator.defined_type = resolved.defined_type;
            }else{
                assert(context->should_sleep && !context->error);
                if(!is_typedef){
                    goto end;
                }else{
                    context->should_sleep = false;
                }
            }
        }
        
        b32 should_break = false;
        struct ast_declaration *decl;
        if(!is_typedef && declarator.type->kind == AST_function_type){
            
            {   // :unresolved_types
                // we have to check here whether the return type is unresolved, because we want to disallow
                //     struct unresolved asd(){}
                // especially if 'unresolved' was just misspelled type or something.
                struct ast_function_type *function_type = cast(struct ast_function_type *)declarator.type;
                if(function_type->return_type->kind == AST_unresolved_type){
                    struct type_info_return resolved = resolve_unresolved_type_or_sleep(context, function_type->return_type);
                    if(resolved.type){
                        function_type->return_type = resolved.type;
                        function_type->return_type_defined_type = resolved.defined_type;
                    }else{
                        assert(context->should_sleep);
                        goto end;
                    }
                }
            }
            
            struct ast_function *function = parser_ast_push(context, function);
            function->type = cast(struct ast_function_type *)declarator.type;
            function->base.token = declarator.ident;
            function->identifier = declarator.ident->value;
            function->memory_location = null;
            function->offset_in_text_section = -1; // set atomically when done emitting
            
            // check if this function is intrinsic
            struct intrinsic_info *function_is_intrinsic = lookup_intrinsic(function->identifier);
            if(function_is_intrinsic){
                function->type->flags |= FUNCTION_TYPE_FLAGS_is_intrinsic;
                function->is_defined = function;
                
                validate_intrinsic(context, function_is_intrinsic, function);
                if(context->should_sleep) goto end;
            }
            
            if(is_dllimport){
                if(function_is_intrinsic){
                    report_error(context, function->base.token, "Intrinsic function cannot be declared 'dllimport'.");
                    goto end;
                }
                // we check that this declaration is actually contained in some dll only for the ones we actually use. this is done in 'explain.c'
                function->type->flags |= FUNCTION_TYPE_FLAGS_is_dllimport;
                
                // @note: there is no need for this to be atomic, as we currently _own_ this declaration.
                function->is_defined = function;
            }else if(peek_token(context, TOKEN_open_curly)){
                if(function_is_intrinsic){
                    report_error(context, function->base.token, "Intrinsic function cannot be defined.");
                    goto end;
                }
                
                if(context->current_scope && !is_static){
                    report_error(context, declarator.ident, "Locally defined function has to be static.");
                    goto end;
                }
                
                if(!sll_is_empty(ret)){
                    report_error(context, get_current_token(context), "Cannot define a function in a compound declaration list.");
                    return ret;
                }
                // @note: there is no need for this to be atomic, as we currently own this declaration.
                function->is_defined = function;
                
                // @note: we push the work outside if its global.
                if(context->current_scope){
                    next_token(context);
                    // @note: if we are at function scope compilation either failes or workes,
                    //        there is no sleeping.
                    //        so we can just fuck with the token array.
                    struct token_marker begin_marker = get_current_token_marker(context);
                    
                    if(!skip_until_tokens_are_ballanced(context, TOKEN_open_curly, TOKEN_closed_curly)){
                        goto end;
                    }
                    
                    struct token_marker end_marker = get_current_token_marker(context);
                    struct token_bucket_array replace_with = zero_struct;
                    struct token_bucket *past_end = replace_token_range(context, end_marker, end_marker, replace_with);
                    
                    struct token_bucket *last_bucket_in_local_function = past_end->prev;
                    last_bucket_in_local_function->next = null;
                    
                    struct parse_work *parse_work = push_struct(context->arena, struct parse_work);
                    parse_work->function = function;
                    parse_work->tokens.first = begin_marker.bucket;
                    parse_work->tokens.last  = last_bucket_in_local_function;
                    parse_work->start_at = begin_marker.token_at;
                    
                    work_queue_push_work(context, &globals.work_queue_stage_two, WORK_parse_function_body, parse_work);
                    
                    context->current_token_bucket = past_end;
                    context->token_at = 0;
                }
                
                if(context->current_scope){
                    // if we are not at global scope this is a local function
                    function->as_decl.flags |= DECLARATION_FLAGS_is_local_persist;
                    ast_list_append(&globals.local_functions, context->arena, &function->base);
                }
                
                should_break = true; // disallow int a(){}, b;
            }
            
            
            // for functions this might not have been the first declaration (and maybe in the future the same holds)
            // for everything else, because of 'extern', se we might have to 'replace' the declaration with the
            // one that was already there.
            decl = parser_register_declaration(context, &function->as_decl);
        }else{
            decl = push_declaration_for_declarator(context, declarator);
            if(is_typedef){
                decl->base.kind = AST_typedef;
            }
            
            // register it up here already in case of 'void *asd = &asd;' @cleanup: threading? :threading
            parser_register_declaration(context, decl);
            
            if(peek_token(context, TOKEN_equals)){
                struct token *equals = next_token(context);
                if(is_typedef){
                    // :Error
                    report_error(context, equals, "Cannot initialize a type.");
                    goto end;
                }
                
                if(decl->type == &globals.typedef_void){
                    report_error(context, equals, "Cannot assign to variable of type 'void'.");
                    goto end;
                }
                
                struct ast_identifier *ident = parser_ast_push(context, identifier);
                ident->base.token = declarator.ident;
                ident->decl = decl;
                set_resolved_type(&ident->base, decl->type, decl->defined_type);
                
                struct ast *rhs = parse_initializer(context, &ident->base, equals);
                decl->assign_expr = rhs;
                
                if(context->should_sleep) goto end;
            }else{
                // if it's an array of unknown size (u32 arr[];) that does not have an assignment, thats an error
                if(decl->type->kind == AST_array_type){
                    struct ast_array_type *arr = cast(struct ast_array_type *)decl->type;
                    if(arr->is_of_unknown_size){
                        // :Error
                        report_error(context, declarator.ident, "Size is unknown for this declaration.");
                        return ret;
                    }
                }
            }
            
            if(context->current_scope && is_static){
                // @copy and paste from 'parser_do_work'
#if ENABLE_DEAD_OLD_IN_MEMORY_EXECUTION_CODE_PATHS
                if(!globals.want_executable){
                    assert(decl->type->size >= 0);
                    if(decl->type == &globals.typedef_void){
                        // @note: we currently allow declarations of type void as long as they are not initialized.
                        //        but 'alignof(void) == 0', and this asserts in 'push_struct_'.
                        decl->memory_location = arena_current(context->emit_arena);
                    }else{
                        decl->memory_location = push_struct_(context->emit_arena, decl->type->size, (u32)decl->type->alignment);
                    }
                }
#endif

                ast_list_append(&context->current_function->static_variables, context->arena, &decl->base);
                decl->flags |= DECLARATION_FLAGS_is_local_persist;
            }
        }
        
        if(!context->current_scope) decl->flags |= DECLARATION_FLAGS_is_global;
        set_resolved_type(&decl->base, &globals.typedef_void, null);
        
        // this is in arena, as we now have 'ast_declaration_list', maybe this is not necessary, as we dont
        // use 'ast_declaration_list' at global scope. @leak
        struct declaration_node *node = push_struct(context->arena, struct declaration_node);
        node->decl = decl;
        sll_push_back(ret, node);
        
        // don't allow int a(){}, b(){} because thats stupid
        if(should_break) break;
    }while(peek_token_eat(context, TOKEN_comma));
    
    end:;
    return ret;
}

func struct ast_scope *parser_push_new_scope(struct context *context, enum scope_flags flags){
    struct ast_scope *scope = parser_ast_push(context, scope);
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

func struct ast *parse_imperative_scope(struct context *context);
func struct ast *parse_statement(struct context *context){
    struct ast *ret = null;
    b32 needs_semicolon = true;
    
    struct token *initial_token = next_token(context); // returns the current token
    switch(initial_token->type){
        case TOKEN_semicolon:{
            // the token has to be correct, so we can set the right line number
            struct ast *empty_statement = _parser_ast_push(context, initial_token, sizeof(struct ast), alignof(struct ast), AST_empty_statement);
            set_resolved_type(empty_statement, &globals.typedef_void, null);
            ret = empty_statement;
            needs_semicolon = false;
        }break;
        case TOKEN_if:{
            struct ast_if *ast_if = parser_ast_push(context, if);
            expect_token(context, TOKEN_open_paren);
            ast_if->condition = parse_expression(context, false);
            if(!casts_implicitly_to_bool(ast_if->condition)){
                // :Error
                report_error(context, ast_if->condition->token, "'if' condition has to cast to bool.");
                return &ast_if->base;
            }
            expect_token(context, TOKEN_closed_paren);
            
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
            
            struct ast_for *ast_for = parser_ast_push(context, for);
            expect_token(context, TOKEN_open_paren);
            
            struct ast_scope *scope = parser_push_new_scope(context, SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            ast_for->scope_for_decl = scope;
            {
                if(!peek_token_eat(context, TOKEN_semicolon)){
                    // @hack: @cleanup: this should be either a declaration or an expression, for now any statement is valid....
                    //ast_for->decl = parse_declaration(context);
                    ast_for->decl = parse_statement(context);
                    
                    //parser_register_declaration(context, cast(struct ast_declaration *)ast_for->decl);
                    //expect_token(context, TOKEN_semicolon);
                }
                
                if(peek_token_eat(context, TOKEN_semicolon)){
                    ast_for->condition = ast_push_s32_literal(context, 1); // desugars to true
                }else{
                    ast_for->condition = parse_expression(context, false);
                    if(!casts_implicitly_to_bool(ast_for->condition)){
                        // :Error
                        report_error(context, ast_for->condition->token, "'for' condition has to cast to bool.");
                        parser_scope_pop(context, scope);
                        return &ast_for->base;
                    }
                    expect_token(context, TOKEN_semicolon);
                }
                
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    ast_for->increment = parse_expression(context, false);
                    expect_token(context, TOKEN_closed_paren);
                }
                
                ast_for->body = parse_statement(context);
            }
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
            set_resolved_type(&ast_for->base, &globals.typedef_void, null);
            ret = &ast_for->base;
        }break;
        case TOKEN_while:{
            // @note: we desugar 'while' to 'for(;condition;)'
            struct ast_for *ast_while = parser_ast_push(context, for);
            
            expect_token(context, TOKEN_open_paren);
            ast_while->condition = parse_expression(context, false);
            if(!casts_implicitly_to_bool(ast_while->condition)){
                report_error(context, ast_while->condition->token, "'while' condtition has to cast to bool.");
                return &ast_while->base;
            }
            expect_token(context, TOKEN_closed_paren);
            
            struct ast_scope *scope = parser_push_new_scope(context, SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            ast_while->scope_for_decl = scope;
            {
                ast_while->body = parse_statement(context);
            }
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
            set_resolved_type(&ast_while->base, &globals.typedef_void, null);
            ret = &ast_while->base;
        }break;
        case TOKEN_do:{
            // :AST_do_while
            struct ast_for *do_while = parser_ast_push(context, for);
            do_while->base.kind = AST_do_while;
            
            struct ast_scope *scope = parser_push_new_scope(context, SCOPE_FLAG_can_continue | SCOPE_FLAG_can_break);
            do_while->scope_for_decl = scope;
            do_while->body = parse_statement(context);
            parser_scope_pop(context, scope);
            
            expect_token(context, TOKEN_while);
            expect_token(context, TOKEN_open_paren);
            do_while->condition = parse_expression(context, false);
            
            if(!casts_implicitly_to_bool(do_while->condition)){
                report_error(context, do_while->condition->token, "'while' condtition has to cast to bool.");
                return &do_while->base;
            }
            expect_token(context, TOKEN_closed_paren);
            set_resolved_type(&do_while->base, &globals.typedef_void, null);
            ret = &do_while->base;
        }break;
        case TOKEN_break:{
            struct ast_break *ast_break = parser_ast_push(context, break);
            
            struct ast_scope *scope = context->current_scope;
            while(scope){
                if(scope->flags & SCOPE_FLAG_can_break){
                    break;
                }
                scope = scope->parent;
            }
            
            if(!scope){
                report_error(context, initial_token, "'break' is not inside of a breakable scope");
                return &ast_break->base;
            }
            ast_break->scope_to_break = scope;
            
            set_resolved_type(&ast_break->base, &globals.typedef_void, null);
            ret = &ast_break->base;
        }break;
        case TOKEN_continue:{
            struct ast_continue *ast_continue = parser_ast_push(context, continue);
            
            struct ast_scope *scope = context->current_scope;
            while(scope){
                if(scope->flags & SCOPE_FLAG_can_break){
                    break;
                }
                scope = scope->parent;
            }
            
            if(!scope){
                report_error(context, initial_token, "'continue' is not inside of a continueable scope");
                return &ast_continue->base;
            }
            ast_continue->scope_to_continue = scope;
            
            set_resolved_type(&ast_continue->base, &globals.typedef_void, null);
            ret = &ast_continue->base;
        }break;
        case TOKEN_switch:{
            struct ast_switch *ast_switch = parser_ast_push(context, switch);
            expect_token(context, TOKEN_open_paren);
            ast_switch->switch_on = parse_expression(context, false);
            
            if(ast_switch->switch_on->resolved_type->kind != AST_integer_type){
                report_error(context, ast_switch->base.token, "Can only switch on integers.");
                return &ast_switch->base;
            }
            
            // "The integer promotions are performed on the controlling expression"
            ast_switch->switch_on = maybe_insert_integer_promotion_cast(context, ast_switch->switch_on);
            
            expect_token(context, TOKEN_closed_paren);
            
            struct ast_switch *previous_switch = context->current_switch;
            context->current_switch = ast_switch;
            
            struct ast_scope *scope = parser_push_new_scope(context, SCOPE_FLAG_can_break);
            ast_switch->statement = parse_statement(context);
            parser_scope_pop(context, scope);
            
            context->current_switch = previous_switch;
            
            set_resolved_type(&ast_switch->base, &globals.typedef_void, null);
            needs_semicolon = false;
            ret = &ast_switch->base;
        }break;
        case TOKEN_case:{
            if(!context->current_switch){
                report_error(context, initial_token, "'case' is only allowed within 'switch'-statement.");
                return &context->empty_statement;
            }
            
            struct ast *const_expr = parse_expression(context, false);
            if(const_expr->kind != AST_integer_literal){
                report_error(context, const_expr->token, "operand of 'case' has to evaluate to a constant.");
                return const_expr;
            }
            
            struct ast *switch_on = context->current_switch->switch_on;
            struct ast *promoted_const_expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, switch_on->resolved_type, switch_on->defined_type, const_expr, const_expr->token);
            assert(promoted_const_expr->kind == AST_integer_literal);
            assert(promoted_const_expr->resolved_type == context->current_switch->switch_on->resolved_type);
            
            struct ast_case *ast_case = parser_ast_push(context, case);
            ast_case->expression = promoted_const_expr;
            expect_token(context, TOKEN_colon);
            ast_list_append(&context->current_switch->case_list, context->arena, &ast_case->base);
            
            set_resolved_type(&ast_case->base, &globals.typedef_void, null);
            needs_semicolon = false;
            ret = &ast_case->base;
        }break;
        case TOKEN_default:{
            if(!context->current_switch){
                report_error(context, initial_token, "'default' is only allowed within 'switch'-statement.");
                return &context->empty_statement;
            }
            
            struct ast_case *ast_case = parser_ast_push(context, case);
            expect_token(context, TOKEN_colon);
            set_resolved_type(&ast_case->base, &globals.typedef_void, null);
            needs_semicolon = false;
            context->current_switch->default_case = ast_case;
            
            ret = &ast_case->base;
        }break;
        case TOKEN_else:{
            report_error(context, initial_token, "'else' without matching 'if'.");
            return &context->empty_statement;
        }break;
        case TOKEN_return:{
            struct ast_return *ast_return = parser_ast_push(context, return);
            struct ast_function_type *current_function_type = context->current_function->type;
            if(peek_token(context, TOKEN_semicolon)){
                if(current_function_type->return_type != &globals.typedef_void){
                    struct string type_string = push_type_string(context->arena, &context->scratch, current_function_type->return_type);
                    report_error(context, get_current_token(context), "expected an expression after 'return' in function of type '%s'", type_string.data);
                    return &ast_return->base;
                }
            }else{
                ast_return->expr = parse_expression(context, false);
                ast_return->expr = maybe_load_address_for_array_or_function(context, ast_return->expr);
                ast_return->expr = maybe_insert_implicit_assignment_cast_and_check_that_types_match(context, current_function_type->return_type, current_function_type->return_type_defined_type, ast_return->expr, ast_return->expr->token);

            }
            set_resolved_type(&ast_return->base, &globals.typedef_void, null);
            ret = &ast_return->base;
        }break;
        case TOKEN_open_curly:{
            struct ast_scope *scope = parser_push_new_scope(context, SCOPE_FLAG_none);
            ret = cast(struct ast *)parse_imperative_scope(context);
            parser_scope_pop(context, scope);
            
            needs_semicolon = false;
        }break;
        case TOKEN_closed_curly:{
            return null; // we are done
        }break;
        case TOKEN_goto:{
            struct ast_goto *ast_goto = parser_ast_push(context, goto);
            struct token *ident = expect_token(context, TOKEN_identifier);
            ast_goto->ident = ident->value;
            
            ast_list_append(&context->goto_list, context->arena, &ast_goto->base);
            ret = &ast_goto->base;
            set_resolved_type(&ast_goto->base, &globals.typedef_void, null);
        }break;
        case TOKEN_identifier:{
            if(peek_token_eat(context, TOKEN_colon)){
                unique_string ident = initial_token->value;
                
                for_ast_list(context->label_list){
                    struct ast_label *label = cast(struct ast_label *)it->value;
                    if(label->ident == ident){
                        report_error(context, initial_token, "Redefinition of label '%.*s'.", ident->amount, ident->data);
                        return &label->base;
                    }
                }
                struct ast_label *label = parser_ast_push(context, label);
                label->ident = ident;
                
                ast_list_append(&context->label_list, context->arena, &label->base);
                
                needs_semicolon = false;
                set_resolved_type(&label->base, &globals.typedef_void, null);
                ret = &label->base;
                break;
            }
            
            if(initial_token->value == globals.keyword_declspec){
                prev_token(context);
                ret = parse_declaration_in_imperative_scope(context, null, null);
                break;
            }
            
            struct ast_declaration *lookup = lookup_typedef(context, initial_token, true);
            if(lookup){
                ret = parse_declaration_in_imperative_scope(context, lookup->type, lookup->defined_type);
                break;
            }
            
            prev_token(context);
            ret = parse_expression(context, false);
        }break;
        
        case TOKEN_const:  case TOKEN_volatile: case TOKEN_static: case TOKEN_extern: case TOKEN_typedef:
        case TOKEN_signed: case TOKEN_unsigned:
        case TOKEN_void:   case TOKEN_Bool: case TOKEN_char: case TOKEN_short: case TOKEN_int: case TOKEN_long:
        case TOKEN_int8:   case TOKEN_int16: case TOKEN_int32: case TOKEN_int64:
        case TOKEN_float:  case TOKEN_double: 
        case TOKEN_struct: case TOKEN_union: case TOKEN_enum:{
            prev_token(context);
            ret = parse_declaration_in_imperative_scope(context, null, null);
            if(ret->kind == AST_function) needs_semicolon = false;
        }break;
        
        default:{
            // this should not be assert(false): take for example +value; that is a valid statement
            
            prev_token(context);
            // if it is nothing else; it's gotta be an assignment or expression.
            ret = parse_expression(context, false);
        }break;
    }
    
    if(needs_semicolon){
        expect_token(context, TOKEN_semicolon);
    }
    
    assert(ret);
    return ret;
}

// @note: we assume the TOKEN_open_curly was allread consumed
func struct ast *parse_imperative_scope(struct context *context){
    
    struct ast_scope *scope = context->current_scope;
    
    while(!context->should_sleep){
        struct ast *stmt = parse_statement(context);
        if(!stmt) break;
        
        assert(scope == context->current_scope);
        ast_list_append(&scope->statement_list, context->arena, stmt);
    }
    
    return cast(struct ast *)scope;
}

func struct declarator_return parse_declarator(struct context*context, struct ast_type *_initial_type, struct ast *_initial_defined_type, enum abstract_flags abstract_flags){
    struct declarator_return ret = zero_struct;
    ret.type = _initial_type;
    ret.defined_type = _initial_defined_type;
    ret.ident = context->invalid_identifier;
    
    
    while(true){
        struct token *begin = get_current_token(context);
        // @cleanup: @hack @we ignore all these right now
        if(get_current_token(context)->value == globals.keyword_cdecl){
            ret.declarator_flags |= DECLARATOR_is_cdecl;
            next_token(context);
        }
        
        if(get_current_token(context)->value == globals.keyword_stdcall){
            ret.declarator_flags |= DECLARATOR_is_stdcall;
            next_token(context);
        }
        
        if(get_current_token(context)->type == TOKEN_const){
            ret.declarator_flags |= DECLARATOR_is_const;
            next_token(context);
        }
        
        if(get_current_token(context)->type == TOKEN_volatile){
            ret.declarator_flags |= DECLARATOR_is_volatile;
            next_token(context);
        }
        
        
        // optional pointer (makes int* a the same as int (*a))
        while(get_current_token(context)->type == TOKEN_times){
            next_token(context);
            ret.type = parser_push_pointer_type(context, ret.type, ret.defined_type);
            ret.defined_type = null;
        }
        
        // we did not advance the token, therefore we didn't do anything -> break.
        if(get_current_token(context) == begin) break; 
        
    }
    b32 got_identifier = false;
    
    // (declarator)
    struct token_marker begin_marker = get_current_token_marker(context);
    struct token *initial_token = get_current_token(context);
    if(peek_token_eat(context, TOKEN_open_paren)){
        if(!skip_until_tokens_are_ballanced(context, TOKEN_open_paren, TOKEN_closed_paren)){
            return ret;
        }
    }else if(peek_token(context, TOKEN_identifier)){
        got_identifier = true;
        ret.ident = next_token(context);
        if(abstract_flags & ABSTRACT_no_identifier){
            // :Error
            report_error(context, prev_token(context), "Identifier in an abstract declarator.");
            return ret;
        }
    }else{
        if(abstract_flags & (ABSTRACT_no_identifier | ABSTRACT_allow_identifier)){
            // we are fine if we dont require an identifier. this is the 'void asd(int[4])' case.
            got_identifier = true;
        }else{
            // :Error
            report_error(context, initial_token, "Exepected a '(' or an identifier in a declarator.");
            return ret;
        }
    }
    
    while(in_current_token_array(context)){
        switch(get_current_token(context)->type){
            case TOKEN_open_paren:{// declarator(parameter list)
                next_token(context);
                if(ret.type->kind == AST_function_type){
                    report_error(context, get_current_token(context), "function returning function is illegal.");
                    return ret;
                }else if(ret.type->kind == AST_array_type){
                    report_error(context, get_current_token(context), "function returning array is illegal.");
                    return ret;
                }
                struct ast_function_type *function = parser_type_push(context, function_type);
                function->return_type = ret.type;
                
                if(peek_token_eat(context, TOKEN_void)){
                    if(peek_token(context, TOKEN_closed_paren)){
                        // this is fine we will not pass the if below
                    }else{
                        // if it is only (void) then we treat it as if there are no arguments
                        prev_token(context);
                    }
                }
                
                if(!peek_token_eat(context, TOKEN_closed_paren)){
                    do{
                        if(peek_token_eat(context, TOKEN_dotdotdot)){
                            function->flags |= FUNCTION_TYPE_FLAGS_is_varargs;
                            // :Error varargs must be the last thing
                            break;
                        }
                        
                        struct ast *defined_type = null;
                        struct ast_type *lhs_type = parse_type_specifier(context, &defined_type);
                        struct declarator_return declarator = parse_declarator(context, lhs_type, defined_type, ABSTRACT_allow_identifier);
                        if(declarator.type->kind == AST_array_type){
                            // "A declaration of a parameter as 'array of type' shall be adjusted to 
                            // 'qualified pointer to type' [...]."
                            struct ast_array_type *array = cast(struct ast_array_type *)declarator.type;
                            declarator.type = parser_push_pointer_type(context, array->element_type, array->element_type_defined_type);
                            if(!declarator.defined_type){
                                declarator.defined_type = cast(struct ast *)array;
                            }
                        }else if(declarator.type->kind == AST_function_type){
                            // "A declaration of a parameter as 'function returning type' shall be adjusted to 
                            //  'pointer to  function returning type'.
                            declarator.type = parser_push_pointer_type(context, declarator.type, null);
                        }
                        
                        struct ast_declaration *decl = push_declaration_for_declarator(context, declarator);
                        
                        ast_list_append(&function->argument_list, context->arena, &decl->base);
                        
                        if(decl->assign_expr){ // @cleanup: can parse_declaration return null?
                            report_error(context, prev_token(context), "Assignment in function declaration. Default arguments are invalid.");
                            return ret;
                        }
                        
                        if(decl->type->kind == AST_struct || decl->type->kind == AST_union){
                            if(decl->type->size > 8) decl->flags |= DECLARATION_FLAGS_is_big_function_argument;
                        }
                        
                    }while(peek_token_eat(context, TOKEN_comma));
                    expect_token(context, TOKEN_closed_paren);
                }
                ret.type = cast(struct ast_type *)function;
                ret.defined_type = null;
            }break;
            case TOKEN_open_index:{  // declarator[???]
                struct ast_array_type *arr = parser_type_push(context, array_type);
                
                next_token(context);
                if(peek_token_eat(context, TOKEN_closed_index)){ // u32 arr[];
                    arr->amount_of_elements = 0;
                    arr->is_of_unknown_size = true;
                    // :array_sizes
                    arr->base.size = 0;
                }else{
                    struct ast *index_expression = parse_expression(context, false);
                    if(index_expression->kind != AST_integer_literal){
                        report_error(context, arr->base.token, "Expected a constant expression in '[]'.");
                        return ret;
                    }
                    
                    s64 value = integer_literal_as_s64(index_expression);
                    if(value < 0){
                        report_error(context, arr->base.token, "Negative subscript is not allowed.");
                        return ret;
                    }
                    arr->amount_of_elements = value;
                    expect_token(context, TOKEN_closed_index);
                    // :array_sizes
                    arr->base.size = ret.type->size * value;
                }
                arr->base.alignment = ret.type->alignment;
                arr->element_type = ret.type;
                if(ret.type == &globals.typedef_void){
                    report_error(context, arr->base.token, "Array of type void is invalid.");
                    return ret;
                }
                if(ret.type->kind == AST_function_type){
                    report_error(context, arr->base.token, "Array of functions is invalid, please use function pointers.");
                    return ret;
                }
                
                ret.type = cast(struct ast_type *)arr;
                ret.defined_type = null;
            }break;
            default: goto double_break;
        }
    }
    
    double_break:;
    if(got_identifier){ 
        // fast path, if we already got an identifier (or we don't want one) no need for the token dance
        return ret;
    }
    
    struct token_marker end_marker = get_current_token_marker(context);
    apply_token_marker(context, begin_marker);
    
    switch(get_current_token(context)->type){
        case TOKEN_identifier:{
            ret.ident = get_current_token(context);
        }break;
        case TOKEN_open_paren:{
            next_token(context);
            ret = parse_declarator(context, ret.type, ret.defined_type, abstract_flags);
            expect_token(context, TOKEN_closed_paren);
        }break;
        default:{
            report_error(context, get_current_token(context), "unexpected token in declaration");
            ret.ident = context->invalid_identifier;
            return ret;
        }break;
    }
    
    apply_token_marker(context, end_marker);// past the declaration, but before the semicolon I guess
    
    return ret;
}

