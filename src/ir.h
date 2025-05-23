

enum ir_type{
    IR_TYPE_void,
    IR_TYPE_bool,
    
    IR_TYPE_s8, 
    IR_TYPE_u8, 
    
    IR_TYPE_s16, 
    IR_TYPE_u16, 
    
    IR_TYPE_s32,
    IR_TYPE_u32,
    
    IR_TYPE_s64, 
    IR_TYPE_u64,
    
    IR_TYPE_f32,
    IR_TYPE_f64,
    
    IR_TYPE_padding1,
    IR_TYPE_padding2,
    
    IR_TYPE_padding3,
    IR_TYPE_atomic_bool,
    
    IR_TYPE_atomic_s8, 
    IR_TYPE_atomic_u8, 
    
    IR_TYPE_atomic_s16, 
    IR_TYPE_atomic_u16, 
    
    IR_TYPE_atomic_s32,
    IR_TYPE_atomic_u32,
    
    IR_TYPE_atomic_s64, 
    IR_TYPE_atomic_u64,
    
    IR_TYPE_pointer,
};

enum ir_kind{
    
    IR_start = AST_count, // For now!
    
    IR_identifier,
    IR_string_literal,
    IR_integer_literal,
    IR_float_literal,
    IR_compound_literal,
    
    IR_pointer_literal,
    IR_pointer_literal_deref,
    
    IR_pop_expression,
    
    IR_duplicate,
    IR_duplicate_lhs,
    
    IR_swap_lhs_rhs,
    IR_skip,
    
    IR_jump_label,
    IR_jump,
    IR_jump_if_true,
    IR_jump_if_false,
    
    IR_switch,
    IR_case,
    
    IR_load_bitfield,
    IR_load_bitfield_lhs,
    IR_load_address,
    IR_load_address_lhs,
    
    IR_store_bitfield,
    IR_store_atomic,
    IR_store,
    
    IR_memcpy,
    
    IR_postinc_u8,
    IR_postinc_u16,
    IR_postinc_u32,
    IR_postinc_u64,
    
    IR_postinc_bool,
    IR_postinc_f32,
    IR_postinc_f64,
    
    IR_postinc_pointer,
    IR_postinc_bitfield,
    
    IR_postdec_u8,
    IR_postdec_u16,
    IR_postdec_u32,
    IR_postdec_u64,
    
    IR_postdec_bool,
    IR_postdec_f32,
    IR_postdec_f64,
    
    IR_postdec_pointer,
    IR_postdec_bitfield,
    
    IR_preinc_u8,
    IR_preinc_u16,
    IR_preinc_u32,
    IR_preinc_u64,
    
    IR_preinc_bool,
    IR_preinc_f32,
    IR_preinc_f64,
    
    IR_preinc_pointer,
    IR_preinc_bitfield,
    
    IR_predec_u8,
    IR_predec_u16,
    IR_predec_u32,
    IR_predec_u64,
    
    IR_predec_bool,
    IR_predec_f32,
    IR_predec_f64,
    
    IR_predec_pointer,
    IR_predec_bitfield,
    
    IR_deref,
    IR_member,
    IR_member_deref,
    
    IR_pointer_subscript,
    IR_array_subscript,
    
    IR_function_call,
    IR_return,
    
    IR_bitwise_not_u32,
    IR_bitwise_not_u64,
    
    IR_negate_u32,
    IR_negate_u64,
    IR_negate_f32,
    IR_negate_f64,
    
    IR_logical_not_u32,
    IR_logical_not_u64,
    IR_logical_not_f32,
    IR_logical_not_f64,
    
    IR_or_u32,
    IR_or_u64,
    
    IR_xor_u32,
    IR_xor_u64,
    
    IR_and_u32,
    IR_and_u64,
    
    IR_left_shift_s32,
    IR_left_shift_u32,
    IR_left_shift_s64,
    IR_left_shift_u64,
    
    IR_right_shift_s32,
    IR_right_shift_u32,
    IR_right_shift_s64,
    IR_right_shift_u64,
    
    IR_add_u32,
    IR_add_u64,
    IR_add_f32,
    IR_add_f64,
    
    IR_subtract_u32,
    IR_subtract_u64,
    IR_subtract_f32,
    IR_subtract_f64,
    
    // @note: Order matches ir_type.
    IR_multiply_s32,
    IR_multiply_u32,
    IR_multiply_s64,
    IR_multiply_u64,
    IR_multiply_f32,
    IR_multiply_f64,
    
    IR_divide_s32,
    IR_divide_u32,
    IR_divide_s64,
    IR_divide_u64,
    IR_divide_f32,
    IR_divide_f64,
    
    IR_mod_s32,
    IR_mod_u32,
    IR_mod_s64,
    IR_mod_u64,
    
    
    // :smaller_bigger_ir_encoding
    // Sorting is important here, as we _calculate_ which ir node to use.
    IR_bigger_equals_s32,
    IR_smaller_equals_s32,
    IR_bigger_s32,
    IR_smaller_s32,
    
    IR_bigger_equals_u32,
    IR_smaller_equals_u32,
    IR_bigger_u32,
    IR_smaller_u32,
    
    IR_bigger_equals_s64,
    IR_smaller_equals_s64,
    IR_bigger_s64,
    IR_smaller_s64,
    
    IR_bigger_equals_u64,
    IR_smaller_equals_u64,
    IR_bigger_u64,
    IR_smaller_u64,
    
    IR_bigger_equals_f32,
    IR_smaller_equals_f32,
    IR_bigger_f32,
    IR_smaller_f32,
    
    IR_bigger_equals_f64,
    IR_smaller_equals_f64,
    IR_bigger_f64,
    IR_smaller_f64,
    
    // 
    // We use the order of these as well.
    // 
    IR_equals_u32,
    IR_unequals_u32,
    
    IR_equals_u64,
    IR_unequals_u64,
    
    IR_equals_f32,
    IR_unequals_f32,
    
    IR_equals_f64,
    IR_unequals_f64,
    
    IR_add_assignment_u8,
    IR_add_assignment_u16,
    IR_add_assignment_u32,
    IR_add_assignment_u64,
    
    IR_add_assignment_f32,
    IR_add_assignment_f64,
    
    IR_add_atomic_assignment_bool,
    IR_add_atomic_assignment_u8,
    IR_add_atomic_assignment_u16,
    IR_add_atomic_assignment_u32,
    IR_add_atomic_assignment_u64,
    
    IR_subtract_assignment_u8,
    IR_subtract_assignment_u16,
    IR_subtract_assignment_u32,
    IR_subtract_assignment_u64,
    
    IR_subtract_assignment_f32,
    IR_subtract_assignment_f64,
    
    IR_subtract_atomic_assignment_bool,
    IR_subtract_atomic_assignment_u8,
    IR_subtract_atomic_assignment_u16,
    IR_subtract_atomic_assignment_u32,
    IR_subtract_atomic_assignment_u64,
    
    IR_and_assignment_u8,
    IR_and_assignment_u16,
    IR_and_assignment_u32,
    IR_and_assignment_u64,
    
    IR_and_atomic_assignment_u8,
    IR_and_atomic_assignment_u16,
    IR_and_atomic_assignment_u32,
    IR_and_atomic_assignment_u64,
    
    IR_or_assignment_u8,
    IR_or_assignment_u16,
    IR_or_assignment_u32,
    IR_or_assignment_u64,
    
    IR_or_atomic_assignment_u8,
    IR_or_atomic_assignment_u16,
    IR_or_atomic_assignment_u32,
    IR_or_atomic_assignment_u64,
    
    IR_xor_assignment_u8,
    IR_xor_assignment_u16,
    IR_xor_assignment_u32,
    IR_xor_assignment_u64,
    
    IR_xor_atomic_assignment_u8,
    IR_xor_atomic_assignment_u16,
    IR_xor_atomic_assignment_u32,
    IR_xor_atomic_assignment_u64,
    
    IR_multiply_assignment_s8,
    IR_multiply_assignment_u8,
    IR_multiply_assignment_s16,
    IR_multiply_assignment_u16,
    IR_multiply_assignment_s32,
    IR_multiply_assignment_u32,
    IR_multiply_assignment_s64,
    IR_multiply_assignment_u64,
    
    IR_multiply_assignment_f32,
    IR_multiply_assignment_f64,
    
    IR_divide_assignment_s8,
    IR_divide_assignment_u8,
    IR_divide_assignment_s16,
    IR_divide_assignment_u16,
    IR_divide_assignment_s32,
    IR_divide_assignment_u32,
    IR_divide_assignment_s64,
    IR_divide_assignment_u64,
    
    IR_divide_assignment_f32,
    IR_divide_assignment_f64,
    
    IR_modulo_assignment_s8,
    IR_modulo_assignment_u8,
    IR_modulo_assignment_s16,
    IR_modulo_assignment_u16,
    IR_modulo_assignment_s32,
    IR_modulo_assignment_u32,
    IR_modulo_assignment_s64,
    IR_modulo_assignment_u64,
    
    IR_left_shift_assignment_s8,
    IR_left_shift_assignment_u8,
    IR_left_shift_assignment_s16,
    IR_left_shift_assignment_u16,
    IR_left_shift_assignment_s32,
    IR_left_shift_assignment_u32,
    IR_left_shift_assignment_s64,
    IR_left_shift_assignment_u64,
    
    IR_right_shift_assignment_s8,
    IR_right_shift_assignment_u8,
    IR_right_shift_assignment_s16,
    IR_right_shift_assignment_u16,
    IR_right_shift_assignment_s32,
    IR_right_shift_assignment_u32,
    IR_right_shift_assignment_s64,
    IR_right_shift_assignment_u64,
    
    IR_nop,
    IR_temp,
    IR_embed,
    IR_initializer,
    IR_asm_block,
    
    IR_cast_to_void,
    IR_cast_to_bool,
    IR_cast_to_bool_lhs,
    
    // Casts are at the end here, so that we can use math to encode the operands.
    IR_cast_f32_to_f64,
    IR_cast_base = IR_cast_f32_to_f64,
    
    IR_cast_f32_to_f64_lhs,
    IR_cast_f64_to_f32,
    IR_cast_f64_to_f32_lhs,
    
    IR_cast_f32_to_u8,
    IR_cast_f32_to_u8_lhs,
    IR_cast_f32_to_u16,
    IR_cast_f32_to_u16_lhs,
    IR_cast_f32_to_u32,
    IR_cast_f32_to_u32_lhs,
    IR_cast_f32_to_u64,
    IR_cast_f32_to_u64_lhs,
    
    IR_cast_f32_to_s8,
    IR_cast_f32_to_s8_lhs,
    IR_cast_f32_to_s16,
    IR_cast_f32_to_s16_lhs,
    IR_cast_f32_to_s32,
    IR_cast_f32_to_s32_lhs,
    IR_cast_f32_to_s64,
    IR_cast_f32_to_s64_lhs,
    
    IR_cast_f64_to_u8,
    IR_cast_f64_to_u8_lhs,
    IR_cast_f64_to_u16,
    IR_cast_f64_to_u16_lhs,
    IR_cast_f64_to_u32,
    IR_cast_f64_to_u32_lhs,
    IR_cast_f64_to_u64,
    IR_cast_f64_to_u64_lhs,
    
    IR_cast_f64_to_s8,
    IR_cast_f64_to_s8_lhs,
    IR_cast_f64_to_s16,
    IR_cast_f64_to_s16_lhs,
    IR_cast_f64_to_s32,
    IR_cast_f64_to_s32_lhs,
    IR_cast_f64_to_s64,
    IR_cast_f64_to_s64_lhs,
    
    IR_cast_s8_to_f32,
    IR_cast_s8_to_f32_lhs,
    IR_cast_s16_to_f32,
    IR_cast_s16_to_f32_lhs,
    IR_cast_s32_to_f32,
    IR_cast_s32_to_f32_lhs,
    IR_cast_s64_to_f32,
    IR_cast_s64_to_f32_lhs,
    
    IR_cast_s8_to_f64,
    IR_cast_s8_to_f64_lhs,
    IR_cast_s16_to_f64,
    IR_cast_s16_to_f64_lhs,
    IR_cast_s32_to_f64,
    IR_cast_s32_to_f64_lhs,
    IR_cast_s64_to_f64,
    IR_cast_s64_to_f64_lhs,
    
    IR_cast_u8_to_f32,
    IR_cast_u8_to_f32_lhs,
    IR_cast_u16_to_f32,
    IR_cast_u16_to_f32_lhs,
    IR_cast_u32_to_f32,
    IR_cast_u32_to_f32_lhs,
    IR_cast_u64_to_f32,
    IR_cast_u64_to_f32_lhs,
    
    IR_cast_u8_to_f64,
    IR_cast_u8_to_f64_lhs,
    IR_cast_u16_to_f64,
    IR_cast_u16_to_f64_lhs,
    IR_cast_u32_to_f64,
    IR_cast_u32_to_f64_lhs,
    IR_cast_u64_to_f64,
    IR_cast_u64_to_f64_lhs,
    
    IR_sign_extend_s8_to_s16,
    IR_sign_extend_s8_to_s16_lhs,
    IR_sign_extend_s8_to_s32,
    IR_sign_extend_s8_to_s32_lhs,
    IR_sign_extend_s16_to_s32,
    IR_sign_extend_s16_to_s32_lhs,
    IR_sign_extend_s8_to_s64,
    IR_sign_extend_s8_to_s64_lhs,
    IR_sign_extend_s16_to_s64,
    IR_sign_extend_s16_to_s64_lhs,
    IR_sign_extend_s32_to_s64,
    IR_sign_extend_s32_to_s64_lhs,
    
    IR_zero_extend_u8_to_u16,
    IR_zero_extend_u8_to_u16_lhs,
    IR_zero_extend_u8_to_u32,
    IR_zero_extend_u8_to_u32_lhs,
    IR_zero_extend_u16_to_u32,
    IR_zero_extend_u16_to_u32_lhs,
    IR_zero_extend_u8_to_u64,
    IR_zero_extend_u8_to_u64_lhs,
    IR_zero_extend_u16_to_u64,
    IR_zero_extend_u16_to_u64_lhs,
    IR_zero_extend_u32_to_u64,
    IR_zero_extend_u32_to_u64_lhs,
    
    IR_truncate_to_u8,
    IR_truncate_to_u8_lhs,    
    IR_truncate_to_u16,
    IR_truncate_to_u16_lhs,
    IR_truncate_to_u32,
    IR_truncate_to_u32_lhs,
};

struct ir{
    enum ir_kind kind;
    s32 s;
};

static int ir_type_size[] = {
    [IR_TYPE_void] = 0,
    [IR_TYPE_bool] = 1,
    
    [IR_TYPE_u8] = 1, 
    [IR_TYPE_u16] = 2, 
    [IR_TYPE_u32] = 4,
    [IR_TYPE_u64] = 8, 
    
    [IR_TYPE_s8] = 1, 
    [IR_TYPE_s16] = 2, 
    [IR_TYPE_s32] = 4,
    [IR_TYPE_s64] = 8, 
    
    [IR_TYPE_f32] = 4,
    [IR_TYPE_f64] = 8, 
    
    [IR_TYPE_pointer] = 8,
};

static int ir_type_signed(enum ir_type ir_type){
    u32 integer_type_index = (u32)(ir_type - IR_TYPE_s8);
    return integer_type_index <= (IR_TYPE_u64 - IR_TYPE_s8) && !(integer_type_index & 1);
}

struct ir_identifier{ 
    struct ir base;
    struct ast_declaration *decl;
};

struct ir_compound_literal{ // @WARNING: This has to match identifier.
    struct ir base;
    struct ast_declaration *decl;
    
    smm initializer_size;
    smm trailing_array_size;
};

struct ir_pointer_increment{
    struct ir base;
    struct ast_pointer_type *pointer_type;
};

struct ir_bitfield_increment{
    struct ir base;
    struct ast_bitfield_type *bitfield_type;
};

struct ir_deref{
    struct ir base;
    struct ast_type *type;
};

struct ir_subscript{
    struct ir base;
    struct ast_type *type;
};

struct ir_dot_or_arrow{
    struct ir base;
    struct compound_member *member; // @cleanup: This should probably be an index.
};

struct ir_function_call{
    struct ir base;
    struct ast_function_type *function_type;
    smm call_arguments_count;
};

struct ir_store_bitfield{
    struct ir base;
    struct ast_bitfield_type *bitfield_type;
};

struct ir_load_bitfield{
    struct ir base;
    struct ast_bitfield_type *bitfield_type;
};

struct ir_skip{
    struct ir base;
    u32 size_to_skip;
};

struct ir_jump_node{
    struct ir base;
    smm label_number;
};

struct ir_temp{ // @cleanup: This thing is sort of stupid.
    struct ir base;
    struct ast_type *type;
};

struct ir_embed{
    struct ir base;
    struct token *token;
};

struct ir_case{
    struct ir base;
    struct ir_case *next;
    struct token *token;
    u64 value;
    
    struct jump_context *jump; // We use this as a label, only used in emit.
};

struct ir_switch{
    struct ir base;
    
    struct ir_jump_node *default_jump_label; // Either the default case or the break label.
    
    struct{
        struct ir_case *first;
        struct ir_case *last;
    }case_list;
};

struct ir_initializer{
    struct ir base;
    u64 offset;
    struct ast_type *lhs_type;
};

struct ir_asm_block{
    struct ir base;
    
    struct token *token;
    struct{
        struct asm_instruction *first;
        struct asm_instruction *last;
    }instructions;
};



// @note: needs to match ast_pointer_literal below
struct ir_integer_literal{
    struct ir base;
    struct ast_type *type;
    union{
        s8 _s8;
        u8 _u8;
        
        u16 _u16;
        s16 _s16;
        
        s32 _s32;
        u32 _u32;
        
        s64 _s64;
        u64 _u64;
    };
};

// @note: needs to match ast_integer_literal above
struct ir_pointer_literal{
    struct ir base;
    struct ast_type *type;
    u8 *pointer;
};

// We have to be able to cast lhs float literals.
struct ir_float_literal{
    struct ir base;
    struct ast_type *type;
    f64 value;
};

// These get allocated in `emit_x64`.
struct ir_emitted_float_literal{
    struct ir base;
    struct ast_type *type;
    f64 value;
    
    struct ir_emitted_float_literal *next;
    u32 relative_virtual_address; // @note: float  literals get loaded rip relative, so this is here to patch
};

struct ir_string_literal{
    struct ir base;
    struct ast_type *type;
    struct ir_string_literal *next;
    
    struct string value;
    enum string_kind string_kind;
    u32 relative_virtual_address;
    u32 symbol_table_index; // needed for .obj (this should maybe be named unique_string_index as that is what it realy is)
};

