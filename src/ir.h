
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
    
    IR_load_atomic_bool,
    IR_load_atomic_u8,
    IR_load_atomic_u16,
    IR_load_atomic_u32,
    IR_load_atomic_u64,
    
    IR_store_bitfield,
    IR_store_atomic_integer,
    
    IR_store_u8,
    IR_store_u16,
    IR_store_u32,
    IR_store_u64,
    IR_store_f32,
    IR_store_f64,
    
    IR_memcpy,
    
    // IR_convert_to_bool,
    IR_cast_f32_to_f64,
    IR_cast_f64_to_f32,
    
    IR_cast_f32_to_u32,
    IR_cast_f32_to_u64,
    
    IR_cast_f32_to_s32,
    IR_cast_f32_to_s64,
    
    IR_cast_f64_to_u32,
    IR_cast_f64_to_u64,
    
    IR_cast_f64_to_s32,
    IR_cast_f64_to_s64,
    
    IR_cast_s32_to_f32,
    IR_cast_s64_to_f32,
    IR_cast_s32_to_f64,
    IR_cast_s64_to_f64,
    
    IR_cast_u32_to_f32,
    IR_cast_u64_to_f32,
    IR_cast_u32_to_f64,
    IR_cast_u64_to_f64,
    
    IR_sign_extend_s8_to_s16,
    IR_sign_extend_s8_to_s32,
    IR_sign_extend_s16_to_s32,
    IR_sign_extend_s8_to_s64,
    IR_sign_extend_s16_to_s64,
    IR_sign_extend_s32_to_s64,
    
    IR_zero_extend_u8_to_u16,
    IR_zero_extend_u8_to_u32,
    IR_zero_extend_u16_to_u32,
    IR_zero_extend_u8_to_u64,
    IR_zero_extend_u16_to_u64,
    IR_zero_extend_u32_to_u64,
    
    IR_inc_u8,
    IR_inc_u16,
    IR_inc_u32,
    IR_inc_u64,
    
    IR_inc_bool,
    IR_inc_bitfield,
    IR_inc_f32,
    IR_inc_f64,
    
    IR_dec_bool,
    IR_dec_bitfield,
    IR_dec_f32,
    IR_dec_f64,
    
    IR_deref,
    IR_member,
    IR_member_deref,
    
    IR_pointer_subscript,
    IR_array_subscript,
    
    IR_function_call,
    IR_return,
    
    IR_negate_u32,
    IR_negate_u64,
    IR_negate_f32,
    IR_negate_f64,
    
    IR_not_u32,
    IR_not_u64,
    IR_not_f32,
    IR_not_f64,
    
    IR_or_u32,
    IR_or_u64,
    
    IR_xor_u32,
    IR_xor_u64,
    
    IR_and_u32,
    IR_and_u64,
    
    IR_left_shift_u32,
    IR_left_shift_u64,
    
    IR_right_shift_u32,
    IR_right_shift_u64,
    
    IR_left_shift_s32,
    IR_left_shift_s64,
    
    IR_right_shift_s32,
    IR_right_shift_s64,
    
    IR_plus_u32,
    IR_plus_u64,
    IR_plus_f32,
    IR_plus_f64,
    
    IR_minus_u32,
    IR_minus_u64,
    IR_minus_f32,
    IR_minus_f64,
    
    IR_times_u32,
    IR_times_u64,
    IR_times_s32,
    IR_times_s64,
    IR_times_f32,
    IR_times_f64,
    
    IR_divide_u32,
    IR_divide_u64,
    IR_divide_s32,
    IR_divide_s64,
    IR_divide_f32,
    IR_divide_f64,
    
    IR_mod_u32,
    IR_mod_u64,
    IR_mod_s32,
    IR_mod_s64,
    
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
    
    IR_equals_u32,
    IR_unequals_u32,
    
    IR_equals_u64,
    IR_unequals_u64,
    IR_equals_f32,
    IR_unequals_f32,
    
    
    IR_equals_f64,
    IR_unequals_f64,
    
    IR_and_assignment_u8,
    IR_and_assignment_u16,
    IR_and_assignment_u32,
    IR_and_assignment_u64,
    
    IR_or_assignment_u8,
    IR_or_assignment_u16,
    IR_or_assignment_u32,
    IR_or_assignment_u64,
    
    IR_xor_assignment_u8,
    IR_xor_assignment_u16,
    IR_xor_assignment_u32,
    IR_xor_assignment_u64,
    
    IR_plus_assignment_u8,
    IR_plus_assignment_u16,
    IR_plus_assignment_u32,
    IR_plus_assignment_u64,
    
    IR_plus_assignment_f32,
    IR_plus_assignment_f64,
    
    IR_minus_assignment_u8,
    IR_minus_assignment_u16,
    IR_minus_assignment_u32,
    IR_minus_assignment_u64,
    
    IR_minus_assignment_f32,
    IR_minus_assignment_f64,
    
    IR_times_assignment_u8,
    IR_times_assignment_u16,
    IR_times_assignment_u32,
    IR_times_assignment_u64,
    
    IR_times_assignment_s8,
    IR_times_assignment_s16,
    IR_times_assignment_s32,
    IR_times_assignment_s64,
    
    IR_times_assignment_f32,
    IR_times_assignment_f64,
    
    IR_divide_assignment_u8,
    IR_divide_assignment_u16,
    IR_divide_assignment_u32,
    IR_divide_assignment_u64,
    
    IR_divide_assignment_s8,
    IR_divide_assignment_s16,
    IR_divide_assignment_s32,
    IR_divide_assignment_s64,
    
    IR_divide_assignment_f32,
    IR_divide_assignment_f64,
    
    IR_modulo_assignment_u8,
    IR_modulo_assignment_u16,
    IR_modulo_assignment_u32,
    IR_modulo_assignment_u64,
    
    IR_modulo_assignment_s8,
    IR_modulo_assignment_s16,
    IR_modulo_assignment_s32,
    IR_modulo_assignment_s64,
    
    IR_left_shift_assignment_u8,
    IR_left_shift_assignment_u16,
    IR_left_shift_assignment_u32,
    IR_left_shift_assignment_u64,
    
    IR_left_shift_assignment_s8,
    IR_left_shift_assignment_s16,
    IR_left_shift_assignment_s32,
    IR_left_shift_assignment_s64,
    
    IR_right_shift_assignment_u8,
    IR_right_shift_assignment_u16,
    IR_right_shift_assignment_u32,
    IR_right_shift_assignment_u64,
    
    IR_right_shift_assignment_s8,
    IR_right_shift_assignment_s16,
    IR_right_shift_assignment_s32,
    IR_right_shift_assignment_s64,
};

struct ir{
    enum ir_kind kind;
    s32 s;
};

enum ir_type{
    IR_void,
    IR_bool,
    
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
    
    IR_TYPE_pointer,
};

static int ir_type_size[] = {
    [IR_void] = 0,
    [IR_bool] = 1,
    
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
