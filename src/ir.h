
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
    
    IR_TYPE_s128, 
    IR_TYPE_u128,
    
    IR_TYPE_f32,
    IR_TYPE_f64,
    IR_TYPE_f80,
    IR_TYPE_f128,
    
    IR_TYPE_padding,
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
    IR_TYPE_bitfield,
};

enum ir_kind{
    IR_invalid,
    
    IR_declaration,
    IR_function,
    IR_typedef,
    
    IR_identifier,
    IR_string_literal,
    IR_integer_literal,
    IR_float_literal,
    IR_compound_literal,
    
    IR_emitted_float_literal, // yuck, we copy float-literals in the back-end.
    
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
    
    IR_start_exception_block,
    IR_end_exception_block,
    
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
    
    IR_postinc,
    IR_postdec,
    
    IR_preinc,
    IR_predec,
    
    IR_deref,
    IR_member,
    IR_member_deref,
    
    IR_pointer_subscript,
    IR_array_subscript,
    
    IR_function_call,
    IR_return,
    
    IR_bitwise_not,
    
    IR_negate,
    
    IR_logical_not,
    
    IR_or,
    IR_xor,
    IR_and,
    
    IR_left_shift,
    IR_right_shift,
    
    IR_add,
    IR_subtract,
    IR_pointer_subtract,
    
    // @note: Order matches ir_type.
    IR_multiply,
    IR_divide,
    IR_mod,
    
    
    // We use the order here.
    IR_bigger_equals,
    IR_smaller_equals,
    IR_bigger,
    IR_smaller,
    
    // 
    // We use the order of these as well.
    // 
    IR_equals,
    IR_unequals,
    
    IR_add_assignment,
    IR_subtract_assignment,
    
    IR_and_assignment,
    IR_or_assignment,
    IR_xor_assignment,
    
    IR_multiply_assignment,
    IR_divide_assignment,
    IR_modulo_assignment,
    
    IR_left_shift_assignment,
    IR_right_shift_assignment,
    
    IR_nop,
    IR_embed,
    IR_initializer,
    IR_range_initializer,
    IR_asm_block,
    
    IR_cast_vector, 
    
    IR_vector_extract,
    IR_vector_add,
    IR_vector_sub,
    IR_vector_xor,
    IR_vector_and,
    IR_vector_or,
    
    // Casts are at the end here, so that we can use math to encode the operands.
    
    IR_cast_to_void,
    IR_cast_to_pointer,
    
    IR_cast_to_bool,
    IR_cast_to_bool_lhs,
    IR_cast_to_s8,
    IR_cast_to_s8_lhs,
    IR_cast_to_u8,
    IR_cast_to_u8_lhs,
    IR_cast_to_s16,
    IR_cast_to_s16_lhs,
    IR_cast_to_u16,
    IR_cast_to_u16_lhs,
    IR_cast_to_s32,
    IR_cast_to_s32_lhs,
    IR_cast_to_u32,
    IR_cast_to_u32_lhs,
    IR_cast_to_s64,
    IR_cast_to_s64_lhs,
    IR_cast_to_u64,
    IR_cast_to_u64_lhs,
    IR_cast_to_s128,
    IR_cast_to_s128_lhs,
    IR_cast_to_u128,
    IR_cast_to_u128_lhs,
    IR_cast_to_f32,
    IR_cast_to_f32_lhs,
    IR_cast_to_f64,
    IR_cast_to_f64_lhs,
    
    IR_cast_base = IR_cast_to_bool,
    
    IR_count,
};

static int ir_type_size[] = {
    [IR_TYPE_void] = 0,
    [IR_TYPE_bool] = 1,
    
    [IR_TYPE_u8] = 1, 
    [IR_TYPE_u16] = 2, 
    [IR_TYPE_u32] = 4,
    [IR_TYPE_u64] = 8, 
    [IR_TYPE_u128] = 16, 
    
    [IR_TYPE_s8] = 1, 
    [IR_TYPE_s16] = 2, 
    [IR_TYPE_s32] = 4,
    [IR_TYPE_s64] = 8, 
    [IR_TYPE_s128] = 16, 
    
    [IR_TYPE_f32] = 4,
    [IR_TYPE_f64] = 8, 
    [IR_TYPE_f80] = 16,
    [IR_TYPE_f128] = 16,
    
    [IR_TYPE_pointer] = 8,
};

static int ir_type_signed(enum ir_type ir_type){
    u32 integer_type_index = (u32)(ir_type - IR_TYPE_s8);
    return integer_type_index <= (IR_TYPE_u64 - IR_TYPE_s8) && !(integer_type_index & 1);
}

#pragma pack(push, 1)

struct ir{
    // enum ir_kind kind;
    u16 kind;
};

static_assert(IR_count < 0x10000);

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

struct ir_deref{
    struct ir base;
    struct ast_type *type;
};

struct ir_typed_cast{
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

struct ir_embed{
    struct ir base;
    struct token *token;
};

struct ir_case{
    struct ir base;
    struct ir_case *next;
    struct token *token;
    
    u64 low_value;
    u64 high_value;
    
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

struct ir_range_initializer{
    struct ir base;
    u64 offset;
    u64 count;
    struct ast_type *type;
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
    union {
        f32 _f32;
        f64 _f64;
    };
};

// These get allocated in `emit_x64`.
struct ir_emitted_float_literal{
    union{
        struct ir base;
        struct ir_float_literal literal;
    };
    
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


struct ir_exception_marker{
    struct ir base;
    struct seh_exception_handler *handler;
};

#pragma pack(pop)
