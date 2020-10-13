
//@cleanup: make errors take asts, so we can print whole token ranges
enum token_type{
    TOKEN_invalid,
    
    TOKEN_float_literal,  // 1.0f, 1.0 1e7 1.f
    TOKEN_character_literal,
    TOKEN_base10_literal,
    TOKEN_hex_literal,
    TOKEN_string_literal,
    
    // tokens only used during preprocessing
    TOKEN_hash,           // #
    TOKEN_hashhash,       // ## (for identifier concatination)
    TOKEN_whitespace,     // one or more ' ' '\v' '\t' '\f' 
    TOKEN_newline,        // one of '\n', '\r', '\n\r', '\r\n',
    TOKEN_comment,        // either of // or /* */
    
    // single character tokens
    TOKEN_at_sign,        // @, not used, but apperantly windows has it in its headers @sigh
    TOKEN_equals,         // =
    TOKEN_open_paren,     // (
    TOKEN_closed_paren,   // )
    TOKEN_open_curly,     // {
    TOKEN_closed_curly,   // }
    TOKEN_open_index,     // [
    TOKEN_closed_index,   // ]
    //TOKEN_quote,          // " <- this is string literal
    TOKEN_semicolon,      // ;
    TOKEN_colon,          // :
    TOKEN_comma,          // ,
    TOKEN_dot,            // .
    TOKEN_smaller,        // <
    TOKEN_bigger,         // >
    TOKEN_bitwise_not,    // ~
    TOKEN_logical_not,    // !
    TOKEN_question_mark,  // ?
    
    TOKEN_plus,           // +
    TOKEN_minus,          // -
    TOKEN_and,            // &
    TOKEN_or,             // |
    TOKEN_xor,            // ^
    TOKEN_times,          // *
    TOKEN_slash,          // /
    TOKEN_mod,            // %
    //TOKEN_end_of_file,    // \0
    
    // double character tokens
    TOKEN_logical_equals, // ==
    TOKEN_logical_unequals,       // !=
    TOKEN_smaller_equals, // <=
    TOKEN_bigger_equals,  // >=
    TOKEN_logical_and,    // &&
    TOKEN_logical_or,     // ||
    //TOKEN_power,          // ** this destroys a **ptr... maybe not, normal * works 
    TOKEN_right_shift,    // >>
    TOKEN_left_shift,     // <<
    TOKEN_increment,      // ++
    TOKEN_decrement,      // --
    TOKEN_plus_equals,    // +=
    TOKEN_minus_equals,   // -=
    TOKEN_div_equals,     // /=
    TOKEN_mod_equals,     // %=
    TOKEN_xor_equals,     // ^=
    TOKEN_and_equals,     // &=
    TOKEN_or_equals,      // |=
    TOKEN_times_equals,   // *=
    TOKEN_arrow,          // ->
    TOKEN_right_shift_equals,    // >>=
    TOKEN_left_shift_equals,     // <<=
    
    TOKEN_dotdotdot,      // ... (for varargs)
    
    TOKEN_identifier,
    
    // keywords
    TOKEN_typedef,
    TOKEN_first_keyword = TOKEN_typedef,
    TOKEN_enum,
    TOKEN_struct,
    TOKEN_union, 
    TOKEN_sizeof,
    TOKEN_alignof,
    TOKEN_return,
    TOKEN_const,
    TOKEN_volatile,
    TOKEN_static,
    TOKEN_extern,
    TOKEN_inline,
    TOKEN___inline,
    TOKEN___forceinline,
    TOKEN_while,
    TOKEN_if,
    TOKEN_else,
    TOKEN_for,
    TOKEN_do,
    TOKEN_break,
    TOKEN_continue,
    TOKEN_case,
    TOKEN_default,
    TOKEN_switch,
    TOKEN_goto,
    
    // basic types
    TOKEN_void,
    TOKEN_first_basic_type = TOKEN_void,
    TOKEN_char,
    TOKEN_unsigned,
    TOKEN_signed,
    TOKEN_Bool,
    TOKEN_short,
    TOKEN_int, 
    TOKEN_long,
    TOKEN_int8,
    TOKEN_int16,
    TOKEN_int32,
    TOKEN_int64,
    TOKEN_float,
    TOKEN_double,
    
    TOKEN___FUNCTION__,
    
    
    TOKEN_count,
    TOKEN_one_past_last_keyword = TOKEN_count,
    TOKEN_one_past_last_basic_type = TOKEN_count,
};

#define AMOUNT_OF_KEYWORDS (TOKEN_one_past_last_keyword - TOKEN_first_keyword)
#define AMOUNT_OF_BASIC_TYPES (TOKEN_one_past_last_basic_type - TOKEN_first_basic_type)


static struct string keyword_strings[] = {
    // keywords
    const_string("typedef"),
    const_string("enum"),
    const_string("struct"),
    const_string("union"), 
    const_string("sizeof"),
    const_string("_Alignof"),
    const_string("return"),
    const_string("const"),
    const_string("volatile"),
    const_string("static"),
    const_string("extern"),
    const_string("inline"),
    const_string("__inline"), // @note: this is an MSVC thing because C89 does not have inline
    const_string("__forceinline"),
    const_string("while"),
    const_string("if"),
    const_string("else"),
    const_string("for"),
    const_string("do"),
    const_string("break"),
    const_string("continue"),
    const_string("case"),
    const_string("default"),
    const_string("switch"),
    const_string("goto"),
    
    // basic types
    const_string("void"),
    const_string("char"),
    const_string("unsigned"),
    const_string("signed"),
    const_string("_Bool"),
    const_string("short"),
    const_string("int"), 
    const_string("long"),
    const_string("__int8"),
    const_string("__int16"),
    const_string("__int32"),
    const_string("__int64"),
    const_string("float"),
    const_string("double"),
    
    
    const_string("__FUNCTION__"),
};

static_assert(array_count(keyword_strings) == AMOUNT_OF_KEYWORDS);

// @cleanup: think about how to make this small
struct token{
    enum token_type type;
    u32 pad;
    struct file_stack_node *file; // @cleanup: this should be an index instead of the pad above.
    
    union{
        struct{
            unique_string value;
            struct define_argument *is_define_argument;
        };
        
        struct{
            union{
                u64 number;
                f64 _f64;
            };
            
            enum{
                NUMBER_KIND_int,
                NUMBER_KIND_unsigned,
                NUMBER_KIND_unsigned_long,
                NUMBER_KIND_unsigned_long_long,
                NUMBER_KIND_long,
                NUMBER_KIND_long_long,
                
                NUMBER_KIND_float32,
                NUMBER_KIND_float64,
                
                STRING_KIND_none,  //   ""
                STRING_KIND_utf8,  // u8""
                STRING_KIND_utf16, //  u""
                STRING_KIND_utf32, //  U""
            }number_kind;
        };
        
        struct string string; // 'data' is also used to store the begining and end for system include (< and >) have it set 
        // :smaller_bigger_and_system_includes
        
        m128 to_copy;
    };
    u32 line;
    u32 column;
};

func b32 token_match(struct token *t1, struct token *t2){
    if(t1->type != t2->type) return false;
    
    switch(t1->type){
        case TOKEN_float_literal:
        case TOKEN_character_literal:
        case TOKEN_base10_literal:
        case TOKEN_hex_literal:{
            return t1->number == t2->number && t1->number_kind == t2->number_kind;
        }break;
        case TOKEN_string_literal: // @note: we intern string literals.
        case TOKEN_identifier:{
            return t1->value == t2->value;
        }break;
        
        default: return true;
    }
}


struct token_bucket{
    struct token_bucket *next;
    struct token_bucket *prev;
    struct token *tokens;
    smm amount;
};

struct token_bucket_array{
    struct token_bucket *first;
    struct token_bucket *last;
};


//////////////////////////

enum ast_kind{
    AST_invalid                 = 0,
    AST_none                    = AST_invalid,
    
    AST_identifier              = 1,
    AST_string_literal          = 2,
    AST_integer_literal         = 3,
    AST_float_literal           = 4,
    AST_struct_or_array_literal = 5,
    AST_declaration             = 6,
    
    AST_void_type               = 7,
    AST_integer_type            = 8,
    AST_float_type              = 9,
    //AST_bitfield_type           = 10,
    AST_pointer_type            = 11,
    AST_function_type           = 12,
    AST_array_type              = 13,
    AST_struct                  = 14,
    AST_enum                    = 15,
    
    AST_cast                    = 16,
    AST_function                = 17,
    
    AST_unary_postinc           = 18,
    AST_unary_preinc            = 19,
    AST_unary_postdec           = 20,
    AST_unary_predec            = 21,
    AST_logical_not             = 22,
    AST_unary_bitwise_not       = 23,
    AST_unary_deref             = 24,
    AST_unary_array_index       = 25,
    AST_unary_function_call     = 26,
    AST_unary_minus             = 27,
    AST_unary_plus              = 28,
    AST_unary_address           = 29,
    
    AST_sizeof                  = 30,
    AST_alignof                 = 31,
    
    AST_binary_left_shift       = 32,
    AST_binary_right_shift      = 33,
    AST_binary_and              = 34,
    AST_binary_or               = 35,
    AST_binary_xor              = 36,
    AST_binary_times            = 37,
    AST_binary_divide           = 38,
    AST_binary_mod              = 39,
    AST_binary_plus             = 40,
    AST_binary_minus            = 41,
    AST_binary_logical_equals   = 42,
    AST_binary_logical_unequals = 43,
    AST_binary_bigger_equals    = 44,
    AST_binary_smaller_equals   = 45,
    AST_binary_bigger           = 46,
    AST_binary_smaller          = 47,
    AST_logical_and             = 48,
    AST_logical_or              = 49,
    
    AST_member                  = 50,
    AST_member_deref            = 51, //now desugared
    
    //AST_array_subscript         = 52, //now desugared
    AST_conditional_expression  = 53,
    AST_assignment              = 54,
    AST_and_assignment          = 55,
    AST_or_assignment           = 56,
    AST_xor_assignment          = 57,
    AST_plus_assignment         = 58,
    AST_minus_assignment        = 59,
    AST_left_shift_assignment   = 60,
    AST_right_shift_assignment  = 61,
    AST_times_assignment        = 62,
    AST_divide_assignment       = 63,
    AST_modulo_assignment       = 64,
    
    AST_comma_expression        = 65,
    
    AST_function_call           = 66,
    AST_scope                   = 67,
    
    AST_empty_statement         = 68, 
    AST_return                  = 69,
    AST_if                      = 70,
    AST_for                     = 71,
    AST_do_while                = 72,
    
    AST_break                   = 73,
    AST_continue                = 74,
    
    AST_switch                  = 75,
    AST_case                    = 76,
    AST_label                   = 77,
    AST_goto                    = 78,
    
    AST_typedef                 = 79,
    
    AST_unresolved_type         = 80,
    AST_declaration_list        = 81,
    AST_union                   = 82,
    
    AST_count,
};

struct ast{
    enum ast_kind kind;
    s32 byte_offset_in_function;
    struct token *token;
    s64 s;
    struct ast_type *resolved_type;
    struct ast *defined_type; 
    // :defined_types
    // if the ast has a 'AST_typedef' or 'AST_enum' type, we store it here for error reporting. 
    // Also if it is just promoted, we retain that information, so we can warn on 'u8 = u8 + u8' usw.
    // :retain_type_information_through_promotion 
};

enum type_flags{
    TYPE_FLAG_pdb_tempoary  = 0x1, 
    TYPE_FLAG_pdb_permanent = 0x2, 
};

struct ast_type{
    enum ast_kind kind;
    enum type_flags flags;
    struct token *token;
    s64 s;
    smm size;      // @note: these could be u32's probably
    smm alignment; // @note: these could be u32's probably
    
    u32 pdb_type_index;
    u32 pdb_predecl_type_index;
};

struct ast_list_node{
    struct ast_list_node *next;
    struct ast* value;
};

struct ast_list{
    struct ast_list_node *first;
    struct ast_list_node *last;
    smm count;
};

#define for_ast_list(list) for(struct ast_list_node *it = (list).first; it; it = it->next)

struct ast_declaration{
    // @WARNING: this needs to match the part in function
    struct ast base;
    struct ast_type *type;
    struct ast *defined_type; // either 'AST_enum' or 'AST_typedef' or 'null' :defined_types
    unique_string identifier;
    
    union{
        smm offset_on_stack; // - stack relative if the declarations  is in a function scope
        smm offset_in_type;  // - the member offset if the declarations is in a struct
    };
    
    u8 *memory_location; // if it is global a pointer to the evaluated intializer.
    smm relative_virtual_address; // only used when we emit an exe
    struct ast *assign_expr; // the rhs of '=' if it exists
    
    
#define DECLARATION_FLAGS_is_global                0x1
#define DECLARATION_FLAGS_is_enum_member           0x2
#define DECLARATION_FLAGS_is_big_function_argument 0x4
#define DECLARATION_FLAGS_is_local_persist         0x8
#define DECLARATION_FLAGS_is_referanced            0x10
    b64 flags;
};

struct declaration_node{
    struct declaration_node *next;
    struct ast_declaration *decl;
};

struct declaration_list{
    struct ast_type *type_specifier;
    struct ast *type_specifier_defined_type;
    struct declaration_node *first;
    struct declaration_node *last;
};

struct ast_declaration_list{
    struct ast base;
    struct declaration_list list;
};

struct ast_identifier{ 
    struct ast base;
    struct ast_declaration *decl;
};

struct ast_pointer_type{
    struct ast_type base;
    struct ast_type *pointer_to;
    struct ast *pointer_to_defined_type; // :defined_types
};

struct ast_unresolved_type{
    struct ast_type base;
    char *type_prefix; // "union", "struct", "enum" or ""
    enum sleep_purpose unresolved_sleep_purpose;
    struct token *sleeping_on;
};

struct ast_array_type{ // :array_sizes
    struct ast_type base;
    b32 is_of_unknown_size;
    smm amount_of_elements;
    struct ast_type *element_type;
    struct ast *element_type_defined_type; // :defined_types
};

struct ast_integer_literal{
    struct ast base;
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

struct ast_float_literal{
    struct ast base;
    struct ast_float_literal *next;
    f64 value;
    
    u32 relative_virtual_address; // @note: float  literals get loaded rip relative, so this is here to patch
};

struct ast_string_literal{
    struct ast base;
    unique_string value;
    // :string_literal_location
    // @note: this is a unique_string because we change value->data to be the right thing in exe_writer
};

struct ast_struct_or_array_literal{
    struct ast base;
    struct ast_declaration *decl;
    struct ast_list assignment_list; // @cleanup: why is it assignment_list but ast_struct->declarations?
};

struct ast_return{
    struct ast base;
    struct ast *expr;
};

struct ast_unary_op{
    struct ast base;
    struct ast *operand;
};

struct ast_binary_op{
    struct ast base;
    struct ast *lhs;
    struct ast *rhs;
};

struct ast_dot_or_arrow{
    struct ast base;
    struct ast *lhs;
    struct ast_declaration *member_decl;
};


struct ast_scope{
    struct ast base;
    struct ast_scope *parent;
    
    struct ast_list statement_list;
    
    enum scope_flags{
        SCOPE_FLAG_none            = 0x0,
        SCOPE_FLAG_can_continue    = 0x1,
        SCOPE_FLAG_can_break       = 0x2,
        //SCOPE_FLAG_has_ending_line = 0x4,
    } flags;
    
    struct ast_declaration **declarations;
    u32 amount_of_declarations;
    u32 current_max_amount_of_declarations;
    
    struct ast_compound_type **compound_types;
    u32 amount_of_compound_types;
    u32 current_max_amount_of_compound_types;
    
    //u32 scope_end_line;
    u32 scope_end_byte_offset_in_function;
};

struct ast_continue{
    struct ast base;
    struct ast_scope *scope_to_continue;
};

struct ast_break{
    struct ast base;
    struct ast_scope *scope_to_break;
};

struct ast_compound_type{
    struct ast_type base;
    unique_string identifier;
    
    struct ast_list declarations;
};

struct ast_function_type{
    struct ast_type base;
    struct ast_type *return_type;
    struct ast *return_type_defined_type; // :defined_type @cleanup: make sure this is filled 
    
#define FUNCTION_TYPE_FLAGS_is_varargs   0x1
#define FUNCTION_TYPE_FLAGS_is_dllimport 0x2
#define FUNCTION_TYPE_FLAGS_is_intrinsic 0x4
    b64 flags;
    
    struct ast_list argument_list;
};

struct ast_function{
    union{
        struct{
            struct ast base;
            struct ast_function_type *type;
            struct ast *enum_or_typedef_name; // either 'AST_enum' or 'AST_typedef' or 'null'
            unique_string identifier;
            smm unused; // @cleanup: make this offset in text section?
            u8 *memory_location; // @cleanup: rename? this is where the thing onces emited. here for patching
            smm relative_virtual_address; // needs to be 64 bit, as we point to it not knowing its size
        };
        struct ast_declaration as_decl;
    };
    
    struct ast_function *is_defined; // the pointer to the function declaration that has a definition
    
    u8 *base_of_prolog;
    smm size_of_prolog; // these could be smaller
    
    // right now _just_ the function without the prolog
    u8 *base_of_main_function;
    smm byte_size_without_prolog;
    
    smm rsp_subtract_offset;
    smm offset_in_text_section;
    smm byte_size;
    
    struct ast *scope;
    smm stack_space_needed; 
    
    struct ast_list static_variables;
    struct{
        struct ast_float_literal *first;
        struct ast_float_literal *last;
    }float_literals; // @note we have to save these, as they are actually in '.rdata'.
    
    // debug info:
    smm debug_size;
    u8 *debug_info;
    u32 debug_symbol_offset;
    u32 ref_offset;
    
    struct dll_import_node *dll_import_node;
    
};

struct ast_for{
    struct ast base;
    struct ast *decl;
    struct ast *condition;
    struct ast *increment;
    struct ast *body;
    struct ast_scope *scope_for_decl; //  we have an implicit scope for the 'decl' so any for has a scope
};

struct ast_if{
    struct ast base;
    struct ast *condition;
    struct ast *statement;
    struct ast *else_statement;
};

struct ast_switch{
    struct ast base;
    struct ast *switch_on;
    struct ast *statement;
    struct ast_case *default_case;
    
    struct ast_list case_list;
};

struct ast_case{
    struct ast base;
    struct ast *expression;
    struct jump_context *jump; // we use this as a label, only used in emit
};

struct ast_function_call{
    struct ast base;
    struct ast *identifier_expression; // @cleanup: garbage name
    struct ast_list call_arguments;
};

struct ast_array_subscript{
    struct ast base;
    struct ast *index;
    struct ast *array;
};

struct ast_label{
    struct ast base;
    unique_string ident;
    
    // for emitting
    smm byte_offset_in_function;
};

struct ast_goto{
    struct ast base;
    struct ast_label *label_to_goto;
    unique_string ident;
    
    // for emiting
    struct jump_node *jump_node;
};

struct ast_conditional_expression{
    struct ast base;
    struct ast *if_true;
    struct ast *if_false;
    struct ast *condition;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////

enum intrinsic_kind{
    INTRINSIC_KIND_va_start,
    INTRINSIC_KIND_rdtsc,
    INTRINSIC_KIND_pause,

    INTRINSIC_KIND_scalar_double_op,
    INTRINSIC_KIND_packed_double_op,
    
    INTRINSIC_KIND_set_scalar_double,
    
    INTRINSIC_KIND_InterlockedCompareExchange64, 
    INTRINSIC_KIND_InterlockedCompareExchange128,
    INTRINSIC_KIND_interlocked_inc_dec_64,
    INTRINSIC_KIND_interlocked_fetch_op_64,
};


