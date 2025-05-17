
// @cleanup: make errors take asts, so we can print whole token ranges
enum token_type{
    TOKEN_invalid,
    
    // tokens only used during preprocessing
    TOKEN_hash,           // #
    TOKEN_hashhash,       // ## (for identifier concatenation)
    TOKEN_newline,        // one of '\n', '\r', '\n\r', '\r\n',
    TOKEN_whitespace,     // one or more ' ' '\v' '\t' '\f' 
    // TOKEN_comment = TOKEN_whitespace, // this is really possible but there is almost no speed gain.
    TOKEN_comment,        // either of // or /* */
    
    TOKEN_at_sign,  // @ - not used by c, but still printable, hence people might put it in there code (in particular, this is used in windows headers).
    TOKEN_backtick, // ` - not used by c, but still printable, hence people might put it in there code (this was in a #error as a markdown thing).
    
    //
    // primary expression tokens
    //
    TOKEN_float_literal,  // 1.0f, 1.0 1e7 1.f
    TOKEN_float_hex_literal,
    TOKEN_character_literal, // has ''
    TOKEN_base10_literal,
    TOKEN_hex_literal,
    TOKEN_binary_literal,
    TOKEN_string_literal, // has ""
    
    TOKEN_identifier,
    TOKEN_identifier_dont_expand_because_it_comes_from_a_fully_expanded_macro,
    
    
    TOKEN_open_paren,     // (
    TOKEN_closed_paren,   // )
    TOKEN_open_curly,     // {
    TOKEN_closed_curly,   // }
    TOKEN_open_index,     // [
    TOKEN_closed_index,   // ]
    TOKEN_semicolon,      // ;
    TOKEN_colon,          // :
    TOKEN_dot,            // .
    TOKEN_bitwise_not,    // ~
    TOKEN_logical_not,    // !
    
    TOKEN_plus,           // +
    TOKEN_minus,          // -
    TOKEN_and,            // &
    TOKEN_increment,        // ++
    TOKEN_decrement,        // --
    TOKEN_arrow,            // ->
    
    // binary ops, no particular order
    TOKEN_or,          // |
    TOKEN_xor,         // ^
    TOKEN_times,       // *
    TOKEN_slash,       // /
    TOKEN_mod,         // %
    TOKEN_right_shift, // >>
    TOKEN_left_shift,  // <<
    
    // compare tokens same order as the AST_*
    TOKEN_logical_equals,   // ==
    TOKEN_logical_unequals, // !=
    TOKEN_bigger_equals,    // >=
    TOKEN_smaller_equals,   // <=
    TOKEN_bigger,           // >
    TOKEN_smaller,          // <
    
    // Assignment tokens, same order as AST_*
    TOKEN_equals,             // =
    TOKEN_and_equals,         // &=
    TOKEN_or_equals,          // |=
    TOKEN_xor_equals,         // ^=
    TOKEN_plus_equals,        // +=
    TOKEN_minus_equals,       // -=
    TOKEN_left_shift_equals,  // <<=
    TOKEN_right_shift_equals, // >>=
    TOKEN_times_equals,       // *=
    TOKEN_div_equals,         // /=
    TOKEN_mod_equals,         // %=
    
    TOKEN_logical_and,      // &&
    TOKEN_logical_or,       // ||
    
    TOKEN_question_mark,  // ?
    TOKEN_comma,          // ,
    
    
    TOKEN_dotdotdot,      // ... (for varargs)
    
    // keywords
    TOKEN_enum,
    TOKEN_first_keyword = TOKEN_enum,
    
    TOKEN_struct,
    TOKEN_union, 
    
    TOKEN_typedef,
    TOKEN_static,
    TOKEN_extern,
    TOKEN_register,
    // TOKEN_auto,
    // TOKEN_constexpr,
    // TOKEN_thread_local,
    
    TOKEN_atomic,
    TOKEN_unaligned,
    TOKEN_restrict,
    TOKEN_const,
    TOKEN_volatile,
    TOKEN_noreturn,
    TOKEN_alignas,
    
    TOKEN_inline,
    TOKEN_forceinline,
    
    TOKEN_sizeof,
    TOKEN_alignof,
    
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
    TOKEN_return,
    
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
    
    TOKEN_last_basic_type = TOKEN_double,
    
    TOKEN___func__,
    TOKEN_static_assert,
    TOKEN_declspec,
    
    TOKEN_ptr32,
    TOKEN_ptr64,
    TOKEN_stdcall,
    TOKEN_cdecl,
    
    TOKEN_asm,
    TOKEN_embed, // Synthetic token coming from #embed.
    TOKEN_pragma_pack, // Synthetic token coming from '#pragma pack(<...>)' or '__pragma(pack(<...>))'
    
    TOKEN_generic, // C11 _Generic
    TOKEN_thread_local, // C11 _Thread_local
    
    TOKEN_count,
    TOKEN_one_past_last_keyword = TOKEN_count,
};

#define AMOUNT_OF_BASIC_TYPES (TOKEN_one_past_last_basic_type - TOKEN_first_basic_type)

static struct{
    struct string keyword;
    enum token_type token_kind;
} keyword_table_entries[] = {
    // keywords
    {const_string("typedef"),       TOKEN_typedef},
    {const_string("enum"),          TOKEN_enum},
    {const_string("struct"),        TOKEN_struct},
    {const_string("union"),         TOKEN_union},
    {const_string("sizeof"),        TOKEN_sizeof},
    {const_string("_Alignof"),      TOKEN_alignof},
    {const_string("__alignof"),     TOKEN_alignof},
    {const_string("return"),        TOKEN_return},
    {const_string("__unaligned"),   TOKEN_unaligned},
    {const_string("__restrict"),    TOKEN_restrict},
    {const_string("restrict"),      TOKEN_restrict},
    {const_string("const"),         TOKEN_const},
    {const_string("volatile"),      TOKEN_volatile},
    {const_string("static"),        TOKEN_static}, 
    {const_string("extern"),        TOKEN_extern},
    {const_string("register"),      TOKEN_register},
    {const_string("inline"),        TOKEN_inline},
    {const_string("_inline"),       TOKEN_inline},
    {const_string("__inline"),      TOKEN_inline},
    {const_string("__forceinline"), TOKEN_forceinline}, 
    {const_string("_Noreturn"),     TOKEN_noreturn},
    {const_string("_Alignas"),      TOKEN_alignas},
    {const_string("_Atomic"),       TOKEN_atomic},
    {const_string("while"),         TOKEN_while},
    {const_string("if"),            TOKEN_if},
    {const_string("else"),          TOKEN_else},
    {const_string("for"),           TOKEN_for},
    {const_string("do"),            TOKEN_do},
    {const_string("break"),         TOKEN_break},
    {const_string("continue"),      TOKEN_continue},
    {const_string("case"),          TOKEN_case},
    {const_string("default"),       TOKEN_default},
    {const_string("switch"),        TOKEN_switch},
    {const_string("goto"),          TOKEN_goto},
    {const_string("__declspec"),    TOKEN_declspec},
    {const_string("__ptr32"),       TOKEN_ptr32},
    {const_string("__ptr64"),       TOKEN_ptr64},
    {const_string("__stdcall"),     TOKEN_stdcall},
    {const_string("__cdecl"),       TOKEN_cdecl},
    {const_string("_cdecl"),        TOKEN_cdecl},
    
    {const_string("__asm__"),       TOKEN_asm},
    
    // basic types
    {const_string("void"),     TOKEN_void},
    {const_string("char"),     TOKEN_char},
    {const_string("unsigned"), TOKEN_unsigned},
    {const_string("signed"),   TOKEN_signed},
    {const_string("_Bool"),    TOKEN_Bool},
    {const_string("short"),    TOKEN_short},
    {const_string("int"),      TOKEN_int},
    {const_string("long"),     TOKEN_long},
    {const_string("__int8"),   TOKEN_int8},
    {const_string("__int16"),  TOKEN_int16},
    {const_string("__int32"),  TOKEN_int32},
    {const_string("__int64"),  TOKEN_int64},
    {const_string("float"),    TOKEN_float},
    {const_string("double"),   TOKEN_double},
    
    {const_string("__func__"),     TOKEN___func__}, // c99
    {const_string("__FUNCTION__"), TOKEN___func__}, // msvc/gcc extension
    {const_string("__FUNCSIG__"),  TOKEN___func__}, // wrong msvc-extension implementation.
    
    {const_string("_Static_assert"), TOKEN_static_assert},
    {const_string("static_assert"), TOKEN_static_assert},
    
    {const_string("_Generic"), TOKEN_generic},
    {const_string("_Thread_local"), TOKEN_thread_local},
};

enum preprocessor_directive{
    DIRECTIVE_invalid,
    
    DIRECTIVE_define,
    DIRECTIVE_undef,
    DIRECTIVE_include,
    DIRECTIVE_include_next,
    DIRECTIVE_embed,
    DIRECTIVE_error,
    DIRECTIVE_pragma,
    DIRECTIVE_line,
    
    DIRECTIVE_if,
    DIRECTIVE_elif,
    DIRECTIVE_ifdef,
    DIRECTIVE_ifndef,
    DIRECTIVE_else,
    DIRECTIVE_endif,
    
    FIRST_DIRECTIVE_WHICH_SHOULD_BE_STILL_BE_RUN_IN_DISABLED_STATIC_IF = DIRECTIVE_if,
};

static struct{
    struct string directive;
    enum preprocessor_directive kind;
} directive_table_entries[] = {
    { const_string("if"), DIRECTIVE_if },
    { const_string("elif"), DIRECTIVE_elif },
    { const_string("ifdef"), DIRECTIVE_ifdef },
    { const_string("ifndef"), DIRECTIVE_ifndef },
    { const_string("else"), DIRECTIVE_else },
    { const_string("endif"), DIRECTIVE_endif },
    
    { const_string("define"), DIRECTIVE_define },
    { const_string("undef"), DIRECTIVE_undef },
    { const_string("include"), DIRECTIVE_include },
    { const_string("include_next"), DIRECTIVE_include_next },
    { const_string("embed"), DIRECTIVE_embed },
    { const_string("error"), DIRECTIVE_error },
    { const_string("pragma"), DIRECTIVE_pragma },
    { const_string("line"), DIRECTIVE_line },
};

struct atom{
    u64 string_hash;
    
    union{
        struct string string;
        struct string;
    };
};

struct atom atom_for_string(struct string string){
    return (struct atom){
        .data = string.data, 
        .size = string.size, 
        .string_hash = string_djb2_hash(string),
    };
}

struct token{
    enum token_type type;
    s32 file_index;
    u32 line;
    u32 column;
    
    union{
        struct atom;
        struct atom atom;
    };
};

static struct string token_get_string(struct token *token){
    return (struct string){.data = token->atom.data, .size = token->atom.size};
}

static b32 atoms_match(struct atom a, struct atom b){
    
    if(a.string_hash != b.string_hash) return false;
    if(a.size != b.size) return false;
    
    return (memcmp(a.data, b.data, a.size) == 0);
}

struct token_array{
    struct token *data;
    union{
        smm amount;
        smm size;
        smm count;
    };
};

struct token_stack_node{
    // WARNING: We explicitly initialize all of the members of this struct in some places instead of zero initializing it.
    struct token_stack_node *next;
    struct token_array tokens;
    smm at;
    
    struct define_node *define_to_reenable_on_exit;
};


//////////////////////////

enum ast_kind{
    AST_invalid,
    AST_none = AST_invalid,
    
    // Declarations
    AST_declaration,
    AST_function,
    AST_typedef,
    
    AST_declaration_list,
    
    // Types
    AST_void_type,
    AST_integer_type,
    AST_atomic_integer_type,
    AST_float_type,
    AST_bitfield_type,
    AST_pointer_type,
    AST_function_type,
    AST_array_type,
    AST_struct,
    AST_union,
    AST_enum,
    
    AST_unresolved_type,
    
    // Primary Expressions
    AST_identifier,
    AST_string_literal,
    AST_integer_literal,
    AST_float_literal,
    AST_compound_literal,
    
    AST_pointer_literal,             // (struct s *)1337
    AST_pointer_literal_deref,       // *(struct s *)1337 or ((struct s *)1337)->member
    
    // Unary Expressions
    AST_cast,
    AST_cast_lhs,
    
    AST_implicit_address_conversion, // Used if an array or a function is implicitly converted to a pointer.
    AST_implicit_address_conversion_lhs,
    
    AST_unary_postinc,
    AST_unary_postdec,
    
    AST_unary_preinc,
    AST_unary_predec,
    
    AST_unary_logical_not,
    AST_unary_bitwise_not,
    AST_unary_deref,
    AST_unary_array_index,
    AST_unary_function_call,
    AST_unary_minus,
    AST_unary_plus,
    AST_unary_address,
    
    AST_sizeof,
    AST_alignof,
    
    // Binary Expressions
    AST_binary_times,
    AST_binary_divide,
    AST_binary_mod,
    
    AST_binary_plus,
    AST_binary_minus,
    
    AST_binary_left_shift,
    AST_binary_right_shift,
    
    AST_binary_and,
    AST_binary_or,
    AST_binary_xor,
    
    // compare AST_* same order as the tokens
    AST_binary_logical_equals,
    AST_binary_logical_unequals,
    
    AST_binary_bigger_equals,
    AST_binary_smaller_equals,
    AST_binary_bigger,
    AST_binary_smaller,
    
    AST_binary_bigger_equals_signed,
    AST_binary_smaller_equals_signed,
    AST_binary_bigger_signed,
    AST_binary_smaller_signed,
    
    // Float comparisons @WARNING: we use the order.
    AST_binary_logical_equals_float,
    AST_binary_logical_unequals_float,
    
    AST_binary_bigger_equals_float,
    AST_binary_smaller_equals_float,
    AST_binary_bigger_float,
    AST_binary_smaller_float,
    
    
    AST_logical_and,
    AST_logical_or,
    
    // Assignment AST_*, same order as TOKEN_*_equals
    AST_assignment,
    AST_and_assignment,
    AST_or_assignment,
    AST_xor_assignment,
    AST_plus_assignment,
    AST_minus_assignment,
    AST_left_shift_assignment,
    AST_right_shift_assignment,
    AST_times_assignment,
    AST_divide_assignment,
    AST_modulo_assignment,
    
    AST_duplicate,
    AST_duplicate_lhs, // For punting compound assignments.
    AST_swap_lhs_rhs,  // For weird stuff like `integer + pointer` or `integer[array]`.
    
    AST_member,
    AST_member_deref,
    AST_pointer_subscript,
    AST_array_subscript,
    
    AST_comma_expression,
    
    AST_conditional_expression,
    AST_conditional_expression_true,  // 1 ? <expr> : <expr>
    AST_conditional_expression_false, // 0 ? <expr> : <expr>
    
    AST_function_call,
    
    AST_initializer, // Used inside initializer lists.
    AST_array_range, // GNU extension: array[1 ... 5] - only allowed in initializers.
    
    // Statements
    AST_scope,
    
    AST_empty_statement, 
    AST_return,
    
    AST_switch,
    AST_case,
    
    AST_asm_block,
    AST_embed,
    
    AST_panic, 
    
    // :ir_refactor
    AST_jump_if_true,
    AST_jump_if_false,
    AST_jump,
    AST_jump_label,
    
    AST_pop_expression,
    
    AST_temp,
    AST_skip,
    
    AST_emitted_float_literal, // yuck, we copy float-literals in the back-end.
    
    AST_count,
};

#define REMOVE_AST 0

struct ast{
    enum ast_kind kind;
    s32 s;
#if !REMOVE_AST
    struct ast_type *resolved_type;
    struct ast *defined_type; 
#endif
    // :defined_types
    // if the ast has a 'AST_typedef' or 'AST_enum' type, we store it here for error reporting. 
    // Also if it is just promoted, we retain that information, so we can warn on 'u8 = u8 + u8' usw.
    // :retain_type_information_through_promotion 
};

struct expr{
    struct ast *ast;
    struct token *token;
    
    struct ast_type *resolved_type;
    struct ast *defined_type; 
    // :defined_types
    // if the ast has a 'AST_typedef' or 'AST_enum' type, we store it here for error reporting. 
    // Also if it is just promoted, we retain that information, so we can warn on 'u8 = u8 + u8' usw.
    // :retain_type_information_through_promotion 
};

enum type_flags{
    TYPE_FLAG_none          = 0x0,
    TYPE_FLAG_pdb_temporary = 0x1, 
    TYPE_FLAG_pdb_permanent = 0x2, 
    TYPE_FLAG_ends_in_array_of_unknown_size = 0x4,
    TYPE_FLAG_is_intrin_type = 0x8,
    TYPE_FLAG_is_atomic      = 0x10,
    TYPE_FLAG_is_user_aligned = 0x20,
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
    struct ast *value;
};

struct ast_list{
    struct ast_list_node *first;
    struct ast_list_node *last;
    smm count;
};

#define for_ast_list(list) for(struct ast_list_node *it = (list).first; it; it = it->next)


#define DECLARATION_FLAGS_is_global                                0x1
#define DECLARATION_FLAGS_is_enum_member                           0x2
#define DECLARATION_FLAGS_is_big_function_argument                 0x4
#define DECLARATION_FLAGS_is_local_persist                         0x8

#define DECLARATION_FLAGS_need_dllimport_stub_function             0x20
#define DECLARATION_FLAGS_is_reachable_from_entry                  0x40
#define DECLARATION_FLAGS_is_static                                0x80

#define DECLARATION_FLAGS_is_dllimport                             0x200
#define DECLARATION_FLAGS_is_dllexport                             0x400
#define DECLARATION_FLAGS_is_selectany                             0x800

#define DECLARATION_FLAGS_is_extern                                0x1000
#define DECLARATION_FLAGS_is_unnamed                               0x2000 // Used for struct and array literals, to not emit a symbol for them.
#define DECLARATION_FLAGS_is_thread_local                          0x4000
#define DECLARATION_FLAGS_is_intrinsic                             0x8000

struct declaration_node{
    struct declaration_node *next;
    struct ast_declaration *decl;
};

struct ast_declaration{
    // @WARNING: This needs to match the part in ast_function.
    struct ast base;
    struct ast_type *type;
    struct ast *defined_type; // Either 'AST_enum' or 'AST_typedef' or 'null' :defined_types
    struct token *identifier;
    
    smm offset_on_stack; // - stack relative if the declarations  is in a function scope
    
    // :dllimport_loading
    // memory_location and relative_virtual_address for dllimports, describe the entry in the dllimport table.
    
    u8 *memory_location; // if it is global a pointer to the evaluated initializer.
    smm relative_virtual_address; // In object files this is the offset in section.
    smm symbol_table_index;
    struct compilation_unit *compilation_unit;
    
    // @note: The 'assign_expr' is either a 'AST_compound_literal' or an 'AST_identifier'.
    //        Both of these start with an 'ast_identifier'.
    //        This is sort of confusing, as the 'initializer' has to be "interpreted",
    //        and starts of with the "lhs".
    //        For enum members this is an AST_integer_literal.
    struct ast *assign_expr; // the rhs of '=' if it exists, @WARNING: same slot as function->scope
    
    s64 overwrite_alignment;
    
    b64 flags;
    
    // Used to report 'declaration is not used' and 'declaration is only ever written' for local declarations.
    u32 _times_referenced;
    u32 _times_written;
    
    struct {
        struct declaration_reference_node *first;
        struct declaration_reference_node *last;
    } referenced_declarations;
};

struct declaration_list{
    struct ast_type *type_specifier;
    struct ast *defined_type_specifier;
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
    enum ast_kind kind; // AST_union, AST_struct or AST_enum
    struct compilation_unit *compilation_unit;
    struct token *sleeping_on;
    struct ast_scope *containing_scope;
};

static struct string type_prefix_for_unresolved_type(struct ast_unresolved_type *unresolved){
    if(unresolved->kind == AST_union)  return string("union");
    if(unresolved->kind == AST_struct) return string("struct");
    if(unresolved->kind == AST_enum)   return string("enum");
    
    invalid_code_path;
}

struct ast_array_type{
    struct ast_type base;
    b32 is_of_unknown_size;
    smm amount_of_elements;
    struct ast_type *element_type;
    struct ast *element_type_defined_type; // :defined_types
};

struct ast_bitfield_type{
    struct ast_type base;
    struct ast_type *base_type;
    u32 bit_index;
    u32 width;
};

// @note: needs to match ast_pointer_literal below
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

// @note: needs to match ast_integer_literal above
struct ast_pointer_literal{
    struct ast base;
    u8 *pointer;
};

// We have to be able to cast lhs float literals.
struct ast_float_literal{
    struct ast base;
    f64 value;
};

// These get allocated in `emit_x64`.
struct ast_emitted_float_literal{
    struct ast base;
    f64 value;
    
    struct ast_emitted_float_literal *next;
    u32 relative_virtual_address; // @note: float  literals get loaded rip relative, so this is here to patch
};

struct ast_string_literal{
    struct ast base;
    struct ast_string_literal *next;
    
    struct string value;
    enum string_kind string_kind;
    u32 relative_virtual_address;
    u32 symbol_table_index; // needed for .obj (this should maybe be named unique_string_index as that is what it realy is)
};

struct ast_compound_literal{
    struct ast base;
    struct ast_declaration *decl;
    
    smm initializer_size;
    smm trailing_array_size;
};

inline smm get_declaration_alignment(struct ast_declaration *decl){
    smm alignment = decl->type->alignment;
    if(decl->overwrite_alignment) alignment = decl->overwrite_alignment;
    return alignment;
}

inline smm get_declaration_size(struct ast_declaration *decl){
    smm size = decl->type->size;
    if(decl->assign_expr && decl->assign_expr->kind == AST_compound_literal){
        struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)decl->assign_expr;
        size += compound_literal->trailing_array_size;
    }
    return size;
}

struct ast_unary_op{
    struct ast base;
};

struct ast_binary_op{
    struct ast base;
};

struct ast_cast{ // @ir_refactor: In the future there should be more than just `AST_cast` such that we can get rid of the `operand` member.
    struct ast base;
    struct ast_type *cast_to;
    struct ast_type *cast_what;
};

// :ir_refactor - This is how all "ast" should look.
struct ast_duplicate_lhs{
    struct ast base;
};

struct ast_skip{
    struct ast base;
    u32 size_to_skip;
};

struct ast_swap_lhs_rhs{
    struct ast base;
};

struct ast_panic{
    struct ast base;
};

struct ast_temp{
    struct ast base;
};

struct ast_pop_expression{
    struct ast base;
};

struct ast_pop_lhs_expression{
    struct ast base;
};

struct ast_jump{
    struct ast base;
    smm label_number;
};

struct ast_jump_if_true{
    struct ast base;
    smm label_number;
};

struct ast_jump_if_false{
    struct ast base;
    smm label_number;
};

struct ast_jump_label{
    struct ast base;
    smm label_number;
};

struct ast_dot_or_arrow{
    struct ast base;
    struct compound_member *member; // @cleanup: This should probably be an index.
};

struct ast_subscript{
    struct ast base;
};

struct ast_array_range{ // array[1 ... 5] - only allowed in initializers.
    struct ast base;
    struct ast *lhs;
    u64 start_index;
    u64 end_index;
};

struct ast_initializer{
    struct ast base;
    u64 offset;
};

struct ast_return{
    struct ast base;
};

enum scope_flags{
    SCOPE_FLAG_none              = 0x0,
    SCOPE_FLAG_can_continue      = 0x1,
    SCOPE_FLAG_can_break         = 0x2,
    SCOPE_FLAG_is_function_scope = 0x4, // This is the root scope of a functions, this might still have a parent, if the function is local.
    
    SCOPE_FLAG_found_an_alive_break = 0x10, // This is set if there is a 'break' statement in a block which does not return.
    
};

struct ast_scope{
    struct ast base;
    struct token *token;
    struct ast_scope *parent;
    
    struct ast_asm_block *asm_block;
    
    struct{
        struct ast_scope *next;
        struct ast_scope *first;
        struct ast_scope *last; // @note: if the order does not matter, we could eliminate this member... not sure.
        smm count;
    } subscopes;
    
    enum scope_flags flags;
    
    // @note: A hash table for the declarations.
    struct ast_declaration **declarations;
    u32 amount_of_declarations;
    u32 current_max_amount_of_declarations;
    
    struct ast_compound_type **compound_types;
    u32 amount_of_compound_types;
    u32 current_max_amount_of_compound_types;
    
    // These are indices into the `function->line_information` array
    // and are used to determine which region of code holds which declarations.
    u32 start_line_index;
    u32 end_line_index;
};

static struct token *get_initializer_token(struct ast_declaration *decl){
    if(decl->flags & DECLARATION_FLAGS_is_enum_member) return decl->identifier;
    
    if(decl->base.kind == AST_function){
        struct ast_scope *scope = (struct ast_scope *)decl->assign_expr;
        return scope->token;
    }
    
    assert(decl->assign_expr && (decl->assign_expr->kind == AST_identifier || decl->assign_expr->kind == AST_compound_literal));
    struct ast_identifier *ident = (struct ast_identifier *)decl->assign_expr;
    return ident->decl->identifier;
}

struct compound_member{
    struct token *name;
    struct ast_type *type;
    struct ast *defined_type;
    
    // :next_member_increment
    // 
    // Sigh, because of :member_list_contains_both_linear_and_nested and designated initializer that might initialize union members we have to keep track of the next member that is to be initialized.
    smm next_member_increment; 
    union{
        smm offset_in_type;
        smm enum_value;
    };
};

struct ast_compound_type{
    struct ast_type base;
    struct atom identifier;
    struct compilation_unit *compilation_unit;
    
    // A dynamic array for the members.
    // Contains both the linear members (meaning unnamed members "expanded") 
    // as well as the nested members. :member_list_contains_both_linear_and_nested
    struct compound_member *members;
    u32 amount_of_members;
    u32 current_max_amount_of_members;
};

#define FUNCTION_TYPE_FLAGS_is_varargs    0x1
// #define FUNCTION_TYPE_FLAGS_is_intrinsic  0x2
#define FUNCTION_TYPE_FLAGS_is_printlike  0x4
#define FUNCTION_TYPE_FLAGS_is_inline_asm 0x8 // @cleanup: This should not be on the type.
#define FUNCTION_TYPE_FLAGS_is_noreturn   0x10
struct ast_function_type{
    struct ast_type base;
    struct ast_type *return_type;
    struct ast *return_type_defined_type; // :defined_type @cleanup: make sure this is filled 
    
    b64 flags;
    
    struct ast_list argument_list;
};

struct ast_function{
    union{
        struct{
            struct ast base;
            struct ast_function_type *type;
            struct ast *defined_type; // either 'AST_enum' or 'AST_typedef' or 'null'
            struct token *identifier;
            smm offset_in_text_section;
            u8 *memory_location; // @cleanup: rename? this is where the thing onces emitted. here for patching
            smm relative_virtual_address; // needs to be 64 bit, as we point to it not knowing its size
            smm symbol_table_index; // for .obj
            struct compilation_unit *compilation_unit;
            struct ast *scope; // @WARNING: We use that this is in the same slot as 'ast_declaration->assign_expr'.
            smm overwrite_alignment;
            u64 decl_flags;
            
            u32 times_referenced;
            u32 times_written;
            
            struct {
                struct declaration_reference_node *first;
                struct declaration_reference_node *last;
            } referenced_declarations;
        };
        struct ast_declaration as_decl;
    };
    
    u8 *start_in_ast_arena;
    u8 *end_in_ast_arena;
    
    
    u8 *base_of_prolog;
    smm size_of_prolog; // these could be smaller
    
    // right now _just_ the function without the prolog
    u8 *base_of_main_function;
    smm byte_size_without_prolog;
    
    smm rsp_subtract_offset;
    smm byte_size;
    
    smm stack_space_needed; // after parsing this is what we need for declarations, and then in emit 
    // it gets adjusted to the full thing (including stack spilling usw)

    smm amount_of_jump_labels;
    
    struct ast_list static_variables;    
    
    // debug info:
    
    struct{ // @cleanup: should we have a line information arena?
        struct function_line_information{
            u32 line;
            u32 offset;// :function_line_information - This is first the offset in the 'ast_arena', then later the offset in the code.
        } *data;
        smm size;
        smm capacity;
    } line_information;
    
    smm debug_size;
    u8 *debug_info;
    u32 debug_symbol_offset;
    u32 pushed_register_mask; // (1 << REGISTER_XXX) is set if we pushed that register in the prolog
    
    struct dll_import_node *dll_import_node;
};

struct ast_switch{
    struct ast base;
    
    struct ast_jump_label *default_jump_label; // Either the default case or the break label.
    
    struct ast_list case_list;
};

struct ast_case{
    struct ast base;
    struct token *token;
    u64 value;
    
    struct jump_context *jump; // We use this as a label, only used in emit.
};

struct ast_function_call{
    struct ast base;
    struct ast_function_type *function_type;
    smm call_arguments_count;
};

struct ast_label{
    struct ast_label *next;
    
    struct token *token;
    
    struct atom ident;
    
    // :ir_refactor - new way.
    struct ast_jump_label *jump_label;
};

struct ast_goto{
    struct ast_goto *next;
    
    struct token *token;
    struct atom ident;
    
    // :ir_refactor - new way.
    struct ast_jump *jump;
};

struct ast_conditional_expression{
    struct ast base;
};

struct conditional_expression_information{
    struct ast *condition;
    struct ast *temp;
    struct ast_jump *end_jump;
};

struct ast_asm_block{
    struct ast base;
    struct token *token;
    struct{
        struct asm_instruction *first;
        struct asm_instruction *last;
    }instructions;
};

struct ast_embed{
    struct ast base;
    struct token *token;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////


