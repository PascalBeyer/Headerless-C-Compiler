
#include "std.c"
#include "windows.c"

#include "options.h"

#if 0
#define log_print(format, ...)  print(format  " (thread %u)\n", __VA_ARGS__, GetCurrentThreadId());
#elif 0
#define log_print(format, ...)  print(format "\n", __VA_ARGS__);
#else
#define log_print(formal, ...)
#endif

#include "timing.c"
#include "ast.c"
#include "cli.c"

struct parse_work{
    struct token_array tokens;
    struct token *sleeping_ident; // the one who sleeps. can be null if we don't know yet. For error reporting
    struct compilation_unit *compilation_unit;
    
    u64 pragma_alignment;
    
    struct ast_function *function; // This is here to communicate between 'worker_parse_global_scope_entry' and 'worker_parse_function'.
};

struct work_tokenize_file{
    struct string absolute_file_path;
    smm file_size;
    struct compilation_unit *compilation_unit;
};

struct work_queue_entry{
    struct work_queue_entry *next;
    struct token *sleeping_on;
    void *data;
};

struct work_queue{
    union{
        struct{
            struct work_queue_entry *end;
            struct work_queue_entry *begin;
        };
        m128 mem;
    };
    
    // incremented when pushed on, decremented if work is finished
    smm work_entries_in_flight;
};

//_____________________________________________________________________________________________________________________

struct thread_info{
    struct context *context;
    s64 thread_index;
};

struct sleeper_node{
    union{
        struct{
            struct token *token;
            struct work_queue_entry *first_sleeper;
        };
        m128 mem;
    };
};

enum {
    SLEEP_on_decl,
    SLEEP_on_struct,
};

struct sleeper_table{
    struct sleeper_node *nodes;
    u64 amount_of_nodes;
    u64 capacity;
    u64 mask;
    
    s64 is_locked_for_growing;
    s64 threads_in_flight;
};


//_____________________________________________________________________________________________________________________
// Ast table

struct ast_node{
    union{
        struct{
            struct token *token;
            struct ast *ast;
        };
        m128 mem;
    };
};

struct ast_table{
    struct ast_node *nodes;
    u64 amount_of_nodes;
    u64 capacity;
    u64 mask;
    
    s64 is_locked_for_growing;
    s64 threads_in_flight;
};


func struct ast_table ast_table_create(smm initial_capacity){
    struct ast_table ret = zero_struct;
    ret.capacity = initial_capacity;
    ret.mask = ret.capacity - 1;
    smm byte_size = ret.capacity * sizeof(struct ast_node);
    ret.nodes = os_allocate_memory(byte_size);
    
    if(!ret.nodes){
        print("Memory failure in ast table creation\n"); 
        os_panic(1);
    }
    
    return ret;
}

func void maybe_grow_ast_table__internal(struct ast_table *table){
    
    if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
        
        // Lets wait until there are not threads in flight.
        while(atomic_load(s64, table->threads_in_flight));
        
        // print_ast_table(table);
        log_print("GROWING AST TABLE! %d\n", table->amount_of_nodes);
        u64 old_capacity = table->capacity;
        struct ast_node *old_nodes = table->nodes;
        table->capacity <<= 1;
        void *memory = os_allocate_memory(table->capacity * sizeof(*table->nodes));
        if(!memory){
            print("Memory failure in ast table.\n");
            os_panic(1);
        }
        table->nodes = memory;
        table->mask = table->capacity - 1;
        
        for(u64 i = 0; i < old_capacity; i++){
            struct ast_node *node = old_nodes + i;
            if(!node->token) continue;
            
            u64 slot = node->token->atom.string_hash & table->mask;
            for(u64 slot_offset = 0; slot_offset < table->capacity; slot_offset++){
                struct ast_node *new_node = table->nodes + ((slot + slot_offset) & table->mask);
                if(new_node->token) continue;
                *new_node = *node;
                break;
            }
        }
        os_free_memory(old_nodes);
        
        // print_ast_table(table);
    }
    
}

func struct ast *ast_table_add_or_return_previous_entry(struct ast_table *table, struct ast *ast_to_insert, struct token *token){
    while(true){
        if(atomic_load(s64, table->is_locked_for_growing)) continue;
        
        // @cleanup: maybe these conditions need atomic loads
        if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
            
            // ***
            
            //spin until WE set table->is_locked_for_growing
            void *old_is_locked = atomic_compare_and_swap(&table->is_locked_for_growing, (void *)true, (void *)false);
            if(old_is_locked) continue;
            // we are the thread that set 'is_locked_for_growing' to true
            
            // we could have set the condition ironiously if the actuall growing thead finished while
            // we were at ***, so lets check the condition again
            
            maybe_grow_ast_table__internal(table);
            // unlock
            table->is_locked_for_growing = false;
        }
        
        break;
    }
    
    atomic_preincrement(&table->threads_in_flight);
    
    // @WARNING: this portion needs to stay in sync with :c_compat_build_global_definition_table
    
    assert(ast_to_insert);
    u64 index = (token->atom.string_hash & table->mask);
    assert(index < table->capacity);
    
    struct ast_node to_insert;
    to_insert.token = token;
    to_insert.ast = ast_to_insert;
    
    for(u64 i = 0; i < table->capacity; i++){
        struct ast_node *node = table->nodes + index;
        struct ast *ast = node->ast;
        
        if(ast){
            if(node->token == token || atoms_match(node->token->atom, token->atom)){
                log_print("   allready inserted");
                
                atomic_predecrement(&table->threads_in_flight);
                return ast;
            }
            index += 1;
            index &= table->mask;
        }else{
            b32 success = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &(m128)zero_struct);
            if(success){
                atomic_add((s64 *)&table->amount_of_nodes, 1);
                atomic_predecrement(&table->threads_in_flight);
                return null;
            }
            // @note: Don't increment, as someone could have just added _us_ in here and we then
            //        want to return the redecl.
        }
    }
    
    invalid_code_path;
}

func struct ast *ast_table_get(struct ast_table *table, struct atom atom){
    while(atomic_load(s64, table->is_locked_for_growing));
    
    atomic_preincrement(&table->threads_in_flight);
    
    u64 index = atom.string_hash & table->mask;
    assert(index < table->capacity);
    
    for(u64 i = 0; i < table->capacity; i++){
        struct ast_node *node = table->nodes + index;
        struct ast *ast = node->ast;
        
        if(!ast) {
            atomic_predecrement(&table->threads_in_flight);
            return null;
        }
        if(atoms_match(node->token->atom, atom)){
            atomic_predecrement(&table->threads_in_flight);
            return ast;
        }
        
        index += 1;
        index &= table->mask;
    }
    
    invalid_code_path;
}


//_____________________________________________________________________________________________________________________

struct file{
    struct os_file file;
    char *absolute_file_path;
    
    s32 file_index;
    int is_system_include;
    
    int in_progress;
    
    smm lines;
    smm amount_of_times_included; // not counting pragma(once)
    
    struct token_array tokens;
    
    // pdb debug info
    u32 ipi;
    u32 offset_in_names;
    u32 offset_in_f4;
};

struct dll_import_node{
    struct dll_import_node *next;
    
    struct string import_name;
    u8 *memory_location;
    u16 ordinal_hint;
    
    // If the declaration has the DECLARATION_FLAGS_need_dllimport_stub_function flag set, we have to emit a stub as well.
    u32 stub_relative_virtual_address;
    u8 *stub_memory_location;
};

struct dll_node{
    struct dll_node *next;
    struct string name;
    
    struct{
        struct dll_import_node *first;
        struct dll_import_node *last;
        smm count;
    } import_list;
};

struct library_node{
    struct library_node *next;
    struct string path;
    
    struct os_file file;
    
    u32  amount_of_members;
    u32 *member_offsets;
    u32  amount_of_symbols;
    u16 *symbol_member_indices;
    
    u64 amount_of_import_symbols;
    
    struct library_import_table_node{
        struct string string;
    } *import_symbol_string_table;
};

struct compilation_unit{
    struct compilation_unit *next;
    
    smm index;
    struct ast_table static_declaration_table;
    
    struct sleeper_table static_sleeper_table;
    
    struct file *main_file;
    
    struct{
        struct is_token_static{
            struct token *token;
            b32 is_static;
        } *data;
        smm size;
        smm capacity;
    } is_token_static_table;
    
};

// for #pragma comment(linker, "/ALTERNATENAME:strdup=_strdup")
struct alternate_name{
    struct alternate_name *next;
    struct token *token;
    struct atom source;
    struct atom destination;
};

struct keyword_table_entry{
    struct atom keyword;
    enum token_type type;
};

struct directive_table_entry{
    struct atom directive;
    enum preprocessor_directive type;
};

enum compile_stage{
    COMPILE_STAGE_none,
    
    COMPILE_STAGE_tokenize_files,
    COMPILE_STAGE_parse_global_scope_entries,
    COMPILE_STAGE_parse_function,
    COMPILE_STAGE_emit_code,
    //COMPILE_STAGE_patch,
    
    COMPILE_STAGE_count,
};

enum output_file_type{
    OUTPUT_FILE_unset,
    OUTPUT_FILE_exe,
    OUTPUT_FILE_dll,
    OUTPUT_FILE_obj,
    OUTPUT_FILE_efi,
};

// :globals
static struct{
    
    // :options (don't change after initialization!)
    struct cli_options cli_options;
    
    struct thread_info *thread_infos;
    smm thread_count;
    b32 threads_should_exit;
    
    // These 3 members are highly dependent on each other.
    enum subsystem subsystem;
    enum output_file_type output_file_type;
    struct atom entry_point_name;
    
    struct ast_function *entry_point;
    
    struct string_list system_include_directories;     // full paths
    
    
    struct compilation_unit hacky_global_compilation_unit;
    
    struct{
        struct compilation_unit *first;
        struct compilation_unit *last;
    } compilation_units;
    
    struct token_array predefined_tokens;
    
    struct{
        struct dll_node *first;
        struct dll_node *last;
        smm amount;
    } dlls;
    
    struct string_list library_paths;
    struct string_list specified_libraries;
    struct{
        struct library_node *first;
        struct library_node *last;
        smm amount;
    } libraries;
    
    struct {
        struct alternate_name *first;
        struct alternate_name *last;
    } alternate_names; // maybe weak aliases?
    
    struct string_list pragma_compilation_units;
    
    // A perfect hash table to lookup keywords.
    smm keyword_table_size;
    struct keyword_table_entry *keyword_table;
    
    // A perfect hash table to lookup preprocessor directives.
    smm directive_table_size;
    struct directive_table_entry *directive_table;
    
    HANDLE preprocessed_file_handle;
    
    struct memonic_info{
        struct string string;
        enum memonic memonic;
    } memonic_table[0x1000];
    
    // Global run time structures (have to be thead aware).
    
    struct{
        struct file *invalid_file; // 'file_index = -1' is the invalid file, used for predefined tokens.
        struct file *data[0x10000];
        smm size;
    } file_table;
    
    struct {
        struct system_include_file_entry{
            struct string include_string;  // the/file.h (for including <the/file.h> or "the/file.h".
            char *absolute_file_path;      // C:/path/to/include/the/file.h
            smm file_size;
            union{
                struct file *file;  // 0, then 1, then the pointer to the file.
                smm in_progress;
            };
        } *entries;
        
        smm capacity;
        smm count;
    } system_include_table;
    
    b32 an_error_has_occurred;
    
    // @cleanup: Maybe put cacheline sized pad around these global structures?
    enum compile_stage compile_stage;
    struct work_queue work_queue_tokenize_files;
    struct work_queue work_queue_parse_global_scope_entries;
    struct work_queue work_queue_parse_functions;
    struct work_queue work_queue_emit_code;
    HANDLE wake_event;
    
    struct sleeper_table compound_sleeper_table;
    struct sleeper_table declaration_sleeper_table;
    
    struct ast_table global_declarations;
    struct ast_table external_declarations_at_function_scope; // This is only used when compiling to an object file, where it is valid to have an unresolved exernal symbol at function scope.
    struct ast_table compound_types;
    // struct ast_table type_defs; this has to be the same as global_declarations because '(asd)expr' would sleep on (asd) as an expression
    
    // constant global stuff
    struct file invalid_file;
    struct atom keyword_cdecl;
    struct atom keyword_stdcall;
    struct atom keyword_dllimport;
    struct atom keyword_dllexport;
    struct atom keyword_thread;
    struct atom keyword_align;
    struct atom keyword_noreturn;
    struct atom keyword_noinline;
    struct atom keyword_selectany;
    struct atom keyword_intrin_type;
    
    // hlc declspecs
    struct atom keyword_inline_asm;
    struct atom keyword_packed;
    struct atom keyword_printlike;
    
    struct atom pragma_once;
    struct atom pragma_pack;
    struct atom pragma_comment;
    struct atom pragma_compilation_unit; // hlc extension
    
    struct atom keyword__VA_ARGS__;
    
    struct atom unnamed_tag;
    struct atom unnamed_enum;
    struct atom invalid_identifier;
    
    // :hlc_extensions
    struct atom keyword_data;
    struct atom keyword_size;
    struct atom keyword_main;
    
    b32 have_absolute_patch;
    
    // We build a graph of all global declaration-references:
    // for executables  this will be all dllexports and the entry point.
    // for object files this will be all declaration with external storage class.
    struct declaration_reference_node{
        struct declaration_reference_node *next;
        struct ast_declaration *declaration;
        struct token *token;
    } *globally_referenced_declarations;
    
    // 
    // Tokens for basic types:
    // 
    
    struct token token_void;
    struct token token_Bool;
    
    struct token token_u8;
    struct token token_u16;
    struct token token_u32;
    struct token token_u64;
    
    struct token token_s8;
    struct token token_s16;
    struct token token_s32;
    struct token token_s64;
    
    struct token token_atomic_bool;
    
    struct token token_atomic_u8;
    struct token token_atomic_u16;
    struct token token_atomic_u32;
    struct token token_atomic_u64;
    
    struct token token_atomic_s8;
    struct token token_atomic_s16;
    struct token token_atomic_s32;
    struct token token_atomic_s64;
    
    struct token token_f32;
    struct token token_f64;
    
    struct token token_poison;
    
    // 
    // Basic types:
    // @WARNING: The non-atomic version need to stay in the same order as the atomic versions, 
    //           so that we can use address manipulation to go from atomic to non atomic.
    //           (See :translate_atomic_to_non_atomic_and_back)
    // 
    
    struct ast_type typedef_void;
    struct ast_type typedef_Bool;
    
    struct ast_type typedef_u8;
    struct ast_type typedef_u16;
    struct ast_type typedef_u32;
    struct ast_type typedef_u64;
    
    struct ast_type typedef_s8;
    struct ast_type typedef_s16;
    struct ast_type typedef_s32;
    struct ast_type typedef_s64;
    
    struct ast_type typedef_f32;
    struct ast_type typedef_f64;
    
    struct ast_type typedef_atomic_bool;
    
    struct ast_type typedef_atomic_u8;
    struct ast_type typedef_atomic_u16;
    struct ast_type typedef_atomic_u32;
    struct ast_type typedef_atomic_u64;
    
    struct ast_type typedef_atomic_s8;
    struct ast_type typedef_atomic_s16;
    struct ast_type typedef_atomic_s32;
    struct ast_type typedef_atomic_s64;
    
    struct ast_type typedef_poison;
    
    struct ast_type *typedef_void_pointer;
    struct ast_type *typedef_u8_pointer;
    struct ast_type *typedef_s8_pointer;
    
    //
    // Invalid/Default values
    //
    struct ast_declaration zero_decl;
    struct ast empty_statement;
    struct ast guard_ast;
    struct token *invalid_identifier_token;
    struct token invalid_token;
    struct ast_declaration *poison_declaration;
    
    struct ast_declaration *tls_index_declaration;
    
} globals;

//_____________________________________________________________________________________________________________________

// @note: should be in 'ast.c' but globals has to be defined...

// this is fine, as typedef's and enum's actually resolved to these.
func b32 type_is_signed(struct ast_type *type){
    if(&globals.typedef_s8  == type) return true;
    if(&globals.typedef_s16 == type) return true;
    if(&globals.typedef_s32 == type) return true;
    if(&globals.typedef_s64 == type) return true;
    return false;
}

func b32 type_is_arithmetic(struct ast_type *type){
    return (type->kind == AST_integer_type || type->kind == AST_atomic_integer_type || type->kind == AST_float_type);
}

func b32 type_is_array_of_unknown_size(struct ast_type *type){
    if(type->kind == AST_array_type){
        struct ast_array_type *array = (struct ast_array_type *)type;
        if(array->is_of_unknown_size) return true;
    }
    return false;
}

//_____________________________________________________________________________________________________________________

// @cleanup: reserve
struct emit_pool{
    u8 *base;
    u8 *current;
    u8 *end;
    smm capacity;
    smm reserved; // assumed to never change
};

enum register_kind{
    REGISTER_KIND_gpr,
    REGISTER_KIND_xmm,
    REGISTER_KIND_count,
};

func enum register_kind get_register_kind_for_type(struct ast_type *type){
    enum register_kind kind = REGISTER_KIND_gpr;
    
    if(type == &globals.typedef_f32 || type == &globals.typedef_f64){
        kind = REGISTER_KIND_xmm;
    }
    
    if(type->flags & TYPE_FLAG_is_intrin_type){
        // All the 'intrin_types' should be loaded into xmm registers.
        kind = REGISTER_KIND_xmm;
    }
    
    return kind;
}

struct register_allocator{
    struct emit_location *emit_location_map[16];
    u32 rolling_index;
};

//_____________________________________________________________________________________________________________________
// jump nodes

struct jump_node{
    struct jump_node *next;
    u8 *patch_location;
    smm jump_from;
};

enum jump_context_condition{
    JUMP_CONTEXT_jump_always,
    JUMP_CONTEXT_jump_on_true,
    JUMP_CONTEXT_jump_on_false,
};

struct jump_context{
    struct{
        struct jump_node *first;
        struct jump_node *last;
    } jump_list;
};

//_____________________________________________________________________________________________________________________


struct pragma_once_list_node{
    struct pragma_once_list_node *next;
    struct file *file;
};


// @note: thread local datum
struct context{
    //
    // General
    //
    struct memory_arena scratch; // Cleared after every thread_do_work.
    struct memory_arena *arena; // Right now never cleared. Think later about how we can minimize memory usage.
    struct memory_arena ast_arena;
    
    struct thread_info *thread_info;
    
    struct{
        struct define_list *lists;
        u64 mask;
        u64 amount;
        u64 capacity;
    } define_table;
    
    struct{
        struct pragma_once_list_node *first;
        struct pragma_once_list_node *last;
    } pragma_once_file_list;
    
    struct{
        struct token_stack_node *first;
        struct token_stack_node *last;
    } token_stack;
    
    struct static_if_evaluate_stack_node{
        enum static_if_evaluate_operation operation;
        int is_unsigned;
        u64 value;
        struct token *token;
        int should_skip_undefined_identifier;
    } static_if_evaluate_stack[1024];
    smm static_if_stack_at;
    
    struct token *macro_expansion_token;
    smm define_depth;
    
    b32 static_if_evaluate_should_skip_undefined_identifier; // used to not report errors for example for '#if defined(_MSC_VER) && _MSC_VER > 1337'
    b32 in_static_if_condition; // used to not process defined as a macro outside of a #if condition.
    
    //
    // parsing
    //
    
    // these members are now only for the parser
    struct token_array tokens;
    smm token_at; // :token_at_overshoot this does not have to be in bounds of the token array
    
#define MAX_STACK_USAGE kilo_bytes(512)
    u8 *low_stack_address;
    b32 in_lhs_expression;
    s32 ast_serializer;
    
    struct{
        struct pragma_pack_node *first;
        struct pragma_pack_node *last;
    } pragma_pack_stack;
    smm pragma_alignment;
    
    // :tracking_conditional_expression_depth_for_noreturn_functions
    // 
    // We keep track of how deep we are in conditional expressions, to only set 'SCOPE_FLAG_returns_a_value' 
    // on _Noreturn functions if we are not in a conditional expression.
    // This value will possibly be wrong if 'parse_expression' returns early, but in that case there was an error,
    // hence we don't care. But it means we have to reset this value in 'reset_context'.
    smm in_conditional_expression;
    int current_statement_returns_a_value;
    
    struct ast_stack_entry{
        enum ast_kind ast_kind;
        struct token *token;
        
        struct expr operand;
        void *other;
    } ast_stack[1024];
    smm ast_stack_at;
    
    struct ast_scope *current_scope;
    struct ast_function *current_function;       // This is set by `worker_parse_function` and later by `worker_emit_code`.
    struct ast_declaration *current_declaration; // This is set by `worker_parse_global_scope_entry`.
    struct compilation_unit *current_compilation_unit;
    
    struct ast_switch *current_switch;
    struct expr current_switch_on;
    struct token *current_switch_default_label_token;
    
    struct token *in_inline_asm_function;
    b32 current_asm_flags;
    
    smm jump_label_index;
    smm current_break_label;
    smm current_continue_label;
    // smm if_true_label;  // Used to track that && and || jump to the correct place if inside 
    // smm if_false_label;
    
    smm function_file_index;
    smm last_line_pushed; // :function_line_information
    smm last_offset_pushed;
    
    struct {
        struct ast_goto *first;
        struct ast_goto *last;
    } goto_list;
    struct {
    struct ast_label *first;
        struct ast_label *last;
    } label_list;
    
    //
    // Error stream
    //
    struct {
        struct error_report_node *first;
        struct error_report_node *last;
    } error_list;
    
    // This is set if we encounter a non-fatal error. We exit the current statement
    // then skip tokens until we see a token we expect, and hope that from there we
    // are able to still provide decent error messages.
    b32 should_exit_statement; 
    
    // An error occurred, it need not be fatal, if it is fatal (no other errors in the 
    // global scope entry should be reported) should_sleep is also set.
    b32 error; 
    
    // Avoid reporting too many errors/warnings.
    int warnings_reported;
    int errors_reported;
    
    //
    // Fields for 'begin_error_report' and 'end_error_report'.
    //
    b32 in_error_report;
    u32 errors_in_this_report; // Used to decide if we should set 'context->error' when the report ends.
    struct error_report_node *current_error_node;
    
    //
    // Sleeping
    //
    b32 should_sleep; // set in '_parser_sleep', we should exit the global scope entry.
    struct token *sleep_on;
    enum sleep_purpose sleep_purpose;
    s64 sleep_line;
    struct token *sleeping_ident;
    struct token *maybe_in_cast; // :sizeof/alignof threading bug
    
    //
    // Emiting
    //
    struct emit_pool emit_pool;
    u8 *current_emit_base;
    smm current_emit_offset_of_rsp; // done in register_declaration
    struct token *inline_asm_mode;
    struct emit_location *asm_block_return;
    
    smm max_amount_of_function_call_arguments;
    smm temporary_stack_allocator;
    smm temporary_stack_high_water_mark;
    
    union{
        struct{
            struct register_allocator gpr_allocator;
            struct register_allocator xmm_allocator;
        };
        struct register_allocator register_allocators[REGISTER_KIND_count];
    };
    
    struct emit_location *register_sp;
    struct emit_location *register_bp;
    
    struct jump_context *break_jump_context;
    struct jump_context *continue_jump_context;
    struct jump_context *jump_to_function_epilog;
    b32 should_not_emit_ret_jump;
    
    struct jump_label_information{
        struct jump_context context;
        smm jump_location;
    } *jump_labels;
    
    struct {
        struct inline_asm_function_argument *first;
        struct inline_asm_function_argument *last;
    } inline_asm_function_arguments;
    
    struct{
        struct ast_emitted_float_literal *first;
        struct ast_emitted_float_literal *last;
        smm amount_of_float_literals;
    } emitted_float_literals;
    
    struct{
        struct ast_string_literal *first;
        struct ast_string_literal *last;
        smm amount_of_strings;
    } string_literals;
    
    struct ast_list global_struct_and_array_literals; // Used for struct and array literals at global (or local persist) scope.
    
    struct ast_list local_functions;
    
    struct{
        struct alloca_patch_node *first;
        struct alloca_patch_node *last;
    } alloca_patch_nodes;
    
    //
    // Patching
    //
    struct {
        struct patch_node *first;
        struct patch_node *last;
    } local_patch_list;
    
    struct timing_events timing_events;
};

//_____________________________________________________________________________________________________________________
// Patching

enum patch_kind{
    PATCH_none,
    PATCH_absolute,
    PATCH_rip_relative,
    PATCH_section_offset, // used for thread local storage.
    PATCH_count,
};

struct patch_node{
    struct patch_node *next;
    
    enum patch_kind kind;
    struct ast *source;
    struct ast_declaration *dest_declaration;
    smm location_offset_in_dest_declaration;
    smm location_offset_in_source_declaration;
    smm rip_at;
};

func void emit_patch(struct context *context, enum patch_kind kind, struct ast *source_decl, smm source_offset, struct ast_declaration *dest_declaration, smm dest_offset, smm rip_at){
    struct patch_node *patch_node = push_struct(context->arena, struct patch_node);
    patch_node->source = source_decl;
    patch_node->dest_declaration = dest_declaration;
    patch_node->location_offset_in_dest_declaration   = dest_offset;
    patch_node->location_offset_in_source_declaration = source_offset;
    patch_node->kind = kind;
    patch_node->rip_at = rip_at;
    
    if(kind == PATCH_absolute){
        assert(patch_node->location_offset_in_dest_declaration + 8 <= get_declaration_size(dest_declaration));
    }
    
    if(!globals.have_absolute_patch && kind == PATCH_absolute){
        globals.have_absolute_patch = true;
    }
    
    sll_push_back(context->local_patch_list, patch_node);
}

//_____________________________________________________________________________________________________________________
// Reference graph.

func void add_global_reference_for_declaration(struct memory_arena *arena, struct ast_declaration *declaration){
    assert(globals.compile_stage == COMPILE_STAGE_parse_global_scope_entries);
    
    struct declaration_reference_node *node = push_struct(arena, struct declaration_reference_node);
    node->declaration = declaration;
    node->token = declaration->identifier;
    
    struct declaration_reference_node *list;
    do{
        list = atomic_load(struct declaration_reference_node *, globals.globally_referenced_declarations);
        node->next = list;
    }while(atomic_compare_and_swap(&globals.globally_referenced_declarations, node, list) != list);
}

//_____________________________________________________________________________________________________________________

#include "diagnostic.c"
//_____________________________________________________________________________________________________________________


func struct string push_token_string(struct context *context, struct token *token, b32 print_whitespace_and_comments){
    if(token->type == TOKEN_comment){
        if(print_whitespace_and_comments){
            return push_format_string(&context->scratch, "%.*s", token->size, token->data);
        }else{
            return string(" ");
        }
    }else if(token->type == TOKEN_whitespace){
        if(print_whitespace_and_comments){
            return push_format_string(&context->scratch, "%.*s", token->size, token->data);
        }else{
            return string(" ");
        }
    }else if(token->type == TOKEN_newline){
        return string("\n");
    }else{
        return token_get_string(token);
    }
}

func void print_token(struct context *context, struct token *token, b32 print_whitespace_and_comments){
    print_string(push_token_string(context, token, print_whitespace_and_comments));
}

//_____________________________________________________________________________________________________________________
#include "ar.c"
//_____________________________________________________________________________________________________________________

func struct string push_type_string(struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type);

func void push_type_string__inner(struct string_list *list, struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type){
    switch(type->kind){
        case AST_void_type:
        case AST_float_type:
        case AST_integer_type:
        case AST_atomic_integer_type:{
            string_list_prefix(list, scratch, token_get_string(type->token));
        }break;
        case AST_union: case AST_struct: case AST_enum:{
            struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
            string_list_prefix(list, scratch, compound->identifier.string);
            char *prefix = "enum ";
            if(compound->base.kind == AST_union)  prefix = "union ";
            if(compound->base.kind == AST_struct) prefix = "struct ";
            string_list_prefix(list, scratch, cstring_to_string(prefix));
        }break;
        case AST_pointer_type:{
            struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)type;
            
            string_list_prefix(list, scratch, string("*"));
            
            push_type_string__inner(list, arena, scratch, pointer->pointer_to);
        }break;
        case AST_unresolved_type:{
            struct ast_unresolved_type *unresolved = (struct ast_unresolved_type *)type;
            string_list_prefix(list, scratch, token_get_string(unresolved->base.token));
            
            struct string type_prefix = type_prefix_for_unresolved_type(unresolved);
            
            string_list_prefix(list, scratch, string(" "));
            string_list_prefix(list, scratch, type_prefix);
        }break;
        case AST_function_type:{
            struct ast_function_type *function = cast(struct ast_function_type *)type;
            
            if(list->amount_of_strings){
                string_list_prefix(list, scratch, string("("));
                string_list_postfix(list, scratch, string(")"));
            }
            
            string_list_postfix(list, scratch, string("("));
            for_ast_list(function->argument_list){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                struct string decl_type = push_type_string(scratch, scratch, decl->type);
                string_list_postfix(list, scratch, decl_type);
                if(it->next) string_list_postfix(list, scratch, string(", "));
            }
            string_list_postfix(list, scratch, string(")"));
            
            push_type_string__inner(list, arena, scratch, function->return_type);
        }break;
        case AST_array_type:{
            struct ast_array_type *arr = (struct ast_array_type *)type;
            
            if(list->amount_of_strings){
                string_list_prefix(list, scratch, string("("));
                string_list_postfix(list, scratch, string(")"));
            }
            
            struct string array_postfix = push_format_string(scratch, "[%lld]", arr->amount_of_elements);
            string_list_postfix(list, scratch, array_postfix);
            
            push_type_string__inner(list, arena, scratch, arr->element_type);
            
        }break;
        case AST_bitfield_type:{
            struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)type;
            push_type_string__inner(list, arena, scratch, bitfield->base_type);
            struct string width = push_format_string(scratch, ": %u", bitfield->width);
            string_list_postfix(list, scratch, width);
            
        }break;
        invalid_default_case();
    }
}


// if arena == scratch this requires an additional copy
func struct string push_type_string(struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type){
    struct temporary_memory temp = begin_temporary_memory(scratch);
    
    struct string_list list = zero_struct;
    push_type_string__inner(&list, arena, scratch, type);
    struct string ret = string_list_flatten(list, arena);
    
    end_temporary_memory(temp);
    
    if(arena == scratch){
        ret = push_string_copy(arena, ret);
    }
    
    return ret;
}

//_____________________________________________________________________________________________________________________

func void ast_list_append(struct ast_list *list, struct memory_arena *arena, struct ast *ast){
    struct ast_list_node *new = push_uninitialized_struct(arena, struct ast_list_node);
    if(list->last){
        list->last->next = new;
    }else{
        list->first = new;
    }
    
    new->value = ast;
    new->next = 0;
    list->last = new;
    
    list->count++;
}

//_____________________________________________________________________________________________________________________
// Work Queue

func void work_queue_append_list(struct work_queue *queue, struct work_queue_entry *list_begin, struct work_queue_entry *list_end, smm amount){
    // first update the 'work_entries_in_flight', so this is not temporary 0.(or in gerneral less, than it should)
    atomic_add(&queue->work_entries_in_flight, amount);
    
    struct work_queue_entry *end;
    
    int queue_was_empty = 0;
    
    while(true){
        end = atomic_load(struct work_queue_entry *, queue->end);
        if(!end){ // queue is empty
            struct work_queue mem;
            mem.begin = list_begin;
            mem.end = list_end;
            b32 success = atomic_compare_and_swap_128(&queue->mem, mem.mem, &(m128)zero_struct);
            if(success){
                queue_was_empty = 1;
                goto end;
            }
            continue;
        }
        if(atomic_compare_and_swap(&queue->end, list_end, end) == end) break;
    }
    
    // @cleanup: should this write be atomic?
    end->next = list_begin;
    
    end:;
    
    if(queue_was_empty && globals.wake_event){
        //log_print("wake up!");
        SetEvent(globals.wake_event);
        ResetEvent(globals.wake_event);
    }
}

func void work_queue_add(struct work_queue *queue, struct work_queue_entry *entry){
    
    // if(queue == &globals.work_queue_stage_one) assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
    
    entry->next = 0;
    work_queue_append_list(queue, entry, entry, 1);
}

func struct work_queue_entry *work_queue_get_work(struct work_queue *queue){
    struct work_queue read;
    while(true){
        memset(&read, 0, sizeof(read));
        b32 empty = atomic_compare_and_swap_128(&queue->mem, read.mem, &read.mem);
        if(empty) return null;
        
        // if not both pointers are null neither should be
        assert(read.begin && read.end);
        
        if(read.begin == read.end){
            struct work_queue zero = zero_struct;
            b32 success = atomic_compare_and_swap_128(&queue->mem, zero.mem, &read.mem);
            if(success) return read.begin;
            continue;
        }
        
        if(read.begin->next == null) continue; // the add write has not yet completed, stall until then
        if(atomic_compare_and_swap(&queue->begin, read.begin->next, read.begin) == read.begin) break;
    }
    
    return read.begin;
}

func void work_queue_push_work(struct context *context, struct work_queue *queue, void *data){
    if(context->should_sleep) return;
    assert(context->arena->temp_count == 0);
    struct work_queue_entry *work = push_struct(context->arena, struct work_queue_entry);
    work->data = data;
    work_queue_add(queue, work);
}

//_____________________________________________________________________________________________________________________
// 

// @note: this function exists partially to decide whether identifiers should sleep in the 
//        'compilation->static_sleeper_table' or in 'globals.sleeper_table'
func enum identifier_is_static_result{
    IDENTIFIER_is_static,
    IDENTIFIER_is_locally_defined_but_non_static,
    IDENTIFIER_is_not_defined_locally,
} compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(struct compilation_unit *compilation_unit, struct atom atom){
    
    smm hash = atom.string_hash;
    for(smm i = 0; i < compilation_unit->is_token_static_table.capacity; i++){
        smm index = (hash + i) & (compilation_unit->is_token_static_table.capacity - 1);
        
        if(!compilation_unit->is_token_static_table.data[index].token){
            // @hmm: if something does not have a declaration in that scope we dont have it 
            //       so we cannot check this table for static, but to be defined somewhere 
            //       so we can see it, it hast to be extern, so 'is_static' is false.
            //                                               - 25.01.2021
            // report_warning(context, null, 
            //         "Non-fatal internal compiler error: Pre-Parse did not discover this identifier. This means that we might process this declarations whit '%s'-storage class, which might be incorrect because of another declaration.", is_static ? "static" : "non-static");
            return IDENTIFIER_is_not_defined_locally;
        }
        
        // @cleanup: warn and shit
        if(atoms_match(atom, compilation_unit->is_token_static_table.data[index].token->atom)){
            if(compilation_unit->is_token_static_table.data[index].is_static){
                return IDENTIFIER_is_static;
            }else{
                return IDENTIFIER_is_locally_defined_but_non_static;
            }
        }
    }
    invalid_code_path;
}

func struct ast_table *compilation_unit_get_declaration_table_for_ident(struct compilation_unit *compilation_unit, struct atom atom){
    
    if(compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(compilation_unit, atom) == IDENTIFIER_is_static){
        // Try to look it up in the compilation unit.
        return &compilation_unit->static_declaration_table;
    }else{
        // Look it up in global scope.
        return &globals.global_declarations;
    }
}

func struct sleeper_table sleeper_table_create(smm capacity){
    struct sleeper_table sleeper_table = zero_struct;
    sleeper_table.capacity = capacity;
    sleeper_table.mask = sleeper_table.capacity - 1;
    smm byte_size = sleeper_table.capacity * sizeof(struct sleeper_node);
    sleeper_table.nodes = os_allocate_memory(byte_size);
    if(!sleeper_table.nodes){ print("Memory failure in sleeper table\n"); os_panic(1);}
    return sleeper_table;
}

func void sleeper_table_maybe_grow(struct sleeper_table *table){
    while(true){
        if(atomic_load(s64, table->is_locked_for_growing)) continue;
        
        if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
            // ***
            
            // spin until WE set table->is_locked_for_growing
            void *old_is_locked = atomic_compare_and_swap(&table->is_locked_for_growing, (void *)true, (void *)false);
            if(old_is_locked) continue;
            // we are the thread that set 'is_locked_for_growing' to true
            
            // we could have set the condition ironiously if the actuall growing thead finished while
            // we were at ***, so lets check the condition again
            
            if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
                
                // Lets wait until there are not threads in flight
                while(atomic_load(s64, table->threads_in_flight));
                
                log_print("GROWING SLEEPER TABLE! %d\n", table->amount_of_nodes);
                
                u64 old_capacity = table->capacity;
                struct sleeper_node *old_nodes = table->nodes;
                table->capacity <<= 1;
                void *memory = os_allocate_memory(table->capacity * sizeof(*table->nodes));
                if(!memory){
                    print("Memory failure in ast table.\n");
                    os_panic(1);
                }
                table->nodes = memory;
                table->mask =(table->capacity - 1);
                
                for(u64 i = 0; i < old_capacity; i++){
                    struct sleeper_node *node = old_nodes + i;
                    if(!node->token) continue;
                    
                    struct token *token = node->token;
                    
                    u64 slot = token->string_hash & table->mask;
                    for(u64 slot_offset = 0; slot_offset < table->capacity; slot_offset++){
                        struct sleeper_node *new_node = table->nodes + ((slot + slot_offset) & table->mask);
                        if(new_node->token) continue;
                        *new_node = *node;
                        break;
                    }
                }
                
                os_free_memory(old_nodes);
            }
            
            // unlock
            table->is_locked_for_growing = false;
        }
        break;
    }
}

func void sleeper_table_add(struct sleeper_table *table, struct work_queue_entry *entry, struct token *token){
    sleeper_table_maybe_grow(table);
    
    atomic_preincrement(&table->threads_in_flight);
    
    assert(token);
    assert(entry);
    
    u64 index = token->string_hash & table->mask;
    assert(index < table->capacity);
    
    struct sleeper_node to_insert;
    to_insert.token = token;
    to_insert.first_sleeper = entry;
    entry->next = null;
    
    for(u32 i = 0; i < table->capacity; i++){
        struct sleeper_node *node = table->nodes + index;
        if(node->token){
            
            struct token *test_token = node->token;
            
            if(atoms_match(test_token->atom, token->atom)){
                // add entry to the list 'node->first_sleeper' atomically
                struct work_queue_entry *list;
                do{
                    list = atomic_load(struct work_queue_entry*, node->first_sleeper);
                    if(!list){
                        // @note: first_sleeper == null means that the node has been "deleted".
                        // if we deleted the entry within the time it took to go to sleep, we requeue it
                        assert(globals.compile_stage == COMPILE_STAGE_parse_global_scope_entries);
                        work_queue_add(&globals.work_queue_parse_global_scope_entries, entry);
                        log_print("         allready woke: adding it globally");
                        
                        atomic_predecrement(&table->threads_in_flight);
                        return;
                    }
                    
                    entry->next = list;
                }while(atomic_compare_and_swap(&node->first_sleeper, entry, list) != list);
                
                log_print("         added to existing!");
                atomic_predecrement(&table->threads_in_flight);
                return;
            }else{
                index += 1;
                index &= table->mask;
            }
        }else{
            b32 success = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &(m128)zero_struct);
            if(success){
                atomic_add((s64 *)&table->amount_of_nodes, 1);
                log_print("         added new!");
                
                atomic_predecrement(&table->threads_in_flight);
                return;
            }
        }
    }
    
    invalid_code_path;
}

func struct work_queue_entry *sleeper_table_delete(struct sleeper_table *table, struct token *token){
    sleeper_table_maybe_grow(table);
    atomic_preincrement(&table->threads_in_flight);
    
    assert(token);
    
    u64 index = token->string_hash & table->mask;
    assert(index < table->capacity);
    
    struct sleeper_node to_insert;
    to_insert.token = token;
    to_insert.first_sleeper = 0;
    
    for(u64 i = 0; i < table->capacity; i++){
        struct sleeper_node *node = table->nodes + index;
        if(!node->token){
            b32 success = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &(m128)zero_struct);
            if(success) {
                atomic_add((s64 *)&table->amount_of_nodes, 1);
                log_print("         added dummy!");
                
                atomic_predecrement(&table->threads_in_flight);
                return null;
            }
            else continue;
        }
        
        struct token *test_token = node->token;
        
        if(atoms_match(test_token->atom, token->atom)){
            // @cleanup: do we actually have to do this?
            // @cleanup: can we just put node->queue = 0?
            struct sleeper_node compare = to_insert;
            b32 already_done = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &compare.mem);
            if(already_done) {
                atomic_predecrement(&table->threads_in_flight);
                return null; // someone beat us to the delete
            }
            
            struct work_queue_entry *ret = compare.first_sleeper;
            b32 success = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &compare.mem);
            if(success){
                atomic_predecrement(&table->threads_in_flight);
                return ret;
            }
        }else{
            index += 1;
            index &= table->mask;
        }
    }
    
    invalid_code_path;
}

func void wake_up_sleepers(struct sleeper_table *sleeper_table, struct token *sleep_on){
    
    log_print("   waking sleepers for: %.*s", sleep_on->amount, sleep_on->data);
    
    assert(globals.compile_stage == COMPILE_STAGE_parse_global_scope_entries);
    
    struct work_queue_entry *first = sleeper_table_delete(sleeper_table, sleep_on);
    if(first){
        struct work_queue_entry *last = first;
        smm amount = 1;
        for(; last->next; last = last->next) amount++;
        
        log_print("   %d sleepers found", amount);
        work_queue_append_list(&globals.work_queue_parse_global_scope_entries, first, last, amount);
        for(struct work_queue_entry *it = first; it; it = it->next){
            log_print("      sleeper: %p", it);
        }
    }else{
        log_print("      no sleepers found.");
    }
}

//_____________________________________________________________________________________________________________________
#include "preprocess.c"
//_____________________________________________________________________________________________________________________

func struct parse_work *push_parse_work(struct context *context, struct token_array tokens, struct compilation_unit *compilation_unit, struct ast_function *function){
    struct parse_work *parse_work = push_struct(context->arena, struct parse_work);
    parse_work->tokens = tokens;
    parse_work->compilation_unit = compilation_unit;
    parse_work->function = function;
    parse_work->pragma_alignment = context->pragma_alignment;
    
    return parse_work;
}

//_____________________________________________________________________________________________________________________
// lookup_* calls. These are here as they have to think about threading.

func struct ast_type *lookup_compound_type(struct context *context, struct atom ident){
    
    for(struct ast_scope *it = context->current_scope; it; it = it->parent){
        for(u32 i = 0; i < it->amount_of_compound_types; i++){
            struct ast_compound_type *type = it->compound_types[i];
            if(atoms_match(ident, type->identifier)){
                return &type->base;
            }
        }
    }
    
    return (struct ast_type *)ast_table_get(&globals.compound_types, ident);
}

func struct ast_declaration *lookup_declaration(struct ast_scope *root_scope, struct compilation_unit *compilation_unit, struct atom ident){
    
    struct ast_declaration *lookup = null;
    
    for(struct ast_scope *scope = root_scope; scope; scope = scope->parent){
        
        smm hash = ident.string_hash;
        for(smm table_index = 0; table_index < scope->current_max_amount_of_declarations; table_index++){
            smm index = (table_index + hash) & (scope->current_max_amount_of_declarations - 1);
            
            struct ast_declaration *decl = scope->declarations[index];
            
            if(!scope->declarations[index]) break;
            
            if(atoms_match(ident, decl->identifier->atom)){
                lookup = decl;
                goto double_break;
            }
        }
        
        //
        // Break if this is a function scope. This will prevent local functions from accessing the local variables.
        //
        if(scope->flags & SCOPE_FLAG_is_function_scope) break;
    }
    double_break:;
    
    if(!lookup){
        struct ast_table *ast_table = compilation_unit_get_declaration_table_for_ident(compilation_unit, ident);
        lookup = cast(struct ast_declaration *)ast_table_get(ast_table, ident);
    }
    
    return lookup;
}


// :unresolved_types
static struct ast_type *lookup_unresolved_type(struct ast_type *type){
    struct ast_unresolved_type *unresolved = (struct ast_unresolved_type *)type;
    struct atom ident = unresolved->base.token->atom;
    
    // 
    // If the unresolve type was declared at global scope, only look it up globally,
    // as it could have been shadowed locally. Furthermore, only look the type up in 
    // its scope, so it cannot be shadowed in a deeper scope we are currently in.
    // 
    // This makes it so
    // 
    // int main(){
    //     struct asd *asd;
    //     {
    //         struct asd {int a;};
    //         asd->a; 
    //     }
    // }
    // 
    // does not compile.
    // 
    
    if(unresolved->containing_scope){
        
        struct ast_scope *scope = unresolved->containing_scope;
        
        for(u32 compound_index = 0; compound_index < scope->amount_of_compound_types; compound_index++){
            struct ast_compound_type *compound = scope->compound_types[compound_index];
            
            if(atoms_match(ident, compound->identifier)){
                return &compound->base;
            }
        }
    }
    
    return (struct ast_type *)ast_table_get(&globals.compound_types, ident);
}

func struct ast_declaration *lookup_typedef(struct context *context, struct compilation_unit *compilation_unit, struct token *type_name, int silent){
    struct ast_declaration *decl = lookup_declaration(context->current_scope, compilation_unit, type_name->atom);
    if(!decl) return null;
    
    if(decl->base.kind != AST_typedef){
        if(!silent){
            // 
            // @note: We need to report an error here in some cases, to prevent infinite loops for something like:
            // 
            //     int main() {}
            //     main asd(){}
            //     
            // Here, it beleaves 'main' to be a typedef, and we need to error on that, otherwise it spins.
            // 
            
            
            begin_error_report(context);
            report_error(context, type_name, "Expected a type, got a declaration.");
            report_error(context, decl->identifier, "... Here is the declaration.");
            end_error_report(context);
        }
        return null;
    }
    
    // @note: Read the type once, as another thread might try to resolve it at the same time.
    struct ast_type *type = atomic_load(struct ast_type *, decl->type);
    
    if(type->kind == AST_unresolved_type){
        // :unresolved_types
        //
        // If we try to lookup a typedef and its an unresolved type, we have to try to resolve its type.
        // This is because otherwise we sleep on the type again when we try to parse the declaration.
        // 
        // On the other hand, we cannot just sleep if the typedef points to an unresolved-unresolved type,
        // as we might be defining another typedef or declaring a pointer or something which never 
        // actually needs the type to be resolved.
        //
        struct ast_type *resolved = lookup_unresolved_type(type);
        
        if(resolved){
            if(resolved->kind == AST_enum){
                decl->type = &globals.typedef_s32;
                decl->defined_type = (struct ast *)resolved;
            }else{
                decl->type = resolved;
            }
        }
    }
    
    
    return decl;
}

func void patch_array_size(struct context *context, struct ast_array_type *array_of_unknown_size, smm amount_of_elements, struct token *site){
    assert(amount_of_elements >= 0);
    
    if(amount_of_elements > 0x7fffffff || array_of_unknown_size->element_type->size > 0x7fffffff || 
            (u64)amount_of_elements * (u64)array_of_unknown_size->element_type->size > 0x7fffffff){
        report_error(context, site, "Array size cannot exceed 0x7fffffff.");
        amount_of_elements = 1; // Make sure no overflow occurs, so no UB shenenigans.
    }
    
    array_of_unknown_size->amount_of_elements = amount_of_elements;
    array_of_unknown_size->base.size = amount_of_elements * array_of_unknown_size->element_type->size;
    array_of_unknown_size->base.alignment = array_of_unknown_size->element_type->alignment; // @cleanup: I think there is a way to specify override alignment.
    array_of_unknown_size->is_of_unknown_size = false;
}

func void register_compound_member(struct context *context, struct ast_compound_type *compound, struct token *ident, struct ast_type *type, smm offset_in_type, smm next_member_increment){
    
    if(compound->amount_of_members == compound->current_max_amount_of_members){
        u32 new_max = max_of(2 * compound->current_max_amount_of_members, 8);
        struct compound_member *new_array = push_uninitialized_data(context->arena, struct compound_member, new_max);
        memcpy(new_array, compound->members, sizeof(*compound->members) * compound->current_max_amount_of_members);
        
        compound->current_max_amount_of_members = new_max;
        compound->members = new_array;
    }
    
    u32 index = compound->amount_of_members++;
    
    // Insert the new member.
    compound->members[index].name = ident;
    compound->members[index].type = type;
    compound->members[index].offset_in_type = offset_in_type;
    compound->members[index].next_member_increment = next_member_increment;
}

func void parser_register_declaration_in_scope(struct context *context, struct ast_scope *scope, struct ast_declaration *decl){
    
    // 
    // Grow the declaration table if necessary.
    // Make sure all of the declarations with the same hash stay in the same order.
    // 
    if(2 * scope->amount_of_declarations++ == scope->current_max_amount_of_declarations){
        u32 new_max = max_of(2 * scope->current_max_amount_of_declarations, 8);
        struct ast_declaration **new_table = push_data(context->arena, struct ast_declaration *, new_max);
        
        for(smm old_index = 0; old_index < scope->current_max_amount_of_declarations; old_index++){
            struct ast_declaration *old_entry = scope->declarations[old_index];
            if(!old_entry) continue;
            if(old_entry == (struct ast_declaration *)1) continue; // skip over tombstones.
            
            // Iterate over all entries in the same hash chain as `old_entry`.
            smm old_hash = old_entry->identifier->atom.string_hash;
            for(smm hash_chain_index = 0; hash_chain_index < scope->current_max_amount_of_declarations; hash_chain_index++){
                
                smm old_hash_index = (hash_chain_index + old_hash) & (scope->current_max_amount_of_declarations - 1);
                
                struct ast_declaration *chain_decl = scope->declarations[old_hash_index];
                if(!chain_decl) break; // We have found the end of the chain.
                if(chain_decl == (struct ast_declaration *)1) continue; // skip over tombstones.
                
                // Check if chain_decl has the same hash.
                smm hash = chain_decl->identifier->atom.string_hash;
                if(hash != old_hash) continue;
                
                // Insert `chain_decl` into the new table.
                
                for(smm new_index = 0; new_index < new_max; new_index++){
                    smm index = (new_index + hash) & (new_max - 1);
                    if(!new_table[index]){
                        new_table[index] = chain_decl;
                        break;
                    }
                }
                
                scope->declarations[old_hash_index] = (void *)1;
            }
        }
        
        scope->current_max_amount_of_declarations = new_max;
        scope->declarations = new_table;
    }
    
    int have_reported_warning = 0;
    
    smm hash = decl->identifier->atom.string_hash;
    for(smm table_index = 0; table_index < scope->current_max_amount_of_declarations; table_index++){
        smm index = (table_index + hash) & (scope->current_max_amount_of_declarations - 1);
        
        struct ast_declaration *redecl = scope->declarations[index];
        if(!redecl){
            scope->declarations[index] = decl;
            break;
        }
        
        if(atoms_match(decl->identifier->atom, redecl->identifier->atom)){
            
            if(!have_reported_warning){
                begin_error_report(context);
                report_warning(context, WARNING_shadowing_in_same_scope, decl->identifier, "Declaration hides previous declaration in same scope.");
                report_warning(context, WARNING_shadowing_in_same_scope, redecl->identifier, "... Here is the previous declaration.");
                end_error_report(context);
            }
            
            // From here on, move the redeclaration.
            // We still have to check the atoms match case, as the declarations of the same name have to stay in order.
            scope->declarations[index] = decl;
            decl = redecl;
            have_reported_warning = 1;
        }
    }
}

func struct ast_type *types_are_equal(struct ast_type *wanted, struct ast_type *given);

func struct ast_declaration *register_declaration(struct context *context, struct ast_declaration *decl){
    if(context->should_sleep) return decl;
    struct ast_scope *scope = context->current_scope;
    
    if(scope){
        parser_register_declaration_in_scope(context, scope, decl);
        
        if((warning_enabled[WARNING_shadowing_local] || warning_enabled[WARNING_shadowing_global]) && !(scope->flags & SCOPE_FLAG_is_function_scope) && should_report_warning_for_token(context, decl->identifier)){
            struct ast_declaration *redecl = lookup_declaration(scope->parent, context->current_compilation_unit, decl->identifier->atom);
            
            if(redecl){
                begin_error_report(context);
                if(redecl->flags & DECLARATION_FLAGS_is_global){
                    report_warning(context, WARNING_shadowing_global, decl->identifier, "Declaration hides previous global declaration.");
                    report_warning(context, WARNING_shadowing_global, redecl->identifier, "... Here is the previous global declaration.");
                }else{
                    report_warning(context, WARNING_shadowing_local, decl->identifier, "Declaration hides previous declaration.");
                    report_warning(context, WARNING_shadowing_local, redecl->identifier, "... Here is the previous declaration.");
                }
                end_error_report(context);
            }
        } 
    }else{
        // we are at global scope: register it globally
        
        struct compilation_unit *compilation_unit = context->current_compilation_unit;
        
        int declaration_is_static = compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(compilation_unit, decl->identifier->atom) == IDENTIFIER_is_static;
        struct ast_table *table = declaration_is_static ? &compilation_unit->static_declaration_table : &globals.global_declarations;
        struct ast_declaration *redecl = (struct ast_declaration *)ast_table_add_or_return_previous_entry(table, &decl->base, decl->identifier);
        
        // @note: if there is a redeclaration this should always return this redeclaraton (the global one, that we can find in the table)
        //        or error in which case whatever.
        
        if(redecl){
            // @cleanup: I think one could cleanup this function really well..
            
            
            if(redecl->type->kind == AST_array_type && decl->type->kind == AST_array_type){
                struct ast_array_type *redecl_type = (struct ast_array_type *)redecl->type;
                struct ast_array_type *decl_type   = (struct ast_array_type *)decl->type;
                
                if(redecl_type->is_of_unknown_size && !decl_type->is_of_unknown_size){
                    
                    // :threading @cleanup: Here we set the size and the amount of elements.
                    // 
                    // Maybe there is a bug where 
                    //     int arr[];
                    //     int arr[3];
                    //     int arr[5];
                    // might go through, as one might go through, after both started in here and then the second one 
                    // set the size and the amount of elements, thus also passing the redeclaration check down below.
                    // I am not gonna handle this right now, as it requires really weird code and I could not think
                    // of a good solution. (maybe compare-and-swap in the correct value for amount of elements?),
                    // but are there races all over the place, where people check if its an array of unknown size?
                    //                                                                                   02.05.2021
                    
                    patch_array_size(context, redecl_type, decl_type->amount_of_elements, redecl->identifier);
                }
                
                if(decl_type->is_of_unknown_size && !redecl_type->is_of_unknown_size){
                    patch_array_size(context, decl_type, redecl_type->amount_of_elements, decl->identifier);
                }
            }
            
            u64   decl_extended_attributes =   decl->flags & (DECLARATION_FLAGS_is_dllimport | DECLARATION_FLAGS_is_dllexport | DECLARATION_FLAGS_is_selectany | DECLARATION_FLAGS_is_thread_local);
            u64 redecl_extended_attributes = redecl->flags & (DECLARATION_FLAGS_is_dllimport | DECLARATION_FLAGS_is_dllexport | DECLARATION_FLAGS_is_selectany | DECLARATION_FLAGS_is_thread_local);
            
            if(decl_extended_attributes != redecl_extended_attributes){
                
                char *attribute = null;
                
                if((decl_extended_attributes & DECLARATION_FLAGS_is_thread_local) != (redecl_extended_attributes & DECLARATION_FLAGS_is_thread_local)){
                    attribute = "_Thread_local";
                }
                
                if(attribute){
                    begin_error_report(context);
                    report_error(context, decl->identifier, "[%lld] Redeclaration differs in %s attribute.", decl->compilation_unit->index, attribute);
                    report_error(context, redecl->identifier, "[%lld] ... Here was the previous declaration.", redecl->compilation_unit->index);
                    end_error_report(context);
                }else{
                    if((decl_extended_attributes & DECLARATION_FLAGS_is_selectany) != (redecl_extended_attributes & DECLARATION_FLAGS_is_selectany)){
                        attribute = "__declspec(selectany)";
                    }else if((decl_extended_attributes & DECLARATION_FLAGS_is_dllexport) != (redecl_extended_attributes & DECLARATION_FLAGS_is_dllexport)){
                        attribute = "__declspec(dllexport)";
                    }else if((decl_extended_attributes & DECLARATION_FLAGS_is_dllimport) != (redecl_extended_attributes & DECLARATION_FLAGS_is_dllimport)){
                        attribute = "__declspec(dllimport)";
                    }
                    
                    
                    if(attribute){
                        begin_error_report(context);
                        report_warning(context, WARNING_declaration_differs_in_attribute, decl->identifier, "[%lld] Redeclaration differs in %s attribute.", decl->compilation_unit->index, attribute);
                        report_warning(context, WARNING_declaration_differs_in_attribute, redecl->identifier, "[%lld] ... Here was the previous declaration.", redecl->compilation_unit->index);
                        end_error_report(context);
                        
                        redecl->flags |= decl_extended_attributes;
                    }
                    
                    if((decl_extended_attributes & DECLARATION_FLAGS_is_dllimport) != (redecl_extended_attributes & DECLARATION_FLAGS_is_dllimport)) redecl->flags &= ~DECLARATION_FLAGS_is_dllimport;
                }
            }
            
            if(redecl->base.kind == AST_declaration && decl->base.kind == AST_declaration){
                
                if((redecl->flags & DECLARATION_FLAGS_is_enum_member) && (decl->flags & DECLARATION_FLAGS_is_enum_member)){
                    // if they are both enums with the same value they are fine.
                    assert(redecl->assign_expr);
                    assert(decl->assign_expr);
                    assert(redecl->assign_expr->kind == AST_integer_literal);
                    assert(decl->assign_expr->kind == AST_integer_literal);
                    struct ast_integer_literal *decl_lit   = (struct ast_integer_literal *)decl->assign_expr;
                    struct ast_integer_literal *redecl_lit = (struct ast_integer_literal *)redecl->assign_expr;
                    
                    if(decl_lit->_s32 == redecl_lit->_s32){
                        return redecl; // This is fine, we redefine the literal as it self.
                    }else{
                        begin_error_report(context);
                        report_error(context, decl->identifier, "[%lld] Redeclaration of enum value as '%d'.", decl->compilation_unit->index, decl_lit->_s32);
                        report_error(context, redecl->identifier, "[%lld] ... Here was the previous declaration of value '%d'.", redecl->compilation_unit->index, redecl_lit->_s32);
                        end_error_report(context);
                        return decl;
                    }
                }else if(types_are_equal(redecl->type, decl->type)){
                    
                    // 
                    // @cleanup: How do we handle this eventually...?
                    // 
                    
                    if((redecl->flags & DECLARATION_FLAGS_is_extern) && !(decl->flags & DECLARATION_FLAGS_is_extern)){
                        redecl->flags &= ~DECLARATION_FLAGS_is_extern;
                    }
                    
                    if((redecl->flags & DECLARATION_FLAGS_is_extern) && (decl->flags & DECLARATION_FLAGS_is_static)){
                        redecl->flags |= DECLARATION_FLAGS_is_static;
                    }
                    
                    assert(!decl->assign_expr);
                    return redecl;
                }else{
                    struct string   decl_type = push_type_string(context->arena, &context->scratch,   decl->type);
                    struct string redecl_type = push_type_string(context->arena, &context->scratch, redecl->type);
                    
                    begin_error_report(context);
                    report_error(context, decl->identifier, "[%lld] Redeclaration with mismatching type '%.*s'.", decl->compilation_unit->index, decl_type.size, decl_type.data);
                    report_error(context, redecl->identifier, "[%lld] ... Here is the previous declaration of type '%.*s'.", redecl->compilation_unit->index, redecl_type.size, redecl_type.data);
                    end_error_report(context);
                    return decl;
                }
            }else if(redecl->base.kind == AST_typedef && decl->base.kind == AST_typedef){
                if(types_are_equal(redecl->type, decl->type)){
                    return redecl;
                }else{
                    struct string   decl_type = push_type_string(context->arena, &context->scratch,   decl->type);
                    struct string redecl_type = push_type_string(context->arena, &context->scratch, redecl->type);
                    
                    begin_error_report(context);
                    report_error(context, decl->identifier, "[%lld] Redeclaration of typedef with mismatching type '%.*s'.", decl->compilation_unit->index, decl_type.size, decl_type.data);
                    report_error(context, redecl->identifier, "[%lld] ... Here is the previous typedef of type '%.*s'.", redecl->compilation_unit->index, redecl_type.size, redecl_type.data);
                    end_error_report(context);
                    return decl;
                }
            }else if(redecl->base.kind == AST_function && decl->base.kind == AST_function){
                struct ast_function *redecl_function = cast(struct ast_function *)redecl;
                struct ast_function *function = cast(struct ast_function *)decl;
                
                if(!types_are_equal(decl->type, redecl->type)){
                    struct string   decl_type = push_type_string(context->arena, &context->scratch,   decl->type);
                    struct string redecl_type = push_type_string(context->arena, &context->scratch, redecl->type);
                    
                    begin_error_report(context);
                    // :Error maybe print compilation units
                    report_error(context, decl->identifier, "[%lld] Redeclaration of function with different type '%.*s'.", decl->compilation_unit->index, decl_type.size, decl_type.data);
                    report_error(context, redecl->identifier, "[%lld] ... Here is the previous declaration with type '%.*s'.", redecl->compilation_unit->index, redecl_type.size, redecl_type.data);
                    end_error_report(context);
                    return decl;
                }
                
                if(function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                    if(!(redecl_function->type->flags & DECLARATION_FLAGS_is_dllexport)){
                        // only set this if it is not set, to not polute write cache.
                        redecl_function->as_decl.flags |= DECLARATION_FLAGS_is_dllexport;
                    }
                }
                
                if(redecl_function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                    // we own this decl so here its fine
                    function->as_decl.flags |= DECLARATION_FLAGS_is_dllexport;
                }
                
                return redecl;
            }
            
            char *redecl_kind = "declaration";
            if(redecl->base.kind == AST_typedef) redecl_kind = "typedef";
            if(redecl->base.kind == AST_function) redecl_kind = "function";
            
            
            char *decl_kind = "declaration";
            if(decl->base.kind == AST_typedef) decl_kind = "typedef";
            if(decl->base.kind == AST_function) decl_kind = "function";
            
            begin_error_report(context);
            report_error(context, decl->identifier, "[%lld] Redeclaration of different kind. It is a %s.", decl->compilation_unit->index, decl_kind);
            report_error(context, redecl->identifier, "[%lld] ... Here is the previous declaration. It is a %s.", redecl->compilation_unit->index, redecl_kind);
            end_error_report(context);
            return decl;
        }
        
        struct sleeper_table *sleeper_table = declaration_is_static ? &compilation_unit->static_sleeper_table : &globals.declaration_sleeper_table;
        
        wake_up_sleepers(sleeper_table, decl->identifier);
    }
    return decl;
}

func int parser_register_definition(struct context *context, struct ast_declaration *decl, struct ast *initializer, struct token *initializer_token, struct ast_type *type){
    assert(!context->should_sleep);
    assert(!(decl->flags & DECLARATION_FLAGS_is_enum_member));
    
    // @note: Used by functions and declarations as of -03.04.2021, 
    //        its either the initializer or the scope.
    //        
    // assert(!context->current_scope);
    // 
    
    if(decl->flags & DECLARATION_FLAGS_is_intrinsic){
        report_error(context, initializer_token, "Cannot define intrinsic declaration '%.*s'.", decl->identifier->size, decl->identifier->data);
        return 0;
    }
    
    if(atomic_compare_and_swap(&decl->assign_expr, initializer, null) != null){
        // 
        // We failed to set the initializer.
        // 
        if(!(decl->flags & DECLARATION_FLAGS_is_selectany)){
            begin_error_report(context);
            report_error(context, initializer_token, "[%lld] Redefinition of '%.*s'.", context->current_compilation_unit->index, decl->identifier->atom.size, decl->identifier->atom.data);
            report_error(context, get_initializer_token(decl), "[%lld] ... Here is the previous definition.", decl->compilation_unit->index);
            end_error_report(context);
        }
        return 0;
    }else{
        // @note: Hard set the type here, we know that this is the definition, this is necessary for functions
        //        e.g: replace 'int _start(int some_wrong_name)' by 'int _start(int the_right_name)'.
        //        We also use this for arrays of unknown size.
        decl->type = type; 
        return 1;
    }
}

func void register_compound_type(struct context *context, struct ast_type *type, struct token *ident){
    if(context->should_sleep) return;
    
    if(context->current_scope){
        struct ast_type *redecl = lookup_compound_type(context, ident->atom);
        if(redecl){
            // 
            // If its the same type, do not register this one. @cleanup: should we return the other type?
            // 
            if(types_are_equal((struct ast_type *)redecl, type)) return;
            
            // 
            // Warn on redeclaration at local scope, but still register this.
            // This is necessary, as the following is valid:
            // 
            // struct asd {int a;};
            // 
            // int main(){
            //     struct asd {float b;} redeclaration;
            //     return 0;
            // }
            // 
            // 
            
            if(should_report_warning_for_token(context, type->token)){
                begin_error_report(context);
                report_warning(context, WARNING_shadowing_local, type->token, "Redeclaration of type.");
                report_warning(context, WARNING_shadowing_local, redecl->token, "... Here was the previous declaration.");
                end_error_report(context);
            }
        }
        
        struct ast_scope *scope = context->current_scope;
        u32 index = scope->amount_of_compound_types++;
        if(index == scope->current_max_amount_of_compound_types){
            u32 new_max = max_of(2 * scope->current_max_amount_of_compound_types, 8);
            struct ast_compound_type **new_array = push_uninitialized_data(context->arena, struct ast_compound_type *, new_max);
            memcpy(new_array, scope->compound_types, sizeof(scope->compound_types[0])* scope->current_max_amount_of_compound_types);
            scope->current_max_amount_of_compound_types = new_max;
            scope->compound_types = new_array;
        }
        scope->compound_types[index] = cast(struct ast_compound_type *)type;
        
        return;
    }
    
    struct ast *redecl;
    
    redecl = ast_table_add_or_return_previous_entry(&globals.compound_types, cast(struct ast*)type, ident);
    
    if(redecl){
        struct ast_compound_type *old = (struct ast_compound_type *)redecl;
        struct ast_compound_type *new = (struct ast_compound_type *)type;
        
        // If we have same token we are the same type so we can. @cleanup: Is this correct?
        if(old->base.token == new->base.token) return;
        
        // 
        // Reallow declarations of the same type.
        // 
        if(types_are_equal((struct ast_type *)redecl, type)) return;
        
        begin_error_report(context);
        report_error(context, new->base.token, "[%lld] Redeclaration of type.", new->compilation_unit->index);
        report_error(context, old->base.token, "[%lld] ... Here was the previous declaration.", old->compilation_unit->index);
        end_error_report(context);
        return;
    }
    
    wake_up_sleepers(&globals.compound_sleeper_table, ident);
}

func void parser_emit_memory_location(struct context *context, struct ast_declaration *decl){
    
    smm alignment = get_declaration_alignment(decl);
    smm size      = get_declaration_size(decl);
    
    context->current_emit_offset_of_rsp = cast(smm)align_up(context->current_emit_offset_of_rsp, alignment);
    context->current_emit_offset_of_rsp += size;
    
    if(context->current_emit_offset_of_rsp > max_s32){
        report_error(context, decl->identifier, "Too many local variables. At most '0x%x' bytes of stack are allowed.", max_s32);
    }
    
    // :MemoryLocations We currently emit '[rbp - offset]' so we want to return the offset
    //                  AFTER it is incremented.
    decl->offset_on_stack = context->current_emit_offset_of_rsp;
}

//_____________________________________________________________________________________________________________________

#include "parse.c"

//_____________________________________________________________________________________________________________________

#include "emit_x64.c"

//_____________________________________________________________________________________________________________________

#include "explain.c"

//_____________________________________________________________________________________________________________________


func void evaluate_static_initializer__internal(struct context *context, struct ast *initializer, struct ast_declaration *patch_declaration, u8 *data, smm data_size, smm root_offset){
    
    // @cleanup: There has to be a better solution.
    struct token *error_token = get_initializer_token(patch_declaration);
    
    smm initializer_end = 0x80000000;
    
    if(initializer->kind == AST_identifier){
        initializer = &((struct ast_identifier *)initializer + 1)->base;
    }else{
        assert(initializer->kind == AST_compound_literal);
        struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)initializer;
        initializer = &(compound_literal + 1)->base;
        initializer_end = compound_literal->initializer_size;
    }
    
    struct{
        struct ast *ast;
        smm offset;
        int is_address;
    } ast_stack[4]; // @paranoid: We should be able to put this to 2, because the only binary expression we handle are + and -, hence we cannot exceed depth 2.
    smm ast_stack_at = 0;
    
    u8 *base = (u8 *)initializer;
    smm ast_offset = 0;
    while(ast_offset < initializer_end){
        struct ast *ast = (struct ast *)(base + ast_offset);
        smm start_ast_offset = ast_offset;
        
        switch(ast->kind){
            
            // 
            // Primary expressions:
            // 
            
            case AST_integer_literal:{
                ast_offset += sizeof(struct ast_integer_literal);
                ast_stack[ast_stack_at].ast = ast;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
            }break;
            case AST_float_literal:{
                ast_offset += sizeof(struct ast_float_literal);
                ast_stack[ast_stack_at].ast = ast;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
            }break;
            case AST_string_literal:{ // @cleanup: mark it as being used?
                ast_offset += sizeof(struct ast_string_literal);
                ast_stack[ast_stack_at].ast = ast;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
                
                // Mark this string literal as being used.
                struct ast_string_literal *string_literal = (struct ast_string_literal *)ast;
                sll_push_back(context->string_literals, string_literal);
                context->string_literals.amount_of_strings += 1;
            }break;
            case AST_pointer_literal:{
                ast_offset += sizeof(struct ast_pointer_literal);
                ast_stack[ast_stack_at].ast = ast;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
            }break;
            
            case AST_identifier:{
                struct ast_identifier *identifier = (struct ast_identifier *)ast;
                struct ast_declaration *decl = identifier->decl;
                
                // @cleanup: This is currently called from main inside of an error_report.
                //           In the future this should not happen on the main thread and then
                //           we should bring back the error report.
                
                if(!(decl->flags & (DECLARATION_FLAGS_is_global|DECLARATION_FLAGS_is_local_persist))){
                    // begin_error_report(context);
                    report_error(context, /*ast->token*/error_token, "Referencing non-constant variable in constant initializer.");
                    report_error(context, decl->identifier, "... Here is the referenced declaration.");
                    // end_error_report(context);
                    return;
                }
                
                if(decl->flags & DECLARATION_FLAGS_is_thread_local){
                    // begin_error_report(context);
                    report_error(context, /*ast->token*/error_token, "Cannot reference _Thread_local declaration in constant initializer.");
                    report_error(context, decl->identifier, "... Here is the referenced declaration.");
                    // end_error_report(context);
                }
                
                if(decl->flags & DECLARATION_FLAGS_is_dllimport){
                    // begin_error_report(context);
                    if(decl->base.kind == AST_declaration){
                        report_error(context, /*ast->token*/error_token, "Cannot reference __declspec(dllimport) declaration in constant initializer.");
                        report_error(context, decl->identifier, "... Here is the referenced declaration.");
                    }else if(decl->base.kind == AST_function){
                        report_warning(context, WARNING_reference_to_dllimport_inserts_stub, /*ast->token*/error_token, "Constant reference of __declspec(dllimport)-function '%.*s' causes a stub to be generated. This causes `==` to potentially produce undesired results.", decl->identifier->size, decl->identifier->data);
                        report_warning(context, WARNING_reference_to_dllimport_inserts_stub, decl->identifier, "... Here is the referenced declaration.");
                        if(!(decl->flags & DECLARATION_FLAGS_need_dllimport_stub_function)) decl->flags |= DECLARATION_FLAGS_need_dllimport_stub_function;
                    }else invalid_code_path;
                    // end_error_report(context);
                }
                
                ast_offset += sizeof(struct ast_identifier);
                ast_stack[ast_stack_at].ast = &decl->base;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
            }break;
            
            case AST_compound_literal:{
                struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)ast;
                
                // Add this compound literal to the list of unnamed global declarations.
                // @cleanup: This might not be emitted I think.
                ast_list_append(&context->global_struct_and_array_literals, context->arena, &compound_literal->decl->base);
                
                ast_offset += sizeof(struct ast_compound_literal);
                ast_offset += compound_literal->initializer_size;
                
                ast_stack[ast_stack_at].ast = &compound_literal->decl->base;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
            }break;
            
            case AST_embed:{
                ast_offset += sizeof(struct ast_embed);
                ast_stack[ast_stack_at].ast = ast;
                ast_stack[ast_stack_at].offset = 0;
                ast_stack[ast_stack_at].is_address = 0;
                ast_stack_at++;
            }break;
            
            // 
            // Unary expressions:
            // 
            
            case AST_member:{
                ast_offset += sizeof(struct ast_dot_or_arrow);
                
                struct ast_dot_or_arrow *dot = (struct ast_dot_or_arrow *)ast;
                ast_stack[ast_stack_at-1].offset += dot->member->offset_in_type;
            }break;
            
            case AST_unary_address:
            case AST_implicit_address_conversion:{
                ast_offset += sizeof(struct ast_unary_op);
                
                ast_stack[ast_stack_at-1].is_address = 1;
            }break;
            case AST_implicit_address_conversion_lhs:{
                ast_offset += sizeof(struct ast_unary_op);
                
                ast_stack[ast_stack_at-2].is_address = 1;
            }break;
            
            case AST_cast_lhs:
            case AST_cast:{
                ast_offset += sizeof(struct ast_cast);
                
                int stack_offset = ast->kind == AST_cast ? 1 : 2;
                if(ast_stack[ast_stack_at - stack_offset].is_address){
                    if(ast->resolved_type->size == 8){
                        // We are good, casting pointer to integer.
                    }else{
                        report_error(context, /*ast->token*/error_token, "Cannot truncate a pointer at compile time.");
                    }
                }else{
                    goto initializer_not_constant;
                }
            }break;
            
            // 
            // Binary expressions:
            // 
            
            case AST_array_subscript:{
                ast_offset += sizeof(struct ast_subscript);
                ast_stack_at -= 1;
                
                struct ast_subscript *subscript = (struct ast_subscript *)ast;
                
                struct ast *index = ast_stack[ast_stack_at].ast;
                
                if(index->kind == AST_integer_literal){
                    ast_stack[ast_stack_at-1].offset += integer_literal_to_bytes(index) * subscript->base.resolved_type->size;
                }else{
                    report_error(context, /*ast->token*/error_token, "Expected a constant in array subscript, when evaluating initializer at compile time.");
                    return;
                }
            }break;
            
            case AST_binary_plus: case AST_binary_minus:{
                ast_offset += sizeof(struct ast_binary_op);
                
                struct ast *binary_rhs = ast_stack[ast_stack_at-1].ast;
                struct ast *binary_lhs = ast_stack[ast_stack_at-2].ast;
                
                struct ast *patch_lhs = 0;
                smm offset_lhs = 0;
                
                if(ast_stack[ast_stack_at-2].is_address){
                    patch_lhs  = binary_lhs;
                    offset_lhs = ast_stack[ast_stack_at-2].offset;
                }else{
                    if(binary_lhs->kind == AST_pointer_literal){
                        offset_lhs += (smm)((struct ast_pointer_literal *)binary_lhs)->pointer;
                    }else if(binary_lhs->kind == AST_integer_literal){
                        offset_lhs += integer_literal_to_bytes(binary_lhs);
                    }else{
                        goto initializer_not_constant;
                    }
                }
                
                struct ast *patch_rhs = 0;
                smm offset_rhs = 0;
                
                if(ast_stack[ast_stack_at-1].is_address){
                    patch_rhs  = binary_rhs;
                    offset_rhs = ast_stack[ast_stack_at-1].offset;
                }else{
                    if(binary_rhs->kind == AST_pointer_literal){
                        offset_rhs += (smm)((struct ast_pointer_literal *)binary_rhs)->pointer;
                    }else if(binary_rhs->kind == AST_integer_literal){
                        offset_rhs += integer_literal_to_bytes(binary_rhs);
                    }else{
                        goto initializer_not_constant;
                    }
                }
                
                ast_stack_at -= 1;
                
                smm offset_in_ast = ast->kind == AST_binary_plus ? offset_lhs + offset_rhs : offset_lhs - offset_rhs;
                
                if(patch_lhs == patch_rhs){
                    // @note: This includes the case where 'patch_lhs == patch_rhs == null'.
                    ast_stack[ast_stack_at-1].ast = 0;
                    ast_stack[ast_stack_at-1].offset = offset_in_ast;
                    ast_stack[ast_stack_at-1].is_address = 1;
                }else if(patch_lhs && patch_rhs){
                    goto initializer_not_constant;
                }else{
                    ast_stack[ast_stack_at-1].ast = patch_lhs ? patch_lhs : patch_rhs;
                    ast_stack[ast_stack_at-1].is_address = 1;
                    ast_stack[ast_stack_at-1].offset = offset_in_ast;
                }
            }break;
            
            case AST_initializer:{
                ast_offset += sizeof(struct ast_initializer);
                struct ast_initializer *initializer_for_offset = (struct ast_initializer *)ast;
                
                ast_stack_at -= 1;
                
                smm offset = root_offset + initializer_for_offset->offset;
                struct ast_type *lhs_type = initializer_for_offset->lhs_type;
                smm lhs_size = initializer_for_offset->lhs_type->size;
                assert(offset + lhs_size <= data_size);
                
                if(ast_stack[ast_stack_at].is_address){
                    // 
                    // We have to emit an absolute patch.
                    // 
                    
                    smm offset_in_rhs = ast_stack[ast_stack_at].offset;
                    struct ast *rhs = ast_stack[ast_stack_at].ast;
                    
                    if(rhs == null){
                        // This happens for a subtraction with the same patch node.
                        memcpy(data + offset, &offset_in_rhs, lhs_size);
                    }else{
                        emit_patch(context, PATCH_absolute, rhs, offset_in_rhs, patch_declaration, offset, -1);
                    }
                }else{
                    struct ast *rhs = ast_stack[ast_stack_at].ast;
                    assert(rhs);
                    
                    switch(rhs->kind){
                        case AST_integer_literal:{
                            u64 value = integer_literal_to_bytes(rhs);
                            // @note: The rhs does not have to fit: if we have 'u8 a = 0xffff;' this only gives a warning.
                            
                            if(lhs_type->kind == AST_bitfield_type){
                                u64 old_value = 0;
                                memcpy(&old_value, data + offset, lhs_type->size);
                                
                                struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)lhs_type;
                                value <<= bitfield->bit_index;
                                
                                u64 mask = ~(((1ull << bitfield->width) - 1) << bitfield->bit_index);
                                u64 new_value = (mask & old_value) | value;
                                memcpy(data + offset, &new_value, lhs_type->size);
                            }else{
                                assert(lhs_type->kind == AST_integer_type || lhs_type->kind == AST_pointer_type);
                                memcpy(data + offset, &value, lhs_type->size);
                            }
                        }break;
                        case AST_pointer_literal:{
                            struct ast_pointer_literal *lit = cast(struct ast_pointer_literal *)rhs;
                            u8 *value = lit->pointer;
                            memcpy(data + offset, &value, lhs_type->size);
                        }break;
                        
                        // case AST_compound_literal:{
                            // struct ast_compound_literal *lit = cast(struct ast_compound_literal *)rhs;
                            
                            // evaluate_static_initializer__internal
                            // @incomplete:
                        // }break;
                        
                        case AST_float_literal:{
                            struct ast_float_literal *f = cast(struct ast_float_literal *)rhs;
                            f64 big_value = f->value;
                            f32 small_value = (f32)f->value;
                            void *copy_from;
                            if(f->base.resolved_type == &globals.typedef_f32){
                                copy_from = &small_value;
                            }else{
                                assert(f->base.resolved_type == &globals.typedef_f64);
                                copy_from = &big_value;
                            }
                            memcpy(data + offset, copy_from, lhs_type->size);
                        }break;
                        
                        case AST_string_literal:{
                            struct ast_string_literal *str = cast(struct ast_string_literal *)rhs;
                            
                            // If the left hand side is not an array, we should be in the 'AST_implicit_address_conversion' case.
                            assert(lhs_type->kind == AST_array_type);
                            struct ast_array_type *lhs_array = (struct ast_array_type *)lhs_type;
                            
                            smm array_size;
                            if(lhs_array->is_of_unknown_size){
                                // We are in an initializer like:
                                // 
                                // struct s{
                                //     char array[];
                                // } arst = {"hello :)"};
                                // 
                                // We have made sure to allocate enough space to hold the initializer.
                                array_size = str->base.resolved_type->size;
                            }else{
                                array_size = lhs_array->amount_of_elements * lhs_array->element_type->size;
                            }
                            
                            // This is the 
                            //      char asd[] = "asd";
                            //      u16  asd[] = L"asd";
                            // case.
                            
                            assert(offset + str->value.size <= data_size); // Make sure at least the string fits.
                            assert(str->value.size <= array_size);
                            
                            memcpy(data + offset, str->value.data, str->value.size);
                            
                            // @cleanup: should we memset the whole "upper" part?
                            if(offset + str->value.size + lhs_array->element_type->size <= array_size){
                                memset(&data[offset + str->value.size], 0, lhs_array->element_type->size); // zero-terminate
                            }
                        }break;
                        
                        case AST_embed:{
                            // We should only be able to initialize 'char []' and 'unsigned char []' with #embed.
                            // But we sadly cannot really check this here, because we made this an initializer 
                            // for the character, which determines the offset.
                            // 
                            // @cleanup: This is all a gross hack, just to get it working.
                            //           We want to rework intialization pretty soon anyway.
                            //           
                            //                                                        23.11.2023
                            struct ast_embed *embed = (struct ast_embed *)rhs;
                            
                            struct string file_data = embed->token->string;
                            assert(offset + file_data.size <= data_size); // Make sure it fits.
                            
                            memcpy(data + offset, file_data.data, file_data.size);
                        }break;
                        default:{
                            goto initializer_not_constant;
                        }break;
                    }
                }
            }break;
            
            case AST_pop_expression:{
                return;
            }break;
            
            default:{
                initializer_not_constant:;
                // @cleanup: This token sucks. We have to think of something better eventually.
                report_error(context, /*ast->token*/error_token, "Initializer is not a constant.");
                return;
            }break;
        }
        
        assert(start_ast_offset != ast_offset);
        assert(ast_stack_at <= 2);
    }
}

func u8 *evaluate_static_initializer(struct context *context, struct ast_declaration *decl){
    
    smm data_size  = get_declaration_size(decl);
    smm data_align = get_declaration_alignment(decl);
    
    push_zero_align(context->arena, data_align); // @cleanup: Do we need this?
    u8 *data = push_uninitialized_data(context->arena, u8, data_size); // @note: This will be zero-initialized, as we never free from arena.
    
    evaluate_static_initializer__internal(context, decl->assign_expr, decl, data, data_size, 0);
    
    return data;
}


//_____________________________________________________________________________________________________________________
// Writer helpers.

#include "obj_writer.c"
#include "coff_writer.c"

#if (DUMP_OUT_PDB || READ_PDB_AFTER_EMITING_IT || BUILD_PDB_DUMPER)
#include "pdb_dumper.c"
#endif

#if DUMP_OBJ
#include "obj_dumper.c"
#endif


func void init_context(struct context *context, struct thread_info *info, struct memory_arena *arena){
    u64 thread_index = cast(u64)info->thread_index;
    
    context->low_stack_address = &(u8){0};
    
    context->thread_info = info;
    context->arena = arena;
    context->ast_arena = create_memory_arena(giga_bytes(8), 1.0f, mega_bytes(1));
    context->ast_serializer = (s32)(thread_index << 24);
    
    smm emit_pool_capacity = mega_bytes(100);
    struct os_virtual_buffer emit_pool_buf = os_reserve_memory(0, emit_pool_capacity);
    if(!emit_pool_buf.memory){ print("memory error!\n"); os_panic(1); }
    context->emit_pool.base     = emit_pool_buf.memory;
    context->emit_pool.current  = emit_pool_buf.memory;
    context->emit_pool.end      = emit_pool_buf.memory;
    context->emit_pool.reserved = emit_pool_capacity;
    
    context->pragma_alignment = 16;
}

func void reset_context(struct context *context){
    // :reset_context :zero_context :clear_context
    context->current_emit_offset_of_rsp = 0;
    context->current_scope              = null;
    context->current_switch             = null;
    context->current_switch_default_label_token = null;
    context->sleep_on                   = null;
    context->sleeping_ident             = null;
    //context->spill_allocator          = 0; zeroed before every statement anyway
    context->ast_stack_at               = 0;
    context->should_sleep               = false;
    context->should_exit_statement      = false;
    context->error                      = false;
    context->current_compilation_unit   = null;
    context->maybe_in_cast              = null;
    context->in_conditional_expression  = 0;
    context->in_static_if_condition     = 0;
    context->current_statement_returns_a_value = 0;
    
    context->jump_label_index = 0;
    context->current_break_label = -1;
    context->current_continue_label = -1;
    
    context->goto_list.first = context->goto_list.last = 0;
    context->label_list.first = context->label_list.last = 0;
    
    context->pragma_pack_stack.first = context->pragma_pack_stack.last = null;
}

func void worker_preprocess_file(struct context *context, struct work_queue_entry *work){
    
    struct work_tokenize_file *work_tokenize_file = cast(struct work_tokenize_file *)work->data;
    context->current_compilation_unit = work_tokenize_file->compilation_unit;
    
    if(!globals.cli_options.quiet) print("   [%lld] %.*s\n", context->current_compilation_unit->index, work_tokenize_file->absolute_file_path.size, work_tokenize_file->absolute_file_path.data);
    
    {
        memset(&context->define_table, 0, sizeof(context->define_table));
        
        // 
        // @WARNING: If you ever change the fact, that these buildin defines have the defined token
        //           '&globals.invalid_token', you need to update the code which checks that you
        //           cannot redeclare these.
        // 
        
        struct define_node *defined_builtin = push_struct(&context->scratch, struct define_node);
        defined_builtin->name = atom_for_string(string("defined"));
        defined_builtin->builtin_define_type = BUILTIN_DEFINE_defined;
        defined_builtin->is_builtin = 1;
        defined_builtin->defined_token = &globals.invalid_token;
        register_define(context, defined_builtin);
        
        struct define_node *has_include_builtin = push_struct(&context->scratch, struct define_node);
        has_include_builtin->name = atom_for_string(string("__has_include"));
        has_include_builtin->builtin_define_type = BUILTIN_DEFINE_has_include;
        has_include_builtin->is_builtin = 1;
        has_include_builtin->defined_token = &globals.invalid_token;
        register_define(context, has_include_builtin);
        
        struct define_node *defined___pragma = push_struct(&context->scratch, struct define_node);
        defined___pragma->name = atom_for_string(string("__pragma"));
        defined___pragma->builtin_define_type = BUILTIN_DEFINE__pragma;
        defined___pragma->is_builtin  = 1;
        defined___pragma->defined_token = &globals.invalid_token;
        register_define(context, defined___pragma);
        
        struct define_node *defined__Pragma = push_struct(&context->scratch, struct define_node);
        defined__Pragma->name = atom_for_string(string("_Pragma"));
        defined__Pragma->builtin_define_type = BUILTIN_DEFINE__pragma;
        defined__Pragma->is_builtin  = 1;
        defined__Pragma->defined_token = &globals.invalid_token;
        register_define(context, defined__Pragma);
        
        struct define_node *defined___FILE__ = push_struct(&context->scratch, struct define_node);
        defined___FILE__->name = atom_for_string(string("__FILE__"));
        defined___FILE__->builtin_define_type = BUILTIN_DEFINE___FILE__;
        defined___FILE__->is_builtin  = 1;
        defined___FILE__->defined_token = &globals.invalid_token;
        register_define(context, defined___FILE__);
        
        struct define_node *defined___LINE__ = push_struct(&context->scratch, struct define_node);
        defined___LINE__->name = atom_for_string(string("__LINE__"));
        defined___LINE__->builtin_define_type = BUILTIN_DEFINE___LINE__;
        defined___LINE__->is_builtin  = 1;
        defined___LINE__->defined_token = &globals.invalid_token;
        register_define(context, defined___LINE__);
        
    }
    sll_clear(context->pragma_once_file_list);
    
    begin_counter(context, tokenize_and_preprocess);
    struct token_array tokenized_file = file_tokenize_and_preprocess(context, work_tokenize_file->absolute_file_path, work_tokenize_file->file_size);
    end_counter(context, tokenize_and_preprocess);
    
    if(context->error) return;
    if(tokenized_file.amount == 0) return; // nothing to do if the file is empty
    
    if(globals.preprocessed_file_handle){
        
        if(globals.cli_options.EP){
            char buffer[0x100];
            int length = snprintf(buffer, sizeof(buffer), "%.*s\n", (int)work_tokenize_file->absolute_file_path.size, work_tokenize_file->absolute_file_path.data);
            
            DWORD chars_written;
            HANDLE stderr = GetStdHandle(STD_ERROR_HANDLE);
            WriteFile(stderr, buffer, (u32)length, &chars_written, 0);
        }
        
        struct token *last_token = &globals.invalid_token;
        
        char *preprocessed_file_start = (char *)arena_current(&context->scratch);
        
        for(smm token_index = 0; token_index < tokenized_file.amount; token_index++){
            struct token *token = &tokenized_file.data[token_index];
            
            if(token->line != last_token->line || token->file_index != last_token->file_index){ 
                
                *push_uninitialized_struct(&context->scratch, char) = '\n';
                if(token->line != last_token->line + 1){
                    *push_uninitialized_struct(&context->scratch, char) = '\n';
                    // @cleanup: lines are still not correct...
                }
                
                if(token->file_index != last_token->file_index){
                    char *filename = globals.file_table.data[token->file_index]->absolute_file_path;
                    struct string line_directive = push_format_string(&context->scratch, "#line %u \"%s\"\n", token->line, filename);
                    
                    // @hack: remove the zero-terminator.
                    assert(context->scratch.current == line_directive.data + line_directive.size + 1);
                    context->scratch.current -= 1;
                }
                
                if(token->column){
                    smm amount_of_spaces = token->column-1; // @note: column is one-based;
                    memset(push_uninitialized_data(&context->scratch, char, amount_of_spaces), ' ', amount_of_spaces);
                }
            }else{
                int skip = 0;
                
                if(TOKEN_first_keyword <= last_token->type && last_token->type < TOKEN_one_past_last_keyword && token->type == TOKEN_open_paren) skip = 1;
                if(TOKEN_first_keyword <= last_token->type && last_token->type < TOKEN_one_past_last_keyword && token->type == TOKEN_closed_paren) skip = 1;
                if(TOKEN_float_literal <= last_token->type && last_token->type < TOKEN_string_literal && token->type == TOKEN_open_paren) skip = 1;
                if(TOKEN_float_literal <= last_token->type && last_token->type < TOKEN_string_literal && token->type == TOKEN_closed_paren) skip = 1;
                if(TOKEN_float_literal <= last_token->type && last_token->type < TOKEN_string_literal && token->type == TOKEN_comma) skip = 1;
                if(last_token->type == TOKEN_identifier && token->type == TOKEN_open_paren) skip = 1;
                if(last_token->type == TOKEN_identifier && token->type == TOKEN_closed_paren) skip = 1;
                
                if(TOKEN_first_keyword <= token->type && token->type < TOKEN_one_past_last_keyword && last_token->type == TOKEN_open_paren) skip = 1;
                if(TOKEN_float_literal <= token->type && token->type < TOKEN_string_literal && last_token->type == TOKEN_open_paren) skip = 1;
                if(token->type == TOKEN_identifier && last_token->type == TOKEN_open_paren) skip = 1;
                
                if(last_token->type == TOKEN_identifier && token->type == TOKEN_comma) skip = 1;
                
                if(last_token->type == TOKEN_open_paren && token->type == TOKEN_closed_paren) skip = 1;
                if(last_token->type == TOKEN_open_paren && token->type == TOKEN_open_paren) skip = 1;
                if(last_token->type == TOKEN_closed_paren && token->type == TOKEN_closed_paren) skip = 1;
                
                if(last_token->type == TOKEN_increment && token->type == TOKEN_identifier) skip = 1;
                if(last_token->type == TOKEN_identifier && token->type == TOKEN_increment) skip = 1;
                if(last_token->type == TOKEN_decrement && token->type == TOKEN_identifier) skip = 1;
                if(last_token->type == TOKEN_identifier && token->type == TOKEN_decrement) skip = 1;
                
                if(last_token->type == TOKEN_times && token->type == TOKEN_times) skip = 1;
                if(last_token->type == TOKEN_times && token->type == TOKEN_identifier) skip = 1;
                
                if(token->type == TOKEN_semicolon) skip = 1;
                
                if(!skip) *push_uninitialized_struct(&context->scratch, char) = ' ';
            }
            
            last_token = token;
            
            memcpy(push_uninitialized_data(&context->scratch, char, token->string.size), token->string.data, token->string.size);
        }
        *push_uninitialized_struct(&context->scratch, char) = '\n';
        
        smm preprocessed_file_size = (char *)arena_current(&context->scratch) - preprocessed_file_start;
        
        WriteFile(globals.preprocessed_file_handle, preprocessed_file_start, (DWORD)preprocessed_file_size, NULL, NULL);
        return;
    }
    
    // :chunk the file
    begin_token_array(context, tokenized_file);
    
    // @note: as this just segments the text file into pieces, we require macro expansion to already
    //        have happened.
    
    struct{
        struct work_queue_entry *first;
        struct work_queue_entry *last;
        smm amount;
    } work_queue_entries = zero_struct;
    
    begin_counter(context, chunking);
    while(in_current_token_array(context)){
        
        // Handle top-level pragma pack
        while(peek_token_eat(context, TOKEN_pragma_pack)){
            parse_and_process_pragma_pack(context);
        }
        
        if(!in_current_token_array(context)) break; // In case there was a pragma pack at the end of the file.
        
        smm start_marker = context->token_at;
        
        b32 is_static = false;
        b32 is_extern = false;
        b32 is_inline = false;
        b32 got_type  = false;
        
        struct token *got_identifier = null;
        
        struct pragma_pack_node *start_pragma_pack_node = context->pragma_pack_stack.first;
        
        // pre-"parse" a global scope entry
        while(in_current_token_array(context)){
            struct token *token = next_token(context); // make sure we make progress
            
            if(TOKEN_first_basic_type <= token->type && token->type <= TOKEN_last_basic_type){
                got_type = true;
            }else if(token->type == TOKEN_enum || token->type == TOKEN_struct || token->type == TOKEN_union){
                
                while(peek_token_eat(context, TOKEN_declspec)){
                    if(peek_token(context, TOKEN_open_paren)){
                        skip_until_tokens_are_balanced(context, null, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' in '__declspec'.");
                    }
                }
                
                peek_token_eat(context, TOKEN_identifier); // skip the name of the struct/enum/union
                if(peek_token(context, TOKEN_open_curly)){
                    skip_until_tokens_are_balanced(context, null, TOKEN_open_curly, TOKEN_closed_curly, "Unmatched '{' in type definition.");
                }
                
                got_type = true;
                continue;
            }else if(token->type == TOKEN_declspec){
                if(peek_token(context, TOKEN_open_paren)){
                    skip_until_tokens_are_balanced(context, null, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' in '__declspec'.");
                }
            }else if(token->type == TOKEN_static){ 
                is_static = true;
            }else if(token->type == TOKEN_extern){
                is_extern = true;
            }else if(token->type == TOKEN_inline || token->type == TOKEN_forceinline){
                is_inline = true;
            }else if(token->type == TOKEN_noreturn){
                
            }else if(token->type == TOKEN_identifier){
                if(got_type){
                    got_identifier = token;
                    
                    smm capacity = context->current_compilation_unit->is_token_static_table.capacity;
                    smm size = context->current_compilation_unit->is_token_static_table.size;
                    struct is_token_static *is_static_table = context->current_compilation_unit->is_token_static_table.data;
                    
                    // Register this token as 'is_static' in the 'token_to_is_static_table'.
                    if(2 *size + 1  > capacity){
                        struct is_token_static *new_table = push_data(context->arena, struct is_token_static, capacity * 2);
                        
                        // Copy the hash table.
                        for(smm entry_index = 0; entry_index < capacity; entry_index++){
                            if(is_static_table[entry_index].token == null) continue;
                            smm hash = is_static_table[entry_index].token->atom.string_hash;
                            
                            for(smm table_index = 0; table_index < 2 * capacity; table_index++){
                                smm index = (hash + table_index) & (2 * capacity - 1);
                                
                                if(!new_table[index].token){
                                    new_table[index] = is_static_table[entry_index];
                                    break;
                                }
                            }
                        }
                        
                        context->current_compilation_unit->is_token_static_table.data = new_table;
                        context->current_compilation_unit->is_token_static_table.capacity *= 2;
                        
                        capacity *= 2;
                        is_static_table = new_table;
                    }
                    
                    smm hash = token->string_hash;
                    for(smm i = 0; i < capacity; i++){
                        smm index = (hash + i) & (capacity - 1);
                        
                        b32 actually_static = is_static || (!is_extern && is_inline);
                        
                        if(!is_static_table[index].token){
                            is_static_table[index].token = token;
                            is_static_table[index].is_static = actually_static;
                            context->current_compilation_unit->is_token_static_table.size += 1;
                            break;
                        }
                        
                        if(atoms_match(token->atom, is_static_table[index].token->atom)){
                            
                            if(is_static_table[index].is_static){
                                // We are fine, the whatever the declaration was, it is supposed to be static.
                                // @cleanup: What happens with 'inline' exactly?
                            }else if(is_static){
                                // @cleanup: We should probably report all of these, hence we should have a big 'begin_error_report' thing.
                                begin_error_report(context);
                                report_warning(context, WARNING_redefining_declaration_from_extern_to_static, token, "Redefining declaration from external to static.");
                                report_warning(context, WARNING_redefining_declaration_from_extern_to_static, is_static_table[index].token, "... Here is the previous non-static declaration.");
                                end_error_report(context);
                                
                                is_static_table[index].is_static = 1;
                            }else if(is_inline && !is_extern){
                                begin_error_report(context);
                                report_warning(context, WARNING_inline_function_is_implicitly_external, token, "Function declared 'inline' but not 'extern' is implicitly 'extern', because of previous external declaration.");
                                report_warning(context, WARNING_inline_function_is_implicitly_external, is_static_table[index].token, "... Here is the previous declaration.");
                                end_error_report(context);
                            }
                            break;
                        }
                    }
                }else{
                    got_type = true;
                }
            }else if(token->type == TOKEN_equals){
                b32 got_semi = false;
                while(in_current_token_array(context)){
                    struct token *test = next_token(context);
                    
                    if(test->type == TOKEN_open_paren){
                        skip_until_tokens_are_balanced(context, test, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' in global scope initializer.");
                        continue;
                    }
                    if(test->type == TOKEN_open_curly){
                        skip_until_tokens_are_balanced(context, test, TOKEN_open_curly, TOKEN_closed_curly, "Unmatched '{' in global scope initializer.");
                        continue;
                    }
                    if(test->type == TOKEN_open_index){
                        skip_until_tokens_are_balanced(context, test, TOKEN_open_index, TOKEN_closed_index, "Unmatched '[' in global scope initializer.");
                        continue;
                    }
                    
                    if(test->type == TOKEN_comma) break;
                    if(test->type == TOKEN_semicolon){
                        got_semi = true;
                        break;
                    }
                }
                
                got_identifier = null;
                
                // this is a declaration like 'u32 asd = 1;' (ends in assignment)
                if(got_semi) break;
            }else if(token->type == TOKEN_comma){
                got_identifier = null;
            }else if(token->type == TOKEN_semicolon){
                // this is a declaration like 'u32 asd;' (ends in semicolon);
                break;
            }else if(token->type == TOKEN_open_curly){
                // this is a declaration like 'u32 asd(){}'
                
                // :__FUNCTION__is_resolved_during_chunking
                // 
                // This used to be:
                //    skip_until_tokens_are_balanced(context, token, TOKEN_open_curly, TOKEN_closed_curly);
                // But now we try patching __FUNCTION__ here, so we are doing it manually.
                //                                                                             - 03.10.2021
                
                u64 count = 1;
                while(true){
                    if(!in_current_token_array(context)){
                        report_error(context, token, "Unmatched '{' at global scope.");
                        break;
                    }else if(peek_token(context, TOKEN_closed_curly)){
                        if (--count == 0) break;
                    }else if(peek_token(context, TOKEN_open_curly)){
                        count++;
                    }else if(peek_token(context, TOKEN___func__)){
                        if(got_identifier){
                            struct token *function = get_current_token(context);
                            function->type   = TOKEN_string_literal;
                            struct string function_string = push_format_string(context->arena, "\"%.*s\"", got_identifier->size, got_identifier->data);
                            function->data = function_string.data;
                            function->size = (u32)function_string.size;
                        }else{
                            report_syntax_error(context, get_current_token(context), "__FUNCTION__ outside of a function");
                        }
                    }else if(peek_token_eat(context, TOKEN_pragma_pack)){
                        parse_and_process_pragma_pack(context);
                        continue;
                    }
                    next_token(context);
                }
                next_token(context);
                break;
            }else if(token->type == TOKEN_open_paren){
                // @note: skip the parens for 'int a(int b);'
                //        but don't skip the parens for 'int (a);'
                if(got_identifier){
                    skip_until_tokens_are_balanced(context, token, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' at global scope.");
                }
            }else if(token->type == TOKEN_open_index){
                // this is a declaration like u32 asd[];
                skip_until_tokens_are_balanced(context, token, TOKEN_open_index, TOKEN_closed_index, "Unmatched '[' at global scope.");
            }else if(token->type == TOKEN_static_assert){
                if(peek_token(context, TOKEN_open_paren)){
                    skip_until_tokens_are_balanced(context, 0, TOKEN_open_paren, TOKEN_closed_paren, "Unmatched '(' at global scope.");
                }
            }
        }
        
        if(start_pragma_pack_node != context->pragma_pack_stack.first){
            struct token *token = got_identifier;
            if(!token) token = tokenized_file.data + start_marker;
            
            begin_error_report(context);
            report_error(context, token, "Top of pragma pack stack changed within this global scope entry. This is not supported. E.g.: int main(){ __pragma(pack(pop)) }");
            if(start_pragma_pack_node) report_error(context, start_pragma_pack_node->token, "... This was the pragma responsible for the previous top.");
            if(context->pragma_pack_stack.first) report_error(context, context->pragma_pack_stack.first->token, "... This is the pragma responsible for the new top.");
            end_error_report(context);
        }
        
        if(context->error) return;
        
        smm end_marker = context->token_at;
        
        struct token_array array = {
            .data = tokenized_file.data + start_marker,
            .size = end_marker - start_marker,
        };
        
#if PRINT_PREPROCESSED_FILE
        {
            //
            // @cleanup: if we actually want to make this a user debugging facillity, this 
            //           should be a nice pretty printer for the code.
            //
            print("\n//____________________________________________________________________________\n\n");
            pretty_print_token_array(context, array);
        }
#endif
        
        struct parse_work *parse_work = push_parse_work(context, array, work_tokenize_file->compilation_unit, /*function = */ null);
        
        struct work_queue_entry *entry = push_struct(context->arena, struct work_queue_entry);
        entry->data = parse_work;
        
        sll_push_back(work_queue_entries, entry);
        work_queue_entries.amount++;
    }
    
    end_counter(context, chunking);
    
    if(globals.cli_options.seed){
        struct random_series series = random_series_from_seed(globals.cli_options.seed);
        
        struct{
            struct work_queue_entry *first;
            struct work_queue_entry *last;
        } stacks[0x10] = zero_struct;
        
        for(struct work_queue_entry *it = work_queue_entries.first; it;){
            struct work_queue_entry *next = it->next;
            
            smm index = random_u32(&series) & (array_count(stacks) - 1);
            
            sll_push_front(stacks[index], it);
            it = next;
        }
        
        sll_clear(work_queue_entries);                
        for(smm i = 0; i < array_count(stacks); i++){
            sll_push_back_list(work_queue_entries, stacks[i]);
        }
    }
    
    work_queue_append_list(&globals.work_queue_parse_global_scope_entries, work_queue_entries.first, work_queue_entries.last, work_queue_entries.amount);
    
    assert(!in_current_token_array(context));
}

func void worker_parse_global_scope_entry(struct context *context, struct work_queue_entry *work){
    log_print("parsing a global scope entry");
    
    struct parse_work *parse_work = (struct parse_work *)work->data;
    context->current_compilation_unit = parse_work->compilation_unit;
    context->pragma_alignment = parse_work->pragma_alignment;
    
    begin_token_array(context, parse_work->tokens);
    
    if(peek_token(context, TOKEN_semicolon)){
        // allow for empty ';' at global scope @speed, we could detect this above
        report_warning(context, WARNING_extraneous_semicolon, next_token(context), "Extraneous ';' at global scope.");
        assert(!in_current_token_array(context));
        return;
    }
    
    if(peek_token(context, TOKEN_static_assert)){
        struct token *static_assert_token = next_token(context);
        parse_static_assert(context, static_assert_token); // @cleanup: static_assert and sleeping.
        expect_token(context, TOKEN_semicolon, "Expected a ';' after _Static_assert.");
        return;
    }
    
    struct declaration_list declaration_list = parse_declaration_list(context, null, null);
    if(context->error) return;
    if(context->should_sleep){
        struct token *sleep_on = context->sleep_on;
        work->sleeping_on = context->sleep_on;
        
        parse_work->sleeping_ident = context->sleeping_ident;
        assert(!parse_work->sleeping_ident || parse_work->sleeping_ident->type == TOKEN_identifier);
        
        // @note: this logic needs to match wake_up_sleepers
        struct sleeper_table *sleeper_table = (context->sleep_purpose == SLEEP_on_decl) ? &globals.declaration_sleeper_table : &globals.compound_sleeper_table;
        
        if(context->sleep_purpose == SLEEP_on_decl){
            b32 is_static = compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(context->current_compilation_unit, sleep_on->atom) == IDENTIFIER_is_static;
            if(is_static) sleeper_table = &context->current_compilation_unit->static_sleeper_table;
        }
        
        sleeper_table_add(sleeper_table, work, sleep_on);
        return;
    }
    
    // might not be the full type e.g. int (*a)
    struct ast_type *lhs_type = declaration_list.type_specifier;
    
    if(sll_is_empty(declaration_list)){
        // for struct this might be a struct declaration, in which case  we are done.
        if(lhs_type->kind == AST_struct || lhs_type->kind == AST_union){
            struct ast_compound_type *compound = cast(struct ast_compound_type *)lhs_type;
            if(compound->identifier.size == 0){
                // @cleanup: does this report the error in the wrong spot, if we have:
                //     typedef struct {} asd;
                //     asd;
                report_warning(context, WARNING_does_not_declare_anything, compound->base.token, "Anonymous %s declaration does not declare anything.", compound->base.kind == AST_union ? "union" : "struct");
            }
        }else if(declaration_list.defined_type_specifier && declaration_list.defined_type_specifier->kind == AST_enum){
            // these are fine. we could exectly check for enum{} I guess
        }else if(declaration_list.type_specifier->kind == AST_unresolved_type){
            // these are fine, we got 'struct unresolved;'
        }else{
            // we got 'u32;'
            report_warning(context, WARNING_does_not_declare_anything, get_current_token_for_error_report(context), "Declaration does not define anything.");
        }
        expect_token(context, TOKEN_semicolon, "Expected a ';' after a type definition at global scope.");
        assert(!in_current_token_array(context));
        
        return;
    }
    
    if(declaration_list.last->decl->base.kind == AST_function && peek_token_eat(context, TOKEN_open_curly)){
        struct ast_function *function = cast(struct ast_function *)declaration_list.last->decl;
        parse_work->tokens.data += context->token_at;
        parse_work->tokens.size -= context->token_at;
        assert(parse_work->tokens.size > 0);
        
        parse_work->function = function;
        
        work_queue_push_work(context, &globals.work_queue_parse_functions, parse_work);
    }else{
        expect_token(context, TOKEN_semicolon, "Expected ';' after declaration at global scope.");
        if(context->error) return;
        assert(!in_current_token_array(context));
    }
}

func void worker_parse_function(struct context *context, struct work_queue_entry *work){
    
    struct parse_work *parse_work = cast(struct parse_work *)work->data;
    context->current_compilation_unit = parse_work->compilation_unit;
    
    // @WARNING: 'function->base.token' is not necessarly the token you might expect, because predefines
    //           might be first when we register the token. @cleanup: maybe we should adjust the token?
    //           If you want to conditional breakpoint on some line, for now use scope->base.line instead.
    //           Its token is the '{' that begins the body of the function.      16.05.2021
    
    struct ast_function *function = parse_work->function;
    assert(function->base.kind == AST_function);
    context->current_function = function;
    
    log_print("parsing: %.*s", function->identifier->amount, function->identifier->data);
    
    begin_token_array(context, parse_work->tokens);
    
    {   // 
        // Estimate the amount of lines in the function. :function_line_information
        // 
        smm amount_of_lines = 8;
        struct token *first_token = parse_work->tokens.data;
        struct token *last_token = parse_work->tokens.data + parse_work->tokens.size - 1;
        if(first_token->file_index == last_token->file_index){
            amount_of_lines = last_token->line - first_token->line + 1;
        }
        
        function->line_information.data = push_uninitialized_data(context->arena, struct function_line_information, amount_of_lines);
        function->line_information.capacity = amount_of_lines;
        
        // 
        // Set up data to emit line information.
        // 
        context->last_line_pushed = -1;
        context->last_offset_pushed = -1;
        context->function_file_index = first_token->file_index;
    }
    
    assert(function->scope->kind == AST_scope);
    struct ast_scope *scope = (struct ast_scope *)function->scope;
    context->current_scope = scope;
    
    // :unresolved_types
    //
    // For a function, unresolved arguments and return types are allowed.
    // For example, if there is just a random function declaration at global scope, like:
    //     struct unresolved_return unused_function(struct unresolved_argument argument);
    // This should compile. 
    // This means that we could also have encountered this version of the _first_ and 
    // then later 'struct unresolved_return' and 'struct unresolved_argument' were filled in.
    // Hence, at this point the return type and the arguments might be unresolved and we need to 
    // resolve them here.
    //                                                                      07.02.2023
    
    maybe_resolve_unresolved_type_or_sleep_or_error(context, &function->type->return_type);
    if(context->error) return;
    
    for_ast_list(function->type->argument_list){
        assert(it->value->kind == AST_declaration);
        struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
        if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)) return;
        
        // Allow missing argument names.
        if(decl->identifier != globals.invalid_identifier_token) register_declaration(context, decl);
    }
    
    
    function->start_in_ast_arena = arena_current(&context->ast_arena);
    
    if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm){
        //
        // Special case for __declspec(inline_asm) in this case we only want a single 'ast_asm_block'
        //
        
        struct ast_asm_block *asm_block = push_ast(context, asm_block);
        asm_block->token = scope->token;
        set_resolved_type(&asm_block->base, &globals.typedef_void, null);
        
        context->in_inline_asm_function = function->identifier;
        parse_asm_block(context, asm_block);
        context->in_inline_asm_function = null;
        
        if(function->type->return_type != &globals.typedef_void){
            if(asm_block->instructions.last == null || asm_block->instructions.last->memonic != MEMONIC_return_from_inline_asm_function){
                struct string type_string = push_type_string(&context->scratch, &context->scratch, function->type->return_type);
                report_error(context, function->identifier, "__declspec(inline_asm)-function has return type '%.*s' but last instruction was not 'return'.", type_string.size, type_string.data);
            }
            context->current_statement_returns_a_value = 1;
        }
        
        scope->asm_block = asm_block;
        
        for_ast_list(function->type->argument_list){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            
            if(decl->_times_referenced == 0){
                report_warning(context, WARNING_unused_local_variable, decl->identifier, "Function argument of __declspec(inline_asm)-function is not referenced.");
            }
        }
    }else{
        //
        // Parse the function!
        //
        parse_imperative_scope(context);
    }
    
    if(context->error) return;
    
    assert(!in_current_token_array(context));
    
    parser_scope_pop(context, scope);
    
    for(struct ast_goto *ast_goto = context->goto_list.first; ast_goto; ast_goto = ast_goto->next){
        
        b32 found = false;
        for(struct ast_label *label = context->label_list.first; label; label = label->next){
            
            if(atoms_match(label->ident, ast_goto->ident)){
                ast_goto->jump->label_number = label->jump_label->label_number;
                found = true;
                break;
            }
        }
        
        if(!found){
            report_error(context, ast_goto->token, "'goto' to undefined label '%.*s'.", ast_goto->ident.amount, ast_goto->ident.data);
            return;
        }
    }
    
    function->amount_of_jump_labels = context->jump_label_index;
    
    function->stack_space_needed = context->current_emit_offset_of_rsp;
    
    if(atoms_match(function->identifier->atom, globals.keyword_main)){
        // "If the return type of the 'main' function is a type compatible with int, [...]
        //  reaching the } that terminates the main function returns a value of 0."
        if(function->type->return_type == &globals.typedef_s32){
            
            context->current_statement_returns_a_value = 1;
            
            ast_push_s32_literal(context, 0);
            struct ast_return *ast_return = push_expression(context, return); // @cleanup: is this the correct token?
            set_resolved_type(&ast_return->base, &globals.typedef_void, null);
        }
    }
    
    if(!context->current_statement_returns_a_value){
        
        if((function->type->flags & FUNCTION_TYPE_FLAGS_is_noreturn) && !(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm)){
            report_warning(context, WARNING_return_in_noreturn_function, get_current_token_for_error_report(context), "Control flow reaching the end of '_Noreturn' function.");
        }else if(function->type->return_type != &globals.typedef_void){
            // 
            // We have reached the end of a non-void function, but there was no return.
            // Report a warning.
            // 
            struct string return_type_string = push_type_string(context->arena, &context->scratch, function->type->return_type);
            report_warning(context, WARNING_missing_return, get_current_token_for_error_report(context), "Function of type '%.*s' must return a value.", return_type_string.size, return_type_string.data);
        }
    }
    
    function->end_in_ast_arena = arena_current(&context->ast_arena);
    
    context->current_function = null;
}

func void worker_emit_code(struct context *context, struct work_queue_entry *work){
    struct ast_function *function = (void *)work->data;
    assert(function->base.kind == AST_function);
    assert(!function->memory_location);
    
    emit_code_for_function(context, function);
    
    for_ast_list(function->static_variables){
        struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
        
        // @cleanup: what if they are not referenced?
        decl->flags |= DECLARATION_FLAGS_is_reachable_from_entry;
        
        assert(!decl->memory_location);
        if(decl->assign_expr){
            decl->memory_location = evaluate_static_initializer(context, decl);
        }
    }
}

func int worker_work(struct context *context, struct work_queue *queue, void (*worker)(struct context *context, struct work_queue_entry *work)){
    struct work_queue_entry *work = work_queue_get_work(queue);
    if(!work) return 0;
    
    struct temporary_memory scratch_temp = begin_temporary_memory(&context->scratch);
    
    reset_context(context);
    
    worker(context, work);
    
    end_temporary_memory(scratch_temp);
    
    
    if(context->error){
        atomic_store(b32, globals.an_error_has_occurred, true);
    }
    
    
    atomic_add(&queue->work_entries_in_flight, -1);
    return 1;
}


func u32 work_thread_proc(void *param){
    struct thread_info *info = param;
    
    log_print("starting thread %d", info->thread_index);
    
    struct memory_arena main_arena = create_memory_arena(giga_bytes(8), 2.0f, mega_bytes(1));
    
    struct context *context = push_struct(&main_arena, struct context);
    info->context = context;
    init_context(context, info, &main_arena);
    
    begin_counter(context, thread_total);
    
    for(enum compile_stage stage = COMPILE_STAGE_tokenize_files; stage < COMPILE_STAGE_count; stage++){
        
        static struct{
            struct work_queue *queue;
            void (*worker)(struct context *context, struct work_queue_entry *work);
        } stage_data[COMPILE_STAGE_count] = {
            [COMPILE_STAGE_tokenize_files]             = {.queue = &globals.work_queue_tokenize_files,             .worker = worker_preprocess_file },
            [COMPILE_STAGE_parse_global_scope_entries] = {.queue = &globals.work_queue_parse_global_scope_entries, .worker = worker_parse_global_scope_entry },
            [COMPILE_STAGE_parse_function]             = {.queue = &globals.work_queue_parse_functions,            .worker = worker_parse_function },
            [COMPILE_STAGE_emit_code]                  = {.queue = &globals.work_queue_emit_code,                  .worker = worker_emit_code },
        };
        
        while(globals.compile_stage == stage){
            int should_continue = worker_work(context, stage_data[stage].queue, stage_data[stage].worker);
            
            if(!should_continue){
                WaitForSingleObject(globals.wake_event, INFINITE);
            }
        }
    }
    
    end_counter(context, thread_total);
    
    return 0;
}


//_____________________________________________________________________________________________________________________
// Setup code needed by main

func struct token *push_dummy_token(struct memory_arena *arena, struct atom token_atom, enum token_type token_type){
    
    struct token *token = push_struct(arena, struct token);
    token->type   = token_type;
    token->atom   = token_atom;
    token->file_index = -1; // invalid file index.
    token->line   = 1;
    token->column = 1;
    
    return token;
}

func void register_intrinsic_function_declaration(struct context *context, struct token *token, struct ast_function_type *type){
    
    struct ast_function *declaration = push_ast(context, function);
    declaration->identifier = token;
    declaration->type = type;
    declaration->offset_in_text_section = -1;
    declaration->compilation_unit = &globals.hacky_global_compilation_unit;
    declaration->as_decl.flags |= DECLARATION_FLAGS_is_intrinsic | DECLARATION_FLAGS_is_global;
    set_resolved_type(&declaration->base, &globals.typedef_void, null);
    
    ast_table_add_or_return_previous_entry(&globals.global_declarations, &declaration->base, token);
}


func struct ast_function *get_entry_point_or_error(struct context *context){
    if(globals.an_error_has_occurred) return null;
    
    struct atom entry_point = globals.entry_point_name; // @cleanup: there is no reason for the entry_point to be stored in globals...
    
    struct ast_function *function = cast(struct ast_function *)ast_table_get(&globals.global_declarations, entry_point);
    
    if(!function){
        report_error(context, null, "Error: Specified entry point '%.*s' not found.", entry_point.length, entry_point.data);
        globals.an_error_has_occurred = true;
        os_debug_break();
        return null;
    }
    
    if(function->base.kind != AST_function){
        report_error(context, function->identifier, "Specified entry point '%.*s' is not a function.", entry_point.length, entry_point.data);
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
        report_error(context, function->identifier, "Entry point cannot be '__declspec(dllimport)'.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->as_decl.flags & DECLARATION_FLAGS_is_static){
        report_error(context, function->identifier, "Entry point cannot be static.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->as_decl.flags & DECLARATION_FLAGS_is_intrinsic){
        report_error(context, function->identifier, "Entry point cannot be an intrinsic function.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm){
        report_error(context, function->identifier, "Entry point cannot be declarated __declspec(inline_asm).");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(!function->scope){
        report_error(context, function->identifier, "Entry point has to be defined.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    return function;
}


func struct string hacky_find_newest_system_include_path(struct memory_arena *arena, char *path){
    WIN32_FIND_DATAA find_data;
    char c_file_name[sizeof(find_data.cFileName)];
    
    // @cleanup:
    // "The order in which the search returns the files, such as alphabetical order, 
    //  is not guaranteed, and is dependent on the file system. 
    //  If the data must be sorted, the application must do the ordering 
    //  after obtaining all the results."
    
    smm finds = 0;
    
    HANDLE search_handle = FindFirstFileA(path, &find_data);
    if(search_handle != INVALID_HANDLE_VALUE){
        do{
            u32 FILE_ATTRIBUTE_DIRECTORY = 0x10;
            if(find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY){
                b32 consists_only_of_dots_and_numbers = true;
                char *it = find_data.cFileName;
                
                while(*it){
                    // @note: only the best validation.
                    if('0' <= *it && *it <= '9'){it++; continue;}
                    if(*it == '.'){it++; continue;}
                    consists_only_of_dots_and_numbers = false;
                    break;
                }
                if(consists_only_of_dots_and_numbers){
                    // Take the last one, they are lexically sorted, so the last one _should_ be most recent.
                    memcpy(c_file_name, find_data.cFileName, sizeof(find_data.cFileName));
                    finds++;
                }
            }
        }while(FindNextFileA(search_handle, &find_data));
        
        FindClose(search_handle);
    }
    
    if(finds < 3){ // @note: There always is '.' and '..'
        print("Warning: Implicit system include path '%s' was not found.\n", path);
        print("         This means that some system includes will not be found.\n");
        print("         You can specify system includes manually via '-SI <dir>'\n");
        print("         and get rid of this warning by using '-nostdlib'.\n");
        print("                (Sorry CRT support still needs some work).\n");
        return (struct string)zero_struct;
    }
    
    smm length = cstring_length(path);
    return push_format_string(arena, "%.*s/%s", length - 2, path, c_file_name);
}

func void add_system_include_directory(struct memory_arena *arena, struct string absolute, struct string relative, b32 user_specified){
    
    struct string directory_path = concatenate_file_paths(arena, absolute, relative);
    canonicalize_slashes(directory_path);
    if(!path_is_directory((char *)directory_path.data)){
        if(user_specified){
            print("Error: Specified system include directory '%s' is not a directory.\n", directory_path.data);
            os_panic(1);
        }else{
            print("Warning: Implicit system include path '%s' was not found.\n", directory_path.data);
            print("         This means that some system includes will not be found.\n");
            print("         You can specify system includes manually via '-SI <dir>'\n");
            print("         and get rid of this warning by using '-nostdlib'.\n");
            print("                (Sorry CRT support still needs some work).\n");
            return;
        }
    }
    
    // Ignore whether this was unique or not.
    string_list_add_uniquely(&globals.system_include_directories, arena, directory_path);
}

#pragma comment(lib, "Advapi32.lib")
__declspec(dllimport) u32 RegOpenKeyExA(HANDLE key, char *sub_key, u32 option, u32 desired_access, HANDLE *out_key);
__declspec(dllimport) u32 RegQueryValueExA(HANDLE key, char *value_name, u32 *reserved, u32 *opt_out_type, u8 *opt_out_data, u32 *opt_out_length);
__declspec(dllimport) u32 RegEnumKeyExA(HANDLE key, u32 index, char *out_sub_key_name, u32 *in_out_sub_key_name_size, u32 *reserved, char *opt_in_out_class, u32 *opt_in_out_class_size, void *last_write_time);
__declspec(dllimport) u32 RegCloseKey(HANDLE key);

// 
// https://learn.microsoft.com/en-us/cpp/porting/upgrade-your-code-to-the-universal-crt?view=msvc-170
// 
struct string find_windows_kits_root_and_sdk_version(struct memory_arena *arena, u64 sdk_version[4]){
    // 
    // Get the Windows 10 SDK root by using the registry.
    // 
    struct string ret = zero_struct;
    
    // @note: Why do we specify this KEY_WOW64_32KEY? Seems like we should not?
    //        It might be that we need this because the microsoft compiler is 32-bit and thus uses 32-bit keys.
    //        But not sure. (But I tested and I need it, as the 64-bit version of the key does not have the `OptionId.DesktopCPPx64`.
    
    HANDLE root_key;
    u32 open_registy_result = RegOpenKeyExA(/*HKEY_LOCAL_MACHINE*/(HANDLE)0x80000002ull, "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots", 0, /*KEY_QUERY_VALUE*/1 | /*KEY_WOW64_32KEY*/0x0200 | /*KEY_ENUMERATE_SUB_KEYS*/8, &root_key);
    if(open_registy_result != /*ERROR_SUCCESS*/0){
        print("Warning: Could not find Windows 10 SDK.\n");
        return ret;
    }
    
    u8 buffer[/*MAX_PATH*/260 + 1];
    
    u32 length = sizeof(buffer);
    u32 query_KitsRoot10_result = RegQueryValueExA(root_key, "KitsRoot10", NULL, NULL, buffer, &length);
    if(query_KitsRoot10_result != /*ERROR_SUCCESS*/0){
        print("Warning: Could not find Windows 10 SDK.\n");
        return ret;
    }
    buffer[length] = 0;
    
    // Make sure the length is accurate, they might store null-terminators.
    while(length && buffer[length-1] == 0) length -= 1;
    
    // Make sure the path does not end in a slash.
    while(length && (buffer[length-1] == '\\' || buffer[length-1] == '/')) length -= 1;
    
    // 
    // Search for the newest installed version, by iterating the subkeys.
    // 
    //      Installed Roots
    //         > 10.0.17763.0
    //         > 10.0.18362.0
    //         > 10.0.19041.0
    //         > 10.0.20348.0
    // 
    // We remember the one with the highest number.
    // 
    
    u64 highest_version[4] = {0, 0, 0, 0};
    
    char subkey_name[256];
    for(u32 sub_key_index = 0; ; sub_key_index++){
        
        u32 sub_key_length = sizeof(subkey_name);
        u32 enumerate_key_result = RegEnumKeyExA(root_key, sub_key_index, subkey_name, &sub_key_length, null, null, null, null);
        if(enumerate_key_result != /*ERROR_SUCCESS*/0) break;
        
        struct string version_string = {.data = (u8 *)subkey_name, .size = sub_key_length};
        
        u64 subkey_version[4];
        for(u32 index = 0; index < 4; index++){
            struct string number_string = string_eat_until_characters_front(&version_string, ".");
            subkey_version[index] = string_parse_u64(number_string);
            string_eat_characters_front(&version_string, ".");
        }
        
        int is_higher = false;
        for(u32 index = 0; index < 4; index++){
            if(highest_version[index] > subkey_version[index]) break;
            if(highest_version[index] < subkey_version[index]) {
                is_higher = true;
                break;
            }
        }
        
        if(!is_higher) continue;
        
        // 
        // Check whether or not this sdk installation has `OptionId.DesktopCPPx64`.
        // 
        
        struct string value_string = string("\\Installed Options");
        
        if(value_string.size + sub_key_length + 1 >= sizeof(subkey_name)) continue; // This should never happen... Whatever.
        
        memcpy(subkey_name + sub_key_length, value_string.data, value_string.size + /*zero-terminator*/1);
        
        HANDLE option_key;
        u32 open_option_key_result = RegOpenKeyExA(root_key, subkey_name, 0, /*KEY_QUERY_VALUE*/1 | /*KEY_WOW64_32KEY*/0x0200, &option_key);
        if(open_option_key_result != /*ERROR_SUCCESS*/0) continue;
        
        u32 x64_option = 0;
        u32 x64_option_length = sizeof(x64_option);
        u32 query_x64_option_result = RegQueryValueExA(option_key, "OptionId.DesktopCPPx64", NULL, NULL, (u8 *)&x64_option, &x64_option_length);
        RegCloseKey(option_key);
        if(query_x64_option_result != /*ERROR_SUCCESS*/0 || !x64_option) continue;
        
        // This version was higher, save it.
        for(u32 index = 0; index < 4; index++) highest_version[index] = subkey_version[index];
    }
    
    RegCloseKey(root_key);
    
    for(u32 index = 0; index < 4; index++) sdk_version[index] = highest_version[index];
    return push_zero_terminated_string_copy(arena, create_string(buffer, length));
}


int system(char *);

// :main
int main(int argc, char *argv[]){
    
    if(argc == 1){
        // If we get invoked with only 'hlc' print the usage.
        print("Usage: %s [option...] filename...\n", argv[0]);
        print("For more information see '%s --help'.\n", argv[0]);
        return 0;
    }
    
    u64 begin_cycle_time = __rdtsc();
    f64 begin_time = os_get_time_in_seconds();
    
    f64 stage_one_tokenize_and_preprocess_time = 0.0;
    f64 stage_two_parse_functions_time = 0.0;
    f64 stage_three_emit_code_time = 0.0;
    f64 stage_four_linking = 0.0;
    
    struct context *context = &(struct context)zero_struct;
    begin_counter(context, total);
    begin_counter(context, startup);
    
    struct memory_arena _arena = create_memory_arena(giga_bytes(64), 2.0f, mega_bytes(1));
    struct memory_arena *arena = &_arena;
    
    context->arena = arena;
    
    struct string working_directory = zero_struct;
    // GetCurrentDirectory with 0 returns the size of the buffer including the null terminator
    working_directory.size = GetCurrentDirectoryA(0, null) - 1;
    working_directory.data = push_uninitialized_data(arena, u8, working_directory.length + 1);
    GetCurrentDirectoryA(cast(u32)working_directory.length + 1, working_directory.data);
    canonicalize_slashes(working_directory); // @cleanup: not sure where to put this
    
    u8 compiler_path_buffer[MAX_PATH + 1];
    struct string compiler_path = zero_struct;
    compiler_path.size = GetModuleFileNameA(null, (char *)compiler_path_buffer, MAX_PATH + 1);
    compiler_path.data = compiler_path_buffer;
    GetModuleFileNameA(null, (char *)compiler_path.data, (u32)(compiler_path.size + 1));
    canonicalize_slashes(compiler_path);
    
    // 
    // Parse the command line arguments.
    // 
    
    int cli_parse_options_success = cli_parse_options(&globals.cli_options, arena, argc, argv);
    if(!cli_parse_options_success) return 1;
    
#if PRINT_COMMAND_LINE
    if(!globals.cli_options.quiet){
        print("\n");
        print("command line is: \n    ");
        for(int i = 0; i < argc; i++){
            print("%s ", argv[i]);
        }
        print("\n");
        print("\n");
    }
#endif
    
    // 
    // Check some random arguments.
    // 
    
    if(globals.cli_options.seed_specified && globals.cli_options.seed == 0){
        globals.cli_options.seed = __rdtsc(); // @cleanup: Maybe get some better "random" here?
    }
    
    if(globals.cli_options.EP) globals.cli_options.quiet = 1;
    
    // @cleanup: Warn when we compile to an .exe that none of these options are valid.
    if(globals.cli_options.MD + globals.cli_options.MDd + globals.cli_options.MT + globals.cli_options.MTd > 1){
        print("Error: More than one of the command line options /MD /MDd /MT /MTd are specified.\n");
        return 1;
    }
    
    // 
    // Deduce the files we have to parse.
    // The files parsed in are expected to be '.c', '.lib', '.obj', '.dll', '.exe',
    // with '.obj', '.dll' and '.exe' not yet supported.
    // 
    
    struct{
        struct work_queue_entry *first;
        struct work_queue_entry *last;
        smm amount;
    } files_to_parse = zero_struct;
    
    struct string_list object_files = {0};
    
    for(struct string_list_node *file_name_node = globals.cli_options.files.list.first; file_name_node; file_name_node = file_name_node->next){
        
        struct string path = file_name_node->string;
        
        struct string extension = get_file_extension(path);
        
        if(extension.size > 2){ // Fast-path for .c and .h, not that it matters.
            // 
            // If the file is not a source file, we should use the libraries paths to find the library file.
            // Hence, in this loop we don't want to handle it in any way.
            // 
            if(string_match(extension, string(".lib"))){
                string_list_add_uniquely(&globals.specified_libraries, arena, path);
                continue;
            }else if(string_match(extension, string(".obj"))){
                // @hack: This is a hack to handle linking for now.
                //        We set the output file type to obj and then call into link.exe.
                globals.output_file_type = OUTPUT_FILE_obj;
                
                string_list_postfix(&object_files, arena, path);
                continue;
            }else if(string_match(extension, string(".dll")) || string_match(extension, string(".exe"))){
                print("Error: Currently linking to %.*s files is not implemented.\n", extension.size, extension.data);
                return 1;
            }else if(string_match(extension, string(".res")) || string_match(extension, string(".rc"))){
                print("Warning: Ignoring resource file '%.*s'.\n", path.size, path.data);
                continue;
            }
        }
        
        b32 contains_wildcard = path_contains_wildcard(path);
        
        // 
        // Normalize the path to be an absolute file path.
        // 
        canonicalize_slashes(path);
        
        if(!path_is_absolute(path)){
            path = concatenate_file_paths(arena, working_directory, path);
        }
        
        if(contains_wildcard){
            // 
            // If there is a wildcard, include all files that match the wildcard.
            // 
            
            WIN32_FIND_DATAA find_data;
            HANDLE search_handle = FindFirstFileA((char *)path.data, &find_data);
            
            if(search_handle == INVALID_HANDLE_VALUE){
                print("Error: Specified path '%.*s' does not exist.\n", path.size, path.data);
                return 1;
            }
            
            // @note: From my tests, it seems like the wild character needs to be in the filename 
            //        for FindFirstFileA to succeed.
            struct string path_without_file = strip_file_name(path);
            
            do{
                struct string file_name = string_from_cstring(find_data.cFileName);
                struct string file_path = concatenate_file_paths(arena, path_without_file, file_name);
                
                if(path_is_directory((char *)file_path.data)) continue;
                
                extension = get_file_extension(file_path); // re-get the extension as it might have changed in case of wild-cards.
                
                struct os_file file = os_load_file((char *)file_path.data, 0, 0);
                if(file.file_does_not_exist){
                    print("Error: File '%.*s' could not be loaded.\n", file_path.size, file_path.data);
                    return 1;
                }
                
                if(!string_match(extension, string(".c")) && !string_match(extension, string(".h"))){
                    print("Warning: Input file '%.*s' has uncanonical file extension. File is treated as c-file.\n", file_path.size, file_path.data);
                }
                
                struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
                work->absolute_file_path = file_path;
                work->file_size = file.size;
                
                struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
                work_entry->data  = work;
                
                sll_push_back(files_to_parse, work_entry);
                files_to_parse.amount += 1;
                
            }while(FindNextFileA(search_handle, &find_data));
            
            FindClose(search_handle);
        }else{
            struct os_file file = os_load_file((char *)path.data, 0, 0);
            
            if(file.file_does_not_exist){
                print("Error: Specified input file '%.*s' does not exist.\n", path.size, path.data);
                os_debug_break();
                return 2;
            }
            
            if(!string_match(extension, string(".c")) && !string_match(extension, string(".h"))){
                print("Warning: Input file '%.*s' has uncanonical file extension. File is treated as c-file.\n", path.size, path.data);
            }
            
            struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
            work->absolute_file_path = path;
            work->file_size = file.size;
            
            struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
            work_entry->data  = work;
            
            sll_push_back(files_to_parse, work_entry);
            files_to_parse.amount += 1;
        }
    }
    
    if(sll_is_empty(files_to_parse)){
        
        if(!sll_is_empty(object_files.list)){
            struct string_list command = {0};
            
            string_list_postfix_no_copy(&command, arena, string("link.exe"));
            
            for(struct string_list_node *object_file = object_files.list.first; object_file; object_file = object_file->next){
                string_list_postfix_no_copy(&command, arena, push_format_string(arena, " \"%.*s\"", object_file->string.size, object_file->string.data));
            }
            
            // 
            // @hack: To allow hlc.exe to be used as a linker, we check if only object files are specified
            //        and just call into system. In the future, we should just be a linker :)
            // 
            
            return system((char*)string_list_flatten(command, arena).data);
        }
        
        print("Error: No input files specified.\n");
        return 0;
    }
    
    if(!sll_is_empty(object_files.list)){
        print("Error: Currently linking to .obj files are not implemented.\n");
        return 1;
    }
    
    for(struct string_list_node *dir = globals.cli_options.I.list.first; dir; dir = dir->next){
        
        // 
        // Add all of the specified system include directories.
        // 
        
        struct string file_path = dir->string;
        canonicalize_slashes(file_path);
        if(path_is_relative(file_path)){
            file_path = concatenate_file_paths(arena, working_directory, file_path);
        }
        
        add_system_include_directory(arena, file_path, string(""), true);
    }
    
    {
        // 
        // Try to determine the `output_file_type` from the options.
        // 
        if(globals.cli_options.dll && globals.cli_options.obj){
            print("Error: Found both the -DLL and the -OBJ command line options.\n");
            return 1;
        }
        
        // Check the arguments that hard-set the file type.
        if(globals.cli_options.dll) globals.output_file_type = OUTPUT_FILE_dll;
        if(globals.cli_options.obj) globals.output_file_type = OUTPUT_FILE_obj;
        
        // Check the output string.
        if(globals.output_file_type == OUTPUT_FILE_unset && globals.cli_options.out.data){
            struct string output_extension = get_file_extension(globals.cli_options.out);
            if(string_match(output_extension, string(".exe"))) globals.output_file_type = OUTPUT_FILE_exe;
            if(string_match(output_extension, string(".dll"))) globals.output_file_type = OUTPUT_FILE_dll;
            if(string_match(output_extension, string(".obj"))) globals.output_file_type = OUTPUT_FILE_obj;
            if(string_match(output_extension, string(".efi"))) globals.output_file_type = OUTPUT_FILE_efi;
        }
        
        // A no_entry file ought to be a dll.
        if(globals.output_file_type == OUTPUT_FILE_unset && globals.cli_options.no_entry) globals.output_file_type = OUTPUT_FILE_dll;
    }
    
    // Try to infer the subsystem.
    globals.subsystem = globals.cli_options.subsystem;
    if(globals.subsystem == 0 && globals.output_file_type == OUTPUT_FILE_dll) globals.subsystem = SUBSYSTEM_windows;
    
    if(globals.cli_options.entry.data && globals.cli_options.no_entry){
        print("Error: Found option '/no_entry' and option '/entry %.*s'.\n", globals.cli_options.entry.size, globals.cli_options.entry.data);
        return 1;
    }
    
    if(globals.output_file_type == OUTPUT_FILE_obj) globals.cli_options.no_entry = 1;
    
    
    int no_standard_library = globals.cli_options.no_stdlib;
    
    add_system_include_directory(arena, strip_file_name(compiler_path), string("/implicit/include"), false);
    
    u64 sdk_version[4];
    struct string windows_kits_path = find_windows_kits_root_and_sdk_version(arena, sdk_version);
    
    struct string um_library_path   = push_format_string(arena, "%.*s\\Lib\\%llu.%llu.%llu.%llu\\um\\x64\\",   windows_kits_path.size, windows_kits_path.data, sdk_version[0], sdk_version[1], sdk_version[2], sdk_version[3]);
    struct string ucrt_library_path = push_format_string(arena, "%.*s\\Lib\\%llu.%llu.%llu.%llu\\ucrt\\x64\\", windows_kits_path.size, windows_kits_path.data, sdk_version[0], sdk_version[1], sdk_version[2], sdk_version[3]);
    
    
    globals.library_paths = globals.cli_options.LIBPATH;
    
    // @note: For now we always add the library paths, even if 'no_standard_library'.
    // @cleanup: There is no reason for library_paths to be global anymore.
    string_list_add_uniquely(&globals.library_paths, arena, working_directory);
    string_list_add_uniquely(&globals.library_paths, arena, um_library_path);
    string_list_add_uniquely(&globals.library_paths, arena, ucrt_library_path);
    
    // Process the -l options.
    for(struct string_list_node *library_node = globals.cli_options.l.list.first; library_node; library_node = library_node->next){
        struct string library = library_node->string;
        if(!string_match(get_file_extension(library), string(".lib"))){
            library = string_concatenate(arena, library, string(".lib"));
        }
        string_list_add_uniquely(&globals.specified_libraries, arena, library);
    }
    
    if(globals.output_file_type != OUTPUT_FILE_obj && !no_standard_library){
        struct string ucrt_lib_path = push_format_string(arena, "%.*s\\ucrt.lib", ucrt_library_path.size, ucrt_library_path.data);
        string_list_add_uniquely(&globals.specified_libraries, arena, ucrt_lib_path);
        
        int ucrt_load_error = os_load_file((char *)ucrt_lib_path.data, null, 0).file_does_not_exist;
        if(ucrt_load_error){
            // :Error mention Windows sdk.
            print("Error: Could not load '%.*s'.\n", ucrt_lib_path.size, ucrt_lib_path.data);
            print("       To compile without dynamically linking to the CRT, use \n");
            print("       the command-line option '-nostdlib'.\n");
            return 1;
        }
        
        struct string kernel32_lib_path = push_format_string(arena, "%.*s\\kernel32.lib", um_library_path.size, um_library_path.data);
        string_list_add_uniquely(&globals.specified_libraries, arena, kernel32_lib_path);
        
        int kernel32_load_error = os_load_file((char *)kernel32_lib_path.data, null, 0).file_does_not_exist;
        if(kernel32_load_error){
            // :Error mention Windows sdk.
            print("Error: Could not load '%.*s'.\n", kernel32_lib_path.size, kernel32_lib_path.data);
            print("       To compile without dynamically linking to kernel32.lib, use \n");
            print("       the command-line option '-nostdlib'.\n");
            return 1;
        }
    }
    
    struct string windows_kits_include_base = push_format_string(arena, "%.*s\\Include\\%llu.%llu.%llu.%llu", windows_kits_path.size, windows_kits_path.data, sdk_version[0], sdk_version[1], sdk_version[2], sdk_version[3]);
    
    // @note: For now always add the windows folders.
    add_system_include_directory(arena, windows_kits_include_base, string("/um"),     false);
    add_system_include_directory(arena, windows_kits_include_base, string("/shared"), false);
    add_system_include_directory(arena, windows_kits_include_base, string("/winrt"),  false);
    
    if(!no_standard_library) add_system_include_directory(arena, windows_kits_include_base, string("/ucrt"),   false);
    
#ifdef PRINT_SYSTEM_INCLUDE_PATHS
    if(!globals.cli_options.quiet){
        print("\nSystem include directories:\n");
        for(struct string_list_node *include = globals.system_include_directories.list.first; include; include = include->next){
            print("    %.*s\n", include->string.size, include->string.data);
        }
    }
#endif
    
    {  // :init_globals :globals init globals
        
        
#define make_const_typedef(postfix, TOKEN_kind, AST_kind, _string, _size, _align) \
globals.token_##postfix = (struct token){                                         \
    .type  = TOKEN_kind,                                                          \
    .data = (u8 *)_string,                                                        \
    .size = (u32)(sizeof(_string) - 1),                                           \
    .string_hash = (u32)string_djb2_hash(string(_string)),                        \
    .file_index  = /*this is valid!*/-1,                                          \
    .line  = 0,                                                                   \
    .column = 0,                                                                  \
},                                                                                \
globals.typedef_##postfix = (struct ast_type){                                    \
    .s = -1,                                                                      \
    .size   = _size,                                                              \
    .alignment = _align,                                                          \
    .kind   = AST_kind,                                                           \
    .token  = &globals.token_##postfix,                                           \
}                                                                                 \

        make_const_typedef(void, TOKEN_void,     AST_void_type,    "void",               0, 1);
        
        make_const_typedef(Bool, TOKEN_Bool,     AST_integer_type, "_Bool",              1, 1);
        
        make_const_typedef(u8,   TOKEN_unsigned, AST_integer_type, "unsigned char",      1, 1);
        make_const_typedef(u16,  TOKEN_short,    AST_integer_type, "unsigned short",     2, 2);
        make_const_typedef(u32,  TOKEN_unsigned, AST_integer_type, "unsigned int",       4, 4);
        make_const_typedef(u64,  TOKEN_long,     AST_integer_type, "unsigned long long", 8, 8);
        
        make_const_typedef(s8,   TOKEN_char,     AST_integer_type, "char",               1, 1);
        make_const_typedef(s16,  TOKEN_short,    AST_integer_type, "short",              2, 2);
        make_const_typedef(s32,  TOKEN_int,      AST_integer_type, "int",                4, 4);
        make_const_typedef(s64,  TOKEN_long,     AST_integer_type, "long long",          8, 8);
        
        make_const_typedef(atomic_bool, TOKEN_identifier, AST_atomic_integer_type, "atomic_bool",   1, 1);
        
        make_const_typedef(atomic_u8,   TOKEN_identifier, AST_atomic_integer_type, "atomic_uchar",  1, 1);
        make_const_typedef(atomic_u16,  TOKEN_identifier, AST_atomic_integer_type, "atomic_ushort", 2, 2);
        make_const_typedef(atomic_u32,  TOKEN_identifier, AST_atomic_integer_type, "atomic_uint",   4, 4);
        make_const_typedef(atomic_u64,  TOKEN_identifier, AST_atomic_integer_type, "atomic_ullong", 8, 8);
        
        make_const_typedef(atomic_s8,   TOKEN_identifier, AST_atomic_integer_type, "atomic_char",   1, 1);
        make_const_typedef(atomic_s16,  TOKEN_identifier, AST_atomic_integer_type, "atomic_short",  2, 2);
        make_const_typedef(atomic_s32,  TOKEN_identifier, AST_atomic_integer_type, "atomic_int",    4, 4);
        make_const_typedef(atomic_s64,  TOKEN_identifier, AST_atomic_integer_type, "atomic_llong",  8, 8);
        
        globals.typedef_atomic_bool.flags |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_u8.flags   |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_u16.flags  |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_u32.flags  |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_u64.flags  |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_s8.flags   |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_s16.flags  |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_s32.flags  |= TYPE_FLAG_is_atomic;
        globals.typedef_atomic_s64.flags  |= TYPE_FLAG_is_atomic;
        
        make_const_typedef(f32,  TOKEN_float,    AST_float_type,   "float",              4, 4);
        make_const_typedef(f64,  TOKEN_double,   AST_float_type,   "double",             8, 8);
        make_const_typedef(poison, TOKEN_void,   AST_void_type,    "poison",             8, 8);
        
        
        globals.invalid_file.absolute_file_path = "*predefined token*";
        globals.invalid_file.file_index = -1;
        globals.file_table.invalid_file = &globals.invalid_file;
        {
            struct string_list predefines = {0};
            
            for(struct string_list_node *predefine_node = globals.cli_options.D.list.first; predefine_node; predefine_node = predefine_node->next){
                struct string root_option = predefine_node->string;
                
                struct string define_to = root_option;
                struct string to_define = string_eat_until_characters_front(&define_to, " #=");
                
                if(to_define.size == 0){
                    print("Error: Expected an identifier to immediately follow '-D', e.g. '-Ddefine=1'. Got: -D%.*s\n", root_option.size, root_option.data);
                    return 1;
                }
                
                if(define_to.size){
                    string_eat_front(&define_to, 1); // Eat the delimiter.
                }else{
                    // "By default, the value associated with a symbol is 1."
                    define_to = string("1");
                }
                
                // We cannot simply patch up the value of the predefine node, as it would leave the total string size wrong.
                struct string define = push_format_string(arena, "#define %.*s %.*s\n", to_define.size, to_define.data, define_to.size, define_to.data);
                string_list_postfix_no_copy(&predefines, arena, define);
            }
            
            if(!globals.cli_options.no_predefines){
                struct string hardcoded_predefines = string(
                        "#define __HLC__ 1\n"            
                        "#define _M_X64 100\n"
                        "#define _M_AMD64 100\n"
                        "#define _WIN64 1\n"
                        "#define _WIN32 1\n"
                        
                        "#define _MSC_EXTENSIONS 1\n"
                        
                        "#define _INTEGRAL_MAX_BITS 64\n"
                        
                        // I will define these for now.
                        // In the future, these will only be defined by the "clang-cl.exe"-like variant of my compiler.
                        // For the time being, we are more or less working on that variant.
                        //                                                                             - 21.10.2024
                        "#define _MSC_BUILD 0\n"
                        "#define _MSC_FULL_VER 192829336\n"
                        "#define _MSC_VER 1928\n"
                        
                        "#define __assume(a) (void)0\n"
                        
                        // "#define __STDC_VERSION__ 201112L\n"
                        // "#define __STDC__ 1\n" @note: Don't define this as some Microsoft headers work differently if this is specified.
                        "#define __STDC_NO_COMPLEX__ 1\n"
                        "#define __STDC_NO_THREADS__ 1\n"
                        "#define __STDC_NO_VLA__     1\n"
                        // "#define __STDC_NO_ATOMICS__ 1\n"
                        );
                
                
                if(globals.cli_options.std){
                    static struct string version_strings[] = {
                        [STD_c99] = const_string("#define __STDC_VERSION__ 199901L\n"),
                        [STD_c11] = const_string("#define __STDC_VERSION__ 201112L\n"),
                        [STD_c17] = const_string("#define __STDC_VERSION__ 201710L\n"),
                        [STD_c17] = const_string("#define __STDC_VERSION__ 202311L\n"),
                    };
                    // string_list_postfix_no_copy(&predefines, arena, string("#define __STDC__ 1\n")); This is only included with /Zc:__STDC__ @cleanup:
                    string_list_postfix_no_copy(&predefines, arena, version_strings[globals.cli_options.std]);
                }
                
                string_list_postfix_no_copy(&predefines, arena, hardcoded_predefines);
                
                if(globals.cli_options.MD || globals.cli_options.MDd) string_list_postfix_no_copy(&predefines, arena, string("#define _DLL 1\n"));
                if(globals.cli_options.MT || globals.cli_options.MTd || globals.cli_options.MD || globals.cli_options.MDd) string_list_postfix_no_copy(&predefines, arena, string("#define _MT 1\n"));
                if(globals.cli_options.MTd || globals.cli_options.MDd) string_list_postfix_no_copy(&predefines, arena, string("#define _DEBUG 1\n"));
                
                SYSTEMTIME LocalTime;
                GetLocalTime(&LocalTime); // @cleanup: local time?
                
                const char months[12][4] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
                struct string date_define = push_format_string(arena, "#define __DATE__ \"%s %.2d %.4d\"\n", months[LocalTime.wMonth-1], LocalTime.wDay, LocalTime.wYear);
                
                string_list_postfix_no_copy(&predefines, arena, date_define);
                
                struct string time_define = push_format_string(arena, "#define __TIME__ \"%.2d:%.2d:%.2d\"\n", LocalTime.wHour, LocalTime.wMinute, LocalTime.wSecond); 
                string_list_postfix_no_copy(&predefines, arena, time_define);
                
                if(globals.output_file_type == OUTPUT_FILE_obj){
                    string_list_postfix_no_copy(&predefines, arena, string("#define __HLC_COMPILE_TO_OBJECT__ 1\n"));
                }
                
                
            }
            
            struct string predefines_string = string_list_flatten(predefines, arena);
            
            struct token_array tokens = tokenize_raw(context, predefines_string, (u32)globals.invalid_file.file_index, /*is_stupid_hack*/false, /*out lines*/null);
            globals.predefined_tokens = tokens;
        }
        
        globals.keyword_dllimport   = atom_for_string(string("dllimport"));
        globals.keyword_dllexport   = atom_for_string(string("dllexport"));
        globals.keyword_thread      = atom_for_string(string("thread"));
        globals.keyword_align       = atom_for_string(string("align"));
        globals.keyword_noreturn    = atom_for_string(string("noreturn"));
        globals.keyword_noinline    = atom_for_string(string("noinline"));
        globals.keyword_selectany   = atom_for_string(string("selectany"));
        globals.keyword_intrin_type = atom_for_string(string("intrin_type"));
        
        // hlc declspecs
        globals.keyword_inline_asm = atom_for_string(string("inline_asm"));
        globals.keyword_packed     = atom_for_string(string("packed"));
        globals.keyword_printlike  = atom_for_string(string("printlike"));
        
        
        // pragma directives
        globals.pragma_once    = atom_for_string(string("once"));
        globals.pragma_pack    = atom_for_string(string("pack"));
        globals.pragma_comment = atom_for_string(string("comment"));
        globals.pragma_compilation_unit = atom_for_string(string("compilation_unit"));
        
        globals.keyword__VA_ARGS__ = atom_for_string(string("__VA_ARGS__"));
        
        globals.unnamed_tag        = atom_for_string(string("<unnamed-tag>"));
        globals.unnamed_enum       = atom_for_string(string("<unnamed-enum>"));
        globals.invalid_identifier = atom_for_string(string("<invalid identifier>"));
        
        // :hlc_extension
        globals.keyword_data = atom_for_string(string("data"));
        globals.keyword_size = atom_for_string(string("size"));
        globals.keyword_main = atom_for_string(string("main"));
        
        for(enum memonic memonic = 0; memonic < MEMONIC_count; memonic += 1){
            if(asm_parse_table[memonic].memonic.data){
                add_memonic_to_string_to_memonic_table(asm_parse_table[memonic].memonic, memonic);
            }
        }
        
        for(enum memonic memonic = 1; memonic < MEMONIC_count; memonic += 1){
            //
            // Assert that the inline asm table is filled!
            //
            if(!asm_parse_table[memonic].memonic.data){
                print("Inline asm instruction %d not implemented.\n", memonic);
                os_debug_break();
            }
            
            u32 amount_of_operands = asm_parse_table[memonic].amount_of_operands;
            for(u32 operand_index = 0; operand_index < amount_of_operands; operand_index += 1){
                // Assert that at least _some_ operand flag is set.
                assert(asm_parse_table[memonic].operand_kind_flags[operand_index]);
            }
        }
        
        {
            //
            // set up inline assembly aliases:
            //
            add_memonic_to_string_to_memonic_table(string("rep"), MEMONIC_repe_prefix);
            
            add_memonic_to_string_to_memonic_table(string("cmovb"),   MEMONIC_cmovc);
            add_memonic_to_string_to_memonic_table(string("cmovnae"), MEMONIC_cmovc);
            add_memonic_to_string_to_memonic_table(string("cmovnb"),  MEMONIC_cmovnc);
            add_memonic_to_string_to_memonic_table(string("cmovae"),  MEMONIC_cmovnc);
            add_memonic_to_string_to_memonic_table(string("cmove"),   MEMONIC_cmovz);
            add_memonic_to_string_to_memonic_table(string("cmovne"),  MEMONIC_cmovnz);
            add_memonic_to_string_to_memonic_table(string("cmovna"),  MEMONIC_cmovbe);
            add_memonic_to_string_to_memonic_table(string("cmova"),   MEMONIC_cmovnbe);
            add_memonic_to_string_to_memonic_table(string("cmovpe"),  MEMONIC_cmovp);
            add_memonic_to_string_to_memonic_table(string("cmovpo"),  MEMONIC_cmovnp);
            add_memonic_to_string_to_memonic_table(string("cmovnge"), MEMONIC_cmovl);
            add_memonic_to_string_to_memonic_table(string("cmovge"),  MEMONIC_cmovnl);
            add_memonic_to_string_to_memonic_table(string("cmovng"),  MEMONIC_cmovle);
            add_memonic_to_string_to_memonic_table(string("cmovg"),   MEMONIC_cmovnle);
            
            add_memonic_to_string_to_memonic_table(string("setb"),   MEMONIC_setc);
            add_memonic_to_string_to_memonic_table(string("setnae"), MEMONIC_setc);
            add_memonic_to_string_to_memonic_table(string("setnb"),  MEMONIC_setnc);
            add_memonic_to_string_to_memonic_table(string("setae"),  MEMONIC_setnc);
            add_memonic_to_string_to_memonic_table(string("sete"),   MEMONIC_setz);
            add_memonic_to_string_to_memonic_table(string("setne"),  MEMONIC_setnz);
            add_memonic_to_string_to_memonic_table(string("setna"),  MEMONIC_setbe);
            add_memonic_to_string_to_memonic_table(string("seta"),   MEMONIC_setnbe);
            add_memonic_to_string_to_memonic_table(string("setpe"),  MEMONIC_setp);
            add_memonic_to_string_to_memonic_table(string("setpo"),  MEMONIC_setnp);
            add_memonic_to_string_to_memonic_table(string("setnge"), MEMONIC_setl);
            add_memonic_to_string_to_memonic_table(string("setge"),  MEMONIC_setnl);
            add_memonic_to_string_to_memonic_table(string("setng"),  MEMONIC_setle);
            add_memonic_to_string_to_memonic_table(string("setg"),   MEMONIC_setnle);
            
            add_memonic_to_string_to_memonic_table(string("cmpgeps"),  MEMONIC_cmpnltps);
            add_memonic_to_string_to_memonic_table(string("cmpgtps"),  MEMONIC_cmpnleps);
            add_memonic_to_string_to_memonic_table(string("cmpngeps"), MEMONIC_cmpltps);
            add_memonic_to_string_to_memonic_table(string("cmpngtps"), MEMONIC_cmpleps);
            
            add_memonic_to_string_to_memonic_table(string("cmpgepd"),  MEMONIC_cmpnltpd);
            add_memonic_to_string_to_memonic_table(string("cmpgtpd"),  MEMONIC_cmpnlepd);
            add_memonic_to_string_to_memonic_table(string("cmpngepd"), MEMONIC_cmpltpd);
            add_memonic_to_string_to_memonic_table(string("cmpngtpd"), MEMONIC_cmplepd);
            
            add_memonic_to_string_to_memonic_table(string("cmpgesd"),  MEMONIC_cmpnltsd);
            add_memonic_to_string_to_memonic_table(string("cmpgtsd"),  MEMONIC_cmpnlesd);
            add_memonic_to_string_to_memonic_table(string("cmpngesd"), MEMONIC_cmpltsd);
            add_memonic_to_string_to_memonic_table(string("cmpngtsd"), MEMONIC_cmplesd);
            
            add_memonic_to_string_to_memonic_table(string("cmpgess"),  MEMONIC_cmpnltss);
            add_memonic_to_string_to_memonic_table(string("cmpgtss"),  MEMONIC_cmpnless);
            add_memonic_to_string_to_memonic_table(string("cmpngess"), MEMONIC_cmpltss);
            add_memonic_to_string_to_memonic_table(string("cmpngtss"), MEMONIC_cmpless);
            
        }
        
        begin_counter(context, create_perfect_keyword_tables);
        {
            // create the perfect hash table 'keyword'-> 'TOKEN_KIND'
            struct string keywords[array_count(keyword_table_entries)];
            
            for(u32 i = 0; i < array_count(keywords); i++){
                keywords[i] = keyword_table_entries[i].keyword;
            }
            
            for(u32 size = u32_round_up_to_next_power_of_two(array_count(keywords)); ; size <<= 1){
                struct temporary_memory temp = begin_temporary_memory(arena);
                
                struct keyword_table_entry *table = push_data(arena, struct keyword_table_entry, size);
                b32 should_continue = false;
                for(u32 i = 0; i < array_count(keywords); i++){
                    u64 hash = string_djb2_hash(keywords[i]);
                    if(table[hash & (size - 1)].keyword.data){
                        end_temporary_memory(temp);
                        should_continue = true;
                        break;
                    }
                    table[hash & (size - 1)].keyword = (struct atom){ .string = keywords[i], .string_hash = hash };
                    table[hash & (size - 1)].type   = keyword_table_entries[i].token_kind;
                }
                if(should_continue) continue;
                
                globals.keyword_table_size = size;
                globals.keyword_table = table;
                solidify_temporary_memory(temp);
                break;
            }
        }
        
        {
            // create the perfect hash table 'directive'-> 'TOKEN_KIND'
            struct string directives[array_count(directive_table_entries)];
            
            for(u32 i = 0; i < array_count(directives); i++){
                directives[i] = directive_table_entries[i].directive;
            }
            
            begin_counter(context, create_perfect_directive_table);
            for(u32 size = u32_round_up_to_next_power_of_two(array_count(directives)); ; size <<= 1){
                struct temporary_memory temp = begin_temporary_memory(arena);
                
                struct directive_table_entry *table = push_data(arena, struct directive_table_entry, size);
                b32 should_continue = false;
                for(u32 i = 0; i < array_count(directives); i++){
                    u64 hash = string_djb2_hash(directives[i]);
                    if(table[hash & (size - 1)].directive.data){
                        end_temporary_memory(temp);
                        should_continue = true;
                        break;
                    }
                    table[hash & (size - 1)].directive = (struct atom){ .string = directives[i], .string_hash = hash };
                    table[hash & (size - 1)].type   = directive_table_entries[i].kind;
                }
                if(should_continue) continue;
                
                globals.directive_table_size = size;
                globals.directive_table = table;
                solidify_temporary_memory(temp);
                break;
            }
        }

        end_counter(context, create_perfect_keyword_tables);
        
        struct ast_pointer_type *u8_pointer = push_struct(arena, struct ast_pointer_type);
        u8_pointer->pointer_to = &globals.typedef_u8;
        memset(&u8_pointer->base, 0, sizeof(u8_pointer->base));
        u8_pointer->base.kind = AST_pointer_type;
        u8_pointer->base.size = 8;
        globals.typedef_u8_pointer = &u8_pointer->base;
        
        struct ast_pointer_type *s8_pointer = push_struct(arena, struct ast_pointer_type);
        s8_pointer->pointer_to = &globals.typedef_s8;
        memset(&s8_pointer->base, 0, sizeof(s8_pointer->base));
        s8_pointer->base.kind = AST_pointer_type;
        s8_pointer->base.size = 8;
        globals.typedef_s8_pointer = &s8_pointer->base;
        
        struct ast_pointer_type *void_pointer = push_struct(arena, struct ast_pointer_type);
        void_pointer->pointer_to = &globals.typedef_void;
        memset(&void_pointer->base, 0, sizeof(void_pointer->base));
        void_pointer->base.kind = AST_pointer_type;
        void_pointer->base.size = 8;
        globals.typedef_void_pointer = &void_pointer->base;
        
        globals.wake_event = CreateEventA(0, true, 0, 0);
        
        globals.declaration_sleeper_table = sleeper_table_create(1 << 8);
        globals.compound_sleeper_table = sleeper_table_create(1 << 8);
        
        // :ast_tables
        globals.global_declarations = ast_table_create(1 << 8);
        if(globals.output_file_type == OUTPUT_FILE_obj) globals.external_declarations_at_function_scope = ast_table_create(1 << 8);
        globals.compound_types      = ast_table_create(1 << 8);
        
        globals.hacky_global_compilation_unit.index = -1;
        
        {
            // 
            // The `_alloca` function needs to be intrinsic, as it needs to know a stack offset
            // that is only really known at compile time.
            // 
            
            struct token *alloca_token = push_dummy_token(arena, atom_for_string(string("_alloca")), TOKEN_identifier);
            struct token *size_token = push_dummy_token(arena, atom_for_string(string("size")), TOKEN_identifier);
            
            struct ast_function_type *alloca_type = parser_type_push(context, alloca_token, function_type);
            alloca_type->return_type = &void_pointer->base;
            
            struct declarator_return parameter_declarator = {
                .type = &globals.typedef_u64,
                .ident = size_token,
            };
            
            struct ast_declaration *parameter_declaration = push_declaration_for_declarator(context, parameter_declarator);
            parameter_declaration->compilation_unit = &globals.hacky_global_compilation_unit;
            ast_list_append(&alloca_type->argument_list, context->arena, &parameter_declaration->base);
            set_resolved_type(&parameter_declaration->base, &globals.typedef_void, null);
            
            register_intrinsic_function_declaration(context, alloca_token, alloca_type);
        }
        
        {
            // 
            // The `__noop` function needs to be intrinsic, as it's arguments are not evaluated.
            // 
            struct token *noop_token = push_dummy_token(arena, atom_for_string(string("__noop")), TOKEN_identifier);
            
            struct ast_function_type *noop_type = parser_type_push(context, noop_token, function_type);
            noop_type->return_type = &globals.typedef_s32;
            noop_type->flags |= FUNCTION_TYPE_FLAGS_is_varargs;
            
            register_intrinsic_function_declaration(context, noop_token, noop_type);
        }
        
        {
            // 
            // While the `__debugbreak` function does not _need_ to be intrinsic, we want it to be intrinsic so you do not have to include anything.
            // 
            struct token *debugbreak_token = push_dummy_token(arena, atom_for_string(string("__debugbreak")), TOKEN_identifier);
            
            struct ast_function_type *debugbreak_type = parser_type_push(context, debugbreak_token, function_type);
            debugbreak_type->return_type = &globals.typedef_void;
            
            register_intrinsic_function_declaration(context, debugbreak_token, debugbreak_type);
        }
        
        {   // 
            // void *_AddressOfReturnAddress(void)
            // 
            
            struct token *token = push_dummy_token(arena, atom_for_string(string("_AddressOfReturnAddress")), TOKEN_identifier);
            
            struct ast_function_type *type = parser_type_push(context, token, function_type);
            type->return_type = &void_pointer->base;
            
            register_intrinsic_function_declaration(context, token, type);
        }
        
        
        globals.empty_statement.kind = AST_empty_statement;
        set_resolved_type(&globals.empty_statement, &globals.typedef_void, null);
        
        globals.invalid_identifier_token = push_dummy_token(arena, globals.invalid_identifier, TOKEN_identifier);
        globals.guard_ast.resolved_type = &globals.typedef_void;
        
        struct declarator_return poison_declarator = {.ident = globals.invalid_identifier_token, .type = &globals.typedef_poison };
        globals.poison_declaration = push_declaration_for_declarator(context, poison_declarator);
        
        // 
        // Special _Linker_ declarations. @cleanup _ImageBase.
        // 
        
        struct declarator_return _tls_index_declarator = {
            .ident = push_dummy_token(arena, atom_for_string(string("_tls_index")), TOKEN_identifier),
            .type  = &globals.typedef_u32,
        };
        globals.tls_index_declaration = push_declaration_for_declarator(context, _tls_index_declarator);
        globals.tls_index_declaration->flags |= DECLARATION_FLAGS_is_global | DECLARATION_FLAGS_is_extern | DECLARATION_FLAGS_is_intrinsic;
        globals.tls_index_declaration->compilation_unit = &globals.hacky_global_compilation_unit;
        ast_table_add_or_return_previous_entry(&globals.global_declarations, &globals.tls_index_declaration->base, globals.tls_index_declaration->identifier);
    }
    
#if PRINT_ADDITIONAL_INCLUDE_DIRECTORIES
    if(!globals.cli_options.quiet){
        print("\nSystem include directories:\n");
        for(struct string_list_node *node = globals.system_include_directories.list.first; node; node = node->next){
            print("    %.*s\n", node->string.amount, node->string.data);
        }
        print("\n");
    }
#endif
    
#if 1
    {
        smm capacity = 0x8;
        smm count    = 0;
        
        struct system_include_file_entry *entries = push_data(arena, struct system_include_file_entry, capacity);
        
        for(struct string_list_node *node = globals.system_include_directories.list.first; node; node = node->next){
            
            struct file_path_entry{
                struct file_path_entry *next;
                struct string file_path;
            } initial_directory = {
                .file_path = node->string,
            };
            
            struct file_path_entry *directory_list = &initial_directory;
            
            while(directory_list){
                // 
                // Pop the path off of the 'directory_list'.
                // 
                struct string directory_path = directory_list->file_path;
                directory_list = directory_list->next;
                
                // 
                // Build the 'SearchBuffer' as "<directory_path>\*".
                // 
                char SearchBuffer[MAX_PATH];
                if(directory_path.size + 3 >= MAX_PATH){
                    // @cleanup:
                    print("WARNING: Directory path '%.*s' exceeds MAX_PATH. Ignoring it.\n", directory_path.size, directory_path.data);
                    continue;
                }
                
                memcpy(SearchBuffer, directory_path.data, directory_path.size);
                SearchBuffer[directory_path.size + 0] = '/';
                SearchBuffer[directory_path.size + 1] = '*';
                SearchBuffer[directory_path.size + 2] = 0;
                
                WIN32_FIND_DATAA FindData = {0};
                HANDLE FindHandle = FindFirstFileA(SearchBuffer, &FindData);
                if(FindHandle == INVALID_HANDLE_VALUE) continue; // I don't think this can happen.
                
                do{
                    char *FileName       = FindData.cFileName;
                    DWORD FileAttributes = FindData.dwFileAttributes;
                    
                    smm file_name_length = cstring_length(FileName);
                    
                    if(FileAttributes & /*FILE_ATTRIBUTE_DIRECTORY*/0x10){
                        if(cstring_match(FileName, ".") || cstring_match(FileName, "..")){
                            continue;
                        }
                        
                        // 
                        // If its a directory, allocate a new path and push it to the 'directory_list'.
                        // The new path is of the form <directory_path>\<FileName>
                        // 
                        
                        struct file_path_entry *new_directory_entry = push_struct(arena, struct file_path_entry);
                        new_directory_entry->file_path = push_format_string(arena, "%.*s/%.*s", directory_path.size, directory_path.data, file_name_length, FileName);
                        new_directory_entry->next = directory_list;
                        directory_list = new_directory_entry;
                        
                        continue;
                    }
                    
                    // 
                    // Check that the FileName ends in '.h'.
                    // 
                    // if((file_name_length < 2) || (FileName[file_name_length-1] != 'h') || (FileName[file_name_length-2] != '.')){ continue;}
                    
                    if(2 * count > capacity){
                        struct system_include_file_entry *new_entries = push_data(arena, struct system_include_file_entry, 2 * capacity);
                        
                        // Copy the hash table.
                        for(smm entry_index = 0; entry_index < capacity; entry_index++){
                            if(entries[entry_index].absolute_file_path == null) continue;
                            
                            smm hash = string_djb2_hash(entries[entry_index].include_string);
                            
                            for(smm table_index = 0; table_index < 2 * capacity; table_index++){
                                smm index = (hash + table_index) & (2 * capacity - 1);
                                
                                if(!new_entries[index].absolute_file_path){
                                    new_entries[index] = entries[entry_index];
                                    break;
                                }
                            }
                        }
                        
                        entries = new_entries;
                        capacity *= 2;
                    }
                    
                    struct string absolute_file_path = push_format_string(arena, "%.*s/%.*s", directory_path.size, directory_path.data, file_name_length, FileName);
                    struct string include_string     = create_string(absolute_file_path.data + (node->string.amount + 1), absolute_file_path.size - (node->string.amount + 1));
                    
                    hacky_canonicalize_file_for_case_insensitivity(&include_string);
                    
                    u64 hash = string_djb2_hash(include_string);
                    
                    for(smm table_index = 0; table_index < capacity; table_index++){
                        smm index = (hash + table_index) & (capacity - 1);
                        
                        if(!entries[index].absolute_file_path){
                            entries[index].include_string     = include_string;
                            entries[index].absolute_file_path = (char *)absolute_file_path.data;
                            entries[index].file_size          = ((smm)FindData.nFileSizeHigh << 32) | FindData.nFileSizeLow;
                            count++;
                            break;
                        }
                        
                        // @note: Prefer earlier entries over later ones.
                        if(string_match(entries[index].include_string, include_string)) break;
                    }
                    
                } while(FindNextFileA(FindHandle, &FindData));
                
                FindClose(FindHandle);
            }
        }
        
        globals.system_include_table.entries  = entries;
        globals.system_include_table.capacity = capacity;
        globals.system_include_table.count    = count;
    }
#endif
    
    smm thread_count = 1;
    if(globals.cli_options.thread_count_specified && globals.cli_options.MP){
        print("Error: Found both a /MT and a /thread_count %llu option.\n", globals.cli_options.thread_count);
        return 1;
    }
    
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info); // this cannot fail apperantly
    
    if(globals.cli_options.thread_count_specified){
        thread_count = globals.cli_options.thread_count;
        if(thread_count > 10 * system_info.dwNumberOfProcessors){
            print("Error: /thread_count option specifies a thread count of more than 10 times the number of processors on the system.\n");
            return 1;
        }
    }
    
    if(globals.cli_options.MP){
        // For not limit the amount of threads to the amount of compilation units.
        // This is maybe wrong, but right now it prevents some race conditions when compiling with CMAKE that require big refactors to fix.
        thread_count = (files_to_parse.amount < system_info.dwNumberOfProcessors) ? files_to_parse.amount : system_info.dwNumberOfProcessors;
    }
    
    if(globals.cli_options.EP || globals.cli_options.P){
        
        
        if(globals.cli_options.P){
            struct string output_file_name = globals.cli_options.Fi;
            
            if(!output_file_name.data){
                print("Error: @incomplete Currently you have to specify the file path using /Fi if you use /E.\n");
                return 1;
            }
            
            globals.preprocessed_file_handle = CreateFileA((char *)output_file_name.data, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, NULL);
        }else{
            globals.preprocessed_file_handle = GetStdHandle(STD_OUTPUT_HANDLE);
        }
        thread_count = 1;
    }
    
    // @note: thread '0' is the main thread
    struct thread_info *thread_infos = push_data(arena, struct thread_info, thread_count);
    globals.thread_infos = thread_infos;
    globals.thread_count = thread_count;
    
    thread_infos[0].context = context;
    
    for(u32 thread_index = 1; thread_index < thread_count; thread_index++){
        struct thread_info *thread_info = thread_infos + thread_index;
        thread_info->thread_index = thread_index;
    }
    
    init_context(context, thread_infos + 0, arena);
    
    stage_one_tokenize_and_preprocess_time = os_get_time_in_seconds();
    
    // 
    // get it going:
    // 
    
    struct compilation_unit *compilation_units = push_data(arena, struct compilation_unit, files_to_parse.amount);
    globals.compilation_units.first = compilation_units;
    globals.compilation_units.last  = compilation_units + files_to_parse.amount - 1;
    {
        smm index = 0;
        for(struct work_queue_entry *entry = files_to_parse.first; entry; entry = entry->next){
            
            struct compilation_unit *unit = compilation_units + index;
            unit->index = index++;
            unit->static_declaration_table = ast_table_create(128);
            unit->static_sleeper_table     = sleeper_table_create(1 << 8);
            unit->is_token_static_table.capacity = 0x100;
            unit->is_token_static_table.size = 0;
            unit->is_token_static_table.data = push_data(context->arena, struct is_token_static, 0x100);
            if(entry->next) unit->next = unit + 1;
            
            struct work_tokenize_file *work = entry->data;
            work->compilation_unit = unit;
        }
        
        assert(index == files_to_parse.amount);
    }
    
    end_counter(context, startup);
    
    //
    // COMPILE_STAGE_tokenize_files
    //
    
    // We _just_ pushed all the files into the work 'globals.work_queue_tokenize_files' and started the threads. 
    // Each file is a compilation unit. These 'compilation_units' first get tokenized and preprocessed,
    // then "chopped into pieces", such that we can parse each global scope entry individually.
    // Finally, these global scope entries get appended to the `work_queue_parse_global_scope_entries`.
    
    globals.compile_stage = COMPILE_STAGE_tokenize_files; // Needs to happen before we bring the threads online.
    
    work_queue_append_list(&globals.work_queue_tokenize_files, files_to_parse.first, files_to_parse.last, files_to_parse.amount);
    
    // @note: thread '0' is the main thread, so just start all other ones
    for(u32 thread_index = 1; thread_index < thread_count; thread_index++){
        os_create_thread(work_thread_proc, &thread_infos[thread_index]);
    }
    
    while(globals.work_queue_tokenize_files.work_entries_in_flight > 0) worker_work(context, &globals.work_queue_tokenize_files, worker_preprocess_file);
    
    // Ensure that all thread wrote in `thread_infos[thread_index].context`.
    for(u32 thread_index = 1; thread_index < thread_count; thread_index++){
        while(atomic_load(struct context *, thread_infos[thread_index].context) == null){};
    }
    
    assert(globals.work_queue_tokenize_files.work_entries_in_flight == 0);
    if(globals.an_error_has_occurred) goto end;
    
    // 
    // If no errors occurred, we want to report all warnings.
    // 
    print_warning_or_error_reports(context);
    
    
    
    
    // We have printed the preprocessed files in `worker_preprocess_file`
    if(globals.preprocessed_file_handle) return 0;
    
#if 0
    for(smm compilation_unit_index = 0; compilation_unit_index < globals.compilation_units.amount; compilation_unit_index++){
        struct compilation_unit *compilation_unit = &globals.compilation_units.data[compilation_unit_index];
        
        print("%s:\n", compilation_unit->main_file->absolute_file_path);
        for(u32 entry_index = 0; entry_index < compilation_unit->is_token_static_table.capacity; entry_index++){
            struct token *token = compilation_unit->is_token_static_table.data[entry_index].token;
            if(token){
                if(compilation_unit->is_token_static_table.data[entry_index].is_static){
                    print("    static ");
                }else{
                    print("    extern ");
                }
                print_token(context, token, /*print_whitespace_and_comments*/0);
                print("\n");
            }
        }
        print("\n");
    }
#endif
    
    // 
    // COMPILE_STAGE_parse_global_scope_entries
    // 
    
    // If the global scope entry is a function definition the body gets put into the work queue for the next stage.
    // If we encounter an identifier we do not know about yet, we add this global scope entry into the 'sleeper_table'
    // where it waits for the identifier to resolve.
    // In the end of this phase all types and global declarations are known.
    
    globals.compile_stage = COMPILE_STAGE_parse_global_scope_entries;
    
    while(globals.work_queue_parse_global_scope_entries.work_entries_in_flight > 0) worker_work(context, &globals.work_queue_parse_global_scope_entries, worker_parse_global_scope_entry);
    
    assert(globals.work_queue_parse_global_scope_entries.work_entries_in_flight == 0);
    if(globals.an_error_has_occurred) goto end;
    
    // #pragma comment(linker, "/ALTERNATENAME:strdup=_strdup").
    for(struct alternate_name *alternate_name = globals.alternate_names.first; alternate_name; alternate_name = alternate_name->next){
        struct ast_declaration *source = (struct ast_declaration *)ast_table_get(&globals.global_declarations, alternate_name->source);
        
        struct ast_declaration *destination = (struct ast_declaration *)ast_table_get(&globals.global_declarations, alternate_name->destination);
        if(warning_enabled[WARNING_ALTERNATENAME_type_mismatch] && source && destination){
            if(!types_are_equal(source->type, destination->type)){
                struct string source_type = push_type_string(context->arena, &context->scratch, source->type);
                struct string destination_type = push_type_string(context->arena, &context->scratch, destination->type);
                
                begin_error_report(context);
                report_warning(context, WARNING_ALTERNATENAME_type_mismatch, alternate_name->token, "/ALTERNATENAME source (%.*s) and destination (%.*s) have mismatching type.", alternate_name->source.size, alternate_name->source.data, alternate_name->destination.size, alternate_name->destination.data);
                report_warning(context, WARNING_ALTERNATENAME_type_mismatch, source->identifier, "Source has type '%.*s'.", source_type.size, source_type.data);
                report_warning(context, WARNING_ALTERNATENAME_type_mismatch, destination->identifier, "Destination has type '%.*s'.", destination_type.size, destination_type.data);
                end_error_report(context);
            }
        }
        
        if(destination && destination->assign_expr){
            begin_error_report(context);
            report_error(context, alternate_name->token, "/ALTERNATENAME currently does not support linking to defined identifiers.");
            report_error(context, get_initializer_token(destination), "Here, the destination '%.*s' was defined.", alternate_name->destination.size, alternate_name->destination.data);
            end_error_report(context);
            continue;
        }
    }
    
    struct string_list attempted_entry_point_string_list = {};
    
    if(!globals.cli_options.no_entry){
        
        // 
        // Try to determine the entry point, output file name and subsystem, 
        // from the functions that exist in the executable.
        // 
        static struct{
            enum output_file_type file_type;
            enum subsystem subsystem;
            struct string entry_point_name;
            char *pre_main_file;
            char *pre_main_file_no_args;
            char *pre_main_file_envp;
        } table[] = {
            // Table is in order of preference.
            { OUTPUT_FILE_exe, SUBSYSTEM_console, const_string("_start")},
            { OUTPUT_FILE_exe, SUBSYSTEM_console, const_string("main"),     "implicit/pre_main.c",    "implicit/pre_main_no_args.c",  "implicit/pre_main_envp.c"},
            { OUTPUT_FILE_exe, SUBSYSTEM_console, const_string("wmain"),    "implicit/pre_wmain.c",   "implicit/pre_wmain_no_args.c", "implicit/pre_wmain_envp.c"},
            { OUTPUT_FILE_exe, SUBSYSTEM_windows, const_string("WinMain"),  "implicit/pre_WinMain.c"  },  // These always need to have args I am pretty sure.
            { OUTPUT_FILE_exe, SUBSYSTEM_windows, const_string("wWinMain"), "implicit/pre_wWinMain.c" }, // These always need to have args I am pretty sure.
            
            { OUTPUT_FILE_dll, SUBSYSTEM_windows, const_string("DllMain") },
            
            { OUTPUT_FILE_efi, SUBSYSTEM_efi_application, const_string("_start") },
            { OUTPUT_FILE_efi, SUBSYSTEM_efi_application, const_string("efi_main") },
            { OUTPUT_FILE_efi, SUBSYSTEM_efi_application, const_string("EfiMain") },
        };
        
        for(u32 index = 0; index < array_count(table); index++){
            // Only check entry points, we are allowed to.
            if(globals.output_file_type != OUTPUT_FILE_unset && table[index].file_type != globals.output_file_type) continue;
            if(globals.subsystem != 0 && table[index].subsystem != globals.subsystem) continue;
            if(globals.cli_options.entry.data && !string_match(table[index].entry_point_name, globals.cli_options.entry)) continue;
            
            struct atom atom = atom_for_string(table[index].entry_point_name);
            
            if(attempted_entry_point_string_list.amount_of_strings){
                string_list_postfix(&attempted_entry_point_string_list, arena, string(", "));
            }
            string_list_postfix(&attempted_entry_point_string_list, arena, atom.string);
            
            
            if(!globals.cli_options.entry.data){
                struct ast_declaration *declaration = (struct ast_declaration *)ast_table_get(&globals.global_declarations, atom);
                if(!declaration || !declaration->assign_expr || declaration->type->kind != AST_function_type) continue;
                
                if(table[index].pre_main_file){
                    smm argument_count = ((struct ast_function_type *)declaration->type)->argument_list.count;
                    
                    char *pre_main_file = null;
                    switch(argument_count){
                        case 0: pre_main_file = table[index].pre_main_file_no_args; break;
                        case 2: pre_main_file = table[index].pre_main_file; break;
                        case 3: pre_main_file = table[index].pre_main_file_envp; break;
                        case 4: pre_main_file = table[index].pre_main_file; break;
                    }
                    
                    if(!pre_main_file){
                        // The final reported error will be a better error reporting than anything we could produce here.
                        pre_main_file = table[index].pre_main_file;
                    }
                    
                    // 
                    // We need to load the pre-main file.
                    // 
                    struct string premain_path = concatenate_file_paths(arena, strip_file_name(compiler_path), string_from_cstring(pre_main_file));
                    
                    struct os_file file = os_load_file((char *)premain_path.data, 0, 0);
                    if(file.file_does_not_exist){
                        print("Error: Implicit premain compilation unit was not found.\n");
                        print("       If this is intended you can use the '-no_premain'\n");
                        print("       command line option to squelch this warning.\n");
                        print("       This file is usually contained in: \n");
                        print("             \"<compiler-path>/%s\".", pre_main_file);
                        print("       @incomplete: The -no_premain option currently does not exist :)\n");
                        return 1;
                    }
                    
                    // 
                    // Allocate a new compilation unit.
                    // 
                    struct compilation_unit *compilation_unit = push_struct(arena, struct compilation_unit);
                    compilation_unit->index = globals.compilation_units.last->index + 1; // @cleanup: are we sure 'compilation_unit->last' exists?
                    compilation_unit->static_declaration_table = ast_table_create(32);
                    compilation_unit->static_sleeper_table = sleeper_table_create(32);
                    compilation_unit->is_token_static_table.capacity = 32;
                    compilation_unit->is_token_static_table.size = 0;
                    compilation_unit->is_token_static_table.data = push_data(arena, struct is_token_static, 32);
                    
                    globals.compilation_units.last->next = compilation_unit;
                    globals.compilation_units.last = compilation_unit;
                    
                    // 
                    // This feels somewhat dumb.
                    // 
                    struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
                    work->absolute_file_path = premain_path;
                    work->file_size = file.size;
                    work->compilation_unit = compilation_unit;
                    
                    struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
                    work_entry->data  = work;
                    
                    worker_preprocess_file(context, work_entry); // This preprocesses the file and then adds things into the work_queue below.
                    
                    // Parse the new file.
                    while(globals.work_queue_parse_global_scope_entries.work_entries_in_flight > 0) worker_work(context, &globals.work_queue_parse_global_scope_entries, worker_parse_global_scope_entry);
                    
                    atom = atom_for_string(string("_start"));
                }
            }
            
            globals.output_file_type = table[index].file_type;
            globals.subsystem        = table[index].subsystem;
            globals.entry_point_name = atom;
            break;
        }
        
        if(!globals.entry_point_name.string.data){
            if(globals.cli_options.entry.data){
                // The entry point was not a canonical one, but it was specified on the command line.
                globals.entry_point_name = atom_for_string(globals.cli_options.entry);
            }else if(globals.output_file_type == OUTPUT_FILE_dll){
                // For dlls it should be legal to not have an entry point.
                globals.cli_options.no_entry = 1;
            }else{
                // :DelayCouldNotFindEntryPoint
                // 
                // We cannot immediately report an error here, as we want to report undeclared identifiers and such.
                // We are only supposed to print this error after we made sure there are no other errors at global scope.
            }
        }
        
        // Default to .exe
        if(globals.output_file_type == OUTPUT_FILE_unset) globals.output_file_type = OUTPUT_FILE_exe;
    }
    
    // Infer the subsystem by the output file type.
    if(globals.subsystem == 0){
        switch(globals.output_file_type){
            case OUTPUT_FILE_obj: break;
            case OUTPUT_FILE_exe: globals.subsystem = SUBSYSTEM_console; break;
            case OUTPUT_FILE_dll: globals.subsystem = SUBSYSTEM_windows; break;
            case OUTPUT_FILE_efi: globals.subsystem = SUBSYSTEM_efi_application; break;
            default: invalid_code_path;
        }
    }
    
    
    //
    // At this point no more sleepers will get filled in.
    // So _if_ there are any, report them.
    // The later parse stage (COMPILE_STAGE_parse_function) should error directly if something is 
    // undeclared, as all global symbols are known.
    //
    begin_counter(context, unresolved_sleepers);
    
    report_errors_for_unresolved_sleepers(context);
    if(globals.an_error_has_occurred) goto end;
    
    end_counter(context, unresolved_sleepers);
    
    log_print("Phase 1 completed.\n");
    
    stage_one_tokenize_and_preprocess_time = os_get_time_in_seconds() - stage_one_tokenize_and_preprocess_time;
    
    //
    // COMPILE_STAGE_parse_function
    //
    
    // At this point we know about all global declarations and global types. Pick up a function and parse its body.
    // Here we can report errors _on the spot_ and do not have to sleep on undeclared identifiers.
    // We do not queue the functions to emit code in the next stage, as we want to eliminate dead functions.
    // MSVC makes this actually necessary, as a dead function might call a dll-import which is not linked against.
    
    globals.compile_stage = COMPILE_STAGE_parse_function;
    SetEvent(globals.wake_event);
    ResetEvent(globals.wake_event);
    
    log_print("start phase 2");
    
    stage_two_parse_functions_time = os_get_time_in_seconds();
    
    while(globals.work_queue_parse_functions.work_entries_in_flight > 0){
        worker_work(context, &globals.work_queue_parse_functions, worker_parse_function);
        assert(!context->should_sleep);
    }
    
    assert(globals.work_queue_parse_functions.work_entries_in_flight == 0);
    if(globals.an_error_has_occurred) goto end;
    
    // @cleanup: is there a good way here to assert that nothing is sleeping?
    
    stage_two_parse_functions_time = os_get_time_in_seconds() - stage_two_parse_functions_time;
    
    log_print("end phase 2");
    
    //
    // COMPILE_STAGE_emit_code
    //
    
    // We first have to figure out which functions are used. We do that by analyzing the call graph, 
    // which we have build while parsing. We walk this graph and queue all functions that are referenced
    // into the 'work_queue_stage_three'.
    // We then emit all of the code for these functions. At any point we could encounter a global identifier
    // which we have not yet emitted. In this case we emit a 'patch' which then gets filled in during  'print_coff'.
    
    log_print("start phase 3");
    
    stage_three_emit_code_time = os_get_time_in_seconds();
    
    // At this point all parsing is complete and there should not really be any errors anymore.
    
    globals.compile_stage = COMPILE_STAGE_emit_code;
    
    if(globals.output_file_type != OUTPUT_FILE_obj){
        // If we are not compiling to an object file, we should now be loading / parsing all of the libraries.
        // In theory, we _could_ parse them lazily.
        
        // @cleanup: If the library came from a #pragma comment(lib, "library") we should have the token.
        
        for(struct string_list_node *node = globals.specified_libraries.list.first; node; node = node->next){
            // @note: We always add the 'specified_libraries' as unique strings.
            //        In theory, there could be two paths to the same library, but whatever.
            struct string library = node->string;
            struct string full_library_path = zero_struct;
            
            if(path_is_absolute(library)){
                full_library_path = push_zero_terminated_string_copy(context->arena, library);
            }else{
                for(struct string_list_node *library_path_node = globals.library_paths.list.first; library_path_node;  library_path_node= library_path_node->next){
                    struct string file_path = concatenate_file_paths(context->arena, library_path_node->string, library);
                    struct os_file file = os_load_file((char *)file_path.data, 0, 0);
                    
                    if(!file.file_does_not_exist){
                        full_library_path = file_path;
                        break;
                    }
                }
                
                if(!full_library_path.data){
                    begin_error_report(context);
                    report_error(context, null, "Error: Could not find specified library '%.*s'.\n\n", library.size, library.data);
                    
                    report_error(context, null, "Library paths:\n");
                    for(struct string_list_node *library_path_node = globals.library_paths.list.first; library_path_node;  library_path_node= library_path_node->next){
                                report_error(context, null, "    %.*s\n", library_path_node->string.size, library_path_node->string.data);
                    }
                    report_error(context, null, "\n");
                    report_error(context, null, "You can add your own library search paths using /LIBPATH option.\n");
                    end_error_report(context);
                    globals.an_error_has_occurred = true;
                    continue;
                }
            }
            
            int parse_error = ar_parse_file(full_library_path, context->arena);
            if(parse_error){
                report_error(context, null, "Error: Failed to parse library '%.*s'.", full_library_path.size, full_library_path.data);
                globals.an_error_has_occurred = true;
            }
        }
        
        if(globals.an_error_has_occurred) goto end;
    }
    
    // compute reachability information    :only_emit_functions_that_are_reachable_from_main
    // 
    // Do a depth first search on the call graph to compute
    // what is reachable from 'entry_point'.        
    // 
    
    if(!globals.cli_options.no_entry){
        
        struct atom entry_point = globals.entry_point_name;
        
        if(!entry_point.data){
            // :DelayCouldNotFindEntryPoint
            // 
            // We have waited until here to report errors, because we wanted to possibly report undeclared identifiers 
            // which were responsible for no entry point.
            
            
            char *file_type;
            switch(globals.output_file_type){
                case OUTPUT_FILE_unset: file_type = "unspecified file type"; break;
                case OUTPUT_FILE_exe: file_type = "file type '.exe'"; break;
                case OUTPUT_FILE_dll: file_type = "file type '.dll'"; break;
                case OUTPUT_FILE_efi: file_type = "file type '.efi'"; break;
                default: file_type = "???"; break;
            }
            
            char *subsystem;
            switch((int)globals.subsystem){
                case 0: subsystem = "unspecified 'subsystem'"; break;
                case SUBSYSTEM_console: subsystem = "subsystem 'console'"; break;
                case SUBSYSTEM_windows: subsystem = "subsystem 'windows'"; break;
                case SUBSYSTEM_efi_application: subsystem = "subsystem 'efi application'"; break;
                default: subsystem = (char *)push_format_string(arena, "subystem %d", globals.subsystem).data; break;
            }
            
            begin_error_report(context);
            report_error(context, null, "Error: Could not find a canonical \"main\" function for %s and %s.", file_type, subsystem);
            
            if(attempted_entry_point_string_list.amount_of_strings == 0){
                report_error(context, null, "       Because of the combination of file types and subsystem we did not search for a canonical entry point.");
            }else{
                struct string attempted = string_list_flatten(attempted_entry_point_string_list, arena);
                report_error(context, null, "       Attempted to find the following: %.*s.", attempted.size, attempted.data);
            }
            report_error(context, null, "       You can either add a pre-main file from the '%.*s/implicit' directory as a compilation unit,", compiler_path.size, compiler_path.data);
            report_error(context, null, "       or you can specify an entry point for your program using the '/entry:<name>' command line option.");
            
            end_error_report(context);
            globals.an_error_has_occurred = 1;
            
            goto end;
        }
        
        globals.entry_point = get_entry_point_or_error(context);
        if(globals.an_error_has_occurred) goto end;
        
        // @note: do this manually instead of calling 'add_potential_entry_point' as we are in a later 
        //        compilation stage, and do not have to do it atomically.
        struct declaration_reference_node *entry_point_node = push_struct(arena, struct declaration_reference_node);
        entry_point_node->declaration = &globals.entry_point->as_decl;
        entry_point_node->token       = globals.entry_point->identifier;
        entry_point_node->next = globals.globally_referenced_declarations;
        globals.globally_referenced_declarations = entry_point_node;
    }
    
    if(globals.output_file_type == OUTPUT_FILE_obj){
        // 
        // For an .obj any external function is a potential entry point.
        // 
        
        for(u32 table_index = 0; table_index < globals.global_declarations.capacity; table_index++){
            struct ast *ast = globals.global_declarations.nodes[table_index].ast;
            if(!ast) continue;
            
            if(ast->kind == AST_function){    
                struct ast_function *function = (struct ast_function *)ast;
                assert(!(function->as_decl.flags & DECLARATION_FLAGS_is_static)); // These are 'global_declarations' they should not be static.
                
                // If the function is not defined, thats fine in an obj.
                // Don't add a function_node as otherwise it would get marked as referenced 
                // and hence we would emit a unresolved external, but we only want to do that,
                // if it is actually used which we do in the depth first search below.
                if(!function->scope) continue;
                
                assert(!(function->as_decl.flags & DECLARATION_FLAGS_is_intrinsic)); // Intrinsic functions should not have a scope.
                
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                
                struct declaration_reference_node *function_node = push_struct(arena, struct declaration_reference_node);
                function_node->declaration = &function->as_decl;
                function_node->next = globals.globally_referenced_declarations;
                
                globals.globally_referenced_declarations = function_node;
            }else if(ast->kind == AST_declaration){
                struct ast_declaration *decl = (struct ast_declaration *)ast;
                assert(!(decl->flags & DECLARATION_FLAGS_is_static)); // These are 'global_declarations' they should not be static.
                if(decl->flags & DECLARATION_FLAGS_is_enum_member) continue; // Don't add enum members.
                
                // Don't add reference nodes for explictly external declarations.
                if((decl->flags & (DECLARATION_FLAGS_is_extern | DECLARATION_FLAGS_is_dllimport)) && !decl->assign_expr) continue;
                
                struct declaration_reference_node *declaration_node = push_struct(arena, struct declaration_reference_node);
                declaration_node->declaration = decl;
                declaration_node->next = globals.globally_referenced_declarations;
                
                globals.globally_referenced_declarations = declaration_node;
            }
        }
    }
    
    struct temporary_memory temp = begin_temporary_memory(&context->scratch);
    
    begin_error_report(context);
    
    for(struct declaration_reference_node *entry = globals.globally_referenced_declarations; entry; entry = entry->next){
        
        struct ast_declaration *initial_declaration = entry->declaration;
        
        // 
        // @cleanup: This seems bad, we have to do here essentially the same stuff for
        //           root-level declarations, as we have to for all other declarations.
        //           So we should find a way to handle the 'initial_declaration' in the same way 
        //           as all the referenced declarations.
        // 
        
        if(initial_declaration->flags & DECLARATION_FLAGS_is_reachable_from_entry) continue;
        initial_declaration->flags |= DECLARATION_FLAGS_is_reachable_from_entry;
        
        if((initial_declaration->base.kind == AST_function) && initial_declaration->assign_expr){
            // Queue the entry point, if it is a defined function.
            work_queue_push_work(context, &globals.work_queue_emit_code, initial_declaration);
        }
        
        if((initial_declaration->base.kind == AST_declaration) && initial_declaration->assign_expr){
            // @cleanup: This should probably not happen on the main thread.
            //           Furthermore, we also want to report errors for initializers that are non-constant but not referenced.
            initial_declaration->memory_location = evaluate_static_initializer(context, initial_declaration);
        }
        
        // If this declaration does not reference any other declarations we are done.
        if(!initial_declaration->referenced_declarations.first) continue;
        
        struct{
            struct reachability_stack_node{
                struct reachability_stack_node *next;
                struct ast_declaration *declaration;
                struct declaration_reference_node *at;
            } *first, *last;
        } stack = zero_struct;
        
        // Push the initial_declaration as the root of our search onto the stack.
        struct reachability_stack_node initial_node = zero_struct;
        initial_node.declaration = initial_declaration;
        initial_node.at = initial_declaration->referenced_declarations.first;
        
        sll_push_front(stack, &initial_node);
        
        while(!sll_is_empty(stack)){
            struct reachability_stack_node *node = stack.first;
            
            if(!node->at){
                // recurse up
                sll_pop_front(stack);
            }else{
                // recurse down into 'node->at'
                struct ast_declaration *declaration = node->at->declaration;
                
                if(!(declaration->flags & DECLARATION_FLAGS_is_reachable_from_entry)){
                    declaration->flags |= DECLARATION_FLAGS_is_reachable_from_entry;
                    
                    if(declaration->base.kind == AST_function){
                        // 
                        // If the declaration is a defined function, queue it up.
                        // It might not be defined, as it might be a dllimport with missing `__declspec(dllimport)`.
                        // 
                        struct ast_function *function = (struct ast_function *)declaration;
                        
                        if(function->as_decl.flags & DECLARATION_FLAGS_is_intrinsic){
                            // 
                            // Don't do anything for intrinsic functions.
                            // 
                        }else if(!function->scope){
                            
                            if(globals.output_file_type != OUTPUT_FILE_obj){
                                
                                if(function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                                    report_error(context, function->identifier, "A referenced function marked '__declspec(dllexport)' must be defined."); // :Error
                                    report_error(context, node->at->token, "... Here the function was referenced.");
                                }else{
                                    lookup_declaration_in_libraries(context, &function->as_decl, node->at->token);
                                }
                            }
                        }else if(!(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm)){
                            work_queue_push_work(context, &globals.work_queue_emit_code, function);
                        }
                    }else{
                        
                        // @cleanup: Maybe this should also be done and reported for unreachable declarations.
                        if(type_is_array_of_unknown_size(declaration->type)){
                            // 
                            // From the c-spec:
                            // 
                            // "If at the end of the translation unit containing
                            // 
                            //     int i[];
                            // 
                            // the array i still has incomplete type, the implicit initializer causes it to have one element,
                            // which is set to zero on program startup."
                            // 
                            
                            report_warning(context, WARNING_array_of_unknown_size_never_filled_in, declaration->identifier, "Bounds for array of unknown size were never filled in. Assuming an array length of one.");
                            patch_array_size(context, (struct ast_array_type *)declaration->type, 1, declaration->identifier);
                        }
                        
                        if(declaration->assign_expr){
                            // @cleanup: This should probably not happen on the main thread.
                            //           Furthermore, we also want to report errors for initializers that are non-constant but not referenced.
                            declaration->memory_location = evaluate_static_initializer(context, declaration);
                        }
                        
                        if(globals.output_file_type != OUTPUT_FILE_obj){
                            
                            if(!declaration->assign_expr && (declaration->flags & (DECLARATION_FLAGS_is_extern | DECLARATION_FLAGS_is_dllimport))){
                                
                                lookup_declaration_in_libraries(context, declaration, node->at->token);
                            }
                        }
                    }
                    
                    if(declaration->referenced_declarations.first){
                        // note: we only want to recurse once into any given function.
                        struct reachability_stack_node *new_node = push_struct(&context->scratch, struct reachability_stack_node);
                        new_node->declaration = declaration;
                        new_node->at = declaration->referenced_declarations.first;
                        
                        sll_push_front(stack, new_node);
                    }
                }
                
                node->at = node->at->next;
            }
        }
    }
    
    end_temporary_memory(temp);
    end_error_report(context);
    
    if(context->error){
        globals.an_error_has_occurred = 1;
        goto end;
    }
    
    while(globals.work_queue_emit_code.work_entries_in_flight > 0) worker_work(context, &globals.work_queue_emit_code, worker_emit_code);
    assert(globals.work_queue_emit_code.work_entries_in_flight == 0);
    if(globals.an_error_has_occurred) goto end;
    
    
    // 
    // Evaluate the initializers for all unnamed global declarations (struct/array literals).
    // @cleanup: This could be threaded, is this really where we want to do this?
    // 
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for_ast_list(thread_context->global_struct_and_array_literals){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            decl->memory_location = evaluate_static_initializer(thread_context, decl);
        }
    }
    
    stage_three_emit_code_time = os_get_time_in_seconds() - stage_three_emit_code_time;
    
    //
    // stage four is not really a thing, we just report linker type errors 
    // 
    // @cleanup: does this have to happen after 'emit_code', it feels like it does not and it would be better 
    //           to do it earlier, in the case where emitting code takes a while. (E.g. when we get to optimization).
    // 
    
    if(globals.output_file_type != OUTPUT_FILE_obj){
        #if 0
        
        // @incomplete: Reintroduce these warnings:
        
        if(!(function->as_decl.flags & DECLARATION_FLAGS_is_reachable_from_entry)){
            if(function->scope){
                report_warning(context, WARNING_function_defined_but_unreachable, function->base.token, "Function was defined but unreachable.");
            }else{
                report_warning(context, WARNING_function_declared_but_never_defined, function->base.token, "Function was declared but never defined.");
            }
            continue;
        }
        
        if(!(decl->flags & (DECLARATION_FLAGS_is_extern | DECLARATION_FLAGS_is_dllimport))){
            report_warning(context, WARNING_unreachable_declaration, decl->base.token, "Discarding unreachable, global declaration.");
        }
        #endif
    }
    
    end_counter(context, report_error_for_undefined_functions_and_types);
    
    end:; 
    
    print_warning_or_error_reports(context);
    
    stage_four_linking = os_get_time_in_seconds();
    
    if(!globals.an_error_has_occurred){
        
        // 
        // At this point no error occurred, and we are all ready 
        // to actually assemble the output files.
        // 
        
        struct string output_file_path = globals.cli_options.out;
        {
            // 
            // Build the output file path.
            // 
            // There are a couple of cases for how the output path is determined.
            // 
            //    1) The user does not specify the output path. 
            //       In this case the output path is:
            //           C:/the/working/directory/first_source_file
            //       The resulting files are:
            //           C:/the/working/directory/first_source_file.exe
            //           C:/the/working/directory/first_source_file.pdb
            //           
            //    2) The user specified a relative file path including a file name.
            //       In this case we prepend the working directory.
            //       The resulting files are:
            //           C:/the/working/directory/user/specified.exe
            //           C:/the/working/directory/user/specified.pdb
            //       
            //    3) The user specified a relative file path ending in a directory.
            //       In this case we append the first source file again.
            //       
            //    4) Option 2) and 3) have an analogous version for full paths.
            // 
            
            if(path_is_relative(output_file_path)){
                output_file_path = concatenate_file_paths(arena, working_directory, output_file_path);
            }
            
            if(path_is_directory((char *)output_file_path.data)){
                struct string file_name = strip_file_extension(strip_file_path(((struct work_tokenize_file *)files_to_parse.first->data)->absolute_file_path));
                output_file_path = concatenate_file_paths(arena, output_file_path, file_name);
            }
            
            if(!get_file_extension(output_file_path).size){
                struct string file_extension = zero_struct;
                switch(globals.output_file_type){
                    case OUTPUT_FILE_exe: file_extension = string(".exe"); break;
                    case OUTPUT_FILE_dll: file_extension = string(".dll"); break;
                    case OUTPUT_FILE_obj: file_extension = string(".obj"); break;
                    case OUTPUT_FILE_efi: file_extension = string(".efi"); break;
                    invalid_default_case();
                }
                output_file_path = string_concatenate(arena, output_file_path, file_extension);
            }
        }
        
        // The emit arena is fixed size 4 gigs, so we can print an error, if we exceed it.
        struct memory_arena emit_arena = create_memory_arena(giga_bytes(4), 2.0f, mega_bytes(10));
        emit_arena.out_of_memory_string = "Error: Maximum executable file size exceeded. Cannot emit a valid executable file.\n"; // :Error
        
        if(globals.output_file_type == OUTPUT_FILE_obj){
            print_obj(output_file_path, &emit_arena, arena); 
        }else{
            print_coff(output_file_path, &emit_arena, arena); 
        }
    }
    
    stage_four_linking = os_get_time_in_seconds() - stage_four_linking;
    
    // signal all the threads to sleep, this right now is only here to time the threads
    globals.threads_should_exit = true;
    SetEvent(globals.wake_event);
    
    smm amount_of_lines_preprocessed = 0;
    smm amount_of_lines = 0;
    for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
        struct file *it = globals.file_table.data[file_index];
        if(!it) continue;
        
        amount_of_lines += it->lines;
        amount_of_lines_preprocessed += it->lines * it->amount_of_times_included;
    }
    
    if(globals.an_error_has_occurred){
        print("\nErrors detected, compilation stopped.\n");
        return 1;
    }
    
    f64 end_time = os_get_time_in_seconds();
    f64 time_in_seconds = (end_time - begin_time);
    if(!globals.cli_options.quiet) print("\nTotal Lines: %lld | Total Lines Preprocessed %lld | Time: %.3fs\n", amount_of_lines, amount_of_lines_preprocessed, time_in_seconds);
    
    f64 overhead = time_in_seconds - (stage_one_tokenize_and_preprocess_time + stage_two_parse_functions_time + stage_three_emit_code_time + stage_four_linking);
    if(!globals.cli_options.quiet) print("preprocessing %.3fs (%.3f%%) | compiling %.3fs (%.3f%%) | code gen %.3fs (%.3f%%) | linking %.3fs (%.3f%%) | overhead %.3fs (%.3f%%)\n", 
            stage_one_tokenize_and_preprocess_time,  100.0 * stage_one_tokenize_and_preprocess_time/time_in_seconds,
            stage_two_parse_functions_time,          100.0 * stage_two_parse_functions_time/time_in_seconds,
            stage_three_emit_code_time,              100.0 * stage_three_emit_code_time/time_in_seconds,
            stage_four_linking,                      100.0 * stage_four_linking/time_in_seconds,
            overhead,                                100.0 * overhead/time_in_seconds);
    
#if !time_perfomance
    (void)begin_cycle_time;
#else
    end_counter(context, total);
    u64 time_in_cycles = __rdtsc() - begin_cycle_time;
    
    // :timing :performance :counters
    
    for(u32 i = 0; i < thread_count; i++){
        struct timing_context timing_context = zero_struct;
        print("Thread %d:\n", i);
        collate_timing_info(&timing_context, thread_infos[i].context->timing_events);
        
        if(i == 0){
            collate_timing_info(&timing_context, print_coff_timing_wrapper.timing_events);
        }
        
        report_timing(&timing_context, time_in_cycles, time_in_seconds);
    }
    
    // dump_timing_data(&timing_context, "timing.dump");
#endif
    
}



