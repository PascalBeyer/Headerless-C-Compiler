
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

struct parse_work{
    struct token_array tokens;
    struct token *sleeping_ident; // the one who sleeps. can be null if we don't know yet. For error reporting
    struct compilation_unit *compilation_unit;
    
    struct ast_function *function;
    struct ast_function_type *function_type; // @note: we need this, as functions are now 'unique' but their parameters are determined by their type, and the thus can change between declaration and definition
};

enum work_description{
    WORK_invalid,
    
    WORK_tokenize_file,
    WORK_parse_global_scope_entry,
    WORK_parse_function_body,
    WORK_emit_code,
    
    WORK_count,
};

struct work_tokenize_file{
    struct string absolute_file_path;
    smm file_size;
    struct compilation_unit *compilation_unit;
};

struct work_queue_entry{
    struct work_queue_entry *next;
    enum work_description description;
    u32 pad;
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


// @cleanup: can we just put enum and union in here and it's free?
enum sleep_purpose{
    SLEEP_on_struct, // should be called sleep_on_compound
    SLEEP_on_decl,
};

struct sleeper_node{
    union{
        struct{
            u64 token_and_sleep_purpose;
            struct work_queue_entry *first_sleeper;
        };
        m128 mem;
    };
};

struct token *sleeper_node__get_token(struct sleeper_node *sleeper_node){
    return (struct token *)(sleeper_node->token_and_sleep_purpose & ~1);
}

enum sleep_purpose sleeper_node__get_sleep_purpose(struct sleeper_node *sleeper_node){
    return (enum sleep_purpose)(sleeper_node->token_and_sleep_purpose & 1);
}

struct sleeper_table{
    struct sleeper_node *nodes;
    u64 amount_of_nodes;
    u64 capacity;
    u64 mask;
    
    s64 is_locked_for_growing;
    s64 threads_in_flight;
};

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

//_____________________________________________________________________________________________________________________

struct file{
    struct os_file file;
    char *absolute_file_path;
    // struct string include_string; // 'asd.h' for 'C:/path/to/file/asd.h'
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
    u16 ordinal_hint;
    u8 *memory_location;
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
    
    u64 import_symbol_base;
    u64 amount_of_import_symbols;
    
    struct library_import_table_node{
        struct string string;
        struct dll_import_node *import_node;
    } *import_symbol_string_table;
    
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
    
    COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries,
    COMPILE_STAGE_parse_function,
    COMPILE_STAGE_emit_code,
    //COMPILE_STAGE_patch,
    
    COMPILE_STAGE_count,
};

enum output_file_type{
    OUTPUT_FILE_exe,
    OUTPUT_FILE_dll,
    OUTPUT_FILE_obj,
};

// :globals
static struct{
    
    // :options (don't change after initialization!)
    enum output_file_type output_file_type;
    b32 want_debug_information;
    
    b32 should_print_includes;
    b32 dynamic_base;
    b32 no_entry;
    b32 allow_dot_as_arrow;
    b32 dont_print_the_files_because_we_are_in_a_test_suite;
    smm seed_to_randomize_order_of_global_scope_entries;
    u64 image_base;
    
    enum pe_subsystem{
        PE_SUBSYSTEM_console = 3,
        PE_SUBSYSTEM_windows = 2,
        PE_SUBSYSTEM_efi_application = 10,
    } pe_subsystem;
    
    struct thread_info *thread_infos;
    smm thread_count;
    b32 threads_should_exit;
    
    struct atom entry_point_name;
    struct ast_function *entry_point;
    
    struct string_list additional_include_directories; // full paths
    struct string_list system_include_directories;     // full paths
    
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
    //           C:/the/woring/directory/user/specified.exe
    //           C:/the/woring/directory/user/specified.pdb
    //       
    //    3) The user specified a relative file path ending in a directory.
    //       In this case we append the first source file again.
    //       
    //    4) Option 2) and 3) have an analogus version for full paths.
    // 
    struct string output_file_path;
    
    // Needed to set 'is_system_include' on these files, so we don't report warnings for them.
    struct string intrinsics_path;
    struct string premain_path;
    
    struct{
        struct compilation_unit{
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
            
        } *data;
        smm amount;
    } compilation_units;
    
    struct token_array predefined_tokens;
    
    struct{
        struct dll_node *first;
        struct dll_node *last;
        smm amount;
    } dlls;
    
    struct{
        struct library_node *first;
        struct library_node *last;
        smm amount;
    } libraries;
    
    struct string_list library_paths;
    
    // A perfect hash table to lookup keywords.
    smm keyword_table_size;
    struct keyword_table_entry *keyword_table;
    
    // A perfect hash table to lookup preprocessor directives.
    smm directive_table_size;
    struct directive_table_entry *directive_table;
    
    
#define INTRINSIC_TABLE_CAPACITY 512
#define INTRINSIC_TABLE_MASK (INTRINSIC_TABLE_CAPACITY - 1)
    struct intrinsic_table{
        struct intrinsic_info{
            struct atom name;
            enum intrinsic_kind kind;
        } *nodes;
        debug_only(smm size;)
    } intrinsic_table;
    
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
    struct work_queue work_queue_stage_one;
    struct work_queue work_queue_stage_two;
    struct work_queue work_queue_stage_three;
    HANDLE wake_event;
    
    struct sleeper_table sleeper_table;
    
    struct ast_table global_declarations;
    struct ast_table compound_types;
    // struct ast_table type_defs; this has to be the same as global_declarations because '(asd)expr' would sleep on (asd) as an expression
    // 
    // constant global stuff
    struct file invalid_file;
    struct atom keyword_cdecl;
    struct atom keyword_stdcall;
    struct atom keyword_dllimport;
    struct atom keyword_dllexport;
    struct atom keyword_align;
    struct atom keyword_noreturn;
    struct atom keyword_selectany;
    struct atom keyword_intrin_type;
    
    // hlc declspecs
    struct atom keyword_inline_asm;
    struct atom keyword_packed;
    struct atom keyword_printlike;
    
    struct atom pragma_once;
    
    struct atom keyword__VA_ARGS__;
    
    struct atom unnamed_tag;
    struct atom unnamed_enum;
    struct atom invalid_identifier;
    
    // :hlc_extensions
    struct atom keyword_data;
    struct atom keyword_size;
    struct atom keyword_main;
    
    b32 have_absolute_patch;
    struct function_node *functions_that_are_referenced_by_global_scope_entries;
    
#define members_for_typedef(typedef)    \
struct token token_##typedef;       \
struct ast_type typedef_##typedef   \

    members_for_typedef(void);
    members_for_typedef(Bool);
    
    members_for_typedef(u8);
    members_for_typedef(u16);
    members_for_typedef(u32);
    members_for_typedef(u64);
    
    members_for_typedef(s8);
    members_for_typedef(s16);
    members_for_typedef(s32);
    members_for_typedef(s64);
    
    members_for_typedef(f32);
    members_for_typedef(f64);
    
    members_for_typedef(poison);
    
    // typedef_bool?
    struct ast_type *typedef_void_pointer;
    struct ast_type *typedef_u8_pointer;
    struct ast_type *typedef_s8_pointer;
    struct ast_type *typedef__m128;
    
    //
    // Invalid/Default values
    //
    struct ast_declaration zero_decl;
    struct ast empty_statement;
    struct ast guard_ast;
    struct token *invalid_identifier_token;
    struct token invalid_token;
    struct ast_declaration *poison_declaration;
    
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
    return (type->kind == AST_integer_type || type->kind == AST_float_type);
}

func b32 type_is_array_of_unknown_size(struct ast_type *type){
    if(type->kind == AST_array_type){
        struct ast_array_type *array = (struct ast_array_type *)type;
        if(array->is_of_unknown_size) return true;
    }
    return false;
}

//_____________________________________________________________________________________________________________________

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
        s64 value;
        struct token *token;
        struct token *macro_expansion_token;
        int should_skip_undefined_identifier;
    } static_if_evaluate_stack[1024];
    smm static_if_stack_at;
    
    struct token *macro_expansion_token;
    smm define_depth;
    
    
    // used to not report errors for example for '#if defined(_MSC_VER) && _MSC_VER > 1337'
    b32 static_if_evaluate_should_skip_undefined_identifier;
    
    //
    // parsing
    //
    
    // these members are now only for the parser
    struct token_array tokens;
    smm token_at; // :token_at_overshoot this does not have to be in bounds of the token array
    
#define MAX_STACK_USAGE kilo_bytes(512)
    u8 *low_stack_address;
    b32 in_lhs_expression;
    s64 ast_serializer;
    
    struct ast *ast_stack[1024];
    smm ast_stack_at;
    
    struct ast_scope *current_scope;
    struct ast_function *current_function; // also used in emit
    struct ast_switch *current_switch;
    struct compilation_unit *current_compilation_unit;
    
    struct token *in_inline_asm_function;
    b32 current_asm_flags;
    
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
    
    // An error occured, it need not be fatal, if it is fatal (no other errors in the 
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
    
    struct {
        struct inline_asm_function_argument *first;
        struct inline_asm_function_argument *last;
    } inline_asm_function_arguments;
    
    struct{
        struct ast_float_literal *first;
        struct ast_float_literal *last;
        smm amount_of_float_literals;
    } float_literals;
    
    struct{
        struct ast_string_literal *first;
        struct ast_string_literal *last;
        smm amount_of_strings;
    } string_literals;
    
    struct ast_list global_struct_and_array_literals; // Used for struct and array literals at global (or local persist) scope.
    
    struct ast_list local_functions;
    
    //
    // Patching
    //
    struct memory_arena patch_arena; // This is just a zero initialized arena.
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
    struct patch_node *patch_node = push_struct(&context->patch_arena, struct patch_node);
    patch_node->source = source_decl;
    patch_node->dest_declaration = dest_declaration;
    patch_node->location_offset_in_dest_declaration   = dest_offset;
    patch_node->location_offset_in_source_declaration = source_offset;
    patch_node->kind = kind;
    patch_node->rip_at = rip_at;
    
    if(kind == PATCH_absolute){
        assert(patch_node->location_offset_in_dest_declaration + 8 <= dest_declaration->type->size);
    }
    
    if(!globals.have_absolute_patch && kind == PATCH_absolute){
        globals.have_absolute_patch = true;
    }
    
    sll_push_back(context->local_patch_list, patch_node);
}

// @WARNING: this has to agree with the entry point queuing 
func void add_potential_entry_point(struct function_node *node){
    assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
    struct function_node *list;
    do{
        list = atomic_load(struct function_node *, globals.functions_that_are_referenced_by_global_scope_entries);
        node->next = list;
    }while(atomic_compare_and_swap(&globals.functions_that_are_referenced_by_global_scope_entries, node, list) != list);
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

func void pretty_print_token_array(struct context *context, struct token_array array){
    int depth = 0;
    for(smm i = 0; i < array.amount; i++){
        struct token *token = &array.data[i];
        print_token(context, token, 1);
        if(token->type == TOKEN_open_curly){
            depth++;
            print("\n%*s", depth * 4, "");
        }else if(token->type == TOKEN_closed_curly){
            depth--;
            print("\n%*s", depth * 4, "");
        }else if(token->type == TOKEN_semicolon){
            print("\n%*s", depth * 4, "");
        }else{
            print(" ");
        }
    }
} 

//_____________________________________________________________________________________________________________________
#include "preprocess.c"
//_____________________________________________________________________________________________________________________

func struct string push_type_string(struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type);

func void push_type_string__inner(struct string_list *list, struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type){
    switch(type->kind){
        case AST_void_type:
        case AST_float_type:
        case AST_integer_type:{
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
            struct ast_unresolved_type *unresolved = cast(struct ast_unresolved_type *)type;
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
            
            struct string array_postfix = push_format_string(scratch, "[%d]", arr->amount_of_elements);
            string_list_postfix(list, scratch, array_postfix);
            
            push_type_string__inner(list, arena, scratch, arr->element_type);
            
        }break;
        case AST_bitfield_type:{
            struct ast_bitfield_type *bitfield = cast(struct ast_bitfield_type *)type;
            push_type_string__inner(list, arena, scratch, bitfield->base_type);
            struct string width = push_format_string(scratch, ": %d", bitfield->width);
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
    struct ast_list_node *new = push_struct(arena, struct ast_list_node);
    if(list->last){
        list->last->next = new;
    }else{
        list->first = new;
    }
    new->value = ast;
    new->next = 0; // we could get rid of this
    list->last = new;
    
    list->count++;
}

//_____________________________________________________________________________________________________________________
// Work Queue

func void work_queue_append_list(struct work_queue *queue, struct work_queue_entry *list_begin, struct work_queue_entry *list_end, smm amount){
    // first update the 'work_entries_in_flight', so this is not temporary 0.(or in gerneral less, than it should)
    atomic_add(&queue->work_entries_in_flight, amount);
    
    struct work_queue_entry *end;
    
    while(true){
        end = atomic_load(struct work_queue_entry *, queue->end);
        if(!end){ // queue is empty
            struct work_queue mem;
            mem.begin = list_begin;
            mem.end = list_end;
            b32 success = atomic_compare_and_swap_128(&queue->mem, mem.mem, &(m128)zero_struct);
            if(success) goto end;
            continue;
        }
        if(atomic_compare_and_swap(&queue->end, list_end, end) == end) break;
    }
    
    // @cleanup: should this write be atomic?
    end->next = list_begin;
    
    end:;
    
    if(globals.wake_event){
        //log_print("wake up!");
        SetEvent(globals.wake_event);
        ResetEvent(globals.wake_event);
    }
}

func void work_queue_add(struct work_queue *queue, struct work_queue_entry *entry){
    if(queue == &globals.work_queue_stage_one){
        assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
    }
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

func void work_queue_push_work(struct context *context, struct work_queue *queue, enum work_description desc, void *data){
    if(context->should_sleep) return;
    assert(context->arena->temp_count == 0);
    struct work_queue_entry *work = push_struct(context->arena, struct work_queue_entry);
    work->description = desc;
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
                    if(!node->token_and_sleep_purpose) continue;
                    
                    struct token *token = sleeper_node__get_token(node);
                    
                    u64 slot = token->string_hash & table->mask;
                    for(u64 slot_offset = 0; slot_offset < table->capacity; slot_offset++){
                        struct sleeper_node *new_node = table->nodes + ((slot + slot_offset) & table->mask);
                        if(new_node->token_and_sleep_purpose) continue;
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

func void sleeper_table_add(struct sleeper_table *table, struct work_queue_entry *entry, enum sleep_purpose sleep_purpose, struct token *token){
    sleeper_table_maybe_grow(table);
    
    atomic_preincrement(&table->threads_in_flight);
    
    assert(token);
    assert(entry);
    
    assert(entry->description != WORK_invalid && entry->description < WORK_count);
    u64 index = token->string_hash & table->mask;
    assert(index < table->capacity);
    
    struct sleeper_node to_insert;
    to_insert.token_and_sleep_purpose = (u64)token | sleep_purpose;
    to_insert.first_sleeper = entry;
    entry->next = null;
    
    for(u32 i = 0; i < table->capacity; i++){
        struct sleeper_node *node = table->nodes + index;
        if(node->token_and_sleep_purpose){
            
            struct token *test_token = sleeper_node__get_token(node);
            enum sleep_purpose test_sleep_purpose = sleeper_node__get_sleep_purpose(node);
            
            if(atoms_match(test_token->atom, token->atom) && test_sleep_purpose == sleep_purpose){
                // add entry to the list 'node->first_sleeper' atomically
                struct work_queue_entry *list;
                do{
                    list = atomic_load(struct work_queue_entry*, node->first_sleeper);
                    if(!list){
                        // @note: first_sleeper == null means that the node has been "deleted".
                        // if we deleted the entry within the time it took to go to sleep, we requeue it
                        assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
                        work_queue_add(&globals.work_queue_stage_one, entry);
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

func struct work_queue_entry *sleeper_table_delete(struct sleeper_table *table, enum sleep_purpose sleep_purpose, struct token *token){
    sleeper_table_maybe_grow(table);
    atomic_preincrement(&table->threads_in_flight);
    
    assert(token);
    
    u64 index = token->string_hash & table->mask;
    assert(index < table->capacity);
    
    struct sleeper_node to_insert;
    to_insert.token_and_sleep_purpose = (u64)token | sleep_purpose;
    to_insert.first_sleeper = 0;
    
    for(u64 i = 0; i < table->capacity; i++){
        struct sleeper_node *node = table->nodes + index;
        if(!node->token_and_sleep_purpose){
            b32 success = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &(m128)zero_struct);
            if(success) {
                atomic_add((s64 *)&table->amount_of_nodes, 1);
                log_print("         added dummy!");
                
                atomic_predecrement(&table->threads_in_flight);
                return null;
            }
            else continue;
        }
        
        struct token *test_token = sleeper_node__get_token(node);
        enum sleep_purpose test_purpose = sleeper_node__get_sleep_purpose(node);
        
        if(atoms_match(test_token->atom, token->atom) && test_purpose == sleep_purpose){
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

func void wake_up_sleepers(struct context *context, struct token *sleep_on, enum sleep_purpose purpose){
    
    // @note: this logic needs to match the call to sleeper_table_add.
    struct sleeper_table *sleeper_table = &globals.sleeper_table;
    
    b32 is_static = compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(context->current_compilation_unit, sleep_on->atom) == IDENTIFIER_is_static;
    if(is_static) sleeper_table = &context->current_compilation_unit->static_sleeper_table;
    
    log_print("   waking sleepers for: %.*s", sleep_on->amount, sleep_on->data);
    
    assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
    
    struct work_queue_entry *first = sleeper_table_delete(sleeper_table, purpose, sleep_on);
    if(first){
        struct work_queue_entry *last = first;
        smm amount = 1;
        for(; last->next; last = last->next) amount++;
        
        log_print("   %d sleepers found", amount);
        work_queue_append_list(&globals.work_queue_stage_one, first, last, amount);
        for(struct work_queue_entry *it = first; it; it = it->next){
            log_print("      sleeper: %p", it);
        }
    }else{
        log_print("      no sleepers found.");
    }
}

//_____________________________________________________________________________________________________________________


func struct parse_work *push_parse_work(struct context *context, struct token_array tokens, struct compilation_unit *compilation_unit, struct ast_function *function){
    struct parse_work *parse_work = push_struct(context->arena, struct parse_work);
    parse_work->tokens = tokens;
    parse_work->compilation_unit = compilation_unit;
    parse_work->function = function;
    
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

func struct ast_declaration *lookup_declaration(struct context *context, struct compilation_unit *compilation_unit, struct atom ident){
    
    struct ast_declaration *lookup = null;
    
    for(struct ast_scope *scope = context->current_scope; scope; scope = scope->parent){
        
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
    struct ast_declaration *decl = lookup_declaration(context, compilation_unit, type_name->atom);
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
            report_error(context, decl->base.token, "... Here is the declaration.");
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

func void register_compound_member(struct context *context, struct ast_compound_type *compound, struct token *ident, struct ast_type *type, smm offset_in_type){
    
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
}

func void parser_register_declaration_in_local_scope_without_checking_for_redeclarations(struct context *context, struct ast_scope *scope, struct ast_declaration *decl){
    
    if(2 * scope->amount_of_declarations++ == scope->current_max_amount_of_declarations){
        u32 new_max = max_of(2 * scope->current_max_amount_of_declarations, 8);
        struct ast_declaration **new_table = push_data(context->arena, struct ast_declaration *, new_max);
        
        for(smm old_index = 0; old_index < scope->current_max_amount_of_declarations; old_index++){
            struct ast_declaration *old_entry = scope->declarations[old_index];
            if(!old_entry) continue;
            
            smm hash = old_entry->identifier->atom.string_hash;
            for(smm new_index = 0; new_index < new_max; new_index++){
                smm index = (new_index + hash) & (new_max - 1);
                if(!new_table[index]){
                    new_table[index] = old_entry;
                    break;
                }
            }
        }
        
        scope->current_max_amount_of_declarations = new_max;
        scope->declarations = new_table;
    }
    
    smm hash = decl->identifier->atom.string_hash;
    for(smm table_index = 0; table_index < scope->current_max_amount_of_declarations; table_index++){
        smm index = (table_index + hash) & (scope->current_max_amount_of_declarations - 1);
        
        if(!scope->declarations[index]){
            scope->declarations[index] = decl;
            break;
        }
    }
}

func struct ast_type *types_are_equal(struct ast_type *wanted, struct ast_type *given);

func struct ast_declaration *parser_register_declaration(struct context *context, struct ast_declaration *decl){
    if(context->should_sleep) return decl;
    struct ast_scope *scope = context->current_scope;
    
    if(scope){
        struct ast_declaration *redecl = lookup_declaration(context, context->current_compilation_unit, decl->identifier->atom);
        
        if(redecl){
            // :Error
            begin_error_report(context);
            report_warning(context, WARNING_shadowing, decl->base.token, "Declaration hides previous declaration.");
            report_warning(context, WARNING_shadowing, redecl->base.token, "... Here is the previous declaration.");
            end_error_report(context);
            //return decl;
        }
        
        parser_register_declaration_in_local_scope_without_checking_for_redeclarations(context, scope, decl);
    }else{
        // we are at global scope: register it globally
        
        struct ast_table *table = compilation_unit_get_declaration_table_for_ident(context->current_compilation_unit, decl->identifier->atom);
        struct ast_declaration *redecl = (struct ast_declaration *)ast_table_add_or_return_previous_entry(table, &decl->base, decl->base.token);
        
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
                    
                    patch_array_size(context, redecl_type, decl_type->amount_of_elements, redecl->base.token);
                }
                
                if(decl_type->is_of_unknown_size && !redecl_type->is_of_unknown_size){
                    patch_array_size(context, decl_type, redecl_type->amount_of_elements, decl->base.token);
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
                        report_error(context, decl->base.token, "[%d] Redeclaration of enum value as '%d'.", decl->compilation_unit->index, decl_lit->_s32);
                        report_error(context, redecl->base.token, "[%d] ... Here was the previous declaration of value '%d'.", redecl->compilation_unit->index, redecl_lit->_s32);
                        end_error_report(context);
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
                }
            }else if(redecl->base.kind == AST_typedef && decl->base.kind == AST_typedef){
                if(types_are_equal(redecl->type, decl->type)){
                    return redecl;
                }else{
                    struct string   decl_type = push_type_string(context->arena, &context->scratch,   decl->type);
                    struct string redecl_type = push_type_string(context->arena, &context->scratch, redecl->type);
                    
                    begin_error_report(context);
                    report_error(context, decl->base.token, "[%d] Redeclaration of typedef with mismatching type (%.*s vs %.*s).", decl->compilation_unit->index, decl_type.size, decl_type.data, redecl_type.size, redecl_type.data);
                    report_error(context, redecl->base.token, "[%d] ... Here is the previous typedef.", redecl->compilation_unit->index);
                    end_error_report(context);
                    return decl;
                }
            }else if(redecl->base.kind == AST_function && decl->base.kind == AST_function){
                struct ast_function *redecl_function = cast(struct ast_function *)redecl;
                struct ast_function *function = cast(struct ast_function *)decl;
                
                if(!types_are_equal(decl->type, redecl->type)){
                    begin_error_report(context);
                    // :Error maybe print compilation units
                    report_error(context, decl->base.token, "[%d] Redeclaration of function with different type.", decl->compilation_unit->index);
                    report_error(context, redecl->base.token, "[%d] ... Here is the previous declaration.", redecl->compilation_unit->index);
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
            
            begin_error_report(context);
            report_error(context, decl->base.token, "[%d] Redeclaration.", decl->compilation_unit->index);
            report_error(context, redecl->base.token, "[%d] ... Here is the previous declaration.", redecl->compilation_unit->index);
            end_error_report(context);
            return decl;
        }
        
        wake_up_sleepers(context, decl->identifier, SLEEP_on_decl);
    }
    return decl;
}

func void parser_register_definition(struct context *context, struct ast_declaration *decl, struct ast *initializer, struct ast_type *type){
    assert(!context->should_sleep);
    
    // @note: Used by functions and declarations as of -03.04.2021, 
    //        its either the initalizer or the scope.
    //        
    // assert(!context->current_scope);
    // 
    
    if(atomic_compare_and_swap(&decl->assign_expr, initializer, null) != null){
        // 
        // We failed to set the initializer.
        // 
        if(!(decl->flags & DECLARATION_FLAGS_is_selectany)){
            begin_error_report(context);
            report_error(context, initializer->token, "[%d] Redefinition of '%.*s'.", context->current_compilation_unit->index, decl->identifier->atom.size, decl->identifier->atom.data);
            report_error(context, decl->assign_expr->token, "[%d] ... Here is the previous definition.", decl->compilation_unit->index);
            end_error_report(context);
        }
    }else{
        // @note: Hard set the type here, we know that this is the definition, this is necessary for functions
        //        e.g: replace 'int _start(int some_wrong_name)' by 'int _start(int the_right_name)'.
        //        We also use this for arrays of unknown size.
        decl->type = type; 
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
            begin_error_report(context);
            report_warning(context, WARNING_shadowing, type->token, "Redeclaration of type.");
            report_warning(context, WARNING_shadowing, redecl->token, "... Here was the previous declaration.");
            end_error_report(context);
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
        // If we have same token we are the same type so we can.
        if(redecl->token == type->token) return;
        
        struct ast_compound_type *old = (struct ast_compound_type *)redecl;
        struct ast_compound_type *new = (struct ast_compound_type *)type;
        
        // 
        // Reallow declarations of the same type.
        // 
        if(types_are_equal((struct ast_type *)redecl, type)) return;
        
        begin_error_report(context);
        report_error(context, type->token, "[%d] Redeclaration of type.", new->compilation_unit->index);
        report_error(context, redecl->token, "[%d] ... Here was the previous declaration.", old->compilation_unit->index);
        end_error_report(context);
        return;
    }
    
    wake_up_sleepers(context, ident, SLEEP_on_struct);
    return;
}

func void parser_emit_memory_location(struct context *context, struct ast_declaration *decl){
    
    smm alignment = get_declaration_alignment(decl);
    smm size      = get_declaration_size(decl);
    
    context->current_emit_offset_of_rsp = cast(smm)align_up(context->current_emit_offset_of_rsp, alignment);
    context->current_emit_offset_of_rsp += size;
    
    if(context->current_emit_offset_of_rsp > max_s32){
        report_error(context, decl->identifier, "Too many local variables. At most '0x%llx' bytes of stack are allowed.", max_s32);
    }
    
    // :MemoryLocations We currently emit '[rbp - offset]' so we want to return the offset
    //                  AFTER it is incremented.
    decl->offset_on_stack = context->current_emit_offset_of_rsp;
}

//_____________________________________________________________________________________________________________________

// @cleanup: this should take an atom.
func struct intrinsic_info *lookup_intrinsic(struct atom name){
    u64 hash = name.string_hash;
    struct intrinsic_info *info;
    for(smm i = 0; i < INTRINSIC_TABLE_CAPACITY; i++){
        info = globals.intrinsic_table.nodes + ((hash + i) & INTRINSIC_TABLE_MASK);
        if(atoms_match(info->name, name)) break; // found it
        if(!info->name.data) break; // did not find it
    }
    if(!info->name.data) return null;
    return info;
}

func void register_intrinsic(struct atom name, enum intrinsic_kind kind){
    u64 hash = name.string_hash;
    
    struct intrinsic_info *info;
    for(u32 i = 0; i < INTRINSIC_TABLE_CAPACITY; i++){
        info = globals.intrinsic_table.nodes + ((hash + i) & INTRINSIC_TABLE_MASK);
        if(!info->name.data) break;
        assert(!atoms_match(info->name, name)); // make sure we don't add anyone twice
    }
    assert(!info->name.data);
    assert(globals.intrinsic_table.size < (INTRINSIC_TABLE_CAPACITY/2));
    debug_only(globals.intrinsic_table.size++);
    
    info->name = name;
    info->kind = kind;
}

//_____________________________________________________________________________________________________________________

#include "parse.c"

//_____________________________________________________________________________________________________________________

#include "emit_x64.c"

//_____________________________________________________________________________________________________________________



// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#archive-library-file-format
// 
// The ar (or archiver) file-format is used by .lib-files
// and combines multiple .obj-files into one .lib-file.
// 
// The file begins with the signature '!<arch>\n'.
// After this the file is a sequence of file sections.
// Each file section starts with a header which specifies the size 
// and name of the file section.
// The header is followed by the data of the file-section.
// 
// The first two or three file-sections are special.
// The first and second one are a symbol index and have the name '/'.
// The first one in big-endian, the second one in little-endian.
// The third section is optionally '//', the long name data.
// 
// returns '1' on error.
int ar_parse_file(char *file_name, struct memory_arena *arena){
    
    struct os_file file = load_file_into_arena(file_name, arena);
    
    if(file.file_does_not_exist){
        print("Error: Library '%s' does not exist.\n", file_name);
        return 1;
    }
    
    if(file.size < 8 || memcmp(file.data, "!<arch>\n", 8) != 0){
        print("Error: Library '%s' is not an archive file (.lib, .a, .ar).\n", file_name);
        return 1;
    }
    
    u8 *file_at  = file.data + 8;
    u8 *file_end = file.data + file.size;
    
    if(file_at == file_end){
        print("Warning: Library '%s' is empty.\n", file_name);
        return 0;
    }
    
    struct ar_file_header{
        // 
        // An ASCII file-identifier.
        // 
        u8 file_identifier[16];
        
        // 
        // The modification time in seconds, as ASCII-decimal. 
        // 
        u8 file_modification_timestamp[12];
        
        // 
        // Owner and group ID as ASCII-decimal.
        // 
        u8 owner_identifier[6];
        u8 group_identifier[6];
        
        // 
        // The file type and permissions as ASCII-octal.
        // 
        u8 file_mode[8];
        
        // 
        // The size of the file in bytes as ASCII-decimal.
        // 
        u8 file_size_in_bytes[10];
        
        // 
        // The characters '`\n`
        // 
        u8 ending_characters[2];
    };
    
    u8 *big_endian_symbol_index_base = null;
    u64 big_endian_symbol_index_size = 0;
    
    u8 *little_endian_symbol_index_base = null;
    u64 little_endian_symbol_index_size = 0;
    
    u8 *long_name_data_base = null;
    u64 long_name_data_size = 0;
    
    
    for(u32 file_header_index = 0; file_at + sizeof(struct ar_file_header) <= file_end; file_header_index += 1){
        struct ar_file_header *file_header = (void *)file_at;
        file_at += sizeof(struct ar_file_header);
        
        struct string file_identifier  = string_strip_whitespace((struct string){.data = file_header->file_identifier,    .size = sizeof(file_header->file_identifier)});
        struct string file_size_string = string_strip_whitespace((struct string){.data = file_header->file_size_in_bytes, .size = sizeof(file_header->file_size_in_bytes)});
        
        int parse_size_success = 1;
        u64 file_size = string_to_u64(file_size_string, &parse_size_success);
        if(!parse_size_success || file_size > file.size || file_at + file_size > file_end){
            print("Error: Failed to parse library '%s'.\n", file_name);
            return 1;
        }
        
        // 
        // Each data section has two-byte alignment.
        // 
        file_at += file_size + (file_size & 1);
        
        // 
        // @incomplete: Make sure all of this is also correct for System V archives.
        // 
        
        if(file_header_index == 0 && string_match(file_identifier, string("/"))){
            // The first file section should be the first linker member.
            big_endian_symbol_index_base = (u8 *)(file_header + 1);
            big_endian_symbol_index_size = file_size;
            continue;
        }
        
        if(file_header_index == 1 && string_match(file_identifier, string("/"))){
            // The first file section should be the second linker member.
            // This is a microsoft specific thing.
            little_endian_symbol_index_base = (u8 *)(file_header + 1);
            little_endian_symbol_index_size = file_size;
            continue;
        }
        
        if(string_match(file_identifier, string("//"))){
            // Optionally, the long name data member should be immediately after the headers.
            long_name_data_base = (u8 *)(file_header + 1);
            long_name_data_size = file_size;
        }
        
        break;
    }
    
    if(!little_endian_symbol_index_base){
        print("Error: Failed to parse library '%s', currently only Windows-style import libraries are supported.\n", file_name);
        return 1;
    }
    
    // 
    // The second linker member, or Microsoft specific symbol index, has the following layout:
    //     
    //     u32 amount_of_members;
    //     u32 member_offsets[amount_of_members];
    //     u32 amount_of_symbols;
    //     u16 symbol_member_indices[amount_of_symbols];
    //     char string_table[]; // 'amount_of_symbols' many zero-terminated strings.
    //     
    // The algorithm goes as follows:
    //     
    //     u32 symbol_index  = binary_search(string_table, <identifier>);
    //     u16 member_index  = symbol_member_indices[symbol_index];
    //     u32 member_offset = member_offsets[member_index - 1];
    //     
    //     struct ar_file_header *file_header = (void *)(file.data + member_offset);
    //     <parse the .obj or import-header>
    // 
    
    // Make sure the symbol index can contain the 'amount_of_members' and the string table is zero-terminated.
    if(little_endian_symbol_index_size < 4 || little_endian_symbol_index_base[little_endian_symbol_index_size-1] != 0){
        print("Error: Failed to parse library '%s'.\n", file_name);
        return 1;
    }
    
    u32 *symbol_index_at  = (u32 *)little_endian_symbol_index_base;
    u32 *symbol_index_end = (u32 *)(little_endian_symbol_index_base + little_endian_symbol_index_size);
    
    u32 amount_of_members = *symbol_index_at++;
    if(symbol_index_at + amount_of_members + 1 > symbol_index_end){
        print("Error: Failed to parse library '%s'.\n", file_name);
        return 1;
    }
    
    u32 *member_offsets = symbol_index_at;
    symbol_index_at += amount_of_members;
    
    u32 amount_of_symbols = *symbol_index_at++;
    if(amount_of_symbols == 0){
        print("Warning: Library '%s' does not define any symbols.\n", file_name);
        return 0;
    }
    
    u16 *symbol_member_indices = (u16 *)symbol_index_at;
    
    if(symbol_member_indices + amount_of_symbols >= (u16 *)symbol_index_end){ // @note: Equality so the string buffer is not empty.
        print("Error: Failed to parse library '%s'.\n", file_name);
        return 1;
    }
    
    u8 *string_buffer = (u8 *)(symbol_member_indices + amount_of_symbols);
    
    struct library_import_table_node *string_table = push_data(arena, struct library_import_table_node, 0);
    u64 amount_of_strings = 0;
    smm import_symbol_base = -1;
    
    for(u8 *it = string_buffer; it < (u8 *)symbol_index_end;){
        struct string string = cstring_to_string((char *)it);
        it += string.size + 1;
        
        if(string_front_match_eat(&string, "__imp_")){
            if(import_symbol_base == -1) import_symbol_base = amount_of_strings;
            push_struct(arena, struct library_import_table_node)->string = string;
        }
        
        amount_of_strings++;
    }
    
    if(amount_of_strings != (u64)amount_of_symbols){
        print("Error: Failed to parse library '%s'.\n", file_name);
        return 1;
    }
    
    u64 amount_of_import_symbols = push_data(arena, struct library_import_table_node, 0) - string_table;
    if(amount_of_import_symbols == 0){
        print("Warning: Library '%s' does not export any symbols.\n", file_name);
        return 0;
    }
    
    struct library_node *library_node = push_struct(arena, struct library_node);
    library_node->path = cstring_to_string(file_name);
    library_node->file = file;
    library_node->amount_of_members     = amount_of_members;
    library_node->member_offsets        = member_offsets;
    library_node->amount_of_symbols     = amount_of_symbols;
    library_node->symbol_member_indices = symbol_member_indices;
    library_node->import_symbol_string_table = string_table;
    library_node->amount_of_import_symbols   = amount_of_import_symbols;
    library_node->import_symbol_base         = import_symbol_base;
    
    // @cleanup: In the future, we could thread this part and then this would need to be atomic.
    //           This will also be true for '#pragma comment(lib, <...>)'.
    sll_push_back(globals.libraries, library_node);
    globals.libraries.amount += 1;
    return 0;
}

// returns '1', if found, else '0'.
struct dll_import_node *ar_lookup_symbol(struct memory_arena *arena, struct library_node *library_node, struct string identifier){
    
    // The algorithm goes as follows:
    //     
    //     u32 symbol_index  = binary_search(string_table, <identifier>);
    //     u16 member_index  = symbol_member_indices[symbol_index];
    //     u32 member_offset = member_offsets[member_index - 1];
    //     
    //     struct ar_file_header *file_header = (void *)(file.data + member_offset);
    //     <parse the .obj or import-header>
    
    // 
    // Binary search the 'import_string_table'.
    // 
    u32 symbol_index = 0;
    s64 import_symbol_index = -1;
    {
        
        s64 min = 0;
        s64 max = library_node->amount_of_import_symbols-1;
        while(max - min >= 0){
            s64 at = min + (max - min)/2;
            
            int compare_result = string_compare_lexically(identifier, library_node->import_symbol_string_table[at].string);
            if(compare_result == 0) {
                import_symbol_index = at;
                break;
            }
            
            if(compare_result < 0) max = at - 1;
            if(compare_result > 0) min = at + 1;
        }
        
        // Make sure we found it.
        if(import_symbol_index == -1) return null;
        
        // Check if we have looked this up previously.
        struct dll_import_node *dll_import_node = library_node->import_symbol_string_table[import_symbol_index].import_node;
        if(dll_import_node) return dll_import_node;
        
        symbol_index = (u32)(import_symbol_index + library_node->import_symbol_base);
    }
    
    assert(symbol_index < library_node->amount_of_symbols);
    // @note: These indices are one based for some stupid reason.
    u16 member_index = library_node->symbol_member_indices[symbol_index];
    if(member_index == 0 || member_index > library_node->amount_of_members){
        print("Warning: A parse error occured while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return null;
    }
    member_index -= 1;
    
    struct os_file file = library_node->file;
    
    struct ar_file_header{
        // 
        // An ASCII file-identifier.
        // 
        u8 file_identifier[16];
        
        // 
        // The modification time in seconds, as ASCII-decimal. 
        // 
        u8 file_modification_timestamp[12];
        
        // 
        // Owner and group ID as ASCII-decimal.
        // 
        u8 owner_identifier[6];
        u8 group_identifier[6];
        
        // 
        // The file type and permissions as ASCII-octal.
        // 
        u8 file_mode[8];
        
        // 
        // The size of the file in bytes as ASCII-decimal.
        // 
        u8 file_size_in_bytes[10];
        
        // 
        // The characters '`\n`
        // 
        u8 ending_characters[2];
    };
    
    u64 member_offset = (u64)library_node->member_offsets[member_index];
    if(member_offset + sizeof(struct ar_file_header) > file.size){
        print("Warning: A parse error occured while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return null;
    }
    
    struct ar_file_header *file_header = (void *)(file.data + member_offset);
    u8 *file_data = (u8 *)(file_header + 1);
    
    struct string file_size_string = string_strip_whitespace((struct string){.data = file_header->file_size_in_bytes, .size = sizeof(file_header->file_size_in_bytes)});
    int parse_size_success = 1;
    u64 file_size = string_to_u64(file_size_string, &parse_size_success);
    if(!parse_size_success || file_size > file.size || member_offset + file_size > file.size){
        print("Warning: A parse error occured while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return null;
    }
    
    struct ar_import_header{
        u16 signature_1;
        u16 signature_2;
        u16 version;
        u16 machine;
        u32 time_date_stamp;
        u32 size_of_data;
        u16 ordinal_hint;
        u16 type : 2;      // 0 - code, 1 - data, 2 - const
        u16 name_type : 3; // 0 - ordinal, 1 - name, 2 - noprefix, 3 - undecorated
        u16 reserved : 11;
    } *import_header = (void *)(file_header + 1);
    
    if((file_size <= sizeof(*import_header) + identifier.size + 1) || file_data[file_size-1] != 0){
        print("Warning: A parse error occured while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return null;
    }
    
    u8 *library_name = (u8 *)(import_header + 1) + identifier.size + 1;
    
    struct string dll_name = cstring_to_string((char *)library_name);
    
    struct dll_node *dll_node = globals.dlls.first;
    for(; dll_node; dll_node = dll_node->next){
        if(string_match(dll_node->name, dll_name)) break;
    }
    
    if(!dll_node){
        dll_node = push_struct(arena, struct dll_node);
        dll_node->name = dll_name;
        
        sll_push_back(globals.dlls, dll_node);
        globals.dlls.amount += 1;
    }
    
    // @note: We should not have to lookup the 'identifier' in the 'dll_node' as otherwise,
    //        we should have already been here and set the 'import_node' member of the
    //        lookup table.
    
    struct dll_import_node *import_node = push_struct(arena, struct dll_import_node);
    import_node->import_name  = identifier;
    import_node->ordinal_hint = import_header->ordinal_hint;
    
    sll_push_back(dll_node->import_list, import_node);
    dll_node->import_list.count += 1;
    
    library_node->import_symbol_string_table[import_symbol_index].import_node = import_node;
    
    return import_node;
}

#include "explain.c"

//_____________________________________________________________________________________________________________________


func b32 evaluate_static_address(struct context *context, struct ast *rhs, struct ast **out_patch_ast, smm *out_offset_in_rhs){
    
    struct ast_unary_op *address = cast(struct ast_unary_op *)rhs;
    rhs = address->operand;
    
    struct ast *patch_ast = null; // Either a 'AST_declartation' or 'AST_string_literal'.
    smm offset_in_rhs = 0;
    
    b32 should_loop = 1;
    while(should_loop)
    switch(rhs->kind){
        
        case AST_member:{
            struct ast_dot_or_arrow *member = (struct ast_dot_or_arrow *)rhs;
            offset_in_rhs += member->member->offset_in_type;
            rhs = member->lhs;
        }break;
        
        case AST_array_subscript:{
            struct ast_subscript *subscript = (struct ast_subscript *)rhs;
            struct ast *index = subscript->index;
            
            if(index->kind == AST_integer_literal){
                offset_in_rhs += integer_literal_as_u64(index) * subscript->base.resolved_type->size;
                rhs = subscript->lhs;
            }else{
                // @cleanup: error :Error
                report_error(context, rhs->token, "Expected a constant in array subscript, when evaluating global initializer.");
                return 1;
            }
        }break;
        
        case AST_unary_deref:{
            struct ast_unary_op *deref = (struct ast_unary_op *)rhs;
            if(deref->operand->kind == AST_implicit_address_conversion){
                struct ast_unary_op *address_conversion = (struct ast_unary_op *)deref->operand;
                rhs = address_conversion->operand;
            }else{
                goto cannot_evaluate_to_constant_address;
            }
        }break;
        
        case AST_identifier:{
            struct ast_identifier *identifier = (struct ast_identifier *)rhs;
            struct ast_declaration *decl = identifier->decl;
            
            if(!(decl->flags & (DECLARATION_FLAGS_is_global|DECLARATION_FLAGS_is_local_persist))){
                report_error(context, rhs->token, "Referencing non-constant variable in constant initializer.");
                return 1;
            }
            
            patch_ast = &identifier->decl->base;
            should_loop = false;
        }break;
        
        case AST_string_literal:{
            struct ast_string_literal *string_literal = (struct ast_string_literal *)rhs;
            
            // 
            // Mark this string literal as being used.
            sll_push_back(context->string_literals, string_literal);
            context->string_literals.amount_of_strings += 1;
            
            patch_ast = rhs;
            should_loop = false;
        }break;
        
        case AST_compound_literal:{
            struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)rhs;
            
            // 
            // Add this compound literal to the list of unnamed global declarations.
            ast_list_append(&context->global_struct_and_array_literals, context->arena, &compound_literal->base);
            
            patch_ast = &compound_literal->decl->base;
            should_loop = false;
        }break;
        
        default:{
            cannot_evaluate_to_constant_address:;
            report_error(context, rhs->token, "Cannot evaluate this expression to a constant address.");
            return 1;
        }break;
    }
    
    assert(patch_ast);
    
    *out_patch_ast     = patch_ast;
    *out_offset_in_rhs = offset_in_rhs;
    
    return 0;
}

// returns 1 on error, 0 on success
func b32 evaluate_static_initializer__internal(struct context *context, struct ast *assignment_ast, struct ast_declaration *patch_declaration, u8 *data, smm data_size, smm offset){
    
    assert(assignment_ast->kind == AST_assignment);
    struct ast_binary_op *assign = cast(struct ast_binary_op *)assignment_ast;
    
    {
        //
        // Evaluate the lhs to get the 'offset'.
        //
        
        struct ast *lhs = assign->lhs;
        
        int should_loop = 1;
        while(should_loop){
            switch(lhs->kind){
                case AST_member:{
                    struct ast_dot_or_arrow *member = (struct ast_dot_or_arrow *)lhs;
                    lhs = member->lhs;
                    offset += member->member->offset_in_type;
                }break;
                case AST_array_subscript:{
                    struct ast_subscript *subscript = (struct ast_subscript *)lhs;
                    struct ast *index = subscript->index;
                    
                    if(index->kind == AST_integer_literal){
                        offset += integer_literal_as_u64(index) * subscript->base.resolved_type->size;
                        lhs = subscript->lhs;
                    }else{
                        // @cleanup: error....
                        report_error(context, index->token, "Expected a constant in array designator while evaluating initializer list at compile time.");
                        return 1;
                    }
                }break;
                
                case AST_identifier:{
                    // We have arrived at the root node.
                    should_loop = 0;
                }break;
                
                default:{
                    report_internal_compiler_error(lhs->token, "Unexpected 'lhs' expression of initializer list.");
                    return 1;
                }break;
            }
        }
    }
    
    struct ast_type *lhs_type = assign->lhs->resolved_type;
    struct ast *rhs = assign->rhs;
    
    assert(offset + lhs_type->size <= data_size);
    
    retry_because_we_hit_a_pointer_to_64bit_cast:;
    
    switch(rhs->kind){
        case AST_integer_literal:{
            u64 value = integer_literal_as_u64(rhs);
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
        
        case AST_compound_literal:{
            struct ast_compound_literal *lit = cast(struct ast_compound_literal *)rhs;
            
            for_ast_list(lit->assignment_list){
                evaluate_static_initializer__internal(context, it->value, patch_declaration, data, data_size, offset);
            }
        }break;
        
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
            assert(!lhs_array->is_of_unknown_size);
            
            // This is the 
            //      char asd[] = "asd";
            //      u16  asd[] = L"asd";
            // case.
            
            assert(offset + str->value.size <= data_size); // Make sure at least the string fits.
            assert(str->value.size <= lhs_array->element_type->size * lhs_array->amount_of_elements);
            
            memcpy(data + offset, str->value.data, str->value.size);
            
            // @cleanup: should we memset the whole "upper" part?
            if(offset + str->value.size + lhs_array->element_type->size <= lhs_array->amount_of_elements){
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
            
            struct string file_data = rhs->token->string;
            assert(offset + file_data.size <= data_size); // Make sure it fits.
            
            memcpy(data + offset, file_data.data, file_data.size);
        }break;
        
        case AST_binary_plus: case AST_binary_minus:{
            struct ast_binary_op *binary = (struct ast_binary_op *)rhs;
            
            smm offset_lhs = 0;
            struct ast *patch_lhs = null;
            
            if(binary->lhs->kind == AST_integer_literal){
                offset_lhs = integer_literal_as_s64(binary->lhs);
            }else if(binary->lhs->kind == AST_implicit_address_conversion || binary->lhs->kind == AST_unary_address){
                int error = evaluate_static_address(context, binary->lhs, &patch_lhs, &offset_lhs);
                if(error) return 1;
            }else{
                goto initializer_non_const;
            }
            
            smm offset_rhs = 0;
            struct ast *patch_rhs = null;
            
            if(binary->rhs->kind == AST_integer_literal){
                offset_rhs = integer_literal_as_s64(binary->rhs);
            }else if(binary->rhs->kind == AST_implicit_address_conversion || binary->rhs->kind == AST_unary_address){
                int error = evaluate_static_address(context, binary->rhs, &patch_rhs, &offset_rhs);
                if(error) return 1;
            }else{
                goto initializer_non_const;
            }
            
            smm offset_in_rhs = rhs->kind == AST_binary_plus ? offset_lhs + offset_rhs : offset_lhs - offset_rhs;
            
            if(patch_lhs == patch_rhs){
                // @note: This includes the case where 'patch_lhs == patch_rhs == null'.
                memcpy(data + offset, &offset, lhs_type->size);
            }else if(patch_lhs && patch_rhs){
                goto initializer_non_const;
            }else{
                struct ast *patch = patch_lhs ? patch_lhs : patch_rhs;
                emit_patch(context, PATCH_absolute, patch, offset_in_rhs, patch_declaration, offset, -1);
            }
        }break;
        
        case AST_implicit_address_conversion: case AST_unary_address:{
            
            struct ast *patch_ast = null; // Either a 'AST_declartation' or 'AST_string_literal'.
            smm offset_in_rhs = 0;
            
            int error = evaluate_static_address(context, rhs, &patch_ast, &offset_in_rhs);
            if(error) return 1;
            
            emit_patch(context, PATCH_absolute, patch_ast, offset_in_rhs, patch_declaration, offset, -1);
        }break;
        
        case AST_cast:{
            //
            // We should only get here for casts of a pointer to an integer/pointer, 
            // everything else should have been constant propagated, or is not constant.
            // 
            // @note: Technically, (currently) this is not true. 
            //        There can be expressions like '&x[1] - &x[2]', which are constant.
            //        But I think if we ever make these work, then in the parser, 
            //        by knowing the original declaration for 'x' and then const-prop'ing it.
            //
            
            struct ast_unary_op *cast = cast(struct ast_unary_op *)rhs;
            
            if(cast->operand->resolved_type->kind == AST_pointer_type){
                if(cast->base.resolved_type->size == 8){
                    rhs = cast->operand;
                    goto retry_because_we_hit_a_pointer_to_64bit_cast;
                }
                
                report_error(context, cast->base.token, "Cannot truncate a pointer at compile time.");
            }else{
                goto initializer_non_const;
            }
        }break;
        
        default:{
            initializer_non_const:
            report_error(context, assign->base.token, "Initializer is not a constant.");
            return 1;
        }break;
    }
    
    return 0;
}

func u8 *evaluate_static_initializer(struct context *context, struct ast_declaration *decl){
    
    struct ast *initializer = decl->assign_expr;
    
    smm data_size = decl->type->size;
    
    struct ast_list assignment_list = zero_struct;
    
    if(initializer->kind == AST_compound_literal){
        struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)initializer;
        assignment_list = compound_literal->assignment_list;
        data_size += compound_literal->trailing_array_size;
    }else{
        ast_list_append(&assignment_list, &context->scratch, initializer);
    }
    
    push_zero_align(context->arena, decl->type->alignment); // @cleanup: Do we need this? Also get_declaration_alignment.
    u8 *data = push_uninitialized_data(context->arena, u8, data_size); // @note: This will be zero-initialized, as we never free from arena.
    
    for_ast_list(assignment_list){
        evaluate_static_initializer__internal(context, it->value, decl, data, data_size, 0);
    }
    
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

// @cleanup: rename
func void parser_do_work(struct context *context, struct work_queue_entry *work){
    // :reset_context :zero_context :clear_context
    context->current_emit_offset_of_rsp = 0;
    context->current_scope              = null;
    context->current_switch             = null;
    context->sleep_on                   = null;
    context->sleeping_ident             = null;
    //context->spill_allocator          = 0; zeroed before every statement anyway
    context->ast_stack_at               = 0;
    context->should_sleep               = false;
    context->should_exit_statement      = false;
    context->error                      = false;
    context->current_compilation_unit   = null;
    context->maybe_in_cast              = null;
    
    switch(work->description){
        case WORK_tokenize_file:{
            assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
            
            struct work_tokenize_file *work_tokenize_file = cast(struct work_tokenize_file *)work->data;
            context->current_compilation_unit = work_tokenize_file->compilation_unit;
            
            log_print("tokenizing file %.*s", work_tokenize_file->absolute_file_path.amount, work_tokenize_file->absolute_file_path.data);
            
            {
                memset(&context->define_table, 0, sizeof(context->define_table));
                
                // 
                // @WARNING: If you ever change the fact, that these buildin defines have the defined token
                //           '&globals.invalid_token', you need to update the code which checks that you
                //           cannot redeclare these.
                // 
                
                struct define_node *defined_builtin = push_struct(&context->scratch, struct define_node);
                defined_builtin->name = atom_for_string(string("defined"));
                defined_builtin->is_defined = 1;
                defined_builtin->is_builtin = 1;
                defined_builtin->defined_token = &globals.invalid_token;
                register_define(context, defined_builtin);
                
                struct define_node *defined___pragma = push_struct(&context->scratch, struct define_node);
                defined___pragma->name = atom_for_string(string("__pragma"));
                defined___pragma->is___pragma = 1;
                defined___pragma->is_builtin  = 1;
                defined___pragma->defined_token = &globals.invalid_token;
                register_define(context, defined___pragma);
                
                struct define_node *defined___FILE__ = push_struct(&context->scratch, struct define_node);
                defined___FILE__->name = atom_for_string(string("__FILE__"));
                defined___FILE__->is___FILE__ = 1;
                defined___FILE__->is_builtin  = 1;
                defined___FILE__->defined_token = &globals.invalid_token;
                register_define(context, defined___FILE__);
                
                struct define_node *defined___LINE__ = push_struct(&context->scratch, struct define_node);
                defined___LINE__->name = atom_for_string(string("__LINE__"));
                defined___LINE__->is___LINE__ = 1;
                defined___LINE__->is_builtin  = 1;
                defined___LINE__->defined_token = &globals.invalid_token;
                register_define(context, defined___LINE__);
                
            }
            sll_clear(context->pragma_once_file_list);
            
            begin_counter(context, tokenize_and_preprocess);
            struct token_array tokenized_file = file_tokenize_and_preprocess(context, work_tokenize_file->absolute_file_path, work_tokenize_file->file_size);
            end_counter(context, tokenize_and_preprocess);
            
            if(context->should_sleep) goto handle_sleep_or_error;
            if(tokenized_file.amount == 0) return; // nothing to do if the file is empty
            
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
                
                smm start_marker = context->token_at;
                
                b32 is_static = false;
                b32 is_extern = false;
                b32 is_inline = false;
                b32 got_type  = false;
                
                struct token *got_identifier = null;
                
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
                                
                                // @cleanup: warn and shit
                                if(atoms_match(token->atom, is_static_table[index].token->atom)){
                                    is_static_table[index].is_static |= actually_static;
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
                    }
                }
                
                if(context->should_sleep || context->error) goto handle_sleep_or_error;
                
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
                entry->description = WORK_parse_global_scope_entry;
                entry->data = parse_work;
                
                sll_push_back(work_queue_entries, entry);
                work_queue_entries.amount++;
            }
            
            end_counter(context, chunking);
            
            if(globals.seed_to_randomize_order_of_global_scope_entries){
                struct random_series series =
                        random_series_from_seed(globals.seed_to_randomize_order_of_global_scope_entries);
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
            
            work_queue_append_list(&globals.work_queue_stage_one, work_queue_entries.first, work_queue_entries.last, work_queue_entries.amount);
            
            assert(!in_current_token_array(context));
        }break;
        case WORK_parse_global_scope_entry:{
            log_print("parsing a global scope entry");
            
            assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
            
            struct parse_work *parse_work = (struct parse_work *)work->data;
            context->current_compilation_unit = parse_work->compilation_unit;
            
            begin_token_array(context, parse_work->tokens);
            
            if(peek_token_eat(context, TOKEN_semicolon)){
                // allow for empty ';' at global scope @speed, we could detect this above
                assert(!in_current_token_array(context));
                return;
            }
            
            if(peek_token(context, TOKEN_static_assert)){
                struct token *static_assert_token = next_token(context);
                parse_static_assert(context, static_assert_token);
                if(context->error) goto handle_sleep_or_error;
                return;
            }
            
            struct declaration_list declaration_list = parse_declaration_list(context, null, null);
            if(context->should_sleep || context->error) goto handle_sleep_or_error;
            
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
            
            for(struct declaration_node *node = declaration_list.first; node; node = node->next){
                
                // @bug @incomplete @cleanup: This is apperantly where we evaluate initializers.
                //                            This is not thread safe.
                //                            Maybe this should happen in the backend anyway?
                
                struct ast_declaration *decl = node->decl;
                if(decl->base.kind == AST_declaration){
                    //
                    // @note: if we have 'int a = 1;' and then later (or in a different compilation unit) 'int a;'
                    //        we have already evaluated the initializer and should not do it again!
                    //
                    if(decl->assign_expr && !decl->memory_location){
                        decl->memory_location = evaluate_static_initializer(context, decl);
                    }
                }
            }
            
            if(declaration_list.last->decl->base.kind == AST_function && peek_token_eat(context, TOKEN_open_curly)){
                struct ast_function *function = cast(struct ast_function *)declaration_list.last->decl;
                parse_work->tokens.data += context->token_at;
                parse_work->tokens.size -= context->token_at;
                assert(parse_work->tokens.size > 0);
                
                parse_work->function = function;
                
                work_queue_push_work(context, &globals.work_queue_stage_two, WORK_parse_function_body, parse_work);
            }else{
                expect_token(context, TOKEN_semicolon, "Expected ';' after declaration at global scope.");
                if(context->error) goto handle_sleep_or_error;
                assert(!in_current_token_array(context));
            }
            
            return;
        }break;
        case WORK_parse_function_body:{ // @cleanup: these names should probably agree
            assert(globals.compile_stage == COMPILE_STAGE_parse_function);
            
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
            if(context->should_sleep) goto handle_sleep_or_error;
            
            for_ast_list(function->type->argument_list){
                assert(it->value->kind == AST_declaration);
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                if(maybe_resolve_unresolved_type_or_sleep_or_error(context, &decl->type)){
                    goto handle_sleep_or_error;
                }
                
                parser_register_declaration(context, decl);
            }
            
            if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm){
                //
                // Special case for __declspec(inline_asm) in this case we only want a single 'ast_asm_block'
                //
                
                struct ast_asm_block *asm_block = parser_ast_push(context, function->scope->token, asm_block);
                set_resolved_type(&asm_block->base, &globals.typedef_void, null);
                
                context->in_inline_asm_function = function->base.token;
                parse_asm_block(context, asm_block);
                context->in_inline_asm_function = null;
                
                if(function->type->return_type != &globals.typedef_void){
                    if(asm_block->instructions.last == null || asm_block->instructions.last->memonic != MEMONIC_return_from_inline_asm_function){
                        struct string type_string = push_type_string(&context->scratch, &context->scratch, function->type->return_type);
                        report_error(context, function->base.token, "__declspec(inline_asm)-function has return type '%.*s' but no return statement was found.", type_string.size, type_string.data);
                    }
                }
                
                ast_list_append(&scope->statement_list, context->arena, &asm_block->base);
            }else{
                //
                // Parse the function!
                //
                parse_imperative_scope(context);
            }
            
            if(context->should_sleep || context->error) goto handle_sleep_or_error;
            
            assert(!in_current_token_array(context));
            
            parser_scope_pop(context, scope);
            
            for(struct ast_list_node *node1 = function->goto_list.first; node1; node1 = node1->next){
                struct ast_goto *ast_goto = cast(struct ast_goto *)node1->value;
                b32 found = false;
                for(struct ast_list_node *node2 = function->label_list.first; node2; node2 = node2->next){
                    struct ast_label *label = cast(struct ast_label *)node2->value;
                    if(atoms_match(label->ident, ast_goto->ident)){
                        ast_goto->label_to_goto = label;
                        found = true;
                        break;
                    }
                }
                
                if(!found){
                    report_error(context, ast_goto->base.token, "'goto' to undefined label '%.*s'.", ast_goto->ident.amount, ast_goto->ident.data);
                    goto handle_sleep_or_error;
                }
            }
            function->stack_space_needed = context->current_emit_offset_of_rsp;
            
            if(atoms_match(function->identifier->atom, globals.keyword_main)){
                // "If the return type of the 'main' function is a type compatible with int, [...]
                //  reaching the } that terminates the main function returns a value of 0."
                if(function->type->return_type == &globals.typedef_s32){
                    
                    // @cleanup: is this the correct token?
                    struct token *end_curly = get_current_token_for_error_report(context);
                    struct ast_return *ast_return = parser_ast_push(context, end_curly, return);
                    ast_return->expr = ast_push_s32_literal(context, end_curly, 0);
                    set_resolved_type(&ast_return->base, &globals.typedef_void, null);
                    
                    ast_list_append(&scope->statement_list, context->arena, &ast_return->base);
                }
            }
            
            context->current_function = null;
        }break;
        case WORK_emit_code:{
            struct ast_function *function = (void *)work->data;
            assert(function->base.kind == AST_function);
            assert(!function->memory_location);
            assert(globals.compile_stage == COMPILE_STAGE_emit_code);
            
            emit_code_for_function(context, function);
            
            for_ast_list(function->static_variables){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                assert(!decl->memory_location);
                if(decl->assign_expr){
                    decl->memory_location = evaluate_static_initializer(context, decl);
                }
            }
        }break;
        invalid_default_case();
    }
    
    return;
    
    handle_sleep_or_error:;
    if(context->error) return;
    
    if(globals.an_error_has_occurred) return; // @cleanup: is this what we want? when does this get set?
    
    struct token *sleep_on = context->sleep_on;
    work->sleeping_on = context->sleep_on;
    log_print("   sleeping on: %.*s", sleep_on->amount, sleep_on->data);
    
    // @cleanup: inline this so we don't call sleeper_table_add somewhere else
    if(work->description == WORK_parse_global_scope_entry){
        struct parse_work *parse = cast(struct parse_work *)work->data;
        parse->sleeping_ident = context->sleeping_ident;
        assert(!parse->sleeping_ident || parse->sleeping_ident->type == TOKEN_identifier);
        
    }
    
    // @note: this logic needs to match wake_up_sleepers
    struct sleeper_table *sleeper_table = &globals.sleeper_table;
    
    if(context->sleep_purpose == SLEEP_on_decl){
        b32 is_static = compilation_unit_is_static_table_lookup_whether_this_identifier_is_static(context->current_compilation_unit, sleep_on->atom) == IDENTIFIER_is_static;
        if(is_static) sleeper_table = &context->current_compilation_unit->static_sleeper_table;
    }
    
    sleeper_table_add(sleeper_table, work, context->sleep_purpose, sleep_on);
}

func struct token *push_dummy_token(struct memory_arena *arena, struct atom token_atom, enum token_type token_type){
    
    struct token *token = push_struct(arena, struct token);
    token->type   = token_type;
    token->atom   = token_atom;
    token->file_index = -1; // invalid file index.
    token->line   = 1;
    token->column = 1;
    
    return token;
}


func void init_context(struct context *context, struct thread_info *info, struct memory_arena *arena){
    u64 thread_index = cast(u64)info->thread_index;
    
    context->low_stack_address = &(u8){0};
    
    context->thread_info = info;
    context->arena = arena;
    context->ast_serializer = (thread_index << 48);
    context->register_sp = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_SP, 8);
    context->gpr_allocator.emit_location_map[REGISTER_SP] = null;
    context->register_bp = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_BP, 8);
    context->gpr_allocator.emit_location_map[REGISTER_BP] = null;
    
    smm emit_pool_capacity = mega_bytes(100);
    struct os_virtual_buffer emit_pool_buf = os_reserve_memory(0, emit_pool_capacity);
    if(!emit_pool_buf.memory){ print("memory error!\n"); os_panic(1); }
    context->emit_pool.base     = emit_pool_buf.memory;
    context->emit_pool.current  = emit_pool_buf.memory;
    context->emit_pool.end      = emit_pool_buf.memory;
    context->emit_pool.reserved = emit_pool_capacity;
    
}

// returns whether or not we should continue;
func b32 do_one_work(struct context *context){
    struct work_queue *queue;
    
    switch(globals.compile_stage){
        case COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries:{
            queue = &globals.work_queue_stage_one;
        }break;
        case COMPILE_STAGE_parse_function:{
            queue = &globals.work_queue_stage_two;
        }break;
        case COMPILE_STAGE_emit_code:{
            queue = &globals.work_queue_stage_three;
        }break;
        invalid_default_case(queue = null);
    }
    
    struct work_queue_entry *work = work_queue_get_work(queue);
    
    if(work){
        struct temporary_memory scratch_temp = begin_temporary_memory(&context->scratch);
        
        switch(work->description){
            case WORK_tokenize_file:            begin_counter(context, work_tokenize);           break;
            case WORK_parse_global_scope_entry: begin_counter(context, work_global_scope_entry); break;
            case WORK_parse_function_body:      begin_counter(context, work_parse_function);     break;
            case WORK_emit_code:                begin_counter(context, work_emit_code);          break;
            invalid_default_case();
        }
        
        parser_do_work(context, work);
        assert(&queue->work_entries_in_flight > 0);
        atomic_add(&queue->work_entries_in_flight, -1);
        
        switch(work->description){
            case WORK_tokenize_file:            end_counter(context, work_tokenize);           break;
            case WORK_parse_global_scope_entry: end_counter(context, work_global_scope_entry); break;
            case WORK_parse_function_body:      end_counter(context, work_parse_function);     break;
            case WORK_emit_code:                end_counter(context, work_emit_code);          break;
            invalid_default_case();
        }
        
        if(context->error){
            atomic_store(b32, globals.an_error_has_occurred, true);
        }
        
        end_temporary_memory(scratch_temp);
        return true;
    }
    return false;
}


func u32 work_thread_proc(void *param){
    struct thread_info *info = param;
    
    log_print("starting thread %d", info->thread_index);
    
    struct memory_arena main_arena = create_memory_arena(giga_bytes(64), 2.0f, mega_bytes(1));
    
    struct context *context = push_struct(&main_arena, struct context);
    info->context = context;
    init_context(context, info, &main_arena);
    
    begin_counter(context, thread_total);
    
    while(true){
        b32 should_continue = do_one_work(context);
        assert(context->arena->temp_count == 0);
        
        if(!should_continue){
            begin_counter(context, thread_sleep);
            
            log_print("good night!");
            WaitForSingleObject(globals.wake_event, INFINITE);
            log_print("woke up");
            
            end_counter(context, thread_sleep);
        }
        
        if(globals.threads_should_exit) break;
    }
    
    end_counter(context, thread_total);
    
    return 0;
}

func struct ast_function *get_entry_point_or_error(struct context *context){
    if(globals.an_error_has_occurred) return null;
    
    struct atom entry_point = globals.entry_point_name; // @cleanup: there is no reason for the entry_point to be stored in globals...
    
    struct ast_function *function = cast(struct ast_function *)ast_table_get(&globals.global_declarations, entry_point);
    log_print("amount of global declarations %d\n", globals.global_declarations.amount_of_nodes);
    if(!function){
        char *specified_or_default = string_match(entry_point.string, string("_start")) ? "Default" : "Specified";
        report_error(context, null, "Error: %s entry point '%.*s' not found.", specified_or_default, entry_point.length, entry_point.data);
        globals.an_error_has_occurred = true;
        os_debug_break();
        return null;
    }
    
    if(function->base.kind != AST_function){
        report_error(context, function->base.token, "Specified entry point '%.*s' is not a function.", entry_point.length, entry_point.data);
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
        report_error(context, function->base.token, "Entry point cannot be '__declspec(dllimport)'.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->as_decl.flags & DECLARATION_FLAGS_is_static){
        report_error(context, function->base.token, "Entry point cannot be static.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic){
        report_error(context, function->base.token, "Entry point cannot be an intrinsic function.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm){
        report_error(context, function->base.token, "Entry point cannot be declarated __declspec(inline_asm).");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    if(!function->scope){
        report_error(context, function->base.token, "Entry point has to be defined.");
        globals.an_error_has_occurred = true;
        return null;
    }
    
    // @cleanup: how to do the thing where we at compile time want to pass arguments?
    
    b32 type_matches = true;
    if(function->type->return_type != &globals.typedef_s32 && function->type->return_type != &globals.typedef_void) type_matches = false;
    if(function->type->argument_list.count != 0) type_matches = false;
    if(!type_matches){
        report_warning(context, WARNING_unusual_entry_point, function->base.token, "Entry point has to be of fit definition 'int %.*s()' or 'void %.*s()'.", entry_point.amount, entry_point.data, entry_point.amount, entry_point.data);
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

__declspec(dllimport) u32 RegOpenKeyExA(HANDLE key, char *sub_key, u32 option, u32 desired_access, HANDLE *out_key);
__declspec(dllimport) u32 RegQueryValueExA(HANDLE key, char *value_name, u32 *reserved, u32 *opt_out_type, u8 *opt_out_data, u32 *opt_out_length);
__declspec(dllimport) u32 RegEnumKeyExA(HANDLE key, u32 index, char *out_sub_key_name, u32 *in_out_sub_key_name_size, u32 *reserved, char *opt_in_out_class, u32 *opt_in_out_class_size, void *last_write_time);


// 
// https://learn.microsoft.com/en-us/cpp/porting/upgrade-your-code-to-the-universal-crt?view=msvc-170
// 
struct string find_windows_kits_root_and_sdk_version(struct memory_arena *arena, u64 sdk_version[4]){
    // 
    // Get the Windows 10 SDK root by using the registry.
    // 
    struct string ret = zero_struct;
    
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
        
        // @cleanup: check if the corresponding directory exists.
        
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
        
        if(is_higher){
            for(u32 index = 0; index < 4; index++) highest_version[index] = subkey_version[index];
        }
    }
    
    for(u32 index = 0; index < 4; index++) sdk_version[index] = highest_version[index];
    return push_zero_terminated_string_copy(arena, create_string(buffer, length));
}

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

    
#if PRINT_COMMAND_LINE
    print("\n");
    print("command line is: \n    ");
    for(int i = 0; i < argc; i++){
        print("%s ", argv[i]);
    }
    print("\n");
    print("\n");
#endif
    
    struct{
        struct work_queue_entry *first;
        struct work_queue_entry *last;
        smm amount;
    } files_to_parse = zero_struct;
    
    b32 no_standard_library = false;
    b32 no_intrinsics = false;
    b32 no_premain    = false;
    b32 no_predefines = false;
    b32 no_discard    = false;
    
    b32 test_was_specified = false;
    
    struct string_list predefines = {0};
    struct string_list libraries  = {0};
    
    globals.dynamic_base = true;
    globals.want_debug_information = true;
    
    // @note: does count the main thread
    u64 thread_count = 1;
    
    // :command_line_options
    
    char *c_entry_point_name = null;
    for(int argument_index = 1; argument_index < argc; argument_index++){
        struct string argument = cstring_to_string(argv[argument_index]);
        
        //
        // Allow /, -, --, as indicating options.
        //
        b32 is_option = false;
        if(argument.data[0] == '/'){
            string_eat_front(&argument, 1);
            is_option = true;
        }
        if(argument.data[0] == '-'){
            is_option = true;
            string_eat_front(&argument, 1);
            
            if(argument.data[0] == '-') string_eat_front(&argument, 1);
        }
        
        if(!is_option){
            
            struct string path = argument;
            canonicalize_slashes(path);
            b32 contains_wildcard = path_contains_wildcard(path);
            struct string absolute_file_path;
            if(path_is_absolute(path)){
                absolute_file_path = path;
            }else{
                absolute_file_path = concatenate_file_paths(arena, working_directory, path);
            }
            
            if(contains_wildcard){
                char *cpath = (char *)absolute_file_path.data;
                WIN32_FIND_DATAA find_data;
                HANDLE search_handle = FindFirstFileA(cpath, &find_data);
                
                // @cleanup: what if the wildcard is in the path and not the filename?
                //           like 'devices/*/src/main.c'
                struct string path_without_file = strip_file_name(absolute_file_path);
                
                if(search_handle != INVALID_HANDLE_VALUE){
                    do{
                        struct string file_name = string_from_cstring(find_data.cFileName);
                        struct string file_path = concatenate_file_paths(arena, path_without_file, file_name);
                        
                        struct string extension = get_file_extension(file_name);
                        if(!string_match(extension, string(".c")) && !string_match(extension, string(".h"))){
                            print("Warning: Input file '%.*s' has uncanonical file extension.\n", file_path.size, file_path.data);
                        }
                        
                        struct os_file file = os_load_file((char *)file_path.data, 0, 0);
                        
                        if(file.file_does_not_exist){
                            print("Error: File '%.*s' could not be loaded.\n", file_path.size, file_path.data);
                            return 1;
                        }else{
                            struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
                            work->absolute_file_path = file_path;
                            work->file_size = file.size;
                            
                            struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
                            work_entry->description = WORK_tokenize_file;
                            work_entry->data  = work;
                            
                            sll_push_back(files_to_parse, work_entry);
                            files_to_parse.amount += 1;
                        }
                    }while(FindNextFileA(search_handle, &find_data));
                    
                    FindClose(search_handle);
                }else{
                    print("Error: path '%s' does not exist.\n", cpath);
                    return 1;
                }
            }else{
                struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
                work->absolute_file_path = absolute_file_path;
                
                struct string extension = get_file_extension(absolute_file_path);
                if(!string_match(extension, string(".c")) && !string_match(extension, string(".h"))){
                    // @cleanup: you should be able to disable these!
                    print("Warning: Input file '%.*s' has uncanonical file extension.\n", absolute_file_path.size, absolute_file_path.data);
                }
                
                struct os_file file = os_load_file((char *)work->absolute_file_path.data, 0, 0);
                work->file_size = file.size;
                
                if(file.file_does_not_exist){
                    print("Error: Specified input file '%s' does not exist.\n", work->absolute_file_path.data);
                    os_debug_break();
                    return 2;
                }
                
                struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
                work_entry->description = WORK_tokenize_file;
                work_entry->data  = work;
                
                sll_push_back(files_to_parse, work_entry);
                files_to_parse.amount += 1;
            }
            continue;
        }
        
        if(string_match(argument, string("?")) || string_match(argument, string("h")) || string_match(argument, string("help"))){
            // @cleanup: look at the cl /? that one is really good
            // @cleanup: what happend to 80 columns?
            print("usage: %s [option...] filename...\n", argv[0]);
            static const char help_string[] = ""
                    "\n"
                    "Options:\n"
                    "  -out <filename>     Specify an output path. The file extensions get automatically appended.\n"
                    "                      Default is derived from the first compilation unit.\n"
                    "  -entry <name>       Specify the entry point symbol for the program. Default is '_start'.\n"
                    "  -I <dir>            Specify an additional include directory.\n"
                    "  -D<name>{=<text>}   Define a macro. Equivalent to '#define <name> <text>'.\n"
                    "  -L <filename>       Specify a dynamic library (.dll) to link to. Has to end in '.dll'.\n"
                    "  -no_stdlib          Do not link to 'ucrtbase.dll' and disable system include paths.\n"
                    "  -j<number>          Specify the amount of threads to use. Default is '1'.\n"
                    "  -P                  Print preprocessed files.\n"
                    "  -showIncludes       Print filenames when preprocessing.\n"
                    "  -no_premain         Do not include the implicit compilation unit for the premain stub.\n"
                    "  -no_intrinsics      Do not include the implicit compilation unit containing intrinsics.\n"
                    "  -no_dynamic_base    Do not set the xxx @incomplete PE flag.\n"
                    "  -no_pdb             Do not emit a .pdb file.\n"
                    "  -allow_dot_as_arrow Allow the use of the '.' operator instead of the '->' operator\n"
                    "                      to dereference a pointer.\n"
                    "\n";
            
            os_print_string((char *)help_string, sizeof(help_string) - 1);
            return 0;
        }else if(string_match(argument, string("no_stdlib"))){
            no_standard_library = true;
        }else if(string_match(argument, string("stdlib"))){
            no_standard_library = false; // Mostly for testing.
        }else if(string_match(argument, string("no_intrinsics")) || string_match(argument, string("no_intrinsic"))){
            no_intrinsics = true;
        }else if(string_match(argument, string("intrinsics")) || string_match(argument, string("intrinsic"))){
            no_intrinsics = false; // Mostly for testing.
        }else if(string_match(argument, string("no_premain"))){
            no_premain = true;
        }else if(string_match(argument, string("premain"))){
            no_premain = false; // Mostly for testing.
        }else if(string_match(argument, string("no_predefines"))){
            no_predefines = true;
        }else if(string_match(argument, string("no_pdb"))){
            globals.want_debug_information = false;
        }else if(string_match(argument, string("no_discard"))){
            no_discard = true;
        }else if(string_match(argument, string("no_dynamic_base"))){
            globals.dynamic_base = false;
        }else if(string_match(argument, string("base"))){
            if(argument_index + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                return 1;
            }
            
            struct string base = cstring_to_string(argv[++argument_index]);
            b32 success = true;
            u64 number = string_to_u64(base, &success);
            globals.image_base = number;
            
            if(!success){
                print("Error: Could not parse image base.\n");
                return 1;
            }
        }else if(string_match(argument, string("dont_print_the_files"))){
            globals.dont_print_the_files_because_we_are_in_a_test_suite = true;
        }else if(string_match(argument, string("showIncludes")) || string_match(argument, string("show_includes"))){
            globals.should_print_includes = true;
        }else if(string_match(argument, string("allow_dot_as_arrow"))){
            globals.allow_dot_as_arrow = true;
        }else if(string_front_match_eat(&argument, "subsystem:")){
            
            if(string_match_case_insensitive(argument, string("console"))){
                globals.pe_subsystem = PE_SUBSYSTEM_console;
            }else if(string_match_case_insensitive(argument, string("windows"))){
                globals.pe_subsystem = PE_SUBSYSTEM_windows;
            }else if(string_match_case_insensitive(argument, string("efi_application"))){
                globals.pe_subsystem = PE_SUBSYSTEM_efi_application;
            }
            
        }else if(string_match(argument, string("seed"))){
            if(argument_index + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                return 1;
            }
            
            struct string seed = cstring_to_string(argv[++argument_index]);
            b32 success = true;
            u64 number = string_to_u64(seed, &success);
            if(number == 0){
                globals.seed_to_randomize_order_of_global_scope_entries = __rdtsc();
            }else{
                globals.seed_to_randomize_order_of_global_scope_entries = number;
            }
            print("random seed used 0x%llx\n", globals.seed_to_randomize_order_of_global_scope_entries);
        }else if(string_match(argument, string("o")) || string_match_case_insensitive(argument, string("out"))){
            if(argument_index + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                return 1;
            }
            
            if(globals.output_file_path.data){
                print("Error: Output file specified twice.\n");
                return 1;
            }
            
            globals.output_file_path = string_from_cstring(argv[++argument_index]);
        }else if(string_match_case_insensitive(argument, string("entry"))){
            if(argument_index + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                return 1;
            }
            
            if(c_entry_point_name){
                print("Error: Entry point specified twice.\n");
                return 1;
            }
            
            c_entry_point_name = argv[++argument_index];
            
        }else if(string_match_case_insensitive(argument, string("no_entry"))){
            globals.no_entry = true;
        }else if(string_match(argument, string("I"))){
            if(argument_index + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                return 1;
            }
            
            char *directory = argv[++argument_index];
            if(!path_is_directory(directory)){
                print("Error: Specified additional include directory '%s', is not a directory.", directory);
                return 1;
            }
            
            struct string file_path = string_from_cstring(directory);
            
            canonicalize_slashes(file_path);
            if(path_is_relative(file_path)){
                file_path = concatenate_file_paths(arena, working_directory, file_path);
            }
            
            add_system_include_directory(arena, file_path, string(""), true);
            
        }else if(string_match(argument, string("L"))){
            if(argument_index + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                return 1;
            }
            
            char *file_name = argv[++argument_index];
            string_list_postfix(&libraries, arena, string_from_cstring(file_name));
            
        }else if(string_match(argument, string("DLL")) || string_match(argument, string("dll"))){
            globals.output_file_type = OUTPUT_FILE_dll;
            no_premain = 1;
        }else if(string_match(argument, string("obj")) || string_match(argument, string("OBJ")) || string_match(argument, string("c"))){
            
            no_premain = 1;
            globals.no_entry = 1;
            
            globals.output_file_type = OUTPUT_FILE_obj;
        }else if(string_match(argument, string("test"))){
            no_premain = 1;
            no_intrinsics = 1;
            no_standard_library = 1;
            test_was_specified = 1;
        }else if(argument.data[0] == 'D'){ // argument[0] is always valid as there is at least a zero terminator
            char *start = (char *)argument.data + 1;
            char *end = start;
            while(u8_is_alpha_numeric((u8)*end)){end++;}
            struct string to_define = create_string((u8 *)start, end - start);
            
            if(to_define.size == 0){
                print("Error: Expected an identifier to immediately follow '-D', e.g. '-Ddefine=1'.\n");
                return 1;
            }
            
            struct string define_to;
            if(end[0] == '='){
                char *define_to_start = ++end;
                while(*end) end++;
                
                define_to = create_string((u8 *)define_to_start, end - define_to_start);
            }else{
                // "By default, the value associated with a symbol is 1."
                define_to = string("1");
            }
            
            if(*end){
                print("Error: Junk after command-line option -D: '%s'\n", end);
                return 1;
            }
            
            struct string predefine = push_format_string(arena, "#define %.*s %.*s\n", to_define.size, to_define.data, define_to.size, define_to.data);
            
            string_list_postfix(&predefines, arena, predefine);
            
        }else if(argument.data[0] == 'j' || string_match(argument, string("thread_count"))){
            
            char *arg;
            if(argument.data[0] == 'j'){
                arg = (char *)argument.data + 1;
            }else{
                if(argument_index + 1 == argc){
                    print("Error: Expected argument after '%s'.\n", argv[argument_index]);
                    return 1;
                }
                arg = argv[argument_index + 1];
            }
            
            struct string thread_count_string = cstring_to_string(arg);
            b32 success = 1;
            thread_count = string_to_u64(thread_count_string, &success);
            
            if(!success){
                print("Error: Could not parse specified thread count '%.*s'.\n", thread_count_string.size, thread_count_string.data);
                return 1;
            }
            
            if(!thread_count){
                print("Error: Thread count must be at least one '%s'.\n", arg);
                return 1;
            }
        }else if(string_match(argument, string("Wall"))){
            for(u32 i = 0; i < WARNING_count; i++){
                warning_enabled[i] = 1;
            }
        }else if(string_match(argument, string("Wnone"))){
            for(u32 i = 0; i < WARNING_count; i++){
                warning_enabled[i] = 0;
            }
        }else if(argument.data[0] == 'W'){
            u8 *warning = argument.data + 1;
            u8 enabled = true;
            if(argument.data[1] == 'n' && argument.data[2] == 'o'){
                warning = argument.data + 3;
                enabled = false;
            }
            
            b32 success = 1;
            u64 warning_value = string_to_u64(cstring_to_string((char *)warning), &success);
            
            if(!success){
                print("Warning: Could not parse warning option '%s'.\n", argv[argument_index]);
            }else if(warning_value > WARNING_count){
                print("Warning: Unknown warning option %llu ('%s').\n", warning_value, argv[argument_index]);
            }else{
                warning_enabled[warning_value] = enabled;
            }
        }else{
#if 1
            print("Warning: Unknown command-line option '%s'.\n", argv[argument_index]);
#else
            os_debug_break();
            print("Error: Unknown command-line option '%s'.\n", argv[argument_index]);
            return 1;
#endif
        }
    }
    
    if(sll_is_empty(files_to_parse)){
        print("Error: No input files specified.\n");
        return 1;
    }
    
    {
        // 
        // Build the output file path.
        // 
        // If it is relative, prepend the working directory.
        // If it is a directory, append the name of the first source file.
        // 
        
        if(path_is_relative(globals.output_file_path)){
            globals.output_file_path = concatenate_file_paths(arena, working_directory, globals.output_file_path);
        }
        
        if(path_is_directory((char *)globals.output_file_path.data)){
            struct string file_name = strip_file_extension(strip_file_path(((struct work_tokenize_file *)files_to_parse.first->data)->absolute_file_path));
            globals.output_file_path = concatenate_file_paths(arena, globals.output_file_path, file_name);
        }
        
        struct string file_extension = zero_struct;
        switch(globals.output_file_type){
            case OUTPUT_FILE_exe: file_extension = string(".exe"); break;
            case OUTPUT_FILE_dll: file_extension = string(".dll"); break;
            case OUTPUT_FILE_obj: file_extension = string(".obj"); break;
            invalid_default_case();
        }
        
        // Attempt to infer the output file type from the out string.
        struct string output_file_extension = get_file_extension(globals.output_file_path);
        if(string_match(output_file_extension, file_extension)){
            globals.output_file_path.size -= file_extension.size;
        }
    }
    
    if(globals.no_entry && globals.output_file_type == OUTPUT_FILE_exe){
        print("Error: /NO_ENTRY is invalid when requesting an executable.");
        return 1;
    }
    
    if(c_entry_point_name && globals.no_entry){
        print("Error: /NO_ENTRY and an entry point name was specified.");
        return 1;
    }
    
    if(test_was_specified)  c_entry_point_name = "main";
    if(!c_entry_point_name) c_entry_point_name = "_start";
    
    if(!no_premain){
        struct string premain_path = concatenate_file_paths(arena, strip_file_name(compiler_path), string("implicit/premain.c"));
        globals.premain_path = premain_path;
        
        struct os_file file = os_load_file((char *)premain_path.data, 0, 0);
        if(file.file_does_not_exist){
            print("Error: Implicit premain compilation unit was not found.\n");
            print("       If this is intended you can use the '-no_premain'\n");
            print("       command line option to squelch this warning.\n");
            print("       This file is usually contained in: ");
            print("             \"<compiler-path>/implicit/premain.c\".");
            return 1;
        }else{
            struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
            work->absolute_file_path = premain_path;
            work->file_size = file.size;
            
            struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
            work_entry->description = WORK_tokenize_file;
            work_entry->data  = work;
            
            sll_push_back(files_to_parse, work_entry);
            files_to_parse.amount += 1;
        }
    }
    
    if(!no_intrinsics){
        struct string intrinsics_path = concatenate_file_paths(arena, strip_file_name(compiler_path), string("implicit/intrinsic.c"));
        globals.intrinsics_path = intrinsics_path;
        
        struct os_file file = os_load_file((char *)intrinsics_path.data, 0, 0);
        if(file.file_does_not_exist){
            print("Error: Implicit 'intrinsics.c' compilation unit was not found.\n");
            print("       If this is intended you can use the '-no_intrinsics'\n");
            print("       command line option to squelch this error.\n");
            print("       This file is usually contained in: ");
            print("             \"<compiler-path>/implicit/intrinsics.c\".");
            return 1;
        }else{
            struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
            work->absolute_file_path = intrinsics_path;
            work->file_size = file.size;
            
            struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
            work_entry->description = WORK_tokenize_file;
            work_entry->data  = work;
            
            sll_push_back(files_to_parse, work_entry);
            files_to_parse.amount += 1;
        }
    }
    
    add_system_include_directory(arena, strip_file_name(compiler_path), string("/implicit/include"),  false);
    
    u64 sdk_version[4];
    struct string windows_kits_path = find_windows_kits_root_and_sdk_version(arena, sdk_version);
    
    struct string um_library_path   = push_format_string(arena, "%.*s\\Lib\\%lld.%lld.%lld.%lld\\um\\x64\\",   windows_kits_path.size, windows_kits_path.data, sdk_version[0], sdk_version[1], sdk_version[2], sdk_version[3]);
    struct string ucrt_library_path = push_format_string(arena, "%.*s\\Lib\\%lld.%lld.%lld.%lld\\ucrt\\x64\\", windows_kits_path.size, windows_kits_path.data, sdk_version[0], sdk_version[1], sdk_version[2], sdk_version[3]);
    
    // @note: For now we always add the library paths, even if 'no_standard_library'.
    // @cleanup: There is no reason for library_paths to be global anymore.
    string_list_add_uniquely(&globals.library_paths, arena, working_directory);
    string_list_add_uniquely(&globals.library_paths, arena, um_library_path);
    string_list_add_uniquely(&globals.library_paths, arena, ucrt_library_path);
    
    for(struct string_list_node *library_node = libraries.list.first; library_node; library_node = library_node->next){
        
        struct string full_library_path = zero_struct;
        
        if(path_is_absolute(library_node->string)){
            full_library_path = library_node->string;
        }else{
            for(struct string_list_node *library_path_node = globals.library_paths.list.first; library_path_node;  library_path_node= library_path_node->next){
                struct string file_path = concatenate_file_paths(arena, library_path_node->string, library_node->string);
                struct os_file file = os_load_file((char *)file_path.data, 0, 0);
                
                if(!file.file_does_not_exist){
                    full_library_path = file_path;
                    break;
                }
            }
            
            if(!full_library_path.data){
                print("Error: Could not find specified library '%.*s'.\n\n", library_node->string.size, library_node->string.data);
                
                print("Library paths:\n");
                for(struct string_list_node *library_path_node = globals.library_paths.list.first; library_path_node;  library_path_node= library_path_node->next){
                    print("    %.*s\n", library_path_node->string.size, library_path_node->string.data);
                }
                print("\n");
                // @cleanup: Adding to the 'library_paths'.
                return 1;
            }
        }
        
        int parse_error = ar_parse_file((char *)full_library_path.data, arena);
        if(parse_error) return 1;
    }
    
    if(!no_standard_library){
        int parse_error = ar_parse_file((char *)push_format_string(arena, "%.*s\\ucrt.lib", ucrt_library_path.size, ucrt_library_path.data).data, arena);
        if(parse_error){
            // :Error mention Windows sdk.
            print("Error: Could not load 'ucrt.lib'. Please check your path.\n");
            print("       To compile without dynamically linking to the CRT, use \n");
            print("       the command-line option '-nostdlib'.\n");
            return 1;
        }
    }
    
    if(!no_premain){ // @cleanup: Is there another way? Also maybe use #pragma comment(lib, "kernel32.lib").
        int parse_error = ar_parse_file((char *)push_format_string(arena, "%.*s\\kernel32.lib", um_library_path.size, um_library_path.data).data, arena);
        if(parse_error){
            // :Error mention Windows sdk.
            print("Error: Could not load 'kernel32.lib'. Please check your path.\n");
            print("       To compile without dynamically linking to the CRT, use \n");
            print("       the command-line option '-nostdlib'.\n");
            return 1;
        }
    }
    
    struct string windows_kits_include_base = push_format_string(arena, "%.*s\\Include\\%lld.%lld.%lld.%lld", windows_kits_path.size, windows_kits_path.data, sdk_version[0], sdk_version[1], sdk_version[2], sdk_version[3]);
    
    // @note: For now always add the windows folders.
    add_system_include_directory(arena, windows_kits_include_base, string("/um"),     false);
    add_system_include_directory(arena, windows_kits_include_base, string("/shared"), false);
    add_system_include_directory(arena, windows_kits_include_base, string("/winrt"),  false);
    
    if(!no_standard_library) add_system_include_directory(arena, windows_kits_include_base, string("/ucrt"),   false);
    
#if 1
    {
        // 
        // Here is what I have gathered:
        //     1) The ucrt depends on the vcruntime. ("visual compiler runtime" I think)
        //     2) The vcruntime has "all rights reserved" licencing.
        //     3) The install path of visual studio could be arbitrary, 
        //        and the only way to maybe find it is using vswhere or COM.
        //        
        // So essetially, we have to do a bunch of bullshit to find the install directory,
        // or rely on people to install it in the default directory.
        // 
        // @cleanup: Because I don't want to deal with it right now, I have decided for the latter.
        //           In the future the plan is to add an implementation of the c standard library anyway.
        // 
        
        struct string VISUAL_STUDIO = hacky_find_newest_system_include_path(arena, "C:/Program Files (x86)/Microsoft Visual Studio/*");
        
        if(VISUAL_STUDIO.data){
            struct string MSVC_path = push_format_string(arena, "%.*s/BuildTools", VISUAL_STUDIO.size, VISUAL_STUDIO.data);
            if(!path_is_directory((char *)MSVC_path.data)){
                MSVC_path = push_format_string(arena, "%.*s/Enterprise", VISUAL_STUDIO.size, VISUAL_STUDIO.data);
                if(!path_is_directory((char *)MSVC_path.data)){
                    MSVC_path = push_format_string(arena, "%.*s/Professional", VISUAL_STUDIO.size, VISUAL_STUDIO.data);
                    if(!path_is_directory((char *)MSVC_path.data)){
                        MSVC_path = push_format_string(arena, "%.*s/Community", VISUAL_STUDIO.size, VISUAL_STUDIO.data);
                    }
                }
            }
            
            MSVC_path = push_format_string(arena, "%.*s/VC/Tools/MSVC/*", MSVC_path.size, MSVC_path.data);
            
            struct string MSVC = hacky_find_newest_system_include_path(arena, (char *)MSVC_path.data);
            if(MSVC.data){
                add_system_include_directory(arena, MSVC, string("/include"), false);
                // ... It appearantly does not have that...
                // add_system_include_directory(arena, MSVC, string("/ATLMFC/include"), false); 
            }
        }
        
#if 0
        struct string NETFXSDK = hacky_find_newest_system_include_path(arena, "C:/Program Files (x86)/Windows Kits/NETFXSDK/*");
        if(NETFXSDK.data){
            add_system_include_directory(arena, NETFXSDK, string("/include/um"), false);
        }
#endif
        
#ifdef PRINT_SYSTEM_INCLUDE_PATHS
        print("\nSystem include directories:\n");
        for(struct string_list_node *include = globals.system_include_directories.list.first; include; include = include->next){
            print("    %.*s\n", include->string.size, include->string.data);
        }
#endif
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
        
        make_const_typedef(Bool,   TOKEN_Bool,     AST_integer_type, "_Bool",             1, 1);
        
        make_const_typedef(u8,   TOKEN_unsigned, AST_integer_type, "unsigned char",      1, 1);
        make_const_typedef(u16,  TOKEN_short,    AST_integer_type, "unsigned short",     2, 2);
        make_const_typedef(u32,  TOKEN_unsigned, AST_integer_type, "unsigned int",       4, 4);
        make_const_typedef(u64,  TOKEN_long,     AST_integer_type, "unsigned long long", 8, 8);
        
        make_const_typedef(s8,   TOKEN_char,     AST_integer_type, "char",               1, 1);
        make_const_typedef(s16,  TOKEN_short,    AST_integer_type, "short",              2, 2);
        make_const_typedef(s32,  TOKEN_int,      AST_integer_type, "int",                4, 4);
        make_const_typedef(s64,  TOKEN_long,     AST_integer_type, "long long",          8, 8);
        
        make_const_typedef(f32,  TOKEN_float,    AST_float_type,   "float",              4, 4);
        make_const_typedef(f64,  TOKEN_double,   AST_float_type,   "double",             8, 8);
        make_const_typedef(poison, TOKEN_void,   AST_void_type,    "poison",             8, 8);
        
        
        globals.invalid_file.absolute_file_path = "*predefined token*";
        globals.invalid_file.file_index = -1;
        globals.file_table.invalid_file = &globals.invalid_file;
        {
            if(!no_predefines){
                struct string hardcoded_predefines = string(
                        "#define __PBC__ 1\n"            
                        "#define _M_X64 100\n"
                        "#define _M_AMD64 100\n"
                        "#define _WIN64 1\n"
                        "#define _WIN32 1\n"
                        
                        // @cleanup:  I feel like these should not be defined
                        "#define _MSC_BUILD 0\n"
                        "#define _MSC_EXTENSIONS 1\n"
                        "#define _MSC_FUL_VER 192829336\n"
                        "#define _MSC_VER 1928\n"
                        
                        "#define __assume(a) (void)0\n"
                        
                        // 
                        // @cleanup: should we define these? 
                        // 
                        "#define __STDC_VERSION__ 201112L\n"
                        "#define __STDC__ 1\n"
                        "#define __STDC_NO_COMPLEX__ 1\n"
                        "#define __STDC_NO_THREADS__ 1\n"
                        "#define __STDC_NO_VLA__ 1\n"
                        );
                
                string_list_prefix(&predefines, arena, hardcoded_predefines);
                
                if(globals.output_file_type != OUTPUT_FILE_obj){
                    string_list_postfix(&predefines, arena, string("#define _CRTIMP __declspec(dllimport)\n"));
                }
                
            }
            
            struct string predefines_string = string_list_flatten(predefines, arena);
            
            struct token_array tokens = tokenize_raw(context, predefines_string, (u32)globals.invalid_file.file_index, /*is_stupid_hack*/false, /*out lines*/null);
            globals.predefined_tokens = tokens;
        }
        
        globals.keyword_dllimport   = atom_for_string(string("dllimport"));
        globals.keyword_dllexport   = atom_for_string(string("dllexport"));
        globals.keyword_align       = atom_for_string(string("align"));
        globals.keyword_noreturn    = atom_for_string(string("noreturn"));
        globals.keyword_selectany   = atom_for_string(string("selectany"));
        globals.keyword_intrin_type = atom_for_string(string("intrin_type"));
        
        // hlc declspecs
        globals.keyword_inline_asm = atom_for_string(string("inline_asm"));
        globals.keyword_packed     = atom_for_string(string("packed"));
        globals.keyword_printlike  = atom_for_string(string("printlike"));
        
        
        // pragma directives
        globals.pragma_once  = atom_for_string(string("once"));
        
        globals.keyword__VA_ARGS__ = atom_for_string(string("__VA_ARGS__"));
        
        globals.unnamed_tag        = atom_for_string(string("<unnamed tag>"));
        globals.unnamed_enum       = atom_for_string(string("<unnamed enum>"));
        globals.invalid_identifier = atom_for_string(string("<invalid identifier>"));
        
        // :hlc_extension
        globals.keyword_data      = atom_for_string(string("data"));
        globals.keyword_size      = atom_for_string(string("size"));
        globals.keyword_main      = atom_for_string(string("main"));
        
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
        
        globals.sleeper_table = sleeper_table_create(1 << 8);
        
        // :ast_tables
        globals.global_declarations = ast_table_create(1 << 8);
        globals.compound_types      = ast_table_create(1 << 8);
        
        globals.intrinsic_table.nodes = push_data(arena, struct intrinsic_info, INTRINSIC_TABLE_CAPACITY);
        
        // register_intrinsic(arena, name, intrinsic_kind, opcode);
        
#define register_intrinsic(name, kind) \
register_intrinsic(atom_for_string(string(#name)), INTRINSIC_KIND_##kind)
        
        // miscellaneous intrinsics
        register_intrinsic(__va_start, va_start);
        
#undef register_intrinsic
        
        globals.empty_statement.kind = AST_empty_statement;
        globals.empty_statement.token = push_dummy_token(arena, atom_for_string(string(";")), TOKEN_semicolon);
        set_resolved_type(&globals.empty_statement, &globals.typedef_void, null);
        
        globals.invalid_identifier_token = push_dummy_token(arena, globals.invalid_identifier, TOKEN_identifier);
        
        globals.guard_ast.token = &globals.invalid_token;
        globals.guard_ast.resolved_type = &globals.typedef_void;
        
        struct declarator_return poison_declarator = {.ident = globals.invalid_identifier_token, .type = &globals.typedef_poison };
        globals.poison_declaration = push_declaration_for_declarator(context, poison_declarator);
        
    }
    
#if PRINT_ADDITIONAL_INCLUDE_DIRECTORIES
    print("Additional include directories:\n");
    for(struct string_list_node *node = globals.additional_include_directories.list.first; node; node = node->next){
        print("    %.*s\n", node->string.amount, node->string.data);
    }
    print("\nSystem include directories:\n");
    for(struct string_list_node *node = globals.system_include_directories.list.first; node; node = node->next){
        print("    %.*s\n", node->string.amount, node->string.data);
    }
    print("\n");
#endif
    
#if 1
    {
        smm capacity = 0x8;
        smm count    = 0;
        
        struct system_include_file_entry *entries = push_data(arena, struct system_include_file_entry, capacity);
        
        for(struct string_list_node *node = globals.system_include_directories.list.first; node; node = node->next){
            // print("    %.*s\n", node->string.amount, node->string.data);
            
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
    
    globals.entry_point_name = atom_for_string(string_from_cstring(c_entry_point_name));
    
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
    
    // get it going:
    globals.compile_stage = COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries;
    
    struct compilation_unit *compilation_units = push_data(arena, struct compilation_unit, files_to_parse.amount);
    globals.compilation_units.data   = compilation_units;
    globals.compilation_units.amount = files_to_parse.amount;
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
            
            struct work_tokenize_file *work = entry->data;
            work->compilation_unit = unit;
            
            print("   [%lld] %.*s\n", unit->index, work->absolute_file_path.size, work->absolute_file_path.data);
        }
        print("\n");
        assert(index == files_to_parse.amount);
    }
    
    work_queue_append_list(&globals.work_queue_stage_one, files_to_parse.first, files_to_parse.last, files_to_parse.amount);
    
    // @note: thread '0' is the main thread, so just start all other ones
    for(u32 thread_index = 1; thread_index < thread_count; thread_index++) {
        os_create_thread(work_thread_proc, &thread_infos[thread_index]);
    }
    
    end_counter(context, startup);
    
    //
    // COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries
    //
    
    // we _just_ pushed all the files into the work 'globals.work_queue_stage_one' and started the 
    // threads. Each file is a compilation unit. These 'compilation_units' first get tokenized and preprocessed,
    // then "chopped into pieces", such that we can parse each global scope entry individually.
    // Finally, these global scope entries get appended to the work queue again and from here a thread
    // grabs a global scope entry and attempts to parse it. 
    // If the global scope entry is a function definition the body gets put into the work queue for the next stage.
    // If we encounter an identifier we do not know about yet, we add this global scope entry into the 'sleeper_table'
    // where it waits for the identifier to resolve.
    // In the end of this phase all types and global declarations are known.
    
    while(globals.work_queue_stage_one.work_entries_in_flight > 0) do_one_work(context);
    
    assert(globals.work_queue_stage_one.work_entries_in_flight == 0);
    if(globals.an_error_has_occurred) goto end;
    
    //
    // At this point no more sleepers will get filled in.
    // So _if_ there are any, report them.
    // The later parse stage (COMPILE_STAGE_parse_function) should error directly if something is 
    // undeclared, as all global symbols are known.
    //
    begin_counter(context, unresolved_sleepers);
    
    // 
    // First report errors for global things that sleep.
    // If we found some global thing that sleeps we want to exit before examining the local (static) sleeper tables, 
    // as things in there might sleep on global declarations.
    //
    report_errors_for_unresolved_sleepers(context, &globals.sleeper_table);
    if(globals.an_error_has_occurred) goto end;
    
    for(smm compilation_unit_index = 0; compilation_unit_index < globals.compilation_units.amount; compilation_unit_index++){
        //
        // We report errors for unresolved global static identifiers for all compilation units, 
        // even if one compilation unit already reported an unresolved identifier.
        //
        report_errors_for_unresolved_sleepers(context, &globals.compilation_units.data[compilation_unit_index].static_sleeper_table);
    }
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
    
    while(globals.work_queue_stage_two.work_entries_in_flight > 0){
        do_one_work(context);
        assert(!context->should_sleep);
    }
    assert(globals.work_queue_stage_two.work_entries_in_flight == 0);
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
    // which we have not yet emitted. In this case we emit a 'patch' which then gets filled in during 
    // 'print_coff'.
    
    log_print("start phase 3");
    
    stage_three_emit_code_time = os_get_time_in_seconds();
    
    
    // at this point all parsing is complete and there should not really be any errors anymore.
    
    globals.compile_stage = COMPILE_STAGE_emit_code;
    if(no_discard){
        // 
        // Only really here for fuzzing...
        // We emit code for every function.
        // 
        
        if(!globals.no_entry){
            globals.entry_point = get_entry_point_or_error(context);
            if(globals.an_error_has_occurred) goto end;
        }
        
        for(smm compilation_unit_index = -1; compilation_unit_index < globals.compilation_units.amount; compilation_unit_index++){
            
            struct ast_table *table = &globals.global_declarations;
            
            if(compilation_unit_index >= 0){
                struct compilation_unit *unit = globals.compilation_units.data + compilation_unit_index;
                table = &unit->static_declaration_table;
            }
            
            for(u64 table_index = 0; table_index < table->capacity; table_index++){
                struct ast *ast = table->nodes[table_index].ast;
                if(!ast) continue;
                if(ast->kind != AST_function) continue;
                
                struct ast_function *function = (struct ast_function *)ast;
                
                function->as_decl.flags |= DECLARATION_FLAGS_is_function_that_is_reachable_from_entry;
                
                if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport) continue;
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                
                if(!function->scope){
                    // 
                    // @hack: We emit an empty scope for each function which does not have one already,
                    //        to not crash in 'emit_code_for_function'. 
                    //        This might have as a consequence, that some code paths become unavailable to the fuzzer...
                    // 
                    function->scope = &parser_push_new_scope(context, function->base.token, SCOPE_FLAG_is_function_scope)->base;
                }
                
                work_queue_push_work(context, &globals.work_queue_stage_three, WORK_emit_code, function);
            }
        }
        
        for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
            struct context *thread_context = globals.thread_infos[thread_index].context;
            
            for_ast_list(thread_context->local_functions){
                struct ast_function *function = (struct ast_function *)it->value;
                assert(function->base.kind == AST_function);
                
                function->as_decl.flags |= DECLARATION_FLAGS_is_function_that_is_reachable_from_entry;
                
                if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport) continue;
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                
                work_queue_push_work(context, &globals.work_queue_stage_three, WORK_emit_code, function);
            }
        }
        
    }else{   
        // compute reachability information    :only_emit_functions_that_are_reachable_from_main
        // 
        // Do a depth first search on the call graph to compute
        // what is reachable from 'entry_point'.        
        // 
        
        if(!globals.no_entry){
            globals.entry_point = get_entry_point_or_error(context);
            if(globals.an_error_has_occurred) goto end;
            
            // @note: do this manually instead of calling 'add_potential_entry_point' as we are in a later 
            //        compilation stage, and do not have to do it atomically.
            struct function_node *entry_point_node = push_struct(arena, struct function_node);
            entry_point_node->function = globals.entry_point;
            entry_point_node->next = globals.functions_that_are_referenced_by_global_scope_entries;
            globals.functions_that_are_referenced_by_global_scope_entries = entry_point_node;
        }
        
        if(globals.output_file_type == OUTPUT_FILE_obj){
            // 
            // For an .obj any external function is a potential entry point.
            // 
            
            for(u32 table_index = 0; table_index < globals.global_declarations.capacity; table_index++){
                struct ast *ast = globals.global_declarations.nodes[table_index].ast;
                if(!ast) continue;
                if(ast->kind != AST_function) continue;
                
                struct ast_function *function = (struct ast_function *)ast;
                if(function->as_decl.flags & DECLARATION_FLAGS_is_static) continue;
                if(!function->scope) continue;
                
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                
                struct function_node *function_node = push_struct(arena, struct function_node);
                function_node->function = function;
                function_node->next = globals.functions_that_are_referenced_by_global_scope_entries;
                
                globals.functions_that_are_referenced_by_global_scope_entries = function_node;
            }
        }
        
        struct temporary_memory temp = begin_temporary_memory(&context->scratch);
        
        for(struct function_node *entry = globals.functions_that_are_referenced_by_global_scope_entries; entry; entry = entry->next){
            
            struct ast_function *initial_function = entry->function;
            
            // @cleanup: this first part looks almost exacly like the most inner part of the loop.
            // we have already been here
            if(initial_function->as_decl.flags & DECLARATION_FLAGS_is_function_that_is_reachable_from_entry) continue;
            initial_function->as_decl.flags |= DECLARATION_FLAGS_is_function_that_is_reachable_from_entry;
            
            if(initial_function->as_decl.flags & DECLARATION_FLAGS_is_dllimport) continue;
            if(initial_function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
            
            if(!initial_function->scope){
                // 
                // Might be not defined, e.g. an import with missing 'dllimport'.
                // 
                
                if(!initial_function->scope) continue;
            }
            
            // queue the entry_point.
            work_queue_push_work(context, &globals.work_queue_stage_three, WORK_emit_code, initial_function);
            
            struct reachability_stack_node{
                struct reachability_stack_node *next;
                struct ast_function *function;
                struct function_node *at;
            };
            struct{
                struct reachability_stack_node *first;
                struct reachability_stack_node *last;
            } stack = zero_struct;
            
            
            // and push it to the stack as initial node
            struct reachability_stack_node initial_node = zero_struct;
            initial_node.function = initial_function;
            initial_node.at = initial_function->called_functions.first;
            
            sll_push_front(stack, &initial_node);
            
            while(!sll_is_empty(stack)){
                struct reachability_stack_node *node = stack.first;
                
                if(!node->at){
                    // recurse up
                    sll_pop_front(stack);
                }else{
                    // recurse down into 'node->at'
                    struct ast_function *function = node->at->function;
                    
                    if(!(function->as_decl.flags & DECLARATION_FLAGS_is_function_that_is_reachable_from_entry)){
                        function->as_decl.flags |= DECLARATION_FLAGS_is_function_that_is_reachable_from_entry;
                        
                        if(function->scope){
                            // if the function is not defined, it might be a dllimport without
                            // '__declspec(dllimport)'.
                            // Don't queue it, don't recurse into it, but also do not error.
                            
                            if(!(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm)){
                                work_queue_push_work(context, &globals.work_queue_stage_three, WORK_emit_code, function);
                            }
                            // note: we only want to recurse once into any given function.
                            struct reachability_stack_node *new_node = push_struct(&context->scratch, struct reachability_stack_node);
                            new_node->function = function;
                            new_node->at = function->called_functions.first;
                            
                            sll_push_front(stack, new_node);
                        }
                    }
                    
                    node->at = node->at->next;
                }
            }
        }
        
        end_temporary_memory(temp);
    }
    
    while(globals.work_queue_stage_three.work_entries_in_flight > 0){
        do_one_work(context);
    }
    assert(globals.work_queue_stage_three.work_entries_in_flight == 0);
    if(globals.an_error_has_occurred) goto end;
    
    
    // 
    // Evaluate the initializers for all unnamed global declarations (struct/array literals).
    // @cleanup: This could be threaded, is this really where we want to do this?
    // 
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for_ast_list(thread_context->global_struct_and_array_literals){
            struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)it->value;
            
            compound_literal->decl->memory_location = evaluate_static_initializer(thread_context, compound_literal->decl);
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
        log_print("start phase 4");
        begin_counter(context, report_error_for_undefined_functions_and_types);
        report_errors_for_undefined_functions_and_types(context, &globals.global_declarations);
        
        for(smm i = 0; i < globals.compilation_units.amount; i++){
            if(globals.an_error_has_occurred) goto end;
            report_errors_for_undefined_functions_and_types(context, &globals.compilation_units.data[i].static_declaration_table);
        }
    }
    
    end_counter(context, report_error_for_undefined_functions_and_types);
    
    end:; {
        
        //
        // Error reporting, this is the place where we actually print the errors!
        //
        begin_counter(context, print_errors_to_the_console);
        
        // Gather the errors that were reported
        struct {
            struct error_report_node *first;
            struct error_report_node *last;
        } error_list = zero_struct;
        
        for(u32 thread_index = 0; thread_index < thread_count; thread_index++){
            sll_push_back_list(error_list, thread_infos[thread_index].context->error_list);
        }
        
        sll_sort(error_list, &context->scratch, error_node_smaller_function);
        
        if(globals.an_error_has_occurred){
            //
            // If there was an error, only report errors. This avoids warning spam, that you have to search through to find the error
            //
            u32 error_reported = false;
            for(struct error_report_node *error_reports = error_list.first; error_reports; error_reports = error_reports->next){
                
                for(struct error_report_node *error = error_reports; error; error = error->sub_error){
                    if(error->kind != REPORT_warning){
                        error_reported = true;
                        print_one_error_node(error);
                    }
                }
            }
            assert(error_reported);
        }else{
            //
            // If there was no error, all should be warnings, report them!
            //
            for(struct error_report_node *error_reports = error_list.first; error_reports; error_reports = error_reports->next){
                for(struct error_report_node *warning = error_reports; warning; warning = warning->sub_error){
                    assert(warning->kind == REPORT_warning);
                    print_one_error_node(warning);
                }
            }
        }
        
        end_counter(context, print_errors_to_the_console);
    }
    
    
    stage_four_linking = os_get_time_in_seconds();
    
    if(!globals.an_error_has_occurred){
        
        // 
        // At this point no error occurred, and we are all ready 
        // to actually assemble the output files.
        // 
        
        // The emit arena is fixed size 4 gigs, so we can print an error, if we exceed it.
        struct memory_arena emit_arena = create_memory_arena(giga_bytes(4), 2.0f, mega_bytes(10));
        emit_arena.out_of_memory_string = "Error: Maximum executable file size exceeded. Cannot emit a valid executable file.\n"; // :Error
        
        
        if(globals.output_file_type == OUTPUT_FILE_obj){
            print_obj(&emit_arena, arena); 
        }else{
            begin_counter(context, print_coff);
            print_coff(&emit_arena, arena); 
            end_counter(context, print_coff);
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
    print("\nTotal Lines: %lld | Total Lines Preprocessed %lld | Time: %.3fs\n", amount_of_lines, amount_of_lines_preprocessed, time_in_seconds);
    
    f64 overhead = time_in_seconds - (stage_one_tokenize_and_preprocess_time + stage_two_parse_functions_time + stage_three_emit_code_time + stage_four_linking);
    print("preprocessing %.3fs (%.3f%%) | compiling %.3fs (%.3f%%) | code gen %.3fs (%.3f%%) | linking %.3fs (%.3f%%) | overhead %.3fs (%.3f%%)\n", 
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



