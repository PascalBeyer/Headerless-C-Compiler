#include "std.c"
#include "windows.c"

#include "debug_options.h"

// Main file containing the main control flow and global structures.
// Compilation goes through multiple stages which are threaded using 
// 'work_queue's:
//    'COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries':
//        Preprocess the file and then chunk it into 'work_queue_entries', 
//        that are the sizeof a global declaration.
//        These global declarations are then parsed and if they are functions
//        their body gets queued to be processed in the next phase.
//        At the end of this phase all global declarations are known and all
//        global types are known.
//        At any point, while parsing the declarations, the system might
//        encounter an identifier which is not yet resolved (as this compiler 
//        supports out of order compilation). In this case, the 'work_entry', 
//        goes to 'sleep', i.e. enters the 'sleeper_table'.
//        Whenever a global identifier resolves the corresponding 
//        'sleeper_table' entry gets marked as 'done' and all sleepers 
//        reenter the 'stage_one' work_queue.

//    'COMPILE_STAGE_parse_function_and_emit_code'
//        After all types and global declarations are resolved the system
//        can proceed as if it were a 'normal' (single pass) c-compiler.
//        The function that were queued in the 'stage_two' work_queue
//        are now parsed linearly.
//        Finally we emit code straight from the "Abstract Syntax Tree", 
//        without first emitting any sort if "Intermediate Representation".

// After the above stages are complete, the main thread first does some 
// validation (e.g. 'report_error_for_undefined_functions_and_types') and then
// Serializes the compiled code into an '.exe' and a '.pdb' in 'print_coff'.
// Throughout, whenever we needed to know the address of a function or global
// declaration, we emitted a 'patch', which the main thread now filles in.
// This is essentially the 'linker'. Then we write out the files and exit.
//                                                                -11.09.2020


#if 0
#define log_print(format, ...)  print(format  " (thread %u)\n", __VA_ARGS__, GetCurrentThreadId());
#elif 0
#define log_print(format, ...)  print(format "\n", __VA_ARGS__);
#else
#define log_print(formal, ...)
#endif


#if time_perfomance
// :timing :performance :counters
enum timing_place{
    
    TIMING_PLACE_startup,
    TIMING_PLACE_work_tokenize,
    TIMING_PLACE_work_global_scope_entry,
    TIMING_PLACE_work_parse_function,
    
    TIMING_PLACE_identifier_lookup,
    
    TIMING_PLACE_print_coff,
    TIMING_PLACE_print_exe,
    TIMING_PLACE_print_pdb,
    TIMING_PLACE_patch,
    TIMING_PLACE_write_files,
    TIMING_PLACE_write_pdb,
    TIMING_PLACE_write_exe,
    TIMING_PLACE_virus_scanner,
    
    TIMING_PLACE_emit_code_for_function,
    TIMING_PLACE_emit_debug_for_function,
    
    TIMING_PLACE_tokenize_file_raw,
    TIMING_PLACE_create_perfect_keyword_table,
    
    TIMING_PLACE_sort_functions,
    
    TIMING_PLACE_pdb_stream,
    TIMING_PLACE_names_stream,
    TIMING_PLACE_tpi_stream,
    TIMING_PLACE_tpi_hash_stream,
    TIMING_PLACE_ipi_stream,
    TIMING_PLACE_ipi_hash_stream,
    TIMING_PLACE_module_stream,
    TIMING_PLACE_dbi_stream,
    TIMING_PLACE_gsi_stream,
    TIMING_PLACE_psi_stream,
    TIMING_PLACE_directory_stream,
    TIMING_PLACE_free_page_map,
    
    TIMING_PLACE_module_function_info,
    TIMING_PLACE_module_file_info,
    TIMING_PLACE_module_line_info,
    
    TIMING_PLACE_load_file,
    TIMING_PLACE_tokenize_and_preprocess,
    
    TIMING_PLACE_unresolved_sleepers,
    TIMING_PLACE_report_errror_for_undefined_functions_and_types,
    TIMING_PLACE_lookup_dll_import,
    
    
    TIMING_PLACE_print_error_list,
    TIMING_PLACE_total,
    
    TIMING_PLACE_count,
};

static const char *timing_place_names[] = {
    "starup",
    "work_tokenize",
    "work_global_scope_entry",
    "work_parse_function",
    "identifier_lookup",
    "print_coff",
    "print_exe",
    "print_pdb",
    "patch",
    "write_files",
    "write_pdb",
    "write_exe",
    "virus_scanner",
    "emit_code_for_function",
    "emit_debug_for_function",
    "tokenize_file_raw",
    "create_keyword_table",
    "sort_functions",
    "pdb_stream",
    "names_stream",
    "tpi_stream",
    "tpi_hash_stream",
    "ipi_stream",
    "ipi_hash_stream",
    "module_stream",
    "dbi_stream",
    "gsi_stream",
    "psi_stream",
    "directory_stream",
    "free_page_map",
    "module_function_info",
    "module_file_info",
    "module_line_info",
    "load_file",
    "tokenize_and_preprocess",
    "unresolved_sleepers",
    "undefined_functions_and_types",
    "lookup_dll_import",
    "print_error_list",
    "total/overhead",
};

static_assert(array_count(timing_place_names) == TIMING_PLACE_count);

enum timing_event_kind{
    EVENT_begin_perf_counter,
    EVENT_end_perf_counter,
};

struct perf_timing_event{
    enum timing_place place;
    enum timing_event_kind kind;
    u64 counter;
};

static bucket_array(struct perf_timing_event) timing_events;

#define begin_counter(member) \
{\
    struct perf_timing_event event = {\
        .counter = __rdtsc(),\
        .place = TIMING_PLACE_##member,\
        .kind = EVENT_begin_perf_counter,\
    };\
    bucket_array_add(timing_events, event);\
}

#define end_counter(member) \
{\
    struct perf_timing_event event = {\
        .counter = __rdtsc(),\
        .place = TIMING_PLACE_##member,\
        .kind = EVENT_end_perf_counter, \
    };\
    bucket_array_add(timing_events, event);\
}

#else
#define begin_counter(member)
#define end_counter(member)
#endif

#include "ast.c"

struct parse_work{
    struct token_bucket_array tokens;
    smm start_at; // used for functions
    struct token *sleeping_ident; // the one who sleeps. can be null if we don't know yet. For error reporting
    union{
        struct{ // for parse_body
            struct ast_function *function;
        };
    };
};

enum work_description{
    WORK_invalid,
    
    WORK_tokenize_file,
    WORK_parse_global_scope_entry,
    WORK_parse_function_body,
    
    WORK_count,
};

struct work_tokenize_file{
    struct string absolute_file_path;
};

struct work_queue_entry{
    struct work_queue_entry *next;
    enum work_description description;
    b32 pad;
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

///////////////////////////////////////////////////////////////////////////////////////////////////////

struct thread_info{
    s64 thread_index;
};


struct sleeper_node{
    union{
        struct{
            u64 hash;
            struct work_queue_entry *first_sleeper;
        };
        m128 mem;
    };
};

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
            unique_string hash;
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

///////////////////////////////////////////////////////////////////////////////////////////////////////

struct string_table_entry{
    union{
        struct{
            unique_string value;
            u64 hash;
        };
        m128 mem;
    };
};

struct string_table{
    struct string_table_entry *entries;
    
    s64 size;
    u64 mask;
    s64 capacity;
    
    s64 is_locked_for_growing;
    s64 threads_in_flight;
    
    struct memory_arena copy_arena;
};

struct string_table string_table_init(smm initial_capacity){
    struct string_table ret = zero_struct;
    ret.capacity = initial_capacity;
    ret.mask = ret.capacity - 1;
    smm byte_size = ret.capacity * sizeof(struct string_table_entry);
    ret.entries  = os_allocate_memory(byte_size);
    if(!ret.entries){
        print("Memory failiure allocating string table.\n");
        os_panic(1);
    }
    return ret;
}

// arena is assumed to be thread local. we copy in the string
func unique_string string_table_insert(struct string_table *table, struct string to_insert, struct memory_arena *arena){
    while(true){
        if(atomic_load(s64, table->is_locked_for_growing)) continue;
        
        if(table->size + 1 > (table->capacity >> 1)){
            //spin until WE set table->is_locked_for_growing
            void *old_is_locked = atomic_compare_and_swap(&table->is_locked_for_growing, (void *)true, (void *)false);
            if(old_is_locked) continue;
            // we are the thread that set 'is_locked_for_growing' to true
            
            // we could have set the condition ironiously if the actuall growing thead finished while
            // we were at ***, so lets check the condition again
            if(table->size + 1 > (table->capacity >> 1)){
                
                // Lets wait until there are not threads in flight
                while(table->threads_in_flight);
                
                u64 old_capacity = table->capacity;
                struct string_table_entry *old_entries = table->entries;
                
                table->capacity <<= 1;
                void *memory = os_allocate_memory(table->capacity * sizeof(*table->entries));
                if(!memory){
                    print("Memory failure in string table.\n");
                    os_panic(1);
                }
                table->entries = (struct string_table_entry *)memory;
                table->mask = table->capacity - 1;
                
                for(u64 i = 0; i < old_capacity; i++){
                    struct string_table_entry *entry = old_entries + i;
                    if(!entry->value) continue;
                    
                    u64 slot = entry->hash & table->mask;
                    for(u64 slot_offset = 0;; slot_offset++){
                        struct string_table_entry *new_entry = table->entries + ((slot + slot_offset) & table->mask);
                        if(new_entry->value) continue;
                        *new_entry = *entry;
                        break;
                    }
                }
                
                os_free_memory(old_entries);
            }
            
            // unlock
            table->is_locked_for_growing = false;
        }
        break;
    }
    atomic_preincrement(&table->threads_in_flight);
    
    u64 hash   = 0;
    for(smm i = 0; to_insert.size > i; i++){
        hash = 301 * hash + to_insert.data[i];
    }
    
    struct tempoary_memory temp = begin_tempoary_memory(arena);
    
    unique_string value = push_struct(arena, struct string);
    value->data = (u8 *)push_cstring_from_string(arena, to_insert);
    value->size = to_insert.size;
    
    struct string_table_entry entry_to_insert;
    entry_to_insert.hash = hash;
    entry_to_insert.value = value;
    
    unique_string ret = null;
    
    u64 index = hash & table->mask;
    for(s64 i = 0; i < table->capacity; i++){
        struct string_table_entry *entry = table->entries + index;
        if(!entry->value){
            b32 success = atomic_compare_and_swap_128(&entry->mem, entry_to_insert.mem, &(m128)zero_struct);
            if(success){
                ret = entry->value;
                solidify_tempoary_memory(temp);
                atomic_preincrement(&table->size);
                break;
            }
        }
        
        if(entry->hash == hash){
            if(string_match(*entry->value, to_insert)){
                end_tempoary_memory(temp);
                ret = entry->value;
                break;
            }
        }
        index = (index + 1) & table->mask;
    }
    
    assert(ret);
    
    atomic_predecrement(&table->threads_in_flight);
    return ret;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////

// @cleanup: I think this will now only be used as a _file_ and not as a part of a stack
struct file_stack_node{
    struct file_stack_node *next;
    struct os_file file;
    struct string absolute_file_path;
    u8 *at;
    u8 *end;
    smm line;
    smm column;
    b32 is_system_include;
    
    // pdb debug info
    u32 ipi;
    u32 offset_in_names;
    u32 offset_in_f4;
    m128 md5;
};

struct dll_import_node{
    struct dll_import_node *next;
    struct string name;
    HMODULE module_handle;
    struct ast_list functions;
    // @note: filled in in coff write, NOT in context, as otherwise there might be threading issues
    //        which would lead us to have multiple entries for the same function
};

///////////////////////////////////////////////////////////////////////////////////////////////////////n

struct keyword_table_entry{
    unique_string string;
    enum token_type type;
};

enum compile_stage{
    COMPILE_STAGE_none,
    
    COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries,
    COMPILE_STAGE_parse_function_and_emit_code,
    //COMPILE_STAGE_patch,
    
    COMPILE_STAGE_count,
};

// :globals
global struct{
    
    // :options (read only after initialization)
    b32 want_executable;
    b32 want_pdb;
    b32 print_preprocessed_file;
    
    unique_string entry_point_name;
    
    struct string_list additional_include_directories; // full paths
    struct string_list system_include_directories;     // full paths
    
    struct string working_directory;
    struct string exe_full_path;
    struct string pdb_full_path;
    
    struct{
        struct define_node *first;
        struct define_node *last;
        smm amount;
    } predefines;
    
    struct{
        struct dll_import_node *first;
        struct dll_import_node *last;
        smm amount;
    } dll_imports;
    
    // a perfect hash table to lookup keywords values are the unique strings in 'globals.keywords'
    // hash function is 'pdb_string_hash' because I am lazy
    smm keyword_table_size;
    struct keyword_table_entry *keyword_table;
    
#define INTRINSIC_TABLE_CAPACITY 256
#define INTRINSIC_TABLE_MASK (INTRINSIC_TABLE_CAPACITY - 1)
    struct intrinsic_table{
        struct intrinsic_info{
            unique_string name;
            enum intrinsic_kind kind;
            u8 opcode;
            //u32 return_size;
            //u32 amount_of_arguments;
            //u32 *argument_sizes;
        } *nodes;
        debug_only(smm size;)
    } intrinsic_table;
    
    // global run time structures (have to be thead aware)
    
    struct{
        struct file_stack_node *first;
        smm amount;
    } file_list;
    
    b32 an_error_has_accured;
    
    struct string_table identifier_table;
    struct string_table string_table; // these do not include quotation marks
    
    // @cleanup: maybe put cacheline sized pad around these global structures?
    enum compile_stage compile_stage;
    struct work_queue work_queue_stage_one;
    struct work_queue work_queue_stage_two;
    HANDLE wake_event;
    
    struct sleeper_table sleeper_table;
    
    struct ast_list  local_functions;
    struct ast_table global_declarations;
    struct ast_table compound_types;
    //struct ast_table type_defs; this has to be the same as global_declarations because '(asd)expr' would sleep on (asd) as a expression
    
    // constant global stuff
    struct file_stack_node invalid_file;
    unique_string keywords[AMOUNT_OF_KEYWORDS];
    unique_string keyword_declspec;
    unique_string keyword_cdecl;
    unique_string keyword_stdcall;
    unique_string keyword_dllimport;
    unique_string keyword_align;
    unique_string keyword_noreturn;
    
    // preprocessor keywords
    unique_string keyword_include;
    unique_string keyword_if; // this is also just the usual 'if'
    unique_string keyword_else; // this is also just the usual 'if'
    unique_string keyword_elif;
    unique_string keyword_endif;
    unique_string keyword_ifdef;
    unique_string keyword_ifndef;
    unique_string keyword_error;
    unique_string keyword_define;
    unique_string keyword_undef;
    unique_string keyword_defined;
    unique_string keyword_pragma;
    unique_string keyword___pragma;
    
    unique_string pragma_once;
    
    unique_string keyword__VA_ARGS__;
    unique_string keyword___FILE__;
    unique_string keyword___LINE__;
    
    unique_string unnamed_tag;
    unique_string unnamed_enum;
    unique_string invalid_identifier;
    
    struct patch_node *first_patch;
    
#define members_for_typedef(typedef)    \
    struct string string_for_##typedef; \
    struct token token_##typedef;       \
    struct ast_type typedef_##typedef   \
    
    members_for_typedef(void);
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
    
    // typedef_bool?
    struct ast_type *typedef_void_pointer;
    struct ast_type *typedef_u8_pointer;
    struct ast_type *typedef_s8_pointer;
    
    // :AllocateFunctionLocation
    // from this we allocate atomically where we put function in the text section
    smm text_section_offset_allocator;
    
#if 0
    // we allocate a typeindex for each type to use in the pdb done in register compound type (@cleanup for typedefs)
    smm type_index_allocator;
    struct{
        struct ticket_spinlock type_info_grow_lock;
        smm amount;
        struct memory_buffer *type_infos;
    }type_info_array;
#endif
    
} globals = {
    
#define make_const_typedef(postfix, TOKEN_kind, AST_kind, string, _size) \
    .string_for_##postfix = const_string(string),                        \
    .token_##postfix = {                                                 \
        .type  = TOKEN_kind,                                             \
        .value = &globals.string_for_##postfix,                          \
        .file  = &globals.invalid_file,                                  \
        .line  = 0,                                                      \
        .column = 0,                                                     \
    },                                                                   \
    .typedef_##postfix = {                                               \
        .s = -1,                                                         \
        .size   = _size,                                                 \
        .alignment = _size,                                              \
        .kind   = AST_kind,                                              \
        .token  = &globals.token_##postfix,                              \
    }                                                                    \
    
    make_const_typedef(void, TOKEN_void,     AST_void_type,    "void",               0),
    make_const_typedef(u8,   TOKEN_unsigned, AST_integer_type, "unsigned char",      1),
    make_const_typedef(u16,  TOKEN_short,    AST_integer_type, "unsigned short",     2),
    make_const_typedef(u32,  TOKEN_unsigned, AST_integer_type, "unsigned int",       4),
    make_const_typedef(u64,  TOKEN_long,     AST_integer_type, "unsigned long long", 8),
    
    make_const_typedef(s8,   TOKEN_char,     AST_integer_type, "char",               1),
    make_const_typedef(s16,  TOKEN_short,    AST_integer_type, "short",              2),
    make_const_typedef(s32,  TOKEN_int,      AST_integer_type, "int",                4),
    make_const_typedef(s64,  TOKEN_long,     AST_integer_type, "long long",          8),
    
    make_const_typedef(f32,  TOKEN_float,    AST_float_type,   "float",              4),
    make_const_typedef(f64,  TOKEN_double,   AST_float_type,   "double",             8),
};


///////////////////////////////////////////////////////////////////////////////////////////////////////

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
///////////////////////////////////////////////////////////////////////////////////////////////////////

func struct ast_table ast_table_create(smm initial_capacity){
    struct ast_table ret = zero_struct;
    ret.capacity = initial_capacity;
    ret.mask = ret.capacity - 1;
    smm byte_size = ret.capacity * sizeof(struct sleeper_node);
    ret.nodes = os_allocate_memory(byte_size);
    if(!ret.nodes){ print("Memory failure in sleeper table\n"); os_panic(1);}
    return ret;
}

func struct ast *ast_table_add_or_return_previous_entry(struct ast_table *table, struct ast *ast, unique_string hash){
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
            
            if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
                
                // Lets wait until there are not threads in flight
                while(table->threads_in_flight);
                
                //print_ast_table(table);
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
                    if(!node->hash) continue;
                    
                    u64 slot = xor_shift64((u64)node->hash) & table->mask;
                    for(u64 slot_offset = 0; slot_offset < table->capacity; slot_offset++){
                        struct ast_node *new_node = table->nodes + ((slot + slot_offset) & table->mask);
                        if(new_node->hash) continue;
                        *new_node = *node;
                        break;
                    }
                }
                os_free_memory(old_nodes);
                
                //print_ast_table(table);
            }
            
            // unlock
            table->is_locked_for_growing = false;
        }
        
        break;
    }
    
    atomic_preincrement(&table->threads_in_flight);
    
    assert(hash);
    assert(ast);
    u64 index = (xor_shift64((u64)hash) & table->mask);
    assert(index < table->capacity);
    
    if(table == &globals.compound_types){
        log_print("types ast table insert: %.*s (index %llu)", hash->amount, hash->data, index);
    }else{
        assert(table == &globals.global_declarations);
        log_print(" ast table insert: %.*s (index %llu)", hash->amount, hash->data, index);
    }
    
    struct ast_node to_insert;
    to_insert.hash = hash;
    to_insert.ast = ast;
    
    for(u64 i = 0; i < table->capacity; i++){
        struct ast_node *node = table->nodes + index;
        if(node->ast){
            if(node->hash == hash){
                log_print("   allready inserted");
                
                atomic_predecrement(&table->threads_in_flight);
                return node->ast;
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
            // @note: don't increment, as someone could have just added _us_ in here and we then
            //        want to return the redecl
        }
    }
    
    invalid_code_path;
}

func struct ast *ast_table_get(struct ast_table *table, unique_string hash){
    while(atomic_load(s64, table->is_locked_for_growing));
    
    atomic_preincrement(&table->threads_in_flight);
    assert(hash);
    u64 index = xor_shift64((u64)hash) & table->mask;
    assert(index < table->capacity);
    
    for(u64 i = 0; i < table->capacity; i++){
        struct ast_node *node = table->nodes + index;
        if(!node->ast) {
            atomic_predecrement(&table->threads_in_flight);
            return null;
        }
        if(node->hash == hash){
            atomic_predecrement(&table->threads_in_flight);
            return node->ast;
        }
        
        index += 1;
        index &= table->mask;
    }
    
    invalid_code_path;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
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

struct register_allocator{
    struct emit_location *emit_location_map[16];
    u32 rolling_index;
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////

struct error_report_node{
    struct error_report_node *next;
    struct string error; // @cleanup: this should be a char * I think
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////


struct pragma_once_list_node{
    struct pragma_once_list_node *next;
    struct file_stack_node *file;
};

// @note: thread local datum
struct context{
    // general
    struct memory_arena scratch; // cleared after every thread_do_work
    struct memory_arena* arena; // right now never cleared. Think later about how we can minimize memory usage
    struct thread_info *thread_info;
    
    // tokenizing
    struct{
        struct define_node *data;
        smm amount;
    } predefines;
    
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
    
    struct token_bucket_array tokens;
    struct token_bucket *current_token_bucket;
    smm token_at;
    
    b32 static_if_evaluate_should_skip_undefined_label;
    
    // parsing
    b32 in_lhs_expression;
    s64 ast_serializer;
    struct token invalid_token;
    
    struct ast *ast_stack[1024];
    smm ast_stack_at;
    
    struct ast_list goto_list;
    struct ast_list label_list;
    
    struct ast_scope *current_scope;
    struct ast_function *current_function; // also used in emit
    struct ast_switch *current_switch;
    
    // error stream
    b32 error;
    b32 in_error_report;
    b32 errors_in_this_report; // used to decide if we should set 'context->error' when the report ends
    struct {
        struct error_report_node *first;
        struct error_report_node *last;
    } error_list;
    
    // sleep
    b32 should_sleep;
    struct token *sleep_on;
    enum sleep_purpose sleep_purpose;
    s64 sleep_line;
    struct token *sleeping_ident;
    
    // emiting
    struct emit_pool emit_pool;
    u8 *current_emit_base;
    
    smm current_emit_offset_of_rsp; // done in register_declaration
    //smm max_function_call_stack_needed;
    
    smm max_amount_of_function_call_arguments;
    smm tempoary_stack_allocator;
    smm tempoary_stack_high_water_mark;
    
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
    
    // patching
    struct memory_arena patch_arena; // this is just a zero initialized arena.
    struct{
        struct patch_node *first;
        struct patch_node *last;
    } local_patch_list;
    
    // invalid globals... could be in @cleanup
    
    struct ast_declaration zero_decl;
    struct ast empty_statement;
    struct ast invalid_ast;
    struct token *invalid_identifier;
};

///////////////////////////////////////////////////////////////////////////////////////////

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
    
    sll_push_back(context->local_patch_list, patch_node);
}


///////////////////////////////////////////////////////////////////////////////////////////////

func struct string token_to_string(struct context *context, struct token token){
    switch(token.type){
        case TOKEN_invalid: return token.string;
        
        // single character tokens
        case TOKEN_equals:        return string("=");  // =
        case TOKEN_open_paren:    return string("(");  // (
        case TOKEN_closed_paren:  return string(")");  // )
        case TOKEN_open_curly:    return string("{");  // {
        case TOKEN_closed_curly:  return string("}");  // }
        case TOKEN_open_index:    return string("[");  // [
        case TOKEN_closed_index:  return string("]");  // ]
        case TOKEN_semicolon:     return string(";");  // ;
        case TOKEN_colon:         return string(":");  // :
        case TOKEN_comma:         return string(",");  // ,
        case TOKEN_dot:           return string(".");  // .
        case TOKEN_smaller:       return string("<");  // <
        case TOKEN_bigger:        return string(">");  // >
        case TOKEN_bitwise_not:   return string("~");  // ~
        case TOKEN_logical_not:   return string("!");  // !
        case TOKEN_question_mark: return string("?");  // ?
        
        case TOKEN_plus:          return string("+");  // +
        case TOKEN_minus:         return string("-");  // -
        case TOKEN_and:           return string("&");  // &
        case TOKEN_or:            return string("|");  // |
        case TOKEN_xor:           return string("^");  // ^
        case TOKEN_times:         return string("*");  // *
        case TOKEN_slash:         return string("/");  // /
        case TOKEN_mod:           return string("%");  // %
        case TOKEN_hash:          return string("#");
        //case TOKEN_end_of_file: return string("EOF");     // \0
        
        // double character tokens
        case TOKEN_hashhash:               return string("##");
        case TOKEN_logical_equals:     return string("==");   // ==
        case TOKEN_logical_unequals:   return string("!=");   // ==
        case TOKEN_bigger_equals:      return string(">=");   // >=
        case TOKEN_smaller_equals:     return string("<=");   // <=
        case TOKEN_logical_and:        return string("&&");   // &&
        case TOKEN_logical_or:         return string("||");   // ||
        case TOKEN_right_shift:        return string(">>");   // >>
        case TOKEN_left_shift:         return string("<<");   // <<
        case TOKEN_increment:          return string("++");   // ++
        case TOKEN_decrement:          return string("--");   // --
        case TOKEN_plus_equals:        return string("+=");   // +=
        case TOKEN_minus_equals:       return string("-=");   // -=
        case TOKEN_div_equals:         return string("/=");   // /=
        case TOKEN_mod_equals:         return string("%=");   // %=
        case TOKEN_xor_equals:         return string("^=");   // ^=
        case TOKEN_and_equals:         return string("&=");   // &=
        case TOKEN_or_equals:          return string("|=");   // |=
        case TOKEN_times_equals:       return string("*=");   // *=
        case TOKEN_right_shift_equals: return string(">>=");  // >>
        case TOKEN_left_shift_equals:  return string("<<=");  // <<
        case TOKEN_arrow:              return string("->");   // ->
        
        case TOKEN_dotdotdot:          return string("...");
        case TOKEN_newline:            return string("'\\n'");
        case TOKEN_whitespace:         return string("whitespace");
        
        case TOKEN_comment: return token.string;
        
        case TOKEN_identifier:
        case TOKEN_string_literal: return *token.value;
        
        // @cleanup: check the number flags
        case TOKEN_character_literal:
        case TOKEN_base10_literal: return push_format_string(&context->scratch, "%llu", token.number);
        case TOKEN_hex_literal: return push_format_string(&context->scratch, "0x%llx", token.number);
        case TOKEN_float_literal: return push_format_string(&context->scratch, "%f", token._f64);
        // keywords
        
        case TOKEN_typedef:
        case TOKEN_enum:
        case TOKEN_struct:
        case TOKEN_union:
        case TOKEN_sizeof:
        case TOKEN_alignof:
        case TOKEN_return:
        case TOKEN_const:
        case TOKEN_volatile:
        case TOKEN_static:
        case TOKEN_extern:
        case TOKEN_inline:
        case TOKEN___inline:
        case TOKEN___forceinline:
        case TOKEN_while:
        case TOKEN_if:
        case TOKEN_else:
        case TOKEN_for:
        case TOKEN_do:
        case TOKEN_break:
        case TOKEN_case:
        case TOKEN_default:
        case TOKEN_continue:
        case TOKEN_switch:
        case TOKEN_goto:
        
        // basic types
        //case TOKEN_string:
        case TOKEN_void:
        case TOKEN_char:
        case TOKEN_unsigned:
        case TOKEN_signed:
        case TOKEN_Bool:
        case TOKEN_short:
        case TOKEN_int:
        case TOKEN_long:
        case TOKEN_int8:
        case TOKEN_int16:
        case TOKEN_int32:
        case TOKEN_int64:
        case TOKEN_float:
        case TOKEN_double:
        
        case TOKEN___FUNCTION__: return keyword_strings[token.type - TOKEN_first_keyword];
        
        invalid_default_case(return string(""));
        //default:  os_console_print("DEFAULT(%u)", token.type); break;// @todo
    }
}

// @cleanup: remove this and make expect_token take an error message
func struct string token_type_to_string(enum token_type type){
#define TOKEN_CASE_MACRO(a) case TOKEN_##a : return string(#a)
    switch(type){
        TOKEN_CASE_MACRO(invalid);
        TOKEN_CASE_MACRO(string_literal); // "bla"
        
        TOKEN_CASE_MACRO(float_literal);  // 1.0f); 1.0 1e7 1.f
        TOKEN_CASE_MACRO(character_literal);
        TOKEN_CASE_MACRO(base10_literal);
        TOKEN_CASE_MACRO(hex_literal);
        
        
        TOKEN_CASE_MACRO(identifier);
        
        // keywords
        TOKEN_CASE_MACRO(typedef);
        TOKEN_CASE_MACRO(enum);
        TOKEN_CASE_MACRO(struct);
        TOKEN_CASE_MACRO(union);
        TOKEN_CASE_MACRO(sizeof);
        TOKEN_CASE_MACRO(alignof);
        TOKEN_CASE_MACRO(return);
        TOKEN_CASE_MACRO(static);
        TOKEN_CASE_MACRO(extern);
        TOKEN_CASE_MACRO(while);
        TOKEN_CASE_MACRO(if);
        TOKEN_CASE_MACRO(else);
        TOKEN_CASE_MACRO(for);
        TOKEN_CASE_MACRO(do);
        TOKEN_CASE_MACRO(break);
        TOKEN_CASE_MACRO(case);
        TOKEN_CASE_MACRO(switch);
        TOKEN_CASE_MACRO(goto);
        
        // basic types
        TOKEN_CASE_MACRO(void);
        
        //TOKEN_CASE_MACRO(string);
        default:{
            struct token token;
            token.type = type;
            return token_to_string(null, token);
        }break;
        
        
    }
#undef TOKEN_CASE_MACRO
}

//////////////////////////////////////////////////////////////////////////////////////////////////////

// @note: error nodes are allocated in 'context->scratch' so only valid duing one 'thread_do_work'
func void report_arg_list(struct context *context, b32 new_line, char *format, va_list va){
    struct error_report_node *node = push_struct(&context->scratch, struct error_report_node);
    
    // plus one for the new_line
    node->error.length = vsnprintf(0, 0, format, va) + 1;
    
    // plus one for the null
    node->error.data = push_data(&context->scratch, u8, node->error.length + 1);
    vsnprintf((char *)node->error.data, (int)(node->error.length + 1), format, va);
    if(new_line){
        node->error.data[node->error.length - 1] = '\n';
        node->error.data[node->error.length] = 0;
    }
    sll_push_back(context->error_list, node);
}

func void report_format(struct context *context, b32 new_line, char *format, ...){
    va_list va;
    va_start(va, format);
    report_arg_list(context, new_line, format, va);
    va_end(va);
}

func void report_warning(struct context *context, struct token *token, char *format, ...){
    if(context->should_sleep) return;
    if(token->file->is_system_include) return; // @cleanup: maybe this should be an option
    
    // @copy and paste because var args...
    if(token){
        struct string str = token_to_string(context, *token);
        struct string path = token->file->absolute_file_path;
        report_format(context, false, "%.*s(%u,%u): Warning at '%.*s': ", path.amount, path.data, token->line, token->column, str.length, str.data);
    }
    va_list va;
    va_start(va, format);
    report_arg_list(context, true, format, va);
    va_end(va);
}

func void begin_error_report(struct context *context){
    assert(!context->in_error_report);
    if(!context->error) context->in_error_report = true;
}

func void end_error_report(struct context *context){
    if(!context->error){
        assert(context->in_error_report);
        context->in_error_report = false;
        
        if(context->errors_in_this_report){
            context->error = true;
            context->should_sleep = true;
            context->sleep_line = -1;
            // @hmm: this is just _anything_ random that will never wake up.
            //       maybe we should make a designated token
            context->sleep_on = &globals.token_void;
        }
    }
}

func void report_error(struct context *context, struct token *token, char *format, ...){
    if(context->should_sleep) return;
    if(context->error) return;
    if(token){
        struct string str = token_to_string(context, *token);
        struct string path = token->file->absolute_file_path;
        report_format(context, false, "%.*s(%u,%u): Error at '%.*s': ", path.amount, path.data, token->line, token->column, str.length, str.data);
    }
    
    va_list va;
    va_start(va, format);
    report_arg_list(context, true, format, va);
    va_end(va);
    
    if(!context->in_error_report){
        context->error = true;
        context->should_sleep = true;
        context->sleep_line = -1;
        // @hmm: this is just _anything_ random that will never wake up. maybe we should make a designated token
        context->sleep_on = &globals.token_void;
    }else{
        context->errors_in_this_report += 1;
    }
    os_debug_break();
}

func void print_error_stream(struct context *context){
    if(!context->error_list.first) return; // nothing to do here... don't lock
    begin_counter(print_error_list);
    
    static struct ticket_spinlock spinlock;
    ticket_spinlock_lock(&spinlock);
    for(struct error_report_node *node = context->error_list.first; node; node = node->next){
        os_print_string((char *)node->error.data, node->error.size);
    }
    ticket_spinlock_unlock(&spinlock);
    sll_clear(context->error_list); // done in :zero_parser as well
    end_counter(print_error_list);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

func struct string push_token_string(struct context *context, struct token *token, b32 print_whitespace_and_comments){
    if(token->type == TOKEN_comment){
        if(print_whitespace_and_comments){
            return push_format_string(&context->scratch, "%.*s", token->string.amount, token->string.data);
        }else{
            return string(" ");
        }
    }else if(token->type == TOKEN_whitespace){
        if(print_whitespace_and_comments){
            return push_format_string(&context->scratch, "%.*s", token->string.amount, token->string.data);
        }else{
            return string(" ");
        }
    }else if(token->type == TOKEN_newline){
        return string("\n");
    }else if(token->type == TOKEN_string_literal){
        struct string str = token_to_string(context, *token);
        return push_format_string(&context->scratch, "\"%.*s\"", str.amount, str.data);
    }else{
        return token_to_string(context, *token);
    }
}

func void print_token(struct context *context, struct token *token, b32 print_whitespace_and_comments){
    struct string s = push_token_string(context, token, print_whitespace_and_comments);
    print("%.*s", s.length, s.data);
}

///////////////////////////////////////
#include "float_parse.c"
///////////////////////////////////////
#include "tokenize.c"
//////////////////////////////////////

func struct string push_type_string(struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type);
func void push_type_string__inner(struct string_list *list, struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type){
    switch(type->kind){
        case AST_void_type:
        case AST_float_type:
        case AST_integer_type:{
            string_list_prefix(list, scratch, *type->token->value);
        }break;
        case AST_union: case AST_struct: case AST_enum:{
            struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
            string_list_prefix(list, scratch, *compound->identifier);
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
            string_list_prefix(list, scratch, *unresolved->sleeping_on->value);
            
            if(*unresolved->type_prefix){
                string_list_prefix(list, scratch, string(" "));
                string_list_prefix(list, scratch, string_from_cstring(unresolved->type_prefix));
            }
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
            struct ast_array_type *arr = cast(struct ast_array_type *)type;
            
            if(list->amount_of_strings){
                string_list_prefix(list, scratch, string("("));
                string_list_postfix(list, scratch, string(")"));
            }
            
            struct string array_postfix = push_format_string(scratch, "[%d]", arr->amount_of_elements);
            string_list_postfix(list, scratch, array_postfix);
            
            push_type_string__inner(list, arena, scratch, arr->element_type);
            
        }break;
        invalid_default_case();
    }
}

// if arena == scratch this requres an additional copy
// @cleanup: we really want to have this take 'parser'
func struct string push_type_string(struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *type){
    struct tempoary_memory temp = begin_tempoary_memory(scratch);
    
    struct string_list list = zero_struct;
    push_type_string__inner(&list, arena, scratch, type);
    struct string ret = string_list_flatten(list, arena);
    
    end_tempoary_memory(temp);
    
    if(arena == scratch){
        ret = push_string_copy(arena, ret);
    }
    
    return ret;
}

//////////////////////////////////

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


func void ast_list_zero(struct ast_list *list){
    list->first = null;
    list->last  = null;
    list->count = 0;
}

////////////////
// work_queue //
////////////////

func void work_queue_append_list(struct work_queue *queue, struct work_queue_entry *list_begin, struct work_queue_entry *list_end, smm amount){
    // first update the 'work_entries_in_flight', so this is not tempoary 0.(or in gerneral less, then it should)
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

//////////////////////////

// @cleanup: can we just put enum and union in here and it's free?
enum sleep_purpose{
    SLEEP_invalid,
    
    SLEEP_on_struct,
    SLEEP_on_decl,
    
    SLEEP_count,
};

func void sleeper_table_maybe_grow(struct sleeper_table *table){
    while(true){
        if(atomic_load(s64, table->is_locked_for_growing)) continue;
        
        if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
            // ***
            
            //spin until WE set table->is_locked_for_growing
            void *old_is_locked = atomic_compare_and_swap(&table->is_locked_for_growing, (void *)true, (void *)false);
            if(old_is_locked) continue;
            // we are the thread that set 'is_locked_for_growing' to true
            
            // we could have set the condition ironiously if the actuall growing thead finished while
            // we were at ***, so lets check the condition again
            
            if(table->amount_of_nodes + 1 > (table->capacity >> 1)){
                
                // Lets wait until there are not threads in flight
                while(table->threads_in_flight);
                
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
                    if(!node->hash) continue;
                    
                    u64 slot = xor_shift64(node->hash) & table->mask;
                    for(u64 slot_offset = 0; slot_offset < table->capacity; slot_offset++){
                        struct sleeper_node *new_node = table->nodes + ((slot + slot_offset) & table->mask);
                        if(new_node->hash) continue;
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

func void sleeper_table_add(struct sleeper_table *table, struct work_queue_entry *entry, u64 hash){
    sleeper_table_maybe_grow(table);
    
    atomic_preincrement(&table->threads_in_flight);
    
    assert(hash);
    assert(entry);
    
    assert(entry->description != WORK_invalid && entry->description < WORK_count);
    u64 index = xor_shift64(hash) & table->mask;
    assert(index < table->capacity);
    
    struct sleeper_node to_insert;
    to_insert.hash = hash;
    to_insert.first_sleeper = entry;
    entry->next = null;
    
    for(u32 i = 0; i < table->capacity; i++){
        struct sleeper_node *node = table->nodes + index;
        if(node->hash){
            if(node->hash == hash){
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

func struct work_queue_entry *sleeper_table_delete(struct sleeper_table *table, u64 hash){
    sleeper_table_maybe_grow(table);
    atomic_preincrement(&table->threads_in_flight);
    
    assert(hash);
    u64 index = xor_shift64(hash) & table->mask;
    assert(index < table->capacity);
    
    struct sleeper_node to_insert;
    to_insert.hash = hash;
    to_insert.first_sleeper = 0;
    
    for(u64 i = 0; i < table->capacity; i++){
        struct sleeper_node *node = table->nodes + index;
        if(!node->hash){
            b32 success = atomic_compare_and_swap_128(&node->mem, to_insert.mem, &(m128)zero_struct);
            if(success) {
                atomic_add((s64 *)&table->amount_of_nodes, 1);
                log_print("         added dummy!");
                
                atomic_predecrement(&table->threads_in_flight);
                return null;
            }
            else continue;
        }
        
        if(node->hash == hash){
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

/////////////////////////////////

func unique_string sleep_hash_get_sleep_on(u64 sleep_hash){
    return cast(unique_string)(sleep_hash & ((1ull << 44) - 1));
}

func enum sleep_purpose sleep_hash_get_purpose(u64 sleep_hash){
    return (enum sleep_purpose)(sleep_hash >> 44);
}

func u64 get_sleep_hash(unique_string sleep_on, enum sleep_purpose purpose){
    // @note: on windoes the upperbits of user space addresses are never set, so we use them for the purpose
    assert(!((cast(u64)sleep_on)&(cast(u64)purpose << 44)));
    assert(purpose != SLEEP_invalid && purpose < SLEEP_count);
    
    return(cast(u64)sleep_on | (cast(u64)purpose << 44));
}

func void wake_up_sleepers(unique_string sleep_on, enum sleep_purpose purpose){
    log_print("   waking sleepers for: %.*s", sleep_on->amount, sleep_on->data);
    
    assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
    
    struct work_queue_entry *first = sleeper_table_delete(&globals.sleeper_table, get_sleep_hash(sleep_on, purpose));
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

//////////////////////////////////

func struct ast_type *lookup_compound_type(struct context *context, unique_string ident){
    assert(ident);
    for(struct ast_scope *it = context->current_scope; it; it = it->parent){
        for(u32 i = 0; i < it->amount_of_compound_types; i++){
            struct ast_compound_type *type = it->compound_types[i];
            if(ident == type->identifier){
                return &type->base;
            }
        }
    }
    return cast(struct ast_type *)ast_table_get(&globals.compound_types, ident);
}

func struct ast_declaration *lookup_declaration(struct context *context, unique_string ident){
    assert(ident);
    begin_counter(identifier_lookup);
    
    for(struct ast_scope *it = context->current_scope; it; it = it->parent){
        for(u32 i = 0; i < it->amount_of_declarations; i++){
            struct ast_declaration *decl = it->declarations[i];
            if(ident == decl->identifier){
                end_counter(identifier_lookup);
                return decl;
            }
        }
    }
    
    end_counter(identifier_lookup);
    // look it up in global scope
    return cast(struct ast_declaration *)ast_table_get(&globals.global_declarations, ident);
}

func struct ast_declaration *lookup_typedef(struct context *context, struct token *type_name, b32 silent){
    struct ast_declaration *decl = lookup_declaration(context, type_name->value);
    if(!decl) return null;
    
    if(decl->base.kind != AST_typedef){
        if(silent) return null;
        
        report_error(context, type_name, "Expected a type.");
        return null;
    }
    
    return decl;
}

func struct ast_type *types_are_equal(struct context *context, struct ast_type *wanted, struct ast_type *given);
func struct ast_declaration *parser_register_declaration(struct context *context, struct ast_declaration *decl){
    if(context->should_sleep) return decl;
    
    struct ast_scope *scope = context->current_scope;
    
    if(scope){
        // @cleanup: how do declarations not have identifiers???? explain
        struct ast_declaration *redecl = null;
        
        if(decl->identifier) redecl = lookup_declaration(context, decl->identifier);
        if(redecl){
            // :Error
            begin_error_report(context);
            report_warning(context, decl->base.token, "Declaration hides previons declaration.");
            report_warning(context, redecl->base.token, "Here is the previous declaration.");
            end_error_report(context);
            //return decl;
        }
        
        u32 index = scope->amount_of_declarations++;
        if(index ==  scope->current_max_amount_of_declarations){
            u32 new_max = max_of(2 * scope->current_max_amount_of_declarations, 8);
            struct ast_declaration **new_array = push_data(context->arena, struct ast_declaration *, new_max);
            memcpy(new_array, scope->declarations, sizeof(scope->declarations[0])* scope->current_max_amount_of_declarations);
            scope->current_max_amount_of_declarations = new_max;
            scope->declarations = new_array;
        }
        
        assert(index < scope->current_max_amount_of_declarations);
        scope->declarations[index] = decl;
    }else{
        // we are at global scope: register it globally
        struct ast_declaration *redecl = (struct ast_declaration *)ast_table_add_or_return_previous_entry(&globals.global_declarations, &decl->base, decl->identifier);
        
        if(redecl){
            
            if(redecl->base.kind == AST_typedef && decl->base.kind == AST_typedef){
                if(types_are_equal(context, redecl->type, decl->type)){
                    return redecl;
                }
            }else if(redecl->base.kind == AST_function && decl->base.kind == AST_function){
                struct ast_function *redecl_function = cast(struct ast_function *)redecl;
                struct ast_function *function = cast(struct ast_function *)decl;
                
                if(!types_are_equal(context, decl->type, redecl->type)){
                    begin_error_report(context);
                    report_error(context, decl->base.token, "Redeclaration of function with different type.");
                    report_error(context, redecl->base.token, "...here is the previous declaration");
                    end_error_report(context);
                    return decl;
                }
                
                // if the new declaration is not a definition we are always fine.
                if(function->is_defined){
                    struct ast_function *defined_function = function->is_defined;
                    
                    if(atomic_compare_and_swap(&redecl_function->is_defined, defined_function, null)){
                        // both are defined, this is only legal for dllimport and intrinsics
                        if((redecl_function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport) &&
                           (defined_function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport)){
                            return redecl;
                        }
                        
                        if((redecl_function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) &&
                           (defined_function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic)){
                            return redecl;
                        }
                        
                        begin_error_report(context);
                        report_error(context, redecl_function->base.token, "Multiple definitions.");
                        report_error(context, defined_function->base.token, "... here is the previous definition.");
                        end_error_report(context);
                        return redecl;
                    }
                }
                return redecl;
                
            }else if((redecl->flags & DECLARATION_FLAGS_is_enum_member) &&
                     (decl->flags   & DECLARATION_FLAGS_is_enum_member)){
                assert(redecl->assign_expr);
                assert(decl->assign_expr);
                assert(redecl->assign_expr->kind == AST_integer_literal);
                assert(decl->assign_expr->kind == AST_integer_literal);
                struct ast_integer_literal *decl_lit   = cast(struct ast_integer_literal *)decl->assign_expr;
                struct ast_integer_literal *redecl_lit = cast(struct ast_integer_literal *)redecl->assign_expr;
                
                if(decl_lit->_s32 == redecl_lit->_s32){
                    return redecl; // this is fine, we redefine the literal as it self.
                }
            }
            
            begin_error_report(context);
            report_error(context, decl->base.token, "Redeclaration.");
            report_error(context, redecl->base.token, "Previous declaration was here.");
            end_error_report(context);
            return decl;
        }
        
        wake_up_sleepers(decl->identifier, SLEEP_on_decl);
    }
    return decl;
}

func b32 register_compound_type(struct context *context, struct ast_type *type, unique_string ident){
    if(context->should_sleep) return false;
    
    if(context->current_scope){
        struct ast_type *redecl = lookup_compound_type(context, ident);
        if(redecl){
            // if we are of the same type it's fine
            if(types_are_equal(context, (struct ast_type *)redecl, type)) return true;
            report_error(context, type->token, "Redeclaration of type.");
            report_warning(context, redecl->token, "Previous declaration was here.");
            return false;
        }
        struct ast_scope *scope = context->current_scope;
        u32 index = scope->amount_of_compound_types++;
        if(index == scope->current_max_amount_of_compound_types){
            u32 new_max = max_of(2 * scope->current_max_amount_of_compound_types, 8);
            struct ast_compound_type **new_array = push_data(context->arena, struct ast_compound_type *, new_max);
            memcpy(new_array, scope->compound_types, sizeof(scope->compound_types[0])* scope->current_max_amount_of_compound_types);
            scope->current_max_amount_of_compound_types = new_max;
            scope->compound_types = new_array;
        }
        scope->compound_types[index] = cast(struct ast_compound_type *)type;
        
        return true;
    }
    
    struct ast *redecl = ast_table_add_or_return_previous_entry(&globals.compound_types, cast(struct ast*)type, ident);
    if(redecl){
        // if we have same token we are the same type so we can
        if(redecl->token == type->token) return true;
        
        // if we are of the same type it's fine
        if(types_are_equal(context, (struct ast_type *)redecl, type)) return true;
        
        report_error(context, type->token, "Redeclaration of type.");
        report_warning(context, redecl->token, "Previous declaration was here.");
        return false;
    }
    
    wake_up_sleepers(ident, SLEEP_on_struct);
    return true;
}

func smm parser_emit_memory_location(struct context *context, smm size, smm alignment, struct token *location){
    assert(size >= 0);
    // @cleanup: do we eventually allow declarations of size 0?
    if(size == 0) return context->current_emit_offset_of_rsp;
    
    context->current_emit_offset_of_rsp = cast(smm)align_up(context->current_emit_offset_of_rsp, alignment);
    context->current_emit_offset_of_rsp += size;
    
    if(context->current_emit_offset_of_rsp > max_s32){
        report_error(context, location, "Too many local variables. At most '%llx' bytes of stack are allowed.", max_u32);
    }
    
    // :MemoryLocations we currently emit '[rbp - offset]' so we want to return the offset
    //                  AFTER it is incremented
    return context->current_emit_offset_of_rsp;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////

func struct intrinsic_info *lookup_intrinsic(unique_string ident){
    u64 hash = xor_shift64((u64)ident);
    struct intrinsic_info *info;
    for(smm i = 0; i < INTRINSIC_TABLE_CAPACITY; i++){
        info = globals.intrinsic_table.nodes + ((hash + i) & INTRINSIC_TABLE_MASK);
        if(info->name == ident) break; // found it
        if(!info->name) break; // did not find it
    }
    if(!info->name) return null;
    return info;
}

func void register_intrinsic(struct memory_arena *arena, struct string name, enum intrinsic_kind kind, u8 opcode){
    unique_string unique_name = string_table_insert(&globals.identifier_table, name, arena);
    u64 hash = xor_shift64((u64)unique_name);
    
    struct intrinsic_info *info;
    for(u32 i = 0; i < INTRINSIC_TABLE_CAPACITY; i++){
        info = globals.intrinsic_table.nodes + ((hash + i) & INTRINSIC_TABLE_MASK);
        if(!info->name) break;
        assert(info->name != unique_name); // make sure we don't add anyone twice
    }
    assert(!info->name);
    assert(globals.intrinsic_table.size++ < (INTRINSIC_TABLE_CAPACITY/2));
    
    info->name = unique_name;
    info->kind = kind;
    info->opcode = opcode;
    //info->return_size = return_size;
    //info->amount_of_arguments = amount_of_arguments;
    
    
    //va_list va;
    //va_start(va, amount_of_arguments);
    //info->argument_sizes = collect_varargs_into_u32_array(arena, amount_of_arguments, va);
    //va_end(va);
    
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
#include "parse.c"
//////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////
#include "asm_emit.c"
//////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////
#include "explain.c"
//////////////////////////////////////////////////////////////////////////////////////////////////////

struct exception_function_record{
    unique_string ident;
    u8 *memory_location;
    u8 *end_address;
    
    smm size_of_prolog;
    smm alloc_offset;
    smm stack_size;
};

// @note: idear was to use this when the user chooses in memory execution. Kinda dead right now
func LONG __stdcall my_awesome_exception_handler(struct _EXCEPTION_POINTERS *exception_info){
    struct memory_arena *arena = &(struct memory_arena)zero_struct;
    
    // @cleanup: why do we have exception_function_record for dllimports?
    
    u64 amount_of_functions = 0;
    for(u64 i = 0; i < globals.global_declarations.capacity; i++){
        struct ast_node *it = globals.global_declarations.nodes + i;
        if(it->ast && it->ast->kind == AST_function){
            amount_of_functions++;
        }
    }
    
    struct exception_function_record *data =
        push_data(arena, struct exception_function_record, amount_of_functions);
    
    u64 count = 0;
    for(u64 i = 0; i < globals.global_declarations.capacity; i++){
        struct ast_node *it = globals.global_declarations.nodes + i;
        if(!it->ast || it->ast->kind != AST_function) continue;
        struct exception_function_record *record = data + count++;
        struct ast_function *function = cast(struct ast_function *)it->ast;
        if(function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport) continue;
        
        record->ident           = function->identifier;
        record->memory_location = function->memory_location;
        record->end_address     = function->memory_location + function->byte_size;
        record->size_of_prolog  = function->size_of_prolog;
        record->alloc_offset    = function->rsp_subtract_offset;
        record->stack_size      = function->stack_space_needed;
    }
    
    // @speed: we could quicksort here and then binary search this thing...
    struct _CONTEXT *state = exception_info->ContextRecord;
    u8 *rip = cast(u8 *)state->Rip;
    u8 *rsp = cast(u8 *)state->Rsp;
    
    b32 first_time = true;
    b32 continue_looping;
    
    do{
        continue_looping = false;
        
        //print("rip: %llu\n", rip);
        //print("rsp: %p\n",   rsp);
        
        for(u64 i = 0; i < amount_of_functions; i++){
            struct exception_function_record *it = data + i;
            if(it->memory_location <= rip && rip <= it->end_address){
                
                continue_looping = true;
                if(first_time){
                    first_time = false;
                    print("Exception thrown in function '%.*s'. \n\n", it->ident->amount, it->ident->data);
                    print("Call stack:\n");
                }
                
                print("   %.*s()\n", it->ident->amount, it->ident->data);
                
                if(rip > (it->memory_location + it->alloc_offset)){
                    rsp += it->stack_size;
                }
                rip = *cast(u8 **)rsp;
                rsp += 8; // for poping rip on return
                break;
            }
        }
    }while(continue_looping);
    
    if(first_time){
        print("no callstack data.\n");
        print("Maybe we crashed and not you... oups!\n");
    }
    
    print("\nExiting.\n");
    
    ExitProcess(1);
}

func void flush_local_patch_list(struct context *context){
    if(context->local_patch_list.first){
        // @clenaup: @speed: this should go away we dont need to sync this we should just spinn the treads a second time
        struct patch_node *list;
        do{
            list = atomic_load(struct patch_node *, globals.first_patch);
            context->local_patch_list.last->next = list;
        }while(atomic_compare_and_swap(&globals.first_patch, context->local_patch_list.first, list) != list);
    }
}

// 'out_decl' return the declaration, that defines the memory we are writing to...
func smm offset_in_type_for_constant_lhs_expression(struct context *context, struct ast *lhs, struct ast_declaration **out_decl){
    smm offset = 0;
    b32 should_loop = true;
    while(should_loop){
        switch(lhs->kind){
            case AST_member:{
                struct ast_dot_or_arrow *member = cast(struct ast_dot_or_arrow *)lhs;
                offset += member->member_decl->offset_in_type;
                lhs = member->lhs;
            }break;
            case AST_unary_deref:{ // @note: this is a desugared array subscript
                struct ast_unary_op *deref = cast(struct ast_unary_op *)lhs;
                if(deref->operand->kind == AST_binary_plus){
                    struct ast_binary_op *add = cast(struct ast_binary_op *)deref->operand;
                    if(add->rhs->kind != AST_integer_literal){
                        // :Error
                        report_error(context, deref->base.token, "subscript is not constant.");
                        return 0;
                    }
                    
                    offset += integer_literal_as_u64(add->rhs);
                    lhs = add->lhs;
                }else{
                    lhs = deref->operand;
                }
                
                // @cleanup: I am to dumb right now to figure out if we are done at this point.
                if(lhs->kind == AST_unary_address){
                    struct ast_unary_op *address = cast(struct ast_unary_op *)lhs;
                    lhs = address->operand;
                }
                
            }break;
            case AST_cast:{
                struct ast_unary_op *cast = cast(struct ast_unary_op *)lhs;
                lhs = cast->operand;
            }break;
            case AST_identifier:{
                struct ast_identifier *ident = cast(struct ast_identifier *)lhs;
                if(out_decl) *out_decl = ident->decl;
                should_loop = false;
            }break;
            invalid_default_case();
        }
    }
    return offset;
}

func u8 *evaluate_initializer(struct memory_arena *arena, struct context *context, struct ast *initializer, struct ast_declaration *decl){
    //if(string_match(*decl->identifier, string("globals"))) os_debug_break();
    
    struct ast_list assignment_list = zero_struct;
    if(initializer->kind == AST_struct_or_array_literal){
        struct ast_struct_or_array_literal *lit = cast(struct ast_struct_or_array_literal *)initializer;
        assignment_list = lit->assignment_list;
    }else{
        struct ast_list_node *node = push_struct(&context->scratch, struct ast_list_node);
        node->value = initializer;
        sll_push_back(assignment_list, node);
        assignment_list.count++;
    }
    
    push_zero_align(arena, decl->type->alignment);
    u8 *data = push_zero_data(arena, u8, decl->type->size);
    
    for_ast_list(assignment_list){
        assert(it->value->kind == AST_assignment);
        struct ast_binary_op *assign = cast(struct ast_binary_op *)it->value;
        
        // evaluate the left hand side of the assignment
        smm offset = offset_in_type_for_constant_lhs_expression(context, assign->lhs, null);
        struct ast_type *lhs_type = assign->lhs->resolved_type;
        
        // evaluate the right hand side of the assignment
        
        struct ast *rhs = assign->rhs;
        struct ast_type *rhs_type = rhs->resolved_type;
        
        retry:;
        
        switch(rhs->kind){
            case AST_integer_literal:{
                // @note: the rhs does not have to fit: if we have 'u8 a = 0xffff;' this only gives a warning.>
                assert(offset + lhs_type->size <= decl->type->size);
                u64 value = integer_literal_to_bytes(rhs);
                memcpy(data + offset, &value, lhs_type->size);
            }break;
            case AST_string_literal:{
                struct ast_string_literal *str = cast(struct ast_string_literal *)rhs;
                if(lhs_type->kind == AST_array_type){
                    // @cleanup: not sure if this is just an error?
                    assert(offset + str->value->size + 1 <= decl->type->size);
                    memcpy(data + offset, str->value->data, str->value->size);
                    data[offset + str->value->size] = 0; // zero terminate
                    break; // break from the switch
                }
                
                assert(offset + 8 <= decl->type->size);
                if(globals.want_executable){
                    emit_patch(context, PATCH_absolute, &str->base, 0, decl, offset, -1);
                }else{
                    u8 *value = str->value->data;
                    memcpy(data + offset, &value, rhs_type->size);
                }
            }break;
            case AST_identifier:{
                struct ast_identifier *ident = cast(struct ast_identifier *)rhs;
                if(ident->decl->base.kind == AST_function){
                    emit_patch(context, PATCH_absolute, &ident->decl->base, 0, ident->decl, offset, -1);
                }else{
                    report_error(context, ident->base.token, "Non constant Identifer in compile-time expression.");
                    return null;
                }
            }break;
            case AST_struct_or_array_literal:{
                // @note: this should be resolved, as we merge the assignment_lists of sub struct_literals
                //        while parsing.
                assert(false);
            }break;
            case AST_float_literal:{
                assert(offset + rhs_type->size <= decl->type->size);
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
                memcpy(data + offset, copy_from, f->base.resolved_type->size);
            }break;
            case AST_unary_address:{
                struct ast_unary_op *address = cast(struct ast_unary_op *)rhs;
                // @cleanup: should this work with ast struct or array literal?
                if(address->operand->kind == AST_string_literal){
                    rhs = address->operand;
                    rhs_type = address->operand->resolved_type;
                    goto retry;
                }
                struct ast_declaration *rhs_decl = null;
                smm offset_in_rhs = offset_in_type_for_constant_lhs_expression(context, address->operand, &rhs_decl);
                // @cleanup: want_executable check?
                assert(offset + 8 <= decl->type->size);
                
                //if(string_match(*decl->identifier, string("globals"))) os_debug_break();
                
                // @note: we want to write into
                //      [decl->memory_address + offset]
                // and we want to write
                //      &[rhs_decl->memory_address + offset_in_rhs]
                emit_patch(context, PATCH_absolute, &rhs_decl->base, offset_in_rhs, decl, offset, -1);
            }break;
            case AST_cast:{
                struct ast_unary_op *cast = cast(struct ast_unary_op *)rhs;
                rhs = cast->operand;
                rhs_type = cast->operand->resolved_type;
                goto retry;
            }break;
            default:{
                report_error(context, assign->base.token, "assignment is not constant.");
                return null;
            }break;
        }
    }
    
    return data;
}

#include "coff_writer.c"

#if (DUMP_OUT_PDB || READ_PDB_AFTER_EMITING_IT)
#include "pdb_dumper.c"
#endif

func void parser_do_work(struct context *context, struct work_queue_entry *work){
    // :reset_context :zero_context :clear_context
    context->current_emit_offset_of_rsp = 0;
    context->current_scope              = null;
    context->current_switch             = null;
    context->sleep_on                   = null;
    context->sleeping_ident             = null;
    context->local_patch_list.first     = 0;
    context->local_patch_list.last      = 0;
    //context->spill_allocator          = 0; zeroed before every statement anyway
    context->ast_stack_at               = 0;
    context->should_sleep               = false;
    context->error                      = false;
    sll_clear(context->error_list);
    
    ast_list_zero(&context->label_list);
    ast_list_zero(&context->goto_list);
    
    switch(work->description){
        case WORK_tokenize_file:{
            struct work_tokenize_file *work_tokenize_file = cast(struct work_tokenize_file *)work->data;
            
            log_print("tokenizing file %.*s", work_tokenize_file->absolute_file_path.amount, work_tokenize_file->absolute_file_path.data);
            
            assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
            
            memset(&context->define_table, 0, sizeof(context->define_table));
            for(smm i = 0; i < context->predefines.amount; i++){
                register_define(context, &context->predefines.data[i]);
            }
            sll_clear(context->pragma_once_file_list);
            
            begin_counter(tokenize_and_preprocess);
            struct token_bucket_array tokenized_file = file_tokenize_and_preprocess(context, work_tokenize_file->absolute_file_path);
            end_counter(tokenize_and_preprocess);
            
            if(PRINT_PREPROCESSED_FILE || globals.print_preprocessed_file){
                // @sigh: apperantly windows fails when strings get 'too long' and does not print them, 
                //        so we just lock here I guess...
                static struct ticket_spinlock print_preprocessed_file_lock; 
                ticket_spinlock_lock(&print_preprocessed_file_lock);
                
                print("/**************** START OF FILE '%.*s' ************************/\n", work_tokenize_file->absolute_file_path.amount, work_tokenize_file->absolute_file_path.data);
                for(struct token_bucket *bucket = tokenized_file.first; bucket; bucket = bucket->next){
                    for(smm i = 0; i < bucket->amount; i++){
                        print_token(context, &bucket->tokens[i], true);
                    }
                }
                print("/**************** END OF FILE '%.*s' ************************/\n", work_tokenize_file->absolute_file_path.amount, work_tokenize_file->absolute_file_path.data);
                
                ticket_spinlock_unlock(&print_preprocessed_file_lock);
            }
            
            // clean the list.... @ugh @cleanup: maybe just assert this and make sure that no code ever produces
            // zero token buckets
            for(struct token_bucket *it = tokenized_file.first; it;){
                struct token_bucket *next = it->next;
                if(it->amount == 0) dll_remove(tokenized_file, it);
                it = next;
            }
            
            if(dll_is_empty(tokenized_file)){
                return; // nothing to do if the file is empty
            }
            
            if(context->should_sleep) goto handle_sleep_or_error;
            
            begin_token_array(context, tokenized_file, 0);
            
            // @note: as this just segments the text file into pieces, we require macro expension to allready
            //        have happend.
            while(in_current_token_array(context)){
                // skip to a relavant token
                while(peek_token_eat_raw(context, TOKEN_whitespace) ||
                      peek_token_eat_raw(context, TOKEN_comment) ||
                      peek_token_eat_raw(context, TOKEN_newline));
                
                struct token_bucket *start_bucket = context->current_token_bucket;
                
                //if(get_current_token(parser)->line == 1239) os_debug_break();
                // file ended in whitespace
                if(!start_bucket) break;
                
                smm start_at = context->token_at;
                
                b32 got_equals = false;
                while(in_current_token_array(context)){
                    
                    if(peek_token_eat(context, TOKEN_enum) || peek_token_eat(context, TOKEN_struct) || peek_token_eat(context, TOKEN_union)){
                        // @ugh skip declspec as stuct __declspec() { is allowed
                        while(get_current_token(context)->value == globals.keyword_declspec){
                            next_token(context);
                            expect_token(context, TOKEN_open_paren);
                            skip_until_tokens_are_ballanced(context, TOKEN_open_paren, TOKEN_closed_paren);
                        }
                        peek_token_eat(context, TOKEN_identifier); // the name of the struct/enum/union
                        if(peek_token_eat(context, TOKEN_open_curly)){
                            skip_until_tokens_are_ballanced(context, TOKEN_open_curly, TOKEN_closed_curly);
                        }
                    }
                    
                    if(peek_token_eat(context, TOKEN_open_paren)){
                        skip_until_tokens_are_ballanced(context, TOKEN_open_paren, TOKEN_closed_paren);
                        continue; // continue here for things like __declspec(???) struct{
                    }
                    
                    if(peek_token_eat(context, TOKEN_equals)){
                        got_equals = true;
                    }
                    
                    // @cleanup: this whole thing is really hacky...
                    // if there was not '=' meaning we are in a lhs expression.
                    // {...} is the end of a function if not preceeded by 'enum', 'struct' or 'union'.
                    // note that (struct a){} is not allowed.
                    if(!got_equals && peek_token_eat(context, TOKEN_open_curly)){
                        skip_until_tokens_are_ballanced(context, TOKEN_open_curly, TOKEN_closed_curly);
                        break;
                    }
                    
                    if(peek_token_eat(context, TOKEN_semicolon)){
                        break;
                    }
                    
                    b32 alive = in_current_token_array(context);
                    // ensure we make progress
                    if(alive) next_token(context);
                    
                    if(!alive || !in_current_token_array(context)){
                        report_error(context, prev_token(context), "Invalid token in scope. Maybe you missed a '}'.");
                        goto handle_sleep_or_error;
                    }
                }
                
                if(context->should_sleep) goto handle_sleep_or_error;
                
                
                struct token_bucket *end_bucket = context->current_token_bucket;
                smm end_token_at = context->token_at;
                
                if(!end_bucket){
                    end_bucket = context->tokens.last;
                }else if(end_token_at == 0){
                    struct token_bucket *prev = end_bucket->prev;
                    end_bucket->prev = null;
                    end_bucket = prev;
                    end_bucket->next = null;
                }else{
                    assert(end_bucket->amount > end_token_at);
                    
                    // add a new bucket in the place of 'end_bucket'
                    struct token_bucket *new_bucket = push_struct(context->arena, struct token_bucket);
                    new_bucket->tokens = end_bucket->tokens + end_token_at;
                    new_bucket->amount = end_bucket->amount - end_token_at;
                    
                    new_bucket->next = end_bucket->next;
                    if(end_bucket->next){
                        end_bucket->next->prev = new_bucket;
                    }else{
                        context->tokens.last = new_bucket;
                    }
                    
                    // truncate the 'end_bucket'
                    end_bucket->amount = end_token_at;
                    end_bucket->next = null;
                    
                    context->tokens.first = new_bucket;
                    context->current_token_bucket = new_bucket;
                    context->token_at = 0;
                }
                
                // send ['start_bucket','end_bucket'] on its way
                struct parse_work *parse_work = push_struct(context->arena, struct parse_work);
                parse_work->tokens.first = start_bucket;
                parse_work->tokens.last  = end_bucket;
                parse_work->start_at = start_at;
                work_queue_push_work(context, &globals.work_queue_stage_one, WORK_parse_global_scope_entry, parse_work);
                
                for(struct token_bucket *it = start_bucket; it; it = it->next){
                    if(it->prev){
                        assert(it->prev->next == it);
                    }
                    if(it->next){
                        assert(it->next->prev == it);
                    }else{
                        assert(it == end_bucket);
                    }
                }
            }
            
            assert(!in_current_token_array(context));
        }break;
        case WORK_parse_global_scope_entry:{
            log_print("parsing a global scope entry");
            
            assert(globals.compile_stage == COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries);
            
            struct parse_work *parse_work = cast(struct parse_work *)work->data;
            
            begin_token_array(context, parse_work->tokens, parse_work->start_at);
            
            struct declaration_list declaration_list = parse_declaration_list(context, null, null);
            if(context->should_sleep) goto handle_sleep_or_error;
            
            // might not be the full type e.g. int (*a)
            struct ast_type *lhs_type = declaration_list.type_specifier;
            
            if(sll_is_empty(declaration_list)){
                // for struct this might be a struct declaration, in which case  we are done.
                if(lhs_type->kind == AST_struct || lhs_type->kind == AST_union){
                    struct ast_compound_type *compound = cast(struct ast_compound_type *)lhs_type;
                    if(!compound->identifier){
                        report_warning(context, compound->base.token, "Anonymous %s declaration does not declare anything.", compound->base.kind == AST_union ? "union" : "struct");
                    }
                }else if(declaration_list.type_specifier_defined_type && declaration_list.type_specifier_defined_type->kind == AST_enum){
                    // these are fine. we could exectly check for enum{} I guess
                }else if(declaration_list.type_specifier->kind == AST_unresolved_type){
                    // these are fine, we got 'struct unresolved;'
                    // @cleanup: maybe check for 'typedef's here? (warn on 'asd', where 'typedef struct unresolved asd;')
                }else{
                    // we got 'u32;'
                    os_debug_break();
                    report_warning(context, lhs_type->token, "Declaration does not define anything.");
                }
                expect_token(context, TOKEN_semicolon);
                assert(!in_current_token_array(context));
                
                return;
            }
            
            for(struct declaration_node *node = declaration_list.first; node; node = node->next){
                struct ast_declaration *decl = node->decl;
                if(decl->base.kind == AST_declaration){
                    if(decl->assign_expr){
                        decl->memory_location = evaluate_initializer(context->arena, context, decl->assign_expr, decl);
                    }else{
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
                    }
                }
            }
            
            if(declaration_list.last->decl->base.kind == AST_function && peek_token_eat(context, TOKEN_open_curly)){
                struct ast_function *function = cast(struct ast_function *)declaration_list.last->decl;
                
                parse_work->function = function;
                parse_work->tokens.first = context->current_token_bucket;
                parse_work->start_at = context->token_at;
                
                work_queue_push_work(context, &globals.work_queue_stage_two, WORK_parse_function_body, parse_work);
            }else{
                expect_token(context, TOKEN_semicolon);
                if(context->should_sleep) goto handle_sleep_or_error;
                assert(!in_current_token_array(context));
            }
            
            flush_local_patch_list(context);
            return;
        }break;
        case WORK_parse_function_body:{
            struct parse_work *parse_work = cast(struct parse_work *)work->data;
            struct ast_function *function = parse_work->function->is_defined;
            assert(function->base.kind == AST_function);
            assert(!function->memory_location);
            context->current_function = function;
            
            // @note: this will never sleep. only error/sleep on something that never resolves.
            assert(globals.compile_stage == COMPILE_STAGE_parse_function_and_emit_code);
            // @WARNING: we use this fact and fuck with the token array of the function if we encounter a local
            //           function definition.
            
            log_print("parsing: %.*s", function->identifier->amount, function->identifier->data);
            
            begin_token_array(context, parse_work->tokens, parse_work->start_at);
            
            struct ast_scope *scope = parser_push_new_scope(context, SCOPE_FLAG_none);
            
            for_ast_list(function->type->argument_list){
                assert(it->value->kind == AST_declaration);
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                if(maybe_resolve_declaration_of_unresolved_type_or_sleep(context, decl)){
                    // :unresolved_types
                    goto handle_sleep_or_error;
                }
                
                parser_register_declaration(context, decl);
            }
            
            function->scope = parse_imperative_scope(context);
            if(context->should_sleep) goto handle_sleep_or_error;
            
            assert(!in_current_token_array(context));
            
            parser_scope_pop(context, scope);
            
            for(struct ast_list_node *node1 = context->goto_list.first; node1; node1 = node1->next){
                struct ast_goto *ast_goto = cast(struct ast_goto *)node1->value;
                b32 found = false;
                struct ast_list_node *prev_node = null;
                for(struct ast_list_node *node2 = context->label_list.first; node2; node2 = node2->next){
                    struct ast_label *label = cast(struct ast_label *)node2->value;
                    if(label->ident == ast_goto->ident){
                        ast_goto->label_to_goto = label;
                        found = true;
                        break;
                    }
                    prev_node = node2;
                }
                
                if(!found){
                    report_error(context, ast_goto->base.token, "'goto' to undefined label '%.*s'.", ast_goto->ident->amount, ast_goto->ident->data);
                    goto handle_sleep_or_error;
                }
            }
            
            begin_counter(emit_code_for_function);
            // :emit   <--- THIS IS WHERE THE MAGIC HAPPENS
            emit_code_for_function(context, function);
            end_counter(emit_code_for_function);
            
            for_ast_list(function->static_variables){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                if(decl->assign_expr){
                    decl->memory_location = evaluate_initializer(context->arena, context, decl->assign_expr, decl);
                }
            }
            
            // @cleanup: this should go away
            flush_local_patch_list(context);
            // @note: we cannot actually wake anyone, as they allready _wake_ immediately upon going to sleep. this is done, because someone might wake up, while we unwind the stack to sleep on them.
            context->current_function = null;
        }break;
        invalid_default_case();
    }
    
    return;
    
    handle_sleep_or_error:;
    if(context->error) return;
    
    if(globals.an_error_has_accured) return; // @cleanup: is this what we want? when does this get set?
    
    unique_string sleep_on = context->sleep_on->value;
    assert(sleep_on);
    work->sleeping_on = context->sleep_on;
    log_print("   sleeping on: %.*s", sleep_on->amount, sleep_on->data);
    //if(string_match(*sleep_on, string("__VA_ARGS__"))) os_debug_break();
    
    // @cleanup: inline this so we don't call sleeper_table_add somewhere else
    if(work->description == WORK_parse_global_scope_entry){
        struct parse_work *parse = cast(struct parse_work *)work->data;
        parse->sleeping_ident = context->sleeping_ident;
        assert(!parse->sleeping_ident || parse->sleeping_ident->type == TOKEN_identifier);
    }
    sleeper_table_add(&globals.sleeper_table, work, get_sleep_hash(sleep_on, context->sleep_purpose));
}

func struct token *push_dummy_token(struct memory_arena *arena, struct string token_string, enum token_type token_type){
    struct string *string = push_struct(arena, struct string);
    *string = token_string;
    
    struct token *token = push_struct(arena, struct token);
    token->type = token_type;
    token->value = string;
    
    return token;
}

func void init_context(struct context *context, struct thread_info *info, struct memory_arena *arena){
    u64 thread_index = cast(u64)info->thread_index;
    
    context->thread_info = info;
    context->arena = arena;
    context->ast_serializer = (thread_index << 48);
    context->register_sp = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_SP, 8);
    context->gpr_allocator.emit_location_map[REGISTER_SP] = null;
    context->register_bp = emit_location_loaded(context, REGISTER_KIND_gpr, REGISTER_BP, 8);
    context->gpr_allocator.emit_location_map[REGISTER_BP] = null;
    
    context->empty_statement.kind = AST_empty_statement;
    context->empty_statement.token = push_dummy_token(arena, string(";"), TOKEN_semicolon);
    set_resolved_type(&context->empty_statement, &globals.typedef_void, null);
    
    context->invalid_identifier = push_dummy_token(arena, *globals.invalid_identifier, TOKEN_identifier);
    
    context->invalid_token.file = &globals.invalid_file;
    
    smm emit_pool_capacity = mega_bytes(100);
    struct os_virtual_buffer emit_pool_buf = os_reserve_memory(0, emit_pool_capacity);
    if(!emit_pool_buf.memory){ print("memory error!\n"); os_panic(1); }
    context->emit_pool.base     = emit_pool_buf.memory;
    context->emit_pool.current  = emit_pool_buf.memory;
    context->emit_pool.end      = emit_pool_buf.memory;
    context->emit_pool.reserved = emit_pool_capacity;
    
    context->predefines.data   = push_data(arena, struct define_node, globals.predefines.amount);
    context->predefines.amount = globals.predefines.amount;
    {
        smm i = 0;
        for(struct define_node *it = globals.predefines.first; it; it = it->next, i++){
            assert(i < context->predefines.amount);
            context->predefines.data[i] = *it;
            assert(sll_is_empty(it->arguments));
            
            context->predefines.data[i].tokens.data = push_array_copy(arena, struct token, it->tokens.data, it->tokens.amount);
            context->predefines.data[i].tokens.amount = it->tokens.amount;
            
        }
    }
}

// returns whether or not we should continue;
func b32 do_one_work(struct context *context){
    struct work_queue *queue;
    
    switch(globals.compile_stage){
        case COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries:{
            queue = &globals.work_queue_stage_one;
        }break;
        case COMPILE_STAGE_parse_function_and_emit_code:{
            queue = &globals.work_queue_stage_two;
        }break;
        invalid_default_case(queue = null);
    }
    
    struct work_queue_entry *work = work_queue_get_work(queue);
    
    if(work){
        struct tempoary_memory scratch_temp = begin_tempoary_memory(&context->scratch);
        
        switch(work->description){
            case WORK_tokenize_file:            begin_counter(work_tokenize);           break;
            case WORK_parse_global_scope_entry: begin_counter(work_global_scope_entry); break;
            case WORK_parse_function_body:      begin_counter(work_parse_function);     break;
        }
        
        parser_do_work(context, work);
        assert(&queue->work_entries_in_flight > 0);
        atomic_add(&queue->work_entries_in_flight, -1);
        
        switch(work->description){
            case WORK_tokenize_file:            end_counter(work_tokenize);           break;
            case WORK_parse_global_scope_entry: end_counter(work_global_scope_entry); break;
            case WORK_parse_function_body:      end_counter(work_parse_function);     break;
        }
        
        if(context->error){
            atomic_store(b32, globals.an_error_has_accured, true);
        }
        // if we should not sleep, then emit all warnings, if we errored print the errors
        if(!context->should_sleep || context->error) print_error_stream(context);
        
        end_tempoary_memory(scratch_temp);
        return true;
    }
    return false;
}


func u32 work_thread_proc(void *param){
    struct thread_info *info = param;
    
    log_print("starting thread %d", info->thread_index);
    
    struct memory_arena main_arena = create_memory_arena(os_reserve_memory(0, mega_bytes(100)), 2.0f, mega_bytes(1));
    
    struct context *context = &(struct context)zero_struct;
    init_context(context, info, &main_arena);
    
    while(true){
        b32 should_continue = do_one_work(context);
        if(!should_continue){
            log_print("good night!");
            WaitForSingleObject(globals.wake_event, INFINITE);
            log_print("woke up");
        }
        
        assert(context->arena->temp_count == 0);
    }
    
    return 0;
}

func struct ast_function *get_entry_point_or_error(struct context *context){
    unique_string entry_point = globals.entry_point_name;
    struct ast_function *function = cast(struct ast_function *)ast_table_get(&globals.global_declarations, entry_point);
    log_print("amount of global declarations %d\n", globals.global_declarations.amount_of_nodes);
    if(!function){
        print("Error: Specified entry point '%.*s' not found. \n", entry_point->length, entry_point->data);
        globals.an_error_has_accured = true;
        os_debug_break();
        return null;
    }
    if(function->base.kind != AST_function){
        report_error(context, function->base.token, "Specified entry point '%.*s' is not a function.", entry_point->length, entry_point->data);
        globals.an_error_has_accured = true;
        os_debug_break();
        return null;
    }
    
    // @cleanup: also make sure that the entrypoint is not like dll extern of only declared or something...
    // @cleanup: how to do the thing where we at compile time want to pass arguments?
    
    b32 type_matches = true;
    if(function->type->return_type != &globals.typedef_s32 &&
       function->type->return_type != &globals.typedef_void) type_matches = false;
    if(!function->type->argument_list.count == 0) type_matches = false;
    if(!type_matches){
        report_error(context, function->base.token, "Entry point has to be of fit definition 'int %.*s()'", entry_point->amount, entry_point->data);
        globals.an_error_has_accured = true;
        os_debug_break();
        return null;
    }
    
    return function;
}


func struct string hacky_find_newest_system_include_path(struct memory_arena *arena, char *path){
    WIN32_FIND_DATAA find_data;
    char c_file_name[sizeof(find_data.cFileName)];
    
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
                    // take the last one, they are lexically sorted, so the last one _should_ be most recent
                    memcpy(c_file_name, find_data.cFileName, sizeof(find_data.cFileName));
                    finds++;
                }
            }
        }while(FindNextFileA(search_handle, &find_data));
        
        FindClose(search_handle);
    }
    
    if(finds < 3){
        // couldn't find this.
        print("Error: Implicit system include path '%s' was not found.\n", path);
        print("         This means that some system includes will not be found.\n");
        print("         You can specify system includes manually via '-SI <dir>'\n");
        print("         and get rid of this warning by using '-nostdlib'.\n");
        print("                (Sorry CRT support still needs some work).\n");
        os_panic(1);
    }
    
    smm length = cstring_length(path);
    return push_format_string(arena, "%.*s/%s", length - 2, path, c_file_name);
}

func b32 path_is_directory(char *path){
    u32 INVALID_FILE_ATTRIBUTES = u32_max;
    u32 FILE_ATTRIBUTE_DIRECTORY = 0x10;
    u32 file_attributes = GetFileAttributesA(path);
    return (file_attributes != INVALID_FILE_ATTRIBUTES) && (file_attributes & FILE_ATTRIBUTE_DIRECTORY);
}

func void add_system_include_directory(struct memory_arena *arena, struct string absolute, struct string relative, b32 user_specified){
    struct string directory_path = concatinate_file_paths(arena, absolute, relative);
    canonicalize_slashes(directory_path);
    if(!path_is_directory((char *)directory_path.data)){
        if(user_specified){
            print("Error: Specified system include directoy '%s' is not a directory.\n", directory_path.data);
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
    
    // ignore whether this was unique or not
    string_list_add_uniquely(&globals.system_include_directories, arena, directory_path);
}


// :main
int main(int argc, char**argv){
    
    if(argc == 1){
        // if we get invoked with only 'pbc' print the usage
        print("Usage: %s [option...] filename...\n", argv[0]);
        print("For more information see '%s --help'\n", argv[0]);
        return 0;
    }
    
#if 0
    print("defines:\n");
    print("    _M_X64 %d\n", _M_X64);
    print("    _MSC_VER %d\n", _MSC_VER);
#endif
    
#if time_perfomance
    u64 begin_cycle_time = __rdtsc();
    begin_counter(total);
#endif
    begin_counter(startup);
    f64 begin_time = os_get_time_in_seconds();
    
    struct memory_arena _arena = create_memory_arena(os_reserve_memory(0, mega_bytes(100)), 2.0f, mega_bytes(1));
    struct memory_arena *arena = &_arena;
    
    struct string working_directory;
    // GetCurrentDirectory with 0 returns the size of the buffer including the null terminator
    working_directory.length = GetCurrentDirectoryA(0, null) - 1;
    working_directory.data = push_data(arena, u8, working_directory.length + 1);
    GetCurrentDirectoryA(cast(u32)working_directory.length + 1, working_directory.data);
    canonicalize_slashes(working_directory); // @cleanup: not sure where to put this
    globals.working_directory = working_directory;
    print("working directory is %.*s.\n", working_directory.length, working_directory.data);
    
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
    
    struct predefine_node{
        struct predefine_node *next;
        struct string to_define;
        struct string define_to;
    };
    
    struct{
        struct predefine_node *first;
        struct predefine_node *last;
    } predefines = zero_struct;
    
    b32 no_standard_library = false;
    
    // @note: does count the main thread
    u64 thread_count = 1;
    
    // :command_line_options
    char *out_path = null;
    char *c_entry_point_name = null;
    for(int i = 1; i < argc; i++){
        char *arg = argv[i];
        b32 is_argument = false;
        if(arg[0] == '/'){
            arg += 1;
            is_argument = true;
        }
        if(arg[0] == '-'){
            is_argument = true;
            arg += 1;
            if(arg[0] == '-') arg += 1;
        }
        
        if(!is_argument){
            struct work_tokenize_file *work = push_struct(arena, struct work_tokenize_file);
            struct string path = string_from_cstring(arg);
            if(path_is_absolute(path)){   
                work->absolute_file_path = path;
            }else{
                work->absolute_file_path = concatinate_file_paths(arena, working_directory, path);
            }
            
            if(os_load_file((char *)work->absolute_file_path.data, 0, 0).file_does_not_exist){
                print("Error: Specified input file '%s' does not exist.\n", work->absolute_file_path.data);
                return 1;
            }else{
                print("   %.*s\n", work->absolute_file_path.amount, work->absolute_file_path.data);
            }
            
            struct work_queue_entry *work_entry = push_struct(arena, struct work_queue_entry);
            work_entry->description = WORK_tokenize_file;
            work_entry->data  = work;
            
            sll_push_back(files_to_parse, work_entry);
            files_to_parse.amount += 1;
            continue;
        }
        
        if(cstring_match(arg, "?") || cstring_match(arg, "h") || cstring_match(arg, "help")){
            // @cleanup: look at the cl /? that one is really good
            print("usage: %s [option...] filename...\n", argv[0]);
            static const char help_string[] = ""
                "\n"
                "Options:\n"
                "  -out <filename>     Specify an output path. The file extensions '.exe' and '.pdb'\n"
                "                      get automatically appended. Default is 'a'.\n"
                "  -entry <name>       Specify the entry point symbol for the program. Default is '_start'.\n"
                "  -I <dir>            Specify an additional include directory.\n"
                "  -D<name>{=<text>}   Define a macro. Equivalent to '#define <name> <text>'.\n"
                "  -L <filename>       Specify a dynamic library (.dll) to link to. Has to end in '.dll'.\n"
                "  -nostdlib           Do not link to 'ucrt.dll' and disable system include paths.\n"
                "  -SI <dir>           Specify a system include path. (<>-include path.)\n"
                "  -j<number>          Specify the amount of threads to use. Default is '1'.\n"
                "  -P                  Print preprocessed files. \n"
                "\n";
            
            os_print_string((char *)help_string, sizeof(help_string) - 1);
            return 0;
        }else if(cstring_match(arg, "nostdlib")){
            no_standard_library = true;
        }else if(cstring_match(arg, "o") || cstring_match_case_insensitive(arg, "out")){
            if(i + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[i]);
                return 1;
            }
            
            if(out_path){
                print("Error: Output file specified twice.\n");
                return 1;
            }
            out_path = argv[++i];
        }else if(cstring_match_case_insensitive(arg, "entry")){
            if(i + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[i]);
                return 1;
            }
            if(c_entry_point_name){
                print("Error: Entry point specified twice.\n");
                return 1;
            }
            c_entry_point_name = argv[++i];
        }else if(cstring_match(arg, "I") || cstring_match(arg, "SI")){
            if(i + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[i]);
                return 1;
            }
            
            struct string file_path = string_from_cstring(argv[++i]);
            // @cleanup: check that this is actually a directory
            canonicalize_slashes(file_path);
            if(path_is_relative(file_path)){
                file_path = concatinate_file_paths(arena, working_directory, file_path);
            }
            
            if(cstring_match(arg, "SI")){
                add_system_include_directory(arena, file_path, string(""), true);
            }else{
                if(!path_is_directory((char *)file_path.data)){
                    print("Error: Specified additional include '%s' directiory not found.\n", file_path.data);
                    return 1;
                }
                
                if(string_list_add_uniquely(&globals.additional_include_directories, arena, file_path)){
                    print("Error: Additional include directory '%.*s' specified twice.", file_path.size, file_path.data);
                    return 1;
                }
            }
        }else if(cstring_match(arg, "L")){
            if(i + 1 == argc){
                print("Error: Expected argument after '%s'.\n", argv[i]);
                return 1;
            }
            struct dll_import_node *node = push_struct(arena, struct dll_import_node);
            char *name = argv[++i];
            smm length = cstring_length(name);
            if(length < 4 || !cstring_match(name + length - 4, ".dll")){
                print("Error: Argument to '-L' must be a '.dll' file. '.lib' files are currently not supported.\n");
                return 1;
            }
            
            node->name = string_from_cstring(name);
            node->module_handle = LoadLibraryA(name);
            if(!node->module_handle){
                print("Error: Could not load '%s'\n", name);
                return  1;
            }
            
            print("   %.*s\n", node->name.length, node->name.data);
            
            sll_push_back(globals.dll_imports, node);
            globals.dll_imports.amount += 1;
        }else if(arg[0] == 'D'){ // arg[0] is always valid as there is at least a zero terminator
            char *start = arg + 1;
            char *end = start;
            while(u8_is_alpha_numeric((u8)*end)){end++;}
            struct string to_define = create_string((u8 *)start, end - start);
            
            if(to_define.size == 0){
                print("Error: Expected an identifier to immediatly follow '-D', e.g. '-Ddefine=1'.\n");
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
            
            struct predefine_node *node = push_struct(arena, struct predefine_node);
            node->define_to = define_to;
            node->to_define = to_define;
            
            sll_push_back(predefines, node);
        }else if(arg[0] == 'j'){
            u64 number = 0;
            char *it = arg + 1;
            while(*it){
                if(*it < '0' || '9' < *it){
                    print("Error: Could not parse specified thread count '%s'\n", arg);
                    return 1;
                }
                number = number * 10 + (*it - '0');
            }
            
            thread_count = number;
            if(!thread_count){
                print("Error: Thread count must be at least one '%s'\n", arg);
                return  1;
            }
            
        }else if(cstring_match(arg, "P")){
            if(globals.print_preprocessed_file){
                print("Waring: -P specified twice\n");
            }
            globals.print_preprocessed_file = true;
        }else{
            print("Error: Unknown command-line option '%s'.\n", argv[i]);
            return 1;
        }
    }
    
    if(!c_entry_point_name) c_entry_point_name = "_start";
    
    if(sll_is_empty(files_to_parse)){
        print("Error: No input files specified\n");
        return 1;
    }
    
    if(!no_standard_library){
        struct dll_import_node *node = globals.dll_imports.first;
        for(;node; node = node->next){
            if(string_match(node->name, string("ucrtbased.dll"))){
                break;
            }
        }
        
        if(!node){
            node = push_struct(arena, struct dll_import_node);
            node->name = string("ucrtbased.dll");
            node->module_handle = LoadLibraryA("ucrtbased.dll");
            if(!node->module_handle){
                print("Error: could not load 'ucrtbased.dll'. Please check your path.\n");
                print("       To compile without dynamically linking to the CRT, use \n");
                print("       the command-line option '-nostdlib'.\n");
                return 1;
            }
            
            sll_push_back(globals.dll_imports, node);
            globals.dll_imports.amount += 1;
        }
        
        struct string NETFXSDK = hacky_find_newest_system_include_path(arena, "C:/Program Files (x86)/Windows Kits/NETFXSDK/*");
        add_system_include_directory(arena, NETFXSDK, string("/include/um"), false);
        
        struct string KITS = hacky_find_newest_system_include_path(arena, "C:/Program Files (x86)/Windows Kits/10/include/*");
        add_system_include_directory(arena, KITS, string("/ucrt"),   false);
        add_system_include_directory(arena, KITS, string("/shared"), false);
        add_system_include_directory(arena, KITS, string("/um"),     false);
        add_system_include_directory(arena, KITS, string("/winrt"),  false);
        
        struct string MSVC = hacky_find_newest_system_include_path(arena, "c:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/*");
        add_system_include_directory(arena, MSVC, string("/include"),        false);
        add_system_include_directory(arena, MSVC, string("/ATLMFC/include"), false);
        
    }
        
    // currently time_performance is broken with threads
    assert(!time_perfomance || thread_count == 1);
    
    struct context *context = &(struct context)zero_struct;
    context->arena = arena;
    
    {  // :init_globals :globals init globals
        globals.invalid_file.absolute_file_path = string("*predefined token*");
        
        globals.string_table     = string_table_init(1 << 10);
        globals.identifier_table = string_table_init(1 << 10);
        
        struct string_table *t = &globals.identifier_table;
        //unique_string token_anonymous = string_table_insert(t, string("<anonymous>"));
        globals.keyword_dllimport       = string_table_insert(t, string("dllimport"), arena);
        globals.keyword_align           = string_table_insert(t, string("align"), arena);
        globals.keyword_noreturn        = string_table_insert(t, string("noreturn"), arena);
        globals.keyword_declspec        = string_table_insert(t, string("__declspec"), arena);
        globals.keyword_cdecl           = string_table_insert(t, string("__cdecl"), arena);
        globals.keyword_stdcall         = string_table_insert(t, string("__stdcall"), arena);
        
        // preprocessor keywords
        globals.keyword_include = string_table_insert(t, string("include"), arena);
        globals.keyword_if      = string_table_insert(t, string("if"), arena);
        globals.keyword_else    = string_table_insert(t, string("else"), arena);
        globals.keyword_elif    = string_table_insert(t, string("elif"), arena);
        globals.keyword_endif   = string_table_insert(t, string("endif"), arena);
        globals.keyword_ifdef   = string_table_insert(t, string("ifdef"), arena);
        globals.keyword_ifndef  = string_table_insert(t, string("ifndef"), arena);
        globals.keyword_error   = string_table_insert(t, string("error"), arena);
        globals.keyword_define  = string_table_insert(t, string("define"), arena);
        globals.keyword_undef   = string_table_insert(t, string("undef"), arena);
        globals.keyword_defined = string_table_insert(t, string("defined"), arena);
        globals.keyword_pragma  = string_table_insert(t, string("pragma"), arena);
        
        // msvc thing, to make pragmas not at the beginning of a line
        globals.keyword___pragma  = string_table_insert(t, string("__pragma"), arena);
        
        // pragma directives
        globals.pragma_once  = string_table_insert(t, string("once"), arena);
        
        globals.keyword__VA_ARGS__ = string_table_insert(t, string("__VA_ARGS__"), arena);
        globals.keyword___LINE__   = string_table_insert(t, string("__LINE__"), arena);
        globals.keyword___FILE__   = string_table_insert(t, string("__FILE__"), arena);
        
        globals.unnamed_tag        = string_table_insert(t, string("<unnamed tag>"), arena);
        globals.unnamed_enum       = string_table_insert(t, string("<unnamed enum>"), arena);
        globals.invalid_identifier = string_table_insert(t, string("<invalid identifier>"), arena);
        
        // create the perfect hash table 'keyword'-> 'TOKEN_KIND'
        for(u32 i = 0; i < AMOUNT_OF_KEYWORDS; i++){
            globals.keywords[i] = string_table_insert(t, keyword_strings[i], arena);
        }
        
        begin_counter(create_perfect_keyword_table);
        for(u32 size = u32_round_up_to_next_power_of_two(AMOUNT_OF_KEYWORDS); ; size <<= 1){
            struct tempoary_memory temp = begin_tempoary_memory(arena);
            
            struct keyword_table_entry *table = push_zero_data(arena, struct keyword_table_entry, size);
            b32 should_coninue = false;
            for(u32 i = 0; i < AMOUNT_OF_KEYWORDS; i++){
                u64 hash = xor_shift64((u64)globals.keywords[i]);
                if(table[hash & (size - 1)].string){
                    end_tempoary_memory(temp);
                    should_coninue = true;
                    break;
                }
                table[hash & (size - 1)].string = globals.keywords[i];
                table[hash & (size - 1)].type   = TOKEN_first_keyword + i;
            }
            if(should_coninue) continue;
            
            globals.keyword_table_size = size;
            globals.keyword_table = table;
            solidify_tempoary_memory(temp);
            break;
        }
        //print("keyword table size %d\n", globals.keyword_table_size);
        end_counter(create_perfect_keyword_table);
        
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
        
        {   // :sleeper_table
            globals.sleeper_table.capacity = (1 << 8);
            globals.sleeper_table.mask = globals.sleeper_table.capacity - 1;
            smm byte_size = globals.sleeper_table.capacity * sizeof(struct sleeper_node);
            globals.sleeper_table.nodes = os_allocate_memory(byte_size);
            if(!globals.sleeper_table.nodes){ print("Memory failure in sleeper table\n"); os_panic(1);}
        }
        
        // :ast_tables
        globals.global_declarations = ast_table_create(1 << 8);
        globals.compound_types      = ast_table_create(1 << 8);
        
        
        globals.intrinsic_table.nodes = push_data(arena, struct intrinsic_info, INTRINSIC_TABLE_CAPACITY);
        
        // register_intrinsic(arena, name, intrinsic_kind, opcode);
        
        // miscellaneous intrinsics
        register_intrinsic(arena, string("__va_start"), INTRINSIC_KIND_va_start, 0);
        register_intrinsic(arena, string("__rdtsc"), INTRINSIC_KIND_rdtsc, 0);
        register_intrinsic(arena, string("_mm_pause"), INTRINSIC_KIND_pause, 0);
        
        // atomic intrinsics
        register_intrinsic(arena, string("_InterlockedCompareExchange64"), INTRINSIC_KIND_InterlockedCompareExchange64, 0);
        register_intrinsic(arena, string("_InterlockedCompareExchange128"), INTRINSIC_KIND_InterlockedCompareExchange128, 0);
        register_intrinsic(arena, string("_InterlockedIncrement64"), INTRINSIC_KIND_interlocked_inc_dec_64, FF_INCREMENT_REGM);
        register_intrinsic(arena, string("_InterlockedDecrement64"), INTRINSIC_KIND_interlocked_inc_dec_64, FF_DECREMENT_REGM);
        register_intrinsic(arena, string("_InterlockedExchangeAdd64"), INTRINSIC_KIND_interlocked_fetch_op_64, 0);
        
        // double percision moves
        register_intrinsic(arena, string("_mm_set_sd"), INTRINSIC_KIND_set_scalar_double, 0);
        
        // double percision arithmetic
        register_intrinsic(arena, string("_mm_add_sd"), INTRINSIC_KIND_scalar_double_op, ADD_XMM);
        register_intrinsic(arena, string("_mm_add_pd"), INTRINSIC_KIND_packed_double_op, ADD_XMM);
        register_intrinsic(arena, string("_mm_sub_sd"), INTRINSIC_KIND_scalar_double_op, SUB_XMM);
        register_intrinsic(arena, string("_mm_sub_pd"), INTRINSIC_KIND_packed_double_op, SUB_XMM);
        register_intrinsic(arena, string("_mm_mul_sd"), INTRINSIC_KIND_scalar_double_op, MUL_XMM);
        register_intrinsic(arena, string("_mm_mul_pd"), INTRINSIC_KIND_packed_double_op, MUL_XMM);
        register_intrinsic(arena, string("_mm_div_sd"), INTRINSIC_KIND_scalar_double_op, DIV_XMM);
        register_intrinsic(arena, string("_mm_div_pd"), INTRINSIC_KIND_packed_double_op, DIV_XMM);
        register_intrinsic(arena, string("_mm_min_sd"), INTRINSIC_KIND_scalar_double_op, MIN_XMM);
        register_intrinsic(arena, string("_mm_min_pd"), INTRINSIC_KIND_packed_double_op, MIN_XMM);
        register_intrinsic(arena, string("_mm_max_sd"), INTRINSIC_KIND_scalar_double_op, MAX_XMM);
        register_intrinsic(arena, string("_mm_max_pd"), INTRINSIC_KIND_packed_double_op, MAX_XMM);
        
        // double percision logicals
        register_intrinsic(arena, string("_mm_and_pd"),    INTRINSIC_KIND_packed_double_op, AND_XMM);
        register_intrinsic(arena, string("_mm_andnot_pd"), INTRINSIC_KIND_packed_double_op, AND_NOT_XMM);
        register_intrinsic(arena, string("_mm_or_pd"),     INTRINSIC_KIND_packed_double_op, OR_XMM);
        register_intrinsic(arena, string("_mm_xor_pd"),    INTRINSIC_KIND_packed_double_op, XOR_XMM);
        
        // double percision comparisons
    }
    
    // predefines
    struct predefine_node *predefine_M_X64 = &(struct predefine_node){0, string("_M_X64"), string("100")};
    sll_push_back(predefines, predefine_M_X64);
    
    struct predefine_node *predefine_CRTIMP = &(struct predefine_node){0, string("_CRTIMP"), string("__declspec(dllimport)")};
    if(!no_standard_library) sll_push_back(predefines, predefine_CRTIMP);
    
    struct predefine_node *predefine__PBC__ = &(struct predefine_node){0, string("__PBC__"), string("1")};
    sll_push_back(predefines, predefine__PBC__);
    
    // register all _predefines_ the threads will then copy these to have them locally.
    for(struct predefine_node *node = predefines.first; node; node = node->next){
        // @note: if I eventually don't want to waste memory here, I could do this in each thread into 'scratch'
        //        and then solidify them there...
        // @cleanup:
        struct file_stack_node hack_file = zero_struct;
        hack_file.at  = node->define_to.data;
        hack_file.end = node->define_to.data + node->define_to.size;
        hack_file.absolute_file_path = string("*predefined macro*");
        
        smm define_amount = 0;
        smm define_capacity = 8;
        struct token *define_tokens = push_data(arena, struct token, define_capacity);
        
        while(hack_file.at < hack_file.end){
            struct token cur = tokenize_one_token_raw(context, &hack_file);
            
            if(define_amount + 1 > define_capacity){
                smm new_capacity = 2 * define_capacity;
                struct token *new_define_tokens = push_data(arena, struct token, new_capacity);
                memcpy(new_define_tokens, define_tokens, define_amount * sizeof(struct token));
                define_tokens = new_define_tokens;
                define_capacity = new_capacity;
            }
            
            define_tokens[define_amount++] = cur;
        }
        
        struct define_node *define = push_struct(arena, struct define_node);
        define->tokens.data   = define_tokens;
        define->tokens.amount = define_amount;
        define->name          = string_table_insert(&globals.identifier_table, node->to_define, arena);
        
        struct token *token = push_struct(arena, struct token);
        token->type   = TOKEN_identifier;
        token->file   = &globals.invalid_file;
        token->value  = define->name;
        token->line   = 0;
        token->column = 0;
        
        define->defined_token = push_dummy_token(arena, *define->name, TOKEN_identifier);
        
        sll_push_back(globals.predefines, define);
        globals.predefines.amount++;
    }
    
    {   // :test_code
        struct tempoary_memory temp = begin_tempoary_memory(arena);
#if DUMP_OUT_PDB
        push_align(arena, 0x1000);
        struct os_file file = load_file_into_arena("build/out.pdb", arena);
        //struct os_file file = load_file_into_arena("build/out.pdb", arena);
        //struct os_file file = load_file_into_arena("build/vc140.pdb", arena);
        //print_byte_range(file.memory, file.memory + 0x4000);
        
        read_pdb(arena, file);
#endif
        end_tempoary_memory(temp);
    }
    
    
#if PRINT_ADDITIONAL_INCLUDE_DIRECTORIES
    print("Additional include directiories:\n");
    for(struct string_list_node *node = globals.additional_include_directories.list.first; node; node = node->next){
        print("    %.*s\n", node->string.amount, node->string.data);
    }
    print("\nSystem include directiories:\n");
    for(struct string_list_node *node = globals.system_include_directories.list.first; node; node = node->next){
        print("    %.*s\n", node->string.amount, node->string.data);
    }
    print("\n");
#endif
    
#if PRINT_PREDEFINED_MACROS
    print("\nPredefines are:\n");
    for(struct define_node *define = globals.predefines.first; define; define = define->next){
        print_define(context, define, "    #define ");
    }
    print("\n");
#endif
    
    
    // :options
    globals.want_executable = true; // currently has to be true, in memory execution is dead for now
    globals.want_pdb = true;
    globals.entry_point_name = string_table_insert(&globals.identifier_table, string_from_cstring(c_entry_point_name), arena);
    
    if(out_path){
        struct string path = string_from_cstring(out_path);
        if(path_is_relative(path)){
            path = concatinate_file_paths(arena, working_directory, path);
        }
        globals.exe_full_path = string_concatinate(arena, path, string(".exe"));
        globals.pdb_full_path = string_concatinate(arena, path, string(".pdb"));
    }else{
        struct string file_name = string("a");
        globals.exe_full_path = push_format_string(arena, "%.*s/%.*s.exe", working_directory.amount, working_directory.data, file_name.amount, file_name.data);
        globals.pdb_full_path = push_format_string(arena, "%.*s/%.*s.pdb", working_directory.amount, working_directory.data, file_name.amount, file_name.data);
    }
    
    // @note: thead '0' is the main thread
    struct thread_info *thread_infos = push_data(arena, struct thread_info, thread_count);
    
    for(u32 i = 1; i < thread_count; i++){
        struct thread_info *thread_info = thread_infos + i;
        thread_info->thread_index = i;
    }
    
    init_context(context, thread_infos + 0, arena);
    
    // get it going:
    globals.compile_stage = COMPILE_STAGE_tokenize_files_and_parse_global_scope_entries;
    
    work_queue_append_list(&globals.work_queue_stage_one, files_to_parse.first, files_to_parse.last, files_to_parse.amount);
    
    // @note: thead '0' is the main thread, so just start all other ones
    for(u32 i = 1; i < thread_count; i++) {
        struct thread_info *thread_info = thread_infos + i;
        os_create_thread(true, work_thread_proc, thread_info);
    }
    
    end_counter(startup);
    
    // Wait for STAGE_ONE to complete
    log_print("start phase 1");
    
    while(globals.work_queue_stage_one.work_entries_in_flight > 0){
        do_one_work(context);
    }
    
    assert(globals.work_queue_stage_one.work_entries_in_flight == 0);
    
    log_print("Phase 1 completed.\n");
    
    // Initiate STAGE_TWO
    globals.compile_stage = COMPILE_STAGE_parse_function_and_emit_code;
    SetEvent(globals.wake_event);
    ResetEvent(globals.wake_event);
    
    log_print("start phase 2");
    
    while(globals.work_queue_stage_two.work_entries_in_flight > 0){
        do_one_work(context);
    }
    assert(globals.work_queue_stage_two.work_entries_in_flight == 0);
    
    log_print("start phase 3");
    
    if(globals.an_error_has_accured) goto end;
    
    begin_counter(unresolved_sleepers);
    report_error_for_unresolved_sleepers(context);
    end_counter(unresolved_sleepers);
    if(globals.an_error_has_accured) goto end;
    
    begin_counter(report_errror_for_undefined_functions_and_types);
    report_error_for_undefined_functions_and_types(context);
    end_counter(report_errror_for_undefined_functions_and_types);
    if(globals.an_error_has_accured) goto end;
    
    struct ast_function *entry_point = get_entry_point_or_error(context);
    if(globals.an_error_has_accured) goto end;
    
    struct coff_context coff = zero_struct;
    
    
    struct memory_arena emit_arena = create_memory_arena(os_reserve_memory(0, giga_bytes(4)), 2.0f, mega_bytes(10));
    emit_arena.is_fixed_size = true;
    
    if(globals.want_executable){
        
        // @note: this does NOT actually print the files, as we have to patch up function calls in patch
        // and we before that have to set the actual locations.
        // @cleanup: is this still neccessary as we have offset in text section? I think we could place this
        // after the patch now.
        
        begin_counter(print_coff);
        coff = print_coff(entry_point, &emit_arena, arena); // @cleanup: emit_arena is scratch?
        end_counter(print_coff);
    }
    
    //:patch
    begin_counter(patch);
    for(struct patch_node *patch = globals.first_patch; patch; patch = patch->next){
        assert(patch->dest_declaration->memory_location);
        
        // @ugh: functions might have not been defined at the time, we parsed the call
        if(patch->source->kind == AST_function){
            assert((cast(struct ast_function *)patch->source)->is_defined);
            patch->source = &(cast(struct ast_function *)patch->source)->is_defined->base;
        }
        
        if(patch->dest_declaration->base.kind == AST_function){
            patch->dest_declaration = &(cast(struct ast_function *)patch->dest_declaration)->is_defined->as_decl;
        }
            
        
        u8 *memory_location = patch->dest_declaration->memory_location + patch->location_offset_in_dest_declaration;
        
        // @hack... this is ugly.. we emit first the body then the prolog, so everything is actually
        //          relative to 'function->memory_location + function->size_of_prolog'
        // @cleanup: could we do everything relative to 'emit_pool.base'?
        if(patch->dest_declaration->base.kind == AST_function){
            struct ast_function *function = cast(struct ast_function *)patch->dest_declaration;
            patch->dest_declaration = &function->as_decl;
            assert(function); // otherwise how did we get here?
            
            memory_location += function->size_of_prolog;
            patch->rip_at += function->size_of_prolog;
        }
        
        if(patch->kind == PATCH_rip_relative){
            assert(patch->dest_declaration->base.kind == AST_function);
            assert(patch->rip_at >= 0);
            
            if(patch->source->kind == AST_function || patch->source->kind == AST_declaration){
                struct ast_declaration *source_declaration = cast(struct ast_declaration *)patch->source;
                
                // :patches_are_32_bit all functions that might need a patch are from us thus 32 bit are enough, this is now not true anymore, as we have to patch 64 bit locations for memory accesses
                smm source_location;
                smm dest_location;
                
                if(globals.want_executable){
                    source_location = source_declaration->relative_virtual_address;
                    dest_location   = patch->dest_declaration->relative_virtual_address;
                }else{
                    source_location = cast(smm)source_declaration->memory_location;
                    dest_location   = cast(smm)patch->dest_declaration->memory_location;
                }
                
                // @note: this is used to access a member of a struct or a member of an array.
                source_location += patch->location_offset_in_source_declaration;
                smm rip_at = dest_location + patch->rip_at;
                *cast(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
            }else if(patch->source->kind == AST_float_literal){
                struct ast_float_literal *f = cast(struct ast_float_literal *)patch->source;
                assert(f->relative_virtual_address),
                assert(globals.want_executable); // otherwise we just emit it on the spot, so no patch
                smm dest_location = patch->dest_declaration->relative_virtual_address;
                smm rip_at = dest_location + patch->rip_at;
                smm source_location = f->relative_virtual_address;
                *cast(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
            }else{
                assert(patch->source->kind == AST_string_literal);
                assert(globals.want_executable); // otherwise we just emit it on the spot, so no patch
                struct ast_string_literal *lit = cast(struct ast_string_literal *)patch->source;
                
                // :string_literal_location we saved the virtual address in the .data field of the unique_string
                smm dest_location = patch->dest_declaration->relative_virtual_address;
                
                // :string_literal_location we saved the virtual address in the .data field of the unique_string
                smm source_location = cast(smm)(lit->value->data - coff.exe.header->ImageBase);
                
                smm rip_at = dest_location + patch->rip_at;
                *cast(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
            }
            
            /*log_print("patching rip relative %.*s in dest %.*s",
                      patch->source->token->value->amount,
                      patch->source->token->value->data,
                      patch->dest_declaration->identifier->amount,
                      patch->dest_declaration->identifier->data);*/
        }else{
            assert(patch->kind == PATCH_absolute);
            assert(patch->dest_declaration->base.kind == AST_declaration);
            
            if(patch->source->kind == AST_function || patch->source->kind == AST_declaration){
                struct ast_declaration *decl = cast(struct ast_declaration *)patch->source;
                assert(decl->memory_location);
                
                smm source_location = (smm)decl->memory_location;
                if(globals.want_executable){
                    source_location = decl->relative_virtual_address + coff.exe.header->ImageBase;
                }
                
                source_location += patch->location_offset_in_source_declaration;
                *cast(smm *)memory_location = source_location;
            }else{
                assert(patch->source->kind == AST_string_literal);
                assert(globals.want_executable); // otherwise we just emit it on the spot, so no patch
                struct ast_string_literal *lit = cast(struct ast_string_literal *)patch->source;
                
                // :string_literal_location we saved the virtual address in the .data field of the unique_string
                *cast(u8 **)memory_location = lit->value->data;
            }
            
            /*
            log_print("patching absolute %.*s in dest %.*s",
                      patch->source->token->value->amount,
                      patch->source->token->value->data,
                      patch->dest_declaration->identifier->amount,
                      patch->dest_declaration->identifier->data);*/
        }
    }
    end_counter(patch);
    
    if(!globals.want_executable){  // this has  to be done so we get the exceptionHandlerFilter
        smm base_address = cast(smm)&emit_arena.current_block->base;
        
        RUNTIME_FUNCTION *data = push_data(&emit_arena, RUNTIME_FUNCTION, 0);
        
        for(u64 i = 0; i < globals.global_declarations.capacity; i++){
            struct ast_node *it = globals.global_declarations.nodes + i;
            if(it->ast && it->ast->kind == AST_function){
                struct ast_function *ast_function = cast(struct ast_function *)it->ast;
                if(ast_function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport) continue;
                RUNTIME_FUNCTION *function = push_struct(&emit_arena, RUNTIME_FUNCTION);
                function->BeginAddress = save_truncate_smm_to_u32((smm)(ast_function->memory_location - base_address));
                function->EndAddress = save_truncate_smm_to_u32((smm)(ast_function->memory_location + ast_function->byte_size - base_address));
            }
        }
        
        umm size = cast(umm)(push_data(&emit_arena, RUNTIME_FUNCTION, 0) - data);
        
        u32 count = 0;
        for(u64 i = 0; i < globals.global_declarations.capacity; i++){
            struct ast_node *it = globals.global_declarations.nodes + i;
            if(it->ast && it->ast->kind == AST_function){
                struct ast_function *ast_function = cast(struct ast_function *)it->ast;
                if(ast_function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport) continue;
                RUNTIME_FUNCTION *function = data + count++;
                push_align(&emit_arena, alignof(u32));
                function->UnwindInfoAddress = save_truncate_smm_to_u32(cast(smm)arena_current(&emit_arena) - base_address);
                
                function_fill_in_unwind_info(&emit_arena, ast_function);
            }
        }
        
        RtlAddFunctionTable(data, (u32)size, base_address);
        SetUnhandledExceptionFilter(my_awesome_exception_handler);
    }
    
    if(globals.want_executable){
        
        begin_counter(write_files);
        if(globals.want_pdb){
            begin_counter(write_pdb);
            char *pdb_name = push_cstring_from_string(arena, globals.pdb_full_path);
            smm success = os_write_file(pdb_name, coff.pdb.pdb_base, coff.pdb.pdb_size);
            end_counter(write_pdb);
            
            if(success){
                print("Wrote file '%s'.\n", pdb_name);
                //print("   Size: %lld\n", coff.pdb.pdb_size);
            }else{
                print("Error: Unable to write file '%s'.\n", pdb_name);
                globals.an_error_has_accured = true;
                goto end;
            }
            
        }
        { // write exe file to disk
            begin_counter(write_exe);
            char *exe_name = push_cstring_from_string(arena, globals.exe_full_path);
            smm size = coff.exe.end_address - coff.exe.base_address;
            
            b32 success;
            {
                u8 *buffer = coff.exe.base_address;
                smm buffer_size = size;
                
                //u32 FILE_FLAG_NO_BUFFERING = 0x20000000;
                u32 FILE_FLAG_NO_BUFFERING = 0;
                
                HANDLE file_handle = CreateFileA(exe_name, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_FLAG_NO_BUFFERING, NULL);
                
                // ignore FILE_ALLREADY_EXISTS
                if(GetLastError() == 183) SetLastError(0);
                //if(GetLastError()) print("Warning: GetLastError in create file: %d\n", GetLastError());
                
                DWORD bytes_written;
                
                success = WriteFile(file_handle, buffer, save_truncate_smm_to_s32(buffer_size), &bytes_written, 0);
                
                success = success && (bytes_written == buffer_size);
                
                begin_counter(virus_scanner);
                // @note: this apperantly invokes the anti virus scanner or something thus it takes 200ms...
                CloseHandle(file_handle);
                end_counter(virus_scanner);
            }
            //b32 success = os_write_file(exe_name, coff.exe.base_address, size);
            end_counter(write_exe);
            
            if(success){
                print("Wrote file '%s'.\n", exe_name);
                //print("   Size: %lld\n", size);
            }else{
                //u32 get_last_error = GetLastError();
                print("Error: Unable to write file '%s'.\n", exe_name);
                globals.an_error_has_accured = true;
                goto end;
            }
        }
        end_counter(write_files);
        
    }else{
        print("Compilation complete, starting execution of '%.*s':\n", entry_point->identifier->amount, entry_point->identifier->data);
        int(*f)(void) = cast(int (*)(void))entry_point->memory_location;
        int exit_code = f();
        
        //string name = *function->identifier;
        
        print("Program exited with code: %u (0x%x)\n", exit_code, exit_code);
    }
    //string_intern_table_print(string_intern_table);
    
    end:;
    print_error_stream(context);
    
    smm amount_of_lines = 0;
    for(struct file_stack_node *it = globals.file_list.first; it; it = it->next){
        amount_of_lines += it->line;
    }
    
    f64 end_time = os_get_time_in_seconds();
    f64 time_in_seconds =  (end_time - begin_time);
    print("\nTotal Lines: %lld | Time: %.3fs \n", amount_of_lines, time_in_seconds);
    
#if time_perfomance
    end_counter(total);
    u64 time_in_cycles = __rdtsc() - begin_cycle_time;
    // :timing :performance :counters
    struct timing_place_info{
        char *name;
        u64 include_time;
        u64 exclude_time;
        u64 hit_count;
    } place_timing_info[TIMING_PLACE_count] = zero_struct;
    
    for(u32 i = 0; i < TIMING_PLACE_count; i++){
        place_timing_info[i].name = (char *)timing_place_names[i];
    }
    
    struct timing_node{
        struct timing_node *next;
        struct perf_timing_event *event;
    };
    
    struct{
        struct timing_node *first;
        struct timing_node *last;
    } timing_stack = zero_struct;
    
    
    struct perf_timing_event *last_event = push_struct(arena, struct perf_timing_event);
    last_event->kind = EVENT_end_perf_counter;
    last_event->counter = begin_cycle_time;
    
    for(struct bucket_array *bucket = timing_events.first; bucket; bucket = bucket->next){
        struct perf_timing_event *events = bucket_get_array(bucket);
        for(smm i = 0; i < bucket->size; i++){
            struct perf_timing_event *event = events + i;
            assert(event->place < TIMING_PLACE_count);
            
            if(timing_stack.first){
                place_timing_info[timing_stack.first->event->place].exclude_time += (event->counter - last_event->counter);
            }
            
            if(event->kind == EVENT_begin_perf_counter){
                place_timing_info[event->place].hit_count += 1;
                struct timing_node *node = push_struct(arena, struct timing_node);
                node->event = event;
                sll_push_front(timing_stack, node);
            }else{
                assert(event->kind == EVENT_end_perf_counter);
                
                struct timing_node *node = timing_stack.first;
                sll_pop_front(timing_stack);
                
                assert(event->place < TIMING_PLACE_count);
                place_timing_info[event->place].include_time += (event->counter - node->event->counter);
            }
            last_event = event;
        }
    }
    
    // @cleanup: real sort?
    for(u32 i = 0; i < TIMING_PLACE_count - 1; i++){
        u64 max = place_timing_info[i].include_time;
        u32 index = i;
        for(u32 j = i + 1; j < TIMING_PLACE_count; j++){
            if(place_timing_info[j].include_time > max){
                max = place_timing_info[j].include_time;
                index = j;
            }
        }
        struct timing_place_info temp = place_timing_info[i];
        place_timing_info[i] = place_timing_info[index];
        place_timing_info[index] = temp;
    }
    
    print("Performance counters: (include)                              cycles     hit  percent       time\n");
    for(u32 i = 0; i < TIMING_PLACE_count; i++){
        
        f64 percent = (f64)place_timing_info[i].include_time/(f64)time_in_cycles;
        print("   %-50s %13llu %7llu %7.2f%% %8.2fs\n", place_timing_info[i].name, place_timing_info[i].include_time, place_timing_info[i].hit_count, percent* 100.0, percent * time_in_seconds);
    }
    
    for(u32 i = 0; i < TIMING_PLACE_count - 1; i++){
        u64 max = place_timing_info[i].exclude_time;
        u32 index = i;
        for(u32 j = i + 1; j < TIMING_PLACE_count; j++){
            if(place_timing_info[j].exclude_time > max){
                max = place_timing_info[j].exclude_time;
                index = j;
            }
        }
        struct timing_place_info temp = place_timing_info[i];
        place_timing_info[i] = place_timing_info[index];
        place_timing_info[index] = temp;
    }
    
    print("\n\n");
    print("Performance counters: (exclude)                              cycles     hit  percent       time\n");
    for(u32 i = 0; i < TIMING_PLACE_count; i++){
        f64 percent = (f64)place_timing_info[i].exclude_time/(f64)time_in_cycles;
        print("   %-50s %13llu %7llu %7.2f%% %8.2fs\n", place_timing_info[i].name, place_timing_info[i].exclude_time, place_timing_info[i].hit_count, percent * 100.0, percent * time_in_seconds);
    }
#endif
    
    if(globals.an_error_has_accured){
        print("Errors detected, compilation stopped.\n");
        return 1;
    }
    return 0;
}

