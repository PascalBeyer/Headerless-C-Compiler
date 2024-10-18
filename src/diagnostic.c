
enum error_report_node_kind{
    REPORT_error,
    REPORT_syntax_error,
    REPORT_warning,
};

enum warning_type{
    WARNING_none,
    
    //
    // Preprocessor warnings:
    //
    WARNING_missing_newline_after_backslash,
    WARNING_junk_after_directive,
    WARNING_compile_time_overflow,
    WARNING_compile_time_multiplication_by_zero,
    
    WARNING_shadowing_local,
    WARNING_shadowing_global,
    WARNING_shadowing_in_same_scope,
    
    WARNING_type_mismatch,
    WARNING_compile_time_truncation,
    WARNING_unsigned_negation,
    WARNING_unsupported_declspec,
    WARNING_function_alignment,
    WARNING_ret_in_asm_block,
    WARNING_does_not_declare_anything,
    WARNING_undefined_static_if_operand,
    WARNING_undef_on_undefined,
    WARNING_unsupported_pragma,
    WARNING_function_declared_but_never_defined,
    WARNING_function_defined_but_unreachable,
    
    WARNING_unused_local_variable,
    WARNING_local_variable_only_ever_written,
    
    WARNING_casting_u64_to_float,
    WARNING_double_specifier,
    
    WARNING_incorrect_format_specifier,
    WARNING_unknown_format_specifier,
    
    WARNING_atomic_ignored,
    
    WARNING_assignment_in_condition,
    
    WARNING_extension_used,
    WARNING_unusual_entry_point,
    
    WARNING_array_of_unknown_size_never_filled_in,
    
    WARNING_missing_return,
    WARNING_return_in_noreturn_function,
    
    WARNING_redefining_declaration_from_extern_to_static,
    WARNING_inline_function_is_implicitly_external,
    
    WARNING_count, 
};

static u8 warning_enabled[WARNING_count] = {
    [WARNING_compile_time_overflow]               = 1, 
    [WARNING_missing_newline_after_backslash]     = 1, 
    [WARNING_junk_after_directive]                = 1, 
    [WARNING_shadowing_local]                     = 1, 
    [WARNING_shadowing_in_same_scope]             = 1,
    [WARNING_type_mismatch]                       = 1, 
    [WARNING_compile_time_truncation]             = 1, 
    [WARNING_compile_time_multiplication_by_zero] = 1,
    [WARNING_unsigned_negation]                   = 1, 
    [WARNING_unsupported_declspec]                = 1,
    [WARNING_function_alignment]                  = 1, 
    [WARNING_ret_in_asm_block]                    = 1, 
    [WARNING_does_not_declare_anything]           = 1,
    [WARNING_undefined_static_if_operand]         = 1, 
    [WARNING_undef_on_undefined]                  = 1,
    [WARNING_unsupported_pragma]                  = 1,
    [WARNING_unused_local_variable]               = 1,
    // [WARNING_casting_u64_to_float]                = 1,
    [WARNING_local_variable_only_ever_written]    = 1,
    [WARNING_atomic_ignored]                      = 1,
    [WARNING_unusual_entry_point]                 = 1,
    // [WARNING_function_declared_but_never_defined] = 1,
    // [WARNING_function_defined_but_unreachable]    = 1,
    [WARNING_incorrect_format_specifier] = 1,
    [WARNING_unknown_format_specifier]  = 1,
    [WARNING_assignment_in_condition] = 1,
    [WARNING_double_specifier] = 1,
    [WARNING_array_of_unknown_size_never_filled_in] = 1,
    
    [WARNING_missing_return] = 1,
    [WARNING_return_in_noreturn_function] = 1,
    [WARNING_redefining_declaration_from_extern_to_static] = 1,
    [WARNING_inline_function_is_implicitly_external] = 1,
};




struct error_report_node{
    struct error_report_node *next;
    
    // used for 'begin_error_report' 'end_error_report' situations.
    // the first error_report_node gives the location for sorting
    struct error_report_node *sub_error; 
    enum error_report_node_kind kind;
    
    enum warning_type warning_type; // used if its a warning
    
    u32 compilation_unit_index;
    enum compile_stage compile_stage;
    
    struct token *token;
    struct string error;
};

func int error_node_smaller_function(struct error_report_node *it, struct error_report_node *piveot){
    //
    // sort by compilation unit
    //
    if(it->compilation_unit_index < piveot->compilation_unit_index) return true;
    if(it->compilation_unit_index > piveot->compilation_unit_index) return false;
    
    //
    // Then by compile stage
    //
    if(it->compile_stage < piveot->compile_stage) return true;
    if(it->compile_stage > piveot->compile_stage) return false;
    
    enum compile_stage compile_stage = it->compile_stage;
    
    //
    // List errors that do not have associated tokens *first*
    //
    if(!piveot->token) return false;
    if(!it->token)     return true;
    
    switch(compile_stage){
        case COMPILE_STAGE_tokenize_files:{
            // @hmm: we could have an 'error_index' that we increment every time we emit an error.
            //       but adjusting much for preprocessor errors seems wrong.
            
            // The tokens were copied... Just compare file_index, line and column
            if(it->token->file_index < piveot->token->file_index) return true;
            if(it->token->file_index > piveot->token->file_index) return false;
            
            if(it->token->line < piveot->token->line) return true;
            if(it->token->line > piveot->token->line) return false;
            
            if(it->token->column < piveot->token->column) return true;
            if(it->token->column > piveot->token->column) return false;
            
            // is this impossible?
            return false;
        }break;
        
        case COMPILE_STAGE_parse_global_scope_entries:
        case COMPILE_STAGE_parse_function:
        case COMPILE_STAGE_emit_code:{
            // we know the tokens are in the preprocessed token array of the compilation unit.
            // so we can just sort by _token index_.
            return (smm)it->token < (smm)piveot->token;
        }break;
        invalid_default_case(return false);
    }
}

func void print_one_error_node(struct error_report_node *node){
    // We could have different error styles here!
    
    struct token *token = node->token;
    struct string msg = node->error;
    
    static char *error_kind_string[] = {
        [REPORT_error]        = "Error",
        [REPORT_warning]      = "Warning",
        [REPORT_syntax_error] = "Syntax error",
    };
    assert(node->kind < array_count(error_kind_string));
    
    if(token){
        assert(token->line && token->column);
        
        struct string str = token_get_string(token);
        if(token->type == TOKEN_embed) str = string("<embeded-data>");
        
        // @note: -1 is okay as we have a special file there that tells us * predefined token * as absolute file path.
        assert(token->file_index == -1 || token->file_index < array_count(globals.file_table.data));
        
        char *path = globals.file_table.data[token->file_index]->absolute_file_path;
        print("%s(%u,%u): %s ", path, token->line, token->column, error_kind_string[node->kind]);
        
        if(node->warning_type != WARNING_none){
            print("%d ", node->warning_type);
        }
        
        print("at '%.*s': %.*s\n", str.size, str.data, msg.size, msg.data);
    }else{
        print("%.*s\n", msg.size, msg.data); // @cleanup: add warnings and stuff here?
    }
}


// errors are accumulated in context->error_list and then reported by the main thread
func void push_error_node_to_context(struct context *context, struct token *token, enum error_report_node_kind kind, enum warning_type warning_type, char *format, va_list va){
    
    assert(token != &globals.invalid_token); // @note: token can be zero!
    
    enum compile_stage stage = globals.compile_stage;
    
    struct error_report_node *node = push_struct(context->arena, struct error_report_node);
    node->kind  = kind;
    node->warning_type = warning_type;
    node->compilation_unit_index = 0;
    if(context->current_compilation_unit){
        node->compilation_unit_index = (u32)context->current_compilation_unit->index;
    }
    node->compile_stage = stage;
    
    if(token){
        switch(stage){
            case COMPILE_STAGE_tokenize_files:{
                // while tokenizing copy the tokens, as they might be tempoary
                struct token *copied_token = push_struct(context->arena, struct token);
                *copied_token = *token;
                node->token = copied_token;
            }break;
            case COMPILE_STAGE_parse_global_scope_entries:
            case COMPILE_STAGE_emit_code:
            case COMPILE_STAGE_parse_function:{
                // we don't need to copy the tokens here (and don't want to), as they are the preprocessed tokens!
                node->token = token;
            }break;
            invalid_default_case();
        }
    }
    
    va_list copied_va;
    va_copy(copied_va, va);
    node->error.length = vsnprintf(0, 0, format, copied_va);
    va_end(copied_va);
    
    node->error.data = push_uninitialized_data(context->arena, u8, node->error.length + 1);
    vsnprintf((char *)node->error.data, (int)(node->error.length + 1), format, va);
    node->error.data[node->error.length] = 0;
    
    if(!context->in_error_report){
        sll_push_back(context->error_list, node);
    }else{
        if(!context->current_error_node){
            sll_push_back(context->error_list, node);
        }else{
            context->current_error_node->sub_error = node;
        }
        context->current_error_node = node;
    }
}


func int should_report_warning_for_token(struct context *context, struct token *token){
    return globals.cli_options.report_warnings_in_system_includes || context->in_error_report || !token || !globals.file_table.data[token->file_index]->is_system_include;
}

PRINTLIKE __declspec(noinline) func void report_warning(struct context *context, enum warning_type warning, struct token *token, char *format, ...){
    if(context->should_exit_statement) return;
    if(context->should_sleep) return;
    if(!warning_enabled[warning]) return;
    
    if(!should_report_warning_for_token(context, token)) return;

    if(context->warnings_reported > 100) return;
    context->warnings_reported += 1;
    
    va_list va;
    va_start(va, format);
    push_error_node_to_context(context, token, REPORT_warning, warning, format, va);
    va_end(va);
    
#if DEBUG_BREAK_ON_WARNING
    os_debug_break();
#endif
}

func void maybe_set_should_exit_statement(struct context *context){
    if(globals.compile_stage < COMPILE_STAGE_emit_code){
        // We only want to ever exit the statement while parsing.
        // After parsing just emit every error!
        context->should_exit_statement = true;
    }
}

func void begin_error_report(struct context *context){
    assert(!context->in_error_report);
    context->in_error_report = true;
    context->current_error_node = null;
}

func void end_error_report(struct context *context){
    
    assert(context->in_error_report);
    context->in_error_report = false;
    
    if(context->errors_in_this_report){
        context->error = true;
        maybe_set_should_exit_statement(context);
        context->sleep_line = -1;
        // @hmm: this is just _anything_ random that will never wake up.
        //       maybe we should make a designated token
        context->sleep_on = &globals.token_void;
        
        context->errors_in_this_report = 0;
    }
}

PRINTLIKE __declspec(noinline) func void report_error(struct context *context, struct token *token, char *format, ...){
    if(context->should_sleep) return;
    if(context->should_exit_statement) return;
    
    if(context->errors_reported < 100){
        context->errors_reported += 1;
        
        va_list va;
        va_start(va, format);
        push_error_node_to_context(context, token, REPORT_error, WARNING_none, format, va);
        va_end(va);
    }
    
    if(!context->in_error_report){
        context->error = true;
        maybe_set_should_exit_statement(context);
    }else{
        context->errors_in_this_report += 1;
    }
#if DEBUG_BREAK_ON_ERROR
    os_debug_break();
#endif
}

PRINTLIKE __declspec(noinline) func void report_syntax_error(struct context *context, struct token *token, char *format, ...){
    if(context->should_sleep) return;
    if(context->error){
        maybe_set_should_exit_statement(context);
        return; // stop reporting syntax errors after the first one.
    }
    
    if(context->errors_reported < 100){
        context->errors_reported += 1;
        
        va_list va;
        va_start(va, format);
        push_error_node_to_context(context, token, REPORT_syntax_error, WARNING_none, format, va);
        va_end(va);
    }
        
    if(!context->in_error_report){
        context->error = true;
        maybe_set_should_exit_statement(context);
    }else{
        context->errors_in_this_report += 1;
    }
#if DEBUG_BREAK_ON_ERROR
    os_debug_break();
#endif
}


func b32 maybe_report_error_for_stack_exhaustion(struct context *context, struct token *token, char *error){
    u8 *stack_address = &(u8){0};
    if((context->low_stack_address - stack_address) > MAX_STACK_USAGE){
        report_error(context, token, error);
        return true;
    }
    return false;
}

PRINTLIKE __declspec(noinline) func void report_internal_compiler_error(struct token *token, char *format, ...){
    //
    // @note: be careful here, we might use this for asserts eventually.
    //
    if(token && token->file_index < array_count(globals.file_table.data)){
        char *file_path = globals.file_table.data[token->file_index]->absolute_file_path;
        print("%s(%u,%u): ", file_path, token->line, token->column);
    }
    
#ifdef FUZZING
    ((void (*)(void))format)(); // try to call the format string, which will give us a _unique_ crash.
#endif
    
    if(token){
        struct string string = token_get_string(token);
        print("Internal Compiler Error at '%.*s': ", string.size, string.data);
    }else{
        print("Internal Compiler Error: ");
    }
    
    char buffer[0x200];
    
    va_list va;
    va_start(va, format);
    int length = vsnprintf(buffer, sizeof(buffer), format, va);
    va_end(va);
    
    os_print_string(buffer, length);
    
    print("\n");
    
    os_panic(1);
}

