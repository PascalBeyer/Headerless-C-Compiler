
struct explain_node{
    struct explain_node *next;
    
    struct token *sleeping_identifier;
    struct token_and_compilation_unit{
        struct token *token;
        struct compilation_unit *compilation_unit;
    } *locations;
    u32 amount_of_locations;
    u32 location_capacity;
    
    struct explain_node *outgoing_edge;
    b32 visited;
};

func void report_errors_for_unresolved_sleepers(struct context *context, struct sleeper_table *sleeper_table){
    context->should_exit_statement = false;
    context->should_sleep = false;
    context->error = false;
    
    b32 error = false;
    
    struct {
        struct explain_node *first;
        struct explain_node *last;
    } nodes = zero_struct;
    
    
    begin_error_report(context);
    
    struct temporary_memory temp = begin_temporary_memory(&context->scratch);
    
    // iterate throught the 'sleeper_table' which is a map 'sleeping_on' -> list 'sleeper', and construct
    // a graph 'sleeper' -> 'sleeping_on'. In the below we then only report undeclared identifiers for terminal
    // nodes in this graph                                                                          -10.08.2020
    
    // each 'explain_node' in this graph represents an
    //     an 'identifier' which is sleeping,
    //     an 'outgoing_edge' to what we are sleeping on
    //     a list of tokens which sleep on us (for error reporting)
    
    for(u32 i = 0; i < sleeper_table->capacity; i++){
        struct sleeper_node *sleeper_node = sleeper_table->nodes + i;
        if(!sleeper_node->first_sleeper) continue;
        
        struct token *sleeping_on = sleeper_node__get_token(sleeper_node);
        
        for(struct work_queue_entry *work = sleeper_node->first_sleeper; work; work = work->next){
            // find out who is sleeping. This can be null meaning that we don't know yet. E.g.:
            //      imt foo(){}
            // would sleep at 'imt' (typo for int) and not know, that the sleeper is 'foo'.
            struct token *sleeping = null;
            struct token *sleeping_on_token = work->sleeping_on;
            struct compilation_unit *compilation_unit = null;
            
            switch(work->description){
                case WORK_parse_global_scope_entry:{
                    struct parse_work *parse = work->data;
                    compilation_unit = parse->compilation_unit;
                    // @cleanup: @incomplete: we right now have it be 0 if its invalid identifier is that right?
                    if(parse->sleeping_ident){
                        b32 is_valid = !atoms_match(parse->sleeping_ident->atom, globals.invalid_identifier);
                        sleeping = is_valid ? parse->sleeping_ident : 0;
                    }
                    error = true;
                }break;
                case WORK_parse_function_body:{
                    struct parse_work *parse = work->data;
                    compilation_unit = parse->compilation_unit;
                    
                    struct ast_function *function = parse->function;
                    sleeping = function->base.token;
                    error = true;
                }break;
                invalid_default_case();
            }
            
            // we could have a type and an identifier of the same name... we don't really have good access to
            // the sleep reason right now. Otherwise we could assert here... @cleanup
            //assert(!lookup_identifier(context, sleeping_on) || !lookup_type(context, sleeping_on));
            assert(!sleeping || sleeping->data);
            
            struct explain_node *begin_node = null;
            struct explain_node *end_node   = null;
            
            for(struct explain_node *node = nodes.first; node; node = node->next){
                if(atoms_match(node->sleeping_identifier->atom, sleeping_on->atom)){
                    end_node = node;
                    if(begin_node) break;
                }else if(sleeping && atoms_match(node->sleeping_identifier->atom, sleeping->atom)){
                    begin_node = node;
                    // explain!!! what is this?
                    //  node->token = sleeping;
                    if(end_node) break;
                }
            }
            
            // insert '(begin_node) -> (end_node)', begin_node <-> sleeping, end_node <-> node
            
            if(begin_node){
                // @cleanup: we are throwing away the information that there are two things
                //           with the same name that are sleeping
                // this condition will be true for like
                //
                // void _start(sleeping a){}
                // void _start(sleeping a){}
                //
                // as both _start would go to sleep on _sleeping and never report multiple definitions.
                if(begin_node->outgoing_edge) continue; // @note: not implied as we could have chains
            }else{
                if(sleeping){ // if we know what we are sleeping on add it to the graph
                    struct explain_node *new_node = push_struct(&context->scratch, struct explain_node);
                    new_node->sleeping_identifier = sleeping;
                    new_node->amount_of_locations  = 1;
                    new_node->location_capacity    = 4;
                    struct token_and_compilation_unit *locations = push_uninitialized_data(&context->scratch, struct token_and_compilation_unit, 4);
                    locations[0].token = sleeping;
                    locations[0].compilation_unit = compilation_unit;
                    new_node->locations = locations;
                    
                    sll_push_back(nodes, new_node);
                    begin_node = new_node;
                }
            }
            
            if(!end_node){
                struct explain_node *new_node = push_struct(&context->scratch, struct explain_node);
                new_node->sleeping_identifier = sleeping_on;
                new_node->amount_of_locations  = 1;
                new_node->location_capacity    = 4;
                
                struct token_and_compilation_unit *locations = push_uninitialized_data(&context->scratch, struct token_and_compilation_unit, 4);
                locations[0].token = sleeping_on_token;
                locations[0].compilation_unit = compilation_unit;
                new_node->locations = locations;
                
                sll_push_back(nodes, new_node);
                end_node = new_node;
            }else{
                // if we already have an end node add our 'sleeping_on' token
                if(!end_node->outgoing_edge){
                    dynarray_maybe_grow(struct token_and_compilation_unit, &context->scratch, end_node->locations,
                            end_node->amount_of_locations, end_node->location_capacity);
                    smm index = end_node->amount_of_locations++;
                    end_node->locations[index].token = sleeping_on_token;
                    end_node->locations[index].compilation_unit = compilation_unit;
                }
            }
            
            // @note: if we don't have a begin_node we don't know what we are sleeping on and
            //        just want to make sure we have a node for 'sleeping_on'.
            //        So no edge needed.
            if(begin_node){
                assert(!begin_node->outgoing_edge);
                begin_node->outgoing_edge = end_node;
            }
        }
    }
    
    b32 reported_undeclared_identifiers = false;
    for(struct explain_node *node = nodes.first; node; node = node->next){
        // if the node does not have an outgoing edge, this means that the node does not sleep on anybody.
        // Therefor it was not in the sleeper table and is a unresolved identifier.
        // So report everyone that is sleeping on it here.                               14.05.2021
        
        if(!node->outgoing_edge){
            assert(node->amount_of_locations > 0);
            for(u32 location_index = 0; location_index < node->amount_of_locations; location_index++){
                report_error(context, node->locations[location_index].token, "[%d] Undeclared identifier.", node->locations[location_index].compilation_unit->index);
            }
            reported_undeclared_identifiers = true;
            error = true;
        }
    }
    
    if(!reported_undeclared_identifiers && nodes.first){
        
        // if we have not reported unresolved identifiers, that means every node has 'outgoing_edge' set.
        // This must mean there is a cycle. In fact we can find this cycle by walking the nodes along the 'outgoing_edge'
        // eventually we have to reach a node we have already been at.
        // note that any given identifier doesn't necessarly have to be part of the cycle, as it could just 'lead' to it.
        //                                                                                              14.05.2021
        struct explain_node *cycle = null;
        
        for(struct explain_node *node = nodes.first; ; node = node->outgoing_edge){
            
            if(node->visited){
                cycle = node;
                break;
            }
            
            node->visited = true;
        }

        assert(cycle);
        
        report_error(context, 0, "Program contains circular dependencies:");
        struct explain_node *cycle_it = cycle;
        for(; cycle_it->outgoing_edge != cycle; cycle_it = cycle_it->outgoing_edge){
            report_error(context, cycle_it->locations->token, "Part of the cycle.");
        }
        
        report_error(context, cycle_it->locations->token, "Part of the cycle.");
        
        error = true;
    }
    
    end_error_report(context);
    if(error || context->error){
        assert(!sll_is_empty(context->error_list)); // we better have something to report.
        globals.an_error_has_occurred = true;
        os_debug_break();
    }
    
    end_temporary_memory(temp);
}


func struct dll_import_node *lookup_function_in_dll_imports(struct context *context, struct ast_function *function){
    begin_counter(context, lookup_dll_import);
    
    struct string identifier = function->identifier->string;
    
    for(struct library_node *library = globals.libraries.first; library; library = library->next){
        
        struct dll_import_node *found = ar_lookup_symbol(context->arena, library, identifier);
        if(found) return found;
        
#if 0
        if(found){
            if(first_dll_import_node){
                report_warning(context, WARNING_cannot_distinguish_dll_imports, function->base.token, "Function is defined in both '%.*s' and '%.*s'.", first_dll_import_node->path.size, first_dll_import_node->name.data, dll->name.size, dll->name.data);
                break;
            }
            first_dll_import_node = dll;
            // function->memory_location = location; // @cleanup: right now immediate execution is dead so not sure
        }
#endif
    }
    
    
    end_counter(context, lookup_dll_import);
    return null;
}

func void report_errors_for_undefined_functions_and_types(struct context *context, struct ast_table *ast_table){
    assert(!context->error && !context->should_sleep);
    
    // Everything below is in one giant error report, so we don't have to reset 'context->error' everytime.
    begin_error_report(context);
    
    for(u64 ast_index = 0; ast_index < ast_table->capacity; ast_index++){
        struct ast_node *node = ast_table->nodes + ast_index;
        if(!node->ast) continue;
        
        if(node->ast->kind == AST_function){
            struct ast_function *function = cast(struct ast_function *)node->ast;
            
            if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
            if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
            
            if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
                if(function->as_decl.flags & DECLARATION_FLAGS_is_function_that_is_reachable_from_entry){
                    struct dll_import_node *import = lookup_function_in_dll_imports(context, function);
                    
                    if(!import){
                        report_error(context, function->base.token, "Function is not contained in any of the imported dlls.");
                    }
                    
                    function->dll_import_node = import;
                }
                continue;
            }
            
            if(!function->scope){
                if(function->as_decl.flags & DECLARATION_FLAGS_is_function_that_is_reachable_from_entry){
                    // :dllimports_with_missing_declspec
                    // @note: apparently MSVC does note require 'dllimport' it appears to be a keyword
                    //        that gets rid of one indirection which link.exe/we have to insert
                    //        thus we generate a call to a rip relative jump, i.e.
                    //            jmp [rip + <offset_off_dll_import_in_import_table>]
                    struct dll_import_node *import = lookup_function_in_dll_imports(context, function);
                    if(import){
                        function->as_decl.flags |= DECLARATION_FLAGS_is_dll_import_with_missing_declspec;
                        function->dll_import_node = import;
                    }else{
                        if(function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                            report_error(context, function->base.token, "A function marked '__declspec(dllexport)' must be defined.");
                        }else{
                            assert(function->token_that_referenced_this_function);
                            
                            // @cleanup: what about main?
                            report_error(context, function->base.token, "Function was declared and referenced but never defined.");
                            report_error(context, function->token_that_referenced_this_function, "... Here the function was referenced.");
                        }
                    }
                }else{
                    report_warning(context, WARNING_function_declared_but_never_defined, function->base.token, "Function was declared but never defined.");
                }
            }else if(!(function->as_decl.flags & DECLARATION_FLAGS_is_function_that_is_reachable_from_entry)){
                report_warning(context, WARNING_function_defined_but_unreachable, function->base.token, "Function was defined but unreachable from main.");
            }
            
        }else if(node->ast->kind == AST_declaration){
            struct ast_declaration *decl = (struct ast_declaration *)node->ast;
            
            if(type_is_array_of_unknown_size(decl->type)){
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
                
                report_warning(context, WARNING_array_of_unknown_size_never_filled_in, decl->base.token, "Bounds for array of unknown size were never filled in. Assuming an array length of one.");
                patch_array_size(context, (struct ast_array_type *)decl->type, 1, decl->base.token);
            }
        }else{
            // I think these are fine, maybe one could do type never used or something later.
            assert(node->ast->kind == AST_typedef);
        }
    }
    
    /* not sure if this actually did anything...
    for(u64 i = 0; i < globals.compound_types.capacity; i++){
        struct ast_node *node = globals.compound_types.nodes + i;
        if(!node->ast) continue;
        if(node->ast->kind == AST_enum) continue; // nothing to check here
        struct ast_compound_type *compound = cast(struct ast_compound_type *)node->ast;
        assert(compound->base.kind == AST_union || compound->base.kind == AST_struct);
        
        for_ast_list(compound->declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            if(decl->type->kind == AST_pointer_type){
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)decl->type;
                
    @incomplete: this case will currently not be detected.
                struct{
                    struct unresolved *first;
                    struct unresolved *last;
                } debug_members;
                
                
                // @sigh @speed looping until its not a pointer again @yikes
                while(pointer->pointer_to->kind == AST_pointer_type){
                    pointer = cast(struct ast_pointer_type *)pointer->pointer_to;
                }
                
                if(maybe_resolve_pointer_to_unresolved_type_or_sleep(context, pointer)){
                    //report_info(context, pointer->pointer_to->token, "Type declared but never defined.");
                        
                    //globals.an_error_has_accured = true;
                }
                
            }
        }
    }
    */
    end_error_report(context);
    
    if(context->error){
        globals.an_error_has_occurred = true;
    }
    
}


