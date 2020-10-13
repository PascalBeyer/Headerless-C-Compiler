
struct explain_dependency{
    struct explain_dependency *next;
    struct explain_node *dep;
};

struct explain_node{
    unique_string node;
    struct token *token; // null if we do not know.
    
    struct explain_node *outgoing_edge;
    struct explain_dependency *dep_list;
    b32 visited;
};

func struct explain_node *visit_nodes_fill_in_outgoings_and_return_circular_dependency(struct explain_node *node){
    node->visited = true;
    for(struct explain_dependency *dep = node->dep_list; dep; dep = dep->next){
        if(dep->dep->visited){
            return dep->dep;
        }
        
        struct explain_node *circle = visit_nodes_fill_in_outgoings_and_return_circular_dependency(dep->dep);
        if(circle){
            return circle;
        }
    }
    return null;
}

func void report_error_for_unresolved_sleepers(struct context *context){
    context->should_sleep = false;
    assert(!context->error);
    
    b32 error = false;
    struct memory_arena *node_arena = context->arena;
    struct tempoary_memory temp = begin_tempoary_memory(node_arena);
    struct explain_node *nodes = push_data(node_arena, struct explain_node, 0);
    u32 amount_of_nodes = 0;
    
    begin_error_report(context);
    
    struct memory_arena *edge_arena = &context->scratch;
    struct tempoary_memory temp2 = begin_tempoary_memory(edge_arena);
    
    // iteralte throught the 'sleepr_table' which is a map 'sleeping_on' -> list 'sleeper', and construct
    // a graph 'sleeper' -> 'sleeping_on'. In the below we then only report undeclared identifiers for terminal
    // nodes in this graph    -10.08.2020
    for(u32 i = 0; i < globals.sleeper_table.capacity; i++){
        struct sleeper_node *sleeper_node = globals.sleeper_table.nodes + i;
        if(!sleeper_node->first_sleeper) continue;
        
        unique_string sleeping_on = sleep_hash_get_sleep_on(sleeper_node->hash);;
        for(struct work_queue_entry *work = sleeper_node->first_sleeper; work; work = work->next){
            // find out who is sleeping. This can be null meaning that we don't know yet. E.g.:
            //      imt foo(){}
            // would sleep at 'imt' (typepo for int) and not know, that the sleeper is 'foo'.
            struct token *sleeping = null;
            struct token *sleeping_on_token = work->sleeping_on;
            switch(work->description){
                case WORK_parse_global_scope_entry:{
                    struct parse_work *parse = work->data;
                    // @cleanup: @incomplete: we right now have it be 0 if its invalid identifier is that right?
                    if(parse->sleeping_ident){
                        b32 is_valid = (parse->sleeping_ident->value != globals.invalid_identifier);
                        sleeping = is_valid ? parse->sleeping_ident : 0;
                    }
                    error = true;
                }break;
                case WORK_parse_function_body:{
                    struct parse_work *parse = work->data;
                    struct ast_function *function = parse->function;
                    sleeping = function->base.token;
                    error = true;
                }break;
                invalid_default_case();
            }
            
            
#if 0
            if(work->description == WORK_parse_global_scope_entry){
                if(sleeping && sleeping->value){
                    print("%.*s ", sleeping->value->amount, sleeping->value->data);
                }else{
                    print("??? ");
                }
                char *sleep_purposes[] = {
                    "invalid",
                    "on_struct",
                    "on_decl",
                };
                char *purpose_string = sleep_purposes[sleep_hash_get_purpose(sleeper_node->hash)];
                
                print("sleeping on %.*s with purpose %s\n", sleeping_on->length, sleeping_on->data, purpose_string);
            }
#endif
            
            // we could have a type and an identifier of the same name... we don't really have good access to
            // the sleep reason right now. Otherwise we could assert here... @cleanup
            //assert(!lookup_identifier(context, sleeping_on) || !lookup_type(context, sleeping_on));
            assert(!sleeping || sleeping->value);
            
            struct explain_node *begin_node = null;
            struct explain_node *end_node   = null;
            
            for(u32 node_index = 0; node_index < amount_of_nodes; node_index++){
                struct explain_node *node = nodes + node_index;
                if(node->node == sleeping_on){
                    end_node = node;
                    if(begin_node) break;
                }else if(sleeping && node->node == sleeping->value){
                    begin_node = node;
                    node->token = sleeping;
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
                if(begin_node->outgoing_edge) continue; // @note: not implied as we could have chaines
            }else{
                if(sleeping){ // if we know what we are sleeping on add it to the graph
                    struct explain_node *new_node = push_struct(node_arena, struct explain_node);
                    new_node->node = sleeping->value;
                    new_node->token = sleeping;
                    amount_of_nodes += 1;
                    begin_node = new_node;
                }
            }
            
            if(!end_node){
                struct explain_node *new_node = push_struct(node_arena, struct explain_node);
                new_node->node = sleeping_on;
                new_node->token = sleeping_on_token;
                amount_of_nodes += 1;
                
                end_node = new_node;
            }
            
            // @note: if we don't have a begin_node we don't know what we are sleeping on and
            //        just want to make sure we have a node for 'sleeping_on'.
            //        So no edge needed.
            if(begin_node){ 
                struct explain_dependency *dep = push_struct(edge_arena, struct explain_dependency);
                dep->next = begin_node->dep_list;
                begin_node->dep_list = dep;
                dep->dep = end_node;
                
                assert(!begin_node->outgoing_edge);
                begin_node->outgoing_edge = end_node;
            }
        }
    }
    
    // @cleanup: @note: we still cant print multiple locations for a single undeclared identifier, as we 
    // only get one location for it. this is unfortunate.
    // this might actually be fixable in just this system, but I am not gonna do that right now -16.12.2019
    
    b32 reported_undeclared_identifiers = false;
    for(u32 i = 0; i < amount_of_nodes; i++){
        struct explain_node *node = nodes + i;
        if(!node->outgoing_edge){
            assert(node->token);

            report_error(context, node->token, "Undeclared identifier.");
            reported_undeclared_identifiers = true;
        }
    }
    
    if(!reported_undeclared_identifiers){
        for(u32 i = 0; i < amount_of_nodes; i++){
            struct explain_node *node = nodes + i;
            if(node->visited) continue;
            struct explain_node *circle = visit_nodes_fill_in_outgoings_and_return_circular_dependency(node);
            if(circle){
                print("Program contains circular dependencies:\n");
                struct explain_node *circle_it = circle;
                for(; 
                    circle_it->outgoing_edge != circle; 
                    circle_it = circle_it->outgoing_edge){
                    
                    report_error(context, circle_it->token, "Part of the cycle.");
                }
                
                report_error(context, circle_it->token, "Part of the cycle.");
                
                error = true;
                break;
            }
        }
    }else{
        error = true;
    }
    
    end_error_report(context);
    if(error || context->error){
        assert(!sll_is_empty(context->error_list)); // we better have something to report.
        print_error_stream(context);
        globals.an_error_has_accured = true;
        os_debug_break();
    }
    
    end_tempoary_memory(temp);
    end_tempoary_memory(temp2);
}


func void report_error_for_undefined_functions_and_types(struct context *context){
    assert(!context->error && !context->should_sleep);
    
    // everything below is in one giant error report, so we don't have to reset 'context->error' everytime
    begin_error_report(context); 
    
    for(u64 i = 0; i < globals.global_declarations.capacity; i++){
        struct ast_node *node = globals.global_declarations.nodes + i;
        if(!node->ast) continue;
        
        if(node->ast->kind == AST_function){
            struct ast_function *function = cast(struct ast_function *)node->ast;
            if((function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport) && 
               (function->as_decl.flags & DECLARATION_FLAGS_is_referanced)){
                begin_counter(lookup_dll_import);
                
                struct tempoary_memory temp2 = begin_tempoary_memory(context->arena);
                char *c_string = push_cstring_from_string(context->arena, *function->identifier);
                
                struct dll_import_node *first_dll_import_node = null;
                for(struct dll_import_node *dll = globals.dll_imports.first; dll; dll = dll->next){
                    // @cleanup: specify the index or whatever
                    u8 *location = (u8 *)GetProcAddress(dll->module_handle, c_string);
                    SetLastError(0);
                    
                    if(location){
                        if(first_dll_import_node){
                            report_error(context, function->base.token, "Function is defined in both '%.*s' and '%.*s'.", first_dll_import_node->name.size, first_dll_import_node->name.data, dll->name.size, dll->name.data);
                            break;
                        }
                        first_dll_import_node = dll;
                        function->memory_location = location;
                    }
                }
                
                if(!function->memory_location){
                    report_error(context, function->base.token, "Function is not contained in any of the imported dlls.");
                }
                function->dll_import_node = first_dll_import_node;
                
                end_tempoary_memory(temp2);                
                end_counter(lookup_dll_import);
            }
            
            
            if(!function->is_defined){
                if(function->as_decl.flags & DECLARATION_FLAGS_is_referanced){
                    report_error(context, function->base.token, "Function was declared and referanced but never defined.");
                }else{
                    //report_info(context, function->base.token, "Function was declared but never defined.");    
                }
            }else if(!(function->as_decl.flags & DECLARATION_FLAGS_is_referanced)){
                //report_info(context, function->base.token, "Function was defined but never referanced.");
            }
            
        }else if(node->ast->kind == AST_declaration){
            struct ast_declaration *decl = cast(struct ast_declaration *)node->ast;
            if(!(decl->flags & DECLARATION_FLAGS_is_referanced)){
                assert(string_is_printable(decl->base.token->file->absolute_file_path));
                //report_info(context, decl->base.token, "Global declaration was never referanced.");
            }
            
        }else{
            // I think these are fine, maybe one could do type never used or something later.
            assert(node->ast->kind == AST_typedef);
        }
    }
    
    // @cleanup: later also check both functions and types for unused ones
    
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
                
                /* @incomplete: this case will currently not be detected.
                struct{
                    struct unresolved *first;
                    struct unresolved *last;
                } debug_members;
                */
                
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
    end_error_report(context);
    
    if(context->error){
        globals.an_error_has_accured = true;;
    }
    
    print_error_stream(context);
}


