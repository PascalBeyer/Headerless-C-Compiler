
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

func void report_errors_for_unresolved_sleepers(struct context *context){
    
    assert(globals.compile_stage == COMPILE_STAGE_parse_global_scope_entries);
    
    context->should_exit_statement = false;
    context->should_sleep = false;
    context->error = false;
    
    b32 error = false;
    
    begin_error_report(context);
    
    struct temporary_memory temp = begin_temporary_memory(&context->scratch);
    
    // Here
    struct compilation_unit hack_compilation_units[2] = {
        [0] = {
            .next = &hack_compilation_units[1],
            .static_sleeper_table = globals.compound_sleeper_table,
        },
        [1] = {
            .next = globals.compilation_units.first,
            .static_sleeper_table = globals.declaration_sleeper_table,
        },
    };
    
    // 
    // Iterate throught the 'sleeper_table' which is a map 'sleeping_on' -> list 'sleeper', 
    // and construct a graph 'sleeper' -> 'sleeping_on'. 
    // In the below we then only report undeclared identifiers for terminal nodes in this graph.
    //                                                                                -10.08.2020
    
    // Each 'explain_node' in this graph represents
    //     an 'identifier' that is sleeping,
    //     an 'outgoing_edge' to what we are sleeping on
    //     a list of tokens which sleep on us (for error reporting).
    
    struct {
        struct explain_node *first;
        struct explain_node *last;
    } nodes = zero_struct;
    
    
    for(struct compilation_unit *sleeper_table_compilation_unit = &hack_compilation_units[0]; sleeper_table_compilation_unit; sleeper_table_compilation_unit = sleeper_table_compilation_unit->next){
        
        struct sleeper_table *sleeper_table = &sleeper_table_compilation_unit->static_sleeper_table;
        
        for(u32 sleeper_table_index = 0; sleeper_table_index < sleeper_table->capacity; sleeper_table_index++){
            struct sleeper_node *sleeper_node = sleeper_table->nodes + sleeper_table_index;
            if(!sleeper_node->first_sleeper) continue;
            
            struct token *sleeping_on = sleeper_node->token;
            
            for(struct work_queue_entry *work = sleeper_node->first_sleeper; work; work = work->next){
                // 
                // Find out who is sleeping. This can be null meaning that we don't know yet. E.g.:
                //      imt foo(){}
                // would sleep at 'imt' (typo for int) and not know, that the sleeper is 'foo'.
                // 
                struct token *sleeping = null;
                struct token *sleeping_on_token = work->sleeping_on;
                
                struct parse_work *parse = work->data;
                
                struct compilation_unit *compilation_unit = parse->compilation_unit;
                
                
                // @cleanup: @incomplete: we right now have it be 0 if its invalid identifier is that right?
                if(parse->sleeping_ident){
                    b32 is_valid = !atoms_match(parse->sleeping_ident->atom, globals.invalid_identifier);
                    sleeping = is_valid ? parse->sleeping_ident : 0;
                }
                error = true;
                
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
    }
    
    b32 reported_undeclared_identifiers = false;
    for(struct explain_node *node = nodes.first; node; node = node->next){
        // If the node does not have an outgoing edge, this means that the node does not sleep on anybody.
        // Therefore, it was not in the sleeper table and is a unresolved identifier.
        // So report everyone that is sleeping on it here.                               14.05.2021
        
        if(!node->outgoing_edge){
            assert(node->amount_of_locations > 0);
            for(u32 location_index = 0; location_index < node->amount_of_locations; location_index++){
                report_error(context, node->locations[location_index].token, "[%lld] Undeclared identifier.", node->locations[location_index].compilation_unit->index);
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


struct coff_section_header{
    char name[8];
    u32 virtual_size;
    u32 virtual_address;
    u32 size_of_raw_data;
    u32 pointer_to_raw_data;
    u32 pointer_to_relocations;
    u32 pointer_to_line_numbers;
    u16 number_of_relocations;
    u16 number_of_line_numbers;
    u32 characteristics;
};


struct coff_relocation {
    u32 relocation_address;
    u32 symbol_table_index;
    u16 relocation_type;
};

struct coff_symbol {
    union{
        char short_name[8];
        struct{
            u32 zeroes;
            u32 offset;
        } long_name;
    };
    
    u32 value;
    u16 section_number;
    u16 symbol_type;
    u8 storage_class;
    u8 number_of_auxiliary_symbol_records;
};

int try_to_extract_simple_declaration_from_object_file(struct coff_file_header *coff_file_header, u64 coff_file_size, struct ast_declaration *declaration){
    
    (void)coff_file_size; // @cleanup: Check any offsets.
    
    struct coff_section_header *section_headers = (void *)(coff_file_header + 1);
    
    struct string identifier = declaration->identifier->string;
    
    char *string_table = (char*)coff_file_header + coff_file_header->pointer_to_symbol_table + coff_file_header->number_of_symbols * 18;
    
    u8 *symbol_at = (u8*)coff_file_header + coff_file_header->pointer_to_symbol_table;
    for(u32 symbol_index = 0; symbol_index < coff_file_header->number_of_symbols; symbol_index++){
        struct coff_symbol *symbol = (struct coff_symbol *)(symbol_at + symbol_index * 18);
        symbol_index += symbol->number_of_auxiliary_symbol_records;
        
        struct string symbol_name;
        if(symbol->long_name.zeroes == 0){
            symbol_name = string_from_cstring(string_table + symbol->long_name.offset);
        }else{
            if(symbol->short_name[7] == 0){
                symbol_name = string_from_cstring(symbol->short_name);
            }else{
                symbol_name = create_string((u8 *)symbol->short_name, 8);
            }
        }
        
        if(string_match(symbol_name, identifier)){
            // We found the symbol, get the corresponding section. @cleanup: check in bounds and non-zero.
            struct coff_section_header *section = &section_headers[symbol->section_number-/*one-based*/1];
            if((section->number_of_relocations == 0) && (section->size_of_raw_data == declaration->type->size)){
                // @cleanup: check that the section characteristics match what we expect (data or function).
                declaration->memory_location = (u8 *)coff_file_header + section->pointer_to_raw_data;
                declaration->assign_expr = &globals.empty_statement;// @HACK: Is this how we want to solve this?
                return 1;
            }
        }
    }
    return 0;
}

void lookup_declaration_in_libraries(struct context *context, struct ast_declaration *declaration, struct token *token_that_referenced_this_declaration){
    
    char *function_or_declaration = (declaration->base.kind == AST_function) ? "function" : "declaration";
    char *Function_or_Declaration = (declaration->base.kind == AST_function) ? "Function" : "Declaration";
    int is_dll_import = (declaration->flags & DECLARATION_FLAGS_is_dllimport) != 0;
    
    // 
    // Search the libraries for an identifier, if we found an import header,
    // we can return a dll_import_node. If we found an object file that defines the symbol,
    // remember that, but keep on searching.
    // 
    
    struct string object_file_library_name = {0};
    struct string import_library_name = {0};
    
    struct ar_import_header *ar_import_header = 0;
    struct coff_file_header *coff_file_header = 0;
    u64 coff_file_size = 0;
    
    struct atom identifier = declaration->identifier->atom;
    
    
    while(true){
        for(struct library_node *library = globals.libraries.first; library; library = library->next){
            
            struct ar_symbol_lookup found = ar_lookup_symbol(library, identifier.string);
            if(found.lookup_result == AR_SYMBOL_LOOKUP_failed) continue;
            
            if(found.lookup_result == AR_SYMBOL_LOOKUP_import_header){
                ar_import_header = found.ar_import_header;
                import_library_name = library->path;
                break;
            }else{
                object_file_library_name = library->path;
                if(!is_dll_import){
                    coff_file_header = found.coff_file_header;
                    coff_file_size = found.file_size;
                    break;
                }
            }
        }
        
        if(!ar_import_header && !object_file_library_name.data){
            struct alternate_name *alternate_name = globals.alternate_names.first;
            for(; alternate_name; alternate_name = alternate_name->next){
                if(atoms_match(alternate_name->source, identifier)){ // @cleanup: Cycles?
                    identifier = alternate_name->destination;
                    break;
                }
            }
            if(alternate_name == null) break;  // @cleanup: In the below also report on alternate names being used.
        }else{
            break;
        }
    }
    
    
    if(!ar_import_header && !object_file_library_name.data){
        // We have not found the symbol.
        if(is_dll_import){
            report_error(context, declaration->base.token, "%s is not contained in any of the imported dlls.", Function_or_Declaration);
            report_error(context, token_that_referenced_this_declaration, "... Here the %s was referenced.", function_or_declaration);
        }else{
            report_error(context, declaration->base.token, "External %s was declared and referenced but never defined.", function_or_declaration);
            report_error(context, token_that_referenced_this_declaration, "... Here the %s was referenced.", function_or_declaration);
        }
        return;
    }
    
    if(coff_file_header){
        // 
        // Try to extract the declaration from the object file if it is "simple".
        // 
        int success = try_to_extract_simple_declaration_from_object_file(coff_file_header, coff_file_size, declaration);
        
        if(!success){
            // @incomplete:
            report_error(context, declaration->base.token, "%s is defined in library '%.*s', but static linking is currently not supported.", Function_or_Declaration, object_file_library_name.size, object_file_library_name.data);
            report_error(context, token_that_referenced_this_declaration, "... Here the %s was referenced.", function_or_declaration);
        }
        return;
    }
    
    if(!ar_import_header){
        assert(object_file_library_name.data);
        
        // :Error no static linking
        report_error(context, declaration->base.token, "Referenced %s declared `__declspec(dllimport)` is defined in library '%.*s'.", function_or_declaration, object_file_library_name.size, object_file_library_name.data);
        report_error(context, token_that_referenced_this_declaration, "... Here the %s was referenced.", function_or_declaration);
        return;
    }
    
    if(object_file_library_name.data){
        report_warning(context, WARNING_imported_function_is_also_defined, declaration->base.token, "%s is imported from '%.*s', but also defined in library '%.*s'.", Function_or_Declaration, import_library_name.size, import_library_name.data, object_file_library_name.size, object_file_library_name.data);
    }
    
    if(declaration->base.kind == AST_declaration){
        report_error(context, declaration->base.token, "@incomplete: Currently no __declspec(dllimport) for data declarations?");
        return;
    }
    
    struct ast_function *function = (struct ast_function *)declaration;
    
    u8 *library_name = (u8 *)(ar_import_header + 1) + identifier.size + 1;
    
    struct string dll_name = cstring_to_string((char *)library_name);
    
    struct dll_node *dll_node = globals.dlls.first;
    for(; dll_node; dll_node = dll_node->next){
        if(string_match(dll_node->name, dll_name)) break;
    }
    
    if(!dll_node){
        dll_node = push_struct(context->arena, struct dll_node);
        dll_node->name = dll_name;
        
        sll_push_back(globals.dlls, dll_node);
        globals.dlls.amount += 1;
    }
    
    // @note: We should not have to lookup the 'identifier' in the 'dll_node' as otherwise,
    //        we should have already been here and set the 'import_node' member of the
    //        lookup table.
    
    struct dll_import_node *import_node = push_struct(context->arena, struct dll_import_node);
    import_node->import_name  = identifier.string;
    import_node->ordinal_hint = ar_import_header->ordinal_hint;
    
    sll_push_back(dll_node->import_list, import_node);
    dll_node->import_list.count += 1;
    
    if(is_dll_import){
        function->dll_import_node = import_node;
    }else{
        // :dllimports_with_missing_declspec
        // 
        // Apparently, MSVC does note require 'dllimport'. 
        // We report a warning and then simply mark the declaration as being dllimport.
        // This works fine, because we do this before we emit code for functions.
        // 
        
        if(!globals.file_table.data[declaration->base.token->file_index]->is_system_include){ // @note: Manually shut this warning up, as we are currently in a big error_report.
            // @cleanup: I currently don't see a way of how we can get the name of the library or dll.
            report_warning(context, WARNING_function_is_implicitly_dllimport, declaration->base.token, "%s is treated as dllimport, but was not declared '__declspec(dllimport)'.", Function_or_Declaration);
        }
        
        function->as_decl.flags |= DECLARATION_FLAGS_is_dllimport;
        function->dll_import_node = import_node;
    }
}

