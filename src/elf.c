
struct elf_symbol{
    u32 name_offset;
    u8  info;
    u8  other;
    u16 section_index;
    u64 value;
    u64 size;
};

int elf_parse_file(struct os_file file, struct string file_name, struct memory_arena *arena){
    
    struct elf_header{
        u32 magic;
        u8 size_value;
        u8 endianess_value;
        u8 elf_header_version;
        u8 abi;
        u8 abi_version;
        u8 padding[7];
        u16 object_file_type;
        u16 machine_type;
        u32 elf_version;
        u64 entry_point;
        u64 program_header_offset;
        u64 section_header_offset;
        u32 flags;
        u16 elf_header_size;
        u16 program_header_entry_size;
        u16 program_header_entry_count;
        u16 section_header_entry_size;
        u16 section_header_entry_count;
        u16 section_name_section_table_index;
    } *elf_header = (void *)file.data;
    
    if(file.size < sizeof(*elf_header)){
        return 1;
    }
    
    if(elf_header->size_value != 2){
        print("Error: Shared Object '%.*s' is 32-bit. This is unsupported.\n", file_name.size, file_name.data);
        return 1;
    }
    
    if(elf_header->endianess_value != 1){
        print("Error: Shared Object '%.*s' is big-endian. This is unsupported.\n", file_name.size, file_name.data);
        return 1;
    }
    
    if(elf_header->object_file_type != 3){
        print("Error: Shared Object '%.*s' is not a shared object.\n", file_name.size, file_name.data);
        return 1;
    }
    
    if(elf_header->machine_type != 0x3E){
        print("Error: Shared Object '%.*s' is not x64.\n", file_name.size, file_name.data);
        return 1;
    }
    
    u64 section_header_offset = elf_header->section_header_offset;
    u64 section_header_entry_count = elf_header->section_header_entry_count;
    u64 section_header_entry_size  = elf_header->section_header_entry_size;
    u64 section_header_size = section_header_entry_count * section_header_entry_size;
    u64 section_header_end = section_header_offset + section_header_size;
    
    if(section_header_offset > file.size || section_header_end > file.size){
        print("Error: Shared Object '%.*s' has invalid section header.\n", file_name.size, file_name.data);
        return 1;
    }
    
    struct elf_section_header{
        u32 name_offset;
        u32 type;
        u64 flags;
        u64 address;
        u64 offset;
        u64 size;
        u32 linked_section;
        u32 info;
        u64 alignment;
        u64 entry_size;
    } *dynamic_symbol_section_header = 0, *string_table_section_header = 0, *symbol_hash_section_header = 0;
    
    if(elf_header->section_header_entry_size < sizeof(*dynamic_symbol_section_header)){
        print("Error: Shared Object '%.*s' specifies to small section header size 0x%x. Expected at least 0x%x.\n", file_name.size, file_name.data, elf_header->section_header_entry_size, sizeof(*dynamic_symbol_section_header));
        return 1;
    }
    
    for(u64 offset = section_header_offset; offset < section_header_end; offset += section_header_entry_size){
        struct elf_section_header *section_header = (void *)(file.data + offset);
        if(section_header->type == /*SHT_DYNSYM*/11){
            dynamic_symbol_section_header = section_header;
            break;
        }
    }
    
    if(!dynamic_symbol_section_header){
        print("Warning: Shared Object '%.*s' does not export any symbols.\n", file_name.size, file_name.data);
        return 0;
    }
    
    u64 dynamic_symbol_section_offset = dynamic_symbol_section_header->offset;
    u64 dynamic_symbol_section_size   = dynamic_symbol_section_header->size;
    u64 dynamic_symbol_section_end    = dynamic_symbol_section_offset + dynamic_symbol_section_size;
    u64 dynamic_symbol_table_entry_size = dynamic_symbol_section_header->entry_size;
    u64 dynamic_symbol_count = dynamic_symbol_section_size / dynamic_symbol_table_entry_size;
    
    if(dynamic_symbol_section_offset > file.size || dynamic_symbol_section_end > file.size){
        print("Error: Shared Object '%.*s' specifies invalid dynsym section.\n", file.size, file.data);
        return 1;
    }
    
    u32 string_table_section_index = dynamic_symbol_section_header->linked_section;
    if(string_table_section_index >= section_header_entry_count){
        print("Error: Shared Object '%.*s' dynsym section specifies invalid link section.\n", file.size, file.data);
        return 1;
    }
    
    string_table_section_header = (void *)(file.data + section_header_offset + section_header_entry_size * string_table_section_index);
    
    u64 string_table_section_offset = string_table_section_header->offset;
    u64 string_table_section_size   = string_table_section_header->size;
    u64 string_table_section_end    = string_table_section_offset + string_table_section_size;
    
    u8 *string_table = file.data + string_table_section_offset;
    
    if(string_table_section_offset > file.size || string_table_section_end > file.size || string_table[string_table_section_size-1] != 0){
        print("Error: Shared Object '%.*s' specifies invalid strtab section.\n", file.size, file.data);
        return 1;
    }
    
    u64 dynamic_symbol_section_index = ((u8 *)dynamic_symbol_section_header - (file.data + section_header_offset))/section_header_entry_size;
    
    for(u64 offset = section_header_offset; offset < section_header_end; offset += section_header_entry_size){
        struct elf_section_header *section_header = (void *)(file.data + offset);
        if(section_header->type == /*SHT_HASH*/5 && section_header->linked_section == dynamic_symbol_section_index){
            symbol_hash_section_header = section_header;
            break;
        }
    }
    
    if(!symbol_hash_section_header){
        print("Error: Shared Object '%.*s' does not contain a symbol hash section for the .dynsym section. This is currently not supported.\n", file_name.size, file_name.data);
        return 1;
    }
    
    u64 symbol_hash_section_offset = symbol_hash_section_header->offset;
    u64 symbol_hash_section_size   = symbol_hash_section_header->size;
    u64 symbol_hash_section_end    = symbol_hash_section_offset + symbol_hash_section_size;
    if(symbol_hash_section_offset > file.size || symbol_hash_section_end > file.size || symbol_hash_section_size < 8){
        print("Error: Shared Object '%.*s' specifies invalid hash section.\n", file.size, file.data);
        return 1;
    }
    
    u32 *symbol_hash_section = (u32 *)(file.data + symbol_hash_section_offset);
    u32 hash_bucket_count = symbol_hash_section[0];
    u32 hash_chain_count  = symbol_hash_section[1];
    u32 *hash_buckets = symbol_hash_section + 2;
    u32 *hash_chains = hash_buckets + hash_bucket_count;
    
    if(hash_chain_count < dynamic_symbol_count){
        print("Error: Shared Object '%.*s' specifies invalid hash chain count in its .hash section.\n", file_name.size, file_name.data);
        return 1;
    }
    
    if(dynamic_symbol_table_entry_size < sizeof(struct elf_symbol)){
        print("Error: Shared Object '%.*s' has invalid dynamic symbol size (dynsym->sh_entsize) 0x%x. Expected at least 0x%x.\n", file_name.size, file_name.data, dynamic_symbol_table_entry_size, sizeof(struct elf_symbol));
        return 1;
    }
    
    struct library_node *library_node = push_struct(arena, struct library_node);
    library_node->kind = LIBRARY_NODE_shared_object;
    library_node->path = file_name;
    library_node->file = file;
    
    library_node->shared_object.hash_buckets      = hash_buckets;
    library_node->shared_object.hash_bucket_count = hash_bucket_count;
    library_node->shared_object.hash_chains       = hash_chains;
    library_node->shared_object.dynamic_symbol_count = dynamic_symbol_count;
    library_node->shared_object.dynamic_symbol_section_offset = dynamic_symbol_section_offset;
    library_node->shared_object.dynamic_symbol_table_entry_size = dynamic_symbol_table_entry_size;
    library_node->shared_object.string_table = string_table;
    library_node->shared_object.string_table_size = string_table_section_size;
    
    sll_push_back(globals.libraries, library_node);
    globals.libraries.amount += 1;
    
    return 0;
}

u64 elf_symbol_name_hash(struct string identifier){
    u64 hash = 0;
    for(smm index = 0; index < identifier.size; index++){
        hash = (hash << 4) + identifier.data[index];
        u64 top_nibble = hash & 0xf0000000;
        if(top_nibble){
            hash ^= (top_nibble >> 24);
            hash &= ~top_nibble;
        }
    }
    return hash;
}

struct elf_symbol *elf_lookup_symbol(struct library_node *library_node, struct string identifier){
    
    struct so_library_node *so = &library_node->shared_object;
    
    u64 hash = elf_symbol_name_hash(identifier);
    
    u32 symbol_index = so->hash_buckets[hash % so->hash_bucket_count];
    
    struct elf_symbol *found = 0;
    
    while(1){
        
        if(symbol_index >= so->dynamic_symbol_count){
            print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
            return null;
        }
        
        struct elf_symbol *symbol = (void *)(library_node->file.data + so->dynamic_symbol_section_offset + so->dynamic_symbol_table_entry_size * symbol_index);
        if(symbol->info == 0) break;
        
        u32 name_offset = symbol->name_offset;
        
        if(name_offset >= so->string_table_size){
            print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
            return null;
        }
        
        if(string_match_cstring(identifier, (char *)(so->string_table + name_offset))){
            found = symbol;
            break;
        }
        
        symbol_index = so->hash_chains[symbol_index];
    }
    
    return found;
}

void write_elf(struct string output_file_path, struct memory_arena *arena, struct memory_arena *scratch){
    
    // 
    // Gather the symbols.
    // 
    
    struct ast_list typedefs = zero_struct;
    
    struct ast_list exports = zero_struct;
    struct ast_list imports = zero_struct;
    
    struct ast_list defined_functions = zero_struct;
    
    struct ast_list initialized_declarations = zero_struct;
    struct ast_list uninitialized_declarations = zero_struct;
    struct ast_list tls_declarations = zero_struct;
    
    for(struct compilation_unit *compilation_unit = &globals.hacky_global_compilation_unit; compilation_unit; compilation_unit = compilation_unit->next){
        
        struct ast_table *table = &compilation_unit->static_declaration_table;
        
        for(u64 table_index = 0; table_index < table->capacity; table_index++){
            enum ast_kind *ast = table->nodes[table_index].ast;
            if(!ast) continue;
            
            struct ast_declaration *decl = (struct ast_declaration *)ast;
            
            // If this is one of the local declaration tables, all members in here should be static.
            if(table->nodes != globals.global_declarations.nodes) assert(decl->flags & DECLARATION_FLAGS_is_static);
            
            switch(*ast){
                case IR_function:{
                    struct ast_function *function = (struct ast_function *)ast;
                    
                    if(!(function->as_decl.flags & DECLARATION_FLAGS_is_reachable_from_entry)) continue;
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_intrinsic)  continue;
                    if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                    
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
                        assert(function->import_node);
                        ast_list_append(&imports, scratch, &function->kind);
                        // if(function->as_decl.flags & DECLARATION_FLAGS_need_dllimport_stub_function) ast_list_append(&dll_function_stubs, scratch, &function->kind);
                        continue;
                    }
                    
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                        ast_list_append(&exports, scratch, &function->kind);
                    }
                    
                    ast_list_append(&defined_functions, scratch, &function->kind);
                    
                    for_ast_list(function->static_variables){
                        struct ast_declaration *static_decl = (struct ast_declaration *)it->value;
                        
                        if(static_decl->flags & DECLARATION_FLAGS_is_thread_local){
                            ast_list_append(&tls_declarations, arena, &static_decl->kind);
                            continue;
                        }
                        
                        if(static_decl->assign_expr){
                            ast_list_append(&initialized_declarations, scratch, &static_decl->kind);
                        }else{
                            ast_list_append(&uninitialized_declarations, scratch, &static_decl->kind);
                        }
                    }
                }break;
                
                case IR_typedef:{
                    if(!(decl->flags & DECLARATION_FLAGS_is_reachable_from_entry)) continue;
                    
                    ast_list_append(&typedefs, arena, ast);
                }break;
                
                case IR_declaration:{
                    if(!(decl->flags & DECLARATION_FLAGS_is_reachable_from_entry)) continue;
                    
                    if(decl->flags & DECLARATION_FLAGS_is_thread_local){
                        ast_list_append(&tls_declarations, arena, ast);
                        continue;
                    }
                    
                    if(decl->assign_expr){
                        ast_list_append(&initialized_declarations, arena, ast);
                    }else{
                        ast_list_append(&uninitialized_declarations, arena, ast);
                    }
                }break;
                
                invalid_default_case();
            }
        }
    }
    
    // 
    // Add local functions, these are by definition defined.
    // 
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for(struct ast_list_node *local_function_node = thread_context->local_functions.first; local_function_node; local_function_node = local_function_node->next){
            struct ast_function *function = (struct ast_function *)local_function_node->value;
            
            if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
            
            ast_list_append(&defined_functions, scratch, &function->kind);
            
            for_ast_list(function->static_variables){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                
                if(decl->flags & DECLARATION_FLAGS_is_thread_local){
                    ast_list_append(&tls_declarations, arena, &decl->kind);
                    continue;
                }
                
                if(decl->assign_expr){
                    ast_list_append(&initialized_declarations, scratch, &decl->kind);
                }else{
                    ast_list_append(&uninitialized_declarations, scratch, &decl->kind);
                }
            }
        }
    }
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        // 
        // Append the declarations for 'global_struct_and_array_literals' to the 'initialized_declarations'.
        // 
        
        for_ast_list(thread_context->global_struct_and_array_literals){
            ast_list_append(&initialized_declarations, arena, it->value);
        }
    }
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for_ast_list(thread_context->local_dllimports){
            struct ast_function *function = (struct ast_function *)it->value;
            if(function->as_decl.flags & DECLARATION_FLAGS_is_reachable_from_entry){
                assert(function->import_node);
                ast_list_append(&imports, scratch, &function->kind);
            }
        }
    }
    
    
    // 
    // Start writing the ELF file
    // 
    // ELF-layout:
    // 
    //    Elf-header
    //    Program headers
    //    section headers
    //    
    
    push_align(arena, 0x1000);
    
    u8 *elf_base = arena_current(arena);
    u8 *header_segment_start = elf_base;
    
    struct elf_header{
        u8 magic[4];
        u8 size_value;
        u8 endianess_value;
        u8 elf_header_version;
        u8 abi;
        u8 abi_version;
        u8 padding[7];
        u16 object_file_type;
        u16 machine_type;
        u32 elf_version;
        u64 entry_point;
        u64 program_header_table_offset;
        u64 section_header_table_offset;
        u32 flags;
        u16 elf_header_size;
        u16 program_header_table_entry_size;
        u16 program_header_table_entry_count;
        u16 section_header_table_entry_size;
        u16 section_header_table_entry_count;
        u16 section_name_section_table_index;
    } *elf_header = push_struct(arena, struct elf_header);
    
    elf_header->magic[0] = 0x7f;
    elf_header->magic[1] = 'E';
    elf_header->magic[2] = 'L';
    elf_header->magic[3] = 'F';
    elf_header->size_value = /*64-bit*/2;
    elf_header->endianess_value = /*little*/1;
    elf_header->elf_header_version = 1;
    elf_header->abi = 3;
    elf_header->abi_version = 0;
    elf_header->object_file_type = /*ET_EXEC*/2; // @cleanup: for so /*ET_DYN*/3
    elf_header->machine_type = /*x64*/0x3E;
    elf_header->elf_version = 1;
    elf_header->flags = 0;
    elf_header->elf_header_size = sizeof(*elf_header);
    
    // 
    // Program headers:
    // 
    //    These are sort of the same thing as section headers on windows.
    //    They declare a mapping between stuff in the file to stuff in the loaded image
    //    and permissions. They are not supposed to tell you where what data is.
    //    We have to align for the data after the program headers, and we assume
    //    (for now) that there are never going to be more program headers than fit into 0x1000.
    // 
    
    push_align(arena, 0x1000);
    
    enum elf_program_header_type{
        PT_LOAD = 1,
        PT_DYNAMIC = 2,
        PT_INTERP = 3,
        PT_NOTE = 4,
        PT_PHDR = 6,
        PT_TLS = 7,
    };
    
    enum elf_program_header_flags{
        PF_EXECUTE = 1,
        PF_WRITE = 2,
        PF_READ = 4,
    };
    
    struct elf_program_header{
        u32 type;
        u32 flags;
        u64 offset;
        u64 virtual_address;
        u64 physical_address;
        u64 file_size;
        u64 memory_size;
        u64 alignment;
    } *program_headers = (void *)(elf_header + 1);
    
    elf_header->program_header_table_entry_size = sizeof(*program_headers);
    elf_header->program_header_table_offset = (u8 *)program_headers - elf_base;
    
    u64 program_header_at = 0;
    
#define fill_program_header(segment_name, segment_type, segment_flags, segment_alignment){             \
    struct elf_program_header *program_header = program_headers + program_header_at++;                 \
    u64 segment_size = arena_current(arena) - segment_name##_segment_start;                            \
    u64 segment_va = virtual_image_base + current_relative_virtual_address + (segment_name##_segment_start - current_segment_start); \
    program_header->type  = (segment_type);                                                            \
    program_header->flags = (segment_flags);                                                           \
    program_header->offset = segment_name##_segment_start - elf_base;                                  \
    program_header->virtual_address = segment_va;                                                      \
    program_header->physical_address = segment_va;                                                     \
    program_header->file_size   = segment_size;                                                        \
    program_header->memory_size = segment_size;                                                        \
    program_header->alignment = (segment_alignment);                                                   \
    if((u32){segment_type}== PT_LOAD){                                                                 \
        push_align(arena, segment_alignment);                                                          \
        current_segment_start = arena_current(arena);                                                  \
        current_relative_virtual_address += align_up(segment_size, segment_alignment);                 \
    }                                                                                                  \
}
    
    u64 virtual_image_base = 0x400000;
    if(globals.cli_options.image_base_specified) virtual_image_base = globals.cli_options.image_base;
    
    u64 current_relative_virtual_address = 0;
    u8 *current_segment_start = elf_base;
    
    u8 *program_header_segment_start = (u8 *)program_headers;
    struct elf_program_header *program_header_program_header = program_headers + program_header_at;
    fill_program_header(program_header, PT_PHDR, PF_READ, 8);
    
    // 
    // There is an initial program header covering the elf-header and the program headers.
    // 
    struct elf_program_header *elf_header_program_header = program_headers + program_header_at;
    fill_program_header(header, PT_LOAD, PF_READ, 0x1000);
    
    enum elf_section_header_type{
        SHT_PROGBITS = 1,
        SHT_SYMTAB = 2,
        SHT_STRTAB = 3,
        SHT_RELA = 4,
        SHT_HASH = 5,
        SHT_DYNAMIC = 6,
        SHT_NOBITS = 8,
        SHT_DYNSYM = 11,
    };
    
    enum elf_section_header_flags{
        SHF_WRITE = 1,
        SHF_ALLOC = 2,
        SHF_EXECINSTR = 4,
        SHF_INFO_LINK = 0x40,
    };
    
    struct elf_section_header{
        u32 name_offset;
        u32 type;
        u64 flags;
        u64 address;
        u64 offset;
        u64 size;
        u32 linked_section;
        u32 info;
        u64 alignment;
        u64 entry_size;
    } *section_headers = (void *)(program_headers + 0x10);
    
    elf_header->section_header_table_entry_size = sizeof(*section_headers);
    elf_header->section_header_table_offset = (u8 *)section_headers - elf_base;
    
    u64 section_header_at = 1; // @note: the first section header is always empty.
    
    struct string_list section_name_string_table = {0}; // We gather up the section names and emit them in the end.
    string_list_postfix_no_copy(&section_name_string_table, scratch, string("\0"));
    
#define fill_section_header(section_name, section_type, section_flags, section_alignment, section_link, section_info, section_entry_size){  \
    struct elf_section_header *section_header = section_headers + section_header_at++;                                                      \
    u64 name_offset = section_name_string_table.total_size;                                                                                 \
    string_list_postfix_no_copy(&section_name_string_table, scratch, string("." #section_name "\0"));                                       \
    section_header->name_offset = (u32)name_offset;                                                                                         \
    section_header->type = (section_type);                                                                                                  \
    section_header->flags = (section_flags);                                                                                                \
    section_header->address = virtual_image_base + current_relative_virtual_address + (section_name##_section_start - current_segment_start);                             \
    section_header->offset = section_name##_section_start - elf_base;                                                                       \
    section_header->size = arena_current(arena) - section_name##_section_start;                                                             \
    section_header->linked_section = (section_link);                                                                                        \
    section_header->info = (section_info);                                                                                                  \
    section_header->alignment = (section_alignment);                                                                                        \
    section_header->entry_size = (section_entry_size);                                                                                      \
}
    
#define make_relative_virtual_address(section_start, address) (u32)(current_relative_virtual_address + ((u8 *)(address) - (section_start)))
    
    // 
    // Start to actually emit the sections.
    // 
    
    u8 *rx_segment_start = arena_current(arena);
    u8 *plt_section_start = 0;
    u32 plt_section_rva = 0;
    
    {
        u8 *text_section_start = rx_segment_start;
        
        for_ast_list(defined_functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            smm function_size = function->byte_size;
            
            u8 *memory_for_function = push_uninitialized_data(arena, u8, function_size);
            memcpy(memory_for_function, function->memory_location, function_size);
            push_align_initialized_to_specific_value(arena, 16, 0xcc);
            
            function->offset_in_text_section   = memory_for_function - text_section_start;
            function->memory_location          = memory_for_function;
            function->relative_virtual_address = make_relative_virtual_address(rx_segment_start, memory_for_function);
        }
        
        if(arena_current(arena) != text_section_start){
            fill_section_header(text, SHT_PROGBITS, SHF_ALLOC|SHF_EXECINSTR, /*alignment*/4, /*link*/0, /*info*/0, /*entry_size*/0);
        }
        
        if(imports.count){
            
            push_align_initialized_to_specific_value(arena, 16, 0xcc);
            
            plt_section_start = arena_current(arena);
            plt_section_rva   = make_relative_virtual_address(rx_segment_start, plt_section_start);
            
            u8 *plt = push_data(arena, u8, 0x10 + 0x10 * imports.count);
            
            // We add the got relative virtual address when we allocate the .got
            
            // push qword ptr [.got + 0x08]
            // jmp  qword ptr [.got + 0x10]
            
            // ff 35 <offset>
            // ff 25 <offset>
            plt[0] = 0xff; plt[1] = 0x35; *(u32 *)(plt + 2) = 0x08 - (plt_section_rva +  6);
            plt[6] = 0xff; plt[7] = 0x25; *(u32 *)(plt + 8) = 0x10 - (plt_section_rva + 12);
            plt[12] = 0xcc; plt[13] = 0xcc; plt[14] = 0xcc; plt[15] = 0xcc;
            
            plt += 0x10;
            
            for(u32 symbol_index = 0; symbol_index < imports.count; symbol_index++){
                // jmp qword ptr [.got + 0x10 + 8 * symbol_index] ; Call the .got entry.
                // push <symbol_index>                            ; This is where the .got points initially.
                // jmp rip - (0x10 + symbol_index * 0x10)         ; jump back to the initial entry.
                
                u32 plt_entry_offset = 0x10 + 0x10 * symbol_index;
                
                plt[0] = 0xff; plt[1] = 0x25; *(u32 *)(plt + 2) = 0x18 + symbol_index * 8  - (plt_section_rva + plt_entry_offset + 6);
                plt[6]  = 0x68; *(u32 *)(plt + 7) = symbol_index;
                plt[11] = 0xe9; *(s32 *)(plt + 12) = -(s32)(plt_entry_offset + 0x10);
                
                plt += 0x10;
            }
            
            fill_section_header(plt, SHT_PROGBITS, SHF_ALLOC|SHF_EXECINSTR, /*alignment*/0x10, /*link*/0, /*info*/0, /*entry_size*/0x10);
        }
    }
    
    if(rx_segment_start != arena_current(arena)){
        fill_program_header(rx, PT_LOAD, PF_READ | PF_EXECUTE, 0x1000);
    }
    
    u8 *ro_segment_start = arena_current(arena);
    u8 *rodata_section_start = ro_segment_start;
    
    // 
    // @cleanup: Copy and paste.
    // 
    
    // @note: float literals are loaded rip realtive so emit them here in rdata @cleanup: Dedup?
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for(struct ir_emitted_float_literal *lit = thread_context->emitted_float_literals.first; lit; lit = lit->next){
            if(lit->literal.type == &globals.typedef_f32){
                f32 *_float = push_struct(arena, f32);
                *_float = (f32)lit->literal._f32;
                lit->relative_virtual_address = make_relative_virtual_address(ro_segment_start, _float);
            }else{
                assert(lit->literal.type == &globals.typedef_f64);
                f64 *_float = push_struct(arena, f64);
                *_float = lit->literal._f64;
                lit->relative_virtual_address = make_relative_virtual_address(ro_segment_start, _float);
            }
        }
    }
    
    {
        // 
        // @note: string literals are loaded rip realtive so emit them here in rdata
        // 
        smm amount_of_strings = 0;
        
        for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
            struct context *thread_context = globals.thread_infos[thread_index].context;
            amount_of_strings += thread_context->string_literals.amount_of_strings;
        }
        
        struct temporary_memory temp = begin_temporary_memory(scratch);
        
        smm capacity = u64_round_up_to_next_power_of_two((u64)(1.5 * amount_of_strings));
        struct string *string_table = push_data(scratch, struct string, capacity);
        
        for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
            struct context *thread_context = globals.thread_infos[thread_index].context;
            for(struct ir_string_literal *lit = thread_context->string_literals.first; lit; lit = lit->next){
                
                // :string_kind_is_element_size
                smm element_size = (smm)lit->string_kind;
                
                struct string string_literal = lit->value;
                
                u64 hash = string_djb2_hash(string_literal);
                
                for(smm table_index = 0; table_index < capacity; table_index++){
                    smm index = (hash + table_index) & (capacity - 1);
                    
                    if(string_table[index].data == null){
                        
                        push_zero_align(arena, element_size);
                        u8 *base = push_string_copy(arena, string_literal).data;
                        push_data(arena, u8, element_size);
                        
                        string_table[index].data = base;
                        string_table[index].size = string_literal.size + element_size;
                        
                        lit->relative_virtual_address = make_relative_virtual_address(ro_segment_start, base);
                        
                        break;
                    }
                    
                    // 
                    // The strings match if
                    //  1) The size is the size plus the null terminator.
                    //  2) The string is null terminated for element_size bytes.
                    //  3) The strings minus the null terminator match.
                    // 
                    if(string_table[index].size != string_literal.size + element_size) continue;
                    if(memcmp(string_table[index].data + string_literal.size, (char[]){0, 0, 0, 0}, element_size) != 0) continue;
                    if(memcmp(string_table[index].data, string_literal.data, string_literal.size) != 0) continue;
                    
                    lit->relative_virtual_address = make_relative_virtual_address(ro_segment_start, string_table[index].data);
                    break;
                }
            }
        }
        
        end_temporary_memory(temp);
    }
    
    if(rodata_section_start != arena_current(arena)){
        fill_section_header(rodata, SHT_PROGBITS, SHF_ALLOC, /*alignment*/4, /*link*/0, /*info*/0, /*entry_size*/0);
    }
    
    u8 *interp_section_start = arena_current(arena);
    u8 *interp_segment_start = interp_section_start;
    push_zero_terminated_string_copy(arena, string("/lib64/ld-linux-x86-64.so.2"));
    fill_section_header(interp, SHT_PROGBITS, SHF_ALLOC, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
    fill_program_header(interp, PT_INTERP, PF_READ, /*alignment*/1);
    
    u32 dynsym_section_rva = 0;
    
    u8 *dynstr_section_start = 0;
    u64 dynstr_section_index = 0;
    u32 dynstr_section_rva = 0;
    u64 dynstr_section_size = 0;
    
    u32 hash_section_rva = 0;
    
    u64 rela_plt_section_index = 0;
    u32 rela_plt_section_rva  = 0;
    u64 rela_plt_section_size = 0;
    
    struct elf_relocation_addend{
        u64 offset; 
        u64 info;
        u64 addend;
    } *rela_plt_relocations = 0;
    
    if(imports.count){
        dynstr_section_index = section_header_at;
        dynstr_section_start = arena_current(arena);
        dynstr_section_rva = make_relative_virtual_address(ro_segment_start, dynstr_section_start);
        
        push_zero_terminated_string_copy(arena, string("")); // Start with a zero-sized string.
        
        for_ast_list(imports){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            push_zero_terminated_string_copy(arena, atom_get_string(decl->identifier->atom));
        }
        
        for(struct import_library_node *import_library_node = globals.import_libraries.first; import_library_node; import_library_node = import_library_node->next){
            import_library_node->name = push_zero_terminated_string_copy(arena, strip_file_path(import_library_node->name));
        }
        
        dynstr_section_size = arena_current(arena) - dynstr_section_start;
        
        fill_section_header(dynstr, SHT_STRTAB, SHF_ALLOC, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
        
        push_align(arena, 8);
        u64 dynsym_section_index = section_header_at;
        u8 *dynsym_section_start = arena_current(arena);
        dynsym_section_rva = make_relative_virtual_address(ro_segment_start, dynsym_section_start);
        
        {
            push_struct(arena, struct elf_symbol); // The first one is all zeroes for some reason.
            
            struct elf_symbol *symbols = push_uninitialized_data(arena, struct elf_symbol, imports.count);
            u64 symbol_index = 0;
            u32 symbol_name_offset = 1;
            for_ast_list(imports){
                struct ast_declaration *decl = (struct ast_declaration *)it->value;
                struct elf_symbol *symbol = symbols + symbol_index++;
                symbol->name_offset = symbol_name_offset; symbol_name_offset += decl->identifier->size + 1;
                symbol->section_index = 0;
                symbol->value = 0;
                symbol->size = 0;
                symbol->info = /*STB_GLOBAL*/(1 << 4) | /*STT_FUNC*/2;
                symbol->other = /*STV_DEFAULT*/0;
            }
        }
        
        fill_section_header(dynsym, SHT_DYNSYM, SHF_ALLOC, /*alignment*/8, /*link*/(u32)dynstr_section_index, /*info(local_symbol_count)*/1, /*entry_size*/sizeof(struct elf_symbol));
        
        push_align(arena, 8);
        u8 *hash_section_start = arena_current(arena);
        hash_section_rva = make_relative_virtual_address(ro_segment_start, hash_section_start);
        
        {
            u32 *hash_section_header = push_data(arena, u32, 2);
            
            u32 hash_bucket_count = (u32)(2 * imports.count);
            u32 hash_chain_count  = (u32)(imports.count + 1);
            u32 *hash_buckets = push_data(arena, u32, hash_bucket_count);
            u32 *hash_chains  = push_data(arena, u32, imports.count + 1);
            
            hash_section_header[0] = hash_bucket_count;
            hash_section_header[1] = hash_chain_count;
            
            u32 symbol_index = 1;
            
            for_ast_list(imports){
                struct ast_declaration *decl = (struct ast_declaration *)it->value;
                
                u64 hash = elf_symbol_name_hash(atom_get_string(decl->identifier->atom));
                u32 hash_bucket_index = hash % hash_bucket_count;
                
                u32 collision_symbol_index = hash_buckets[hash_bucket_index];
                if(!collision_symbol_index){
                    hash_buckets[hash_bucket_index] = symbol_index;
                }else{
                    while(1){
                        u32 chain_entry = hash_chains[collision_symbol_index];
                        
                        if(!chain_entry){
                            hash_chains[collision_symbol_index] = symbol_index;
                            break;
                        }
                        
                        collision_symbol_index = hash_chains[collision_symbol_index];
                    }
                }
                
                symbol_index += 1;
            }
        }
        
        fill_section_header(hash, SHT_HASH, SHF_ALLOC, /*alignment*/8, /*link*/(u32)dynsym_section_index, /*info*/0, /*entry_size*/4);
        
        push_align(arena, 8);
        u8 *rela_plt_section_start = arena_current(arena);
        rela_plt_section_rva = make_relative_virtual_address(ro_segment_start, rela_plt_section_start);
        
        rela_plt_relocations = push_uninitialized_data(arena, struct elf_relocation_addend, imports.count);
        rela_plt_section_size = arena_current(arena) - rela_plt_section_start;
        
        rela_plt_section_index = section_header_at;
        fill_section_header(rela_plt, SHT_RELA, SHF_ALLOC | SHF_INFO_LINK, /*alignment*/8, /*link*/(u32)dynsym_section_index, /*info(to be filled in)*/0, /*entry_size*/sizeof(struct elf_relocation_addend));
    }
    
    if(rodata_section_start != arena_current(arena)){
        fill_program_header(ro, PT_LOAD, PF_READ, 0x1000);
    }
    
    u8 *rw_segment_start = arena_current(arena);
    
    if(imports.count){
        push_align(arena, 8);
        
        u8 *got_section_start = arena_current(arena);
        u32 got_section_rva = make_relative_virtual_address(rw_segment_start, got_section_start);
        
        u64 *got = push_uninitialized_data(arena, u64, imports.count + 3);
        got[0] = 0; // filled in below. Not sure if this is actually used.
        got[1] = 0; // reserved
        got[2] = 0; // reserved
        
        *(u32 *)(plt_section_start + 2) += got_section_rva;
        *(u32 *)(plt_section_start + 8) += got_section_rva;
        
        {
            smm import_index = 0;
            for_ast_list(imports){
                u32 plt_entry_offset = (u32)(0x10 + 0x10 * import_index);
                u32 import_plt_rva = plt_section_rva + plt_entry_offset;
                
                struct ast_function *function = (struct ast_function *)it->value;
                assert(function->kind == IR_function);
                function->relative_virtual_address = import_plt_rva;
                
                got[import_index + 3] = virtual_image_base + import_plt_rva + 6;
                *(u32 *)(plt_section_start + plt_entry_offset + 2) += got_section_rva;
                
                import_index++;
            }
        }
        
        u64 got_section_index = section_header_at;
        fill_section_header(got, SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/8, /*link*/0, /*info*/0, /*entry_size*/8);
        
        {
            // Now that we know where the got is, fill out the rela.plt relocations:
            
            for(smm symbol_index = 0; symbol_index < imports.count; symbol_index++){
                struct elf_relocation_addend *relocation = rela_plt_relocations + symbol_index;
                relocation->info = ((symbol_index + 1) << 32) | /*R_X86_64_JUMP_SLOT*/7;
                relocation->offset = virtual_image_base + got_section_rva + 8 * symbol_index + 0x18;
                relocation->addend = 0;
            }
            
            section_headers[rela_plt_section_index].info = (u32)got_section_index;
        }
            
        u8 *dynamic_section_start = arena_current(arena);
        u8 *dynamic_segment_start = dynamic_section_start;
        u32 dynamic_section_rva = make_relative_virtual_address(rw_segment_start, dynamic_section_start);
        got[0] = virtual_image_base + dynamic_section_rva;
        
        for(struct import_library_node *import_library_node = globals.import_libraries.first; import_library_node; import_library_node = import_library_node->next){
            *push_struct(arena, u64) = /*DT_NEEDED*/1;
            *push_struct(arena, u64) = import_library_node->name.data - dynstr_section_start;
        }
        
        *push_struct(arena, u64) = /*DT_HASH*/4;
        *push_struct(arena, u64) = virtual_image_base + hash_section_rva;
        
        *push_struct(arena, u64) = /*DT_STRTAB*/5;
        *push_struct(arena, u64) = virtual_image_base + dynstr_section_rva;
        
        *push_struct(arena, u64) = /*DT_SYMTAB*/6;
        *push_struct(arena, u64) = virtual_image_base + dynsym_section_rva;
        
        *push_struct(arena, u64) = /*DT_STRSZ*/10;
        *push_struct(arena, u64) = dynstr_section_size;
        
        *push_struct(arena, u64) = /*DT_SYMENT*/11;
        *push_struct(arena, u64) = sizeof(struct elf_symbol);
        
        *push_struct(arena, u64) = /*DT_PLTGOT*/3;
        *push_struct(arena, u64) = virtual_image_base + got_section_rva;
        
        *push_struct(arena, u64) = /*DT_PLTRELSZ*/2;
        *push_struct(arena, u64) = rela_plt_section_size;
        
        *push_struct(arena, u64) = /*DT_PLTREL*/20;
        *push_struct(arena, u64) = /*R_X86_64_JUMP_SLOT*/7;
        
        *push_struct(arena, u64) = /*DT_JMPREL*/0x17;
        *push_struct(arena, u64) = virtual_image_base + rela_plt_section_rva;
        
        *push_struct(arena, u64) = /*DT_FLAGS*/0x1e;
        *push_struct(arena, u64) = /*DF_BIND_NOW*/8;
        
        *push_struct(arena, u64) = /*DT_NONE*/0;
        *push_struct(arena, u64) = 0;
        
        fill_section_header(dynamic, SHT_DYNAMIC, SHF_ALLOC | SHF_WRITE, /*alignment*/8, /*link*/(u32)dynstr_section_index, /*info*/0, /*entry_size*/0x10);
        fill_program_header(dynamic, PT_DYNAMIC, PF_READ | PF_WRITE, /*alignment*/8);
    }
    
    
    u8 *data_section_start = arena_current(arena);
    
    for_ast_list(initialized_declarations){
        struct ast_declaration *decl = (struct ast_declaration *)it->value;
        
        smm alignment = get_declaration_alignment(decl);
        smm decl_size = get_declaration_size(decl);
        
        push_zero_align(arena, alignment);
        
        assert(decl->memory_location);
        
        if(decl_size == 0) decl_size = 1; // Ensure even zero-sized declarations have unique addresses.
        
        u8 *mem = push_uninitialized_data(arena, u8, decl_size);
        memcpy(mem, decl->memory_location, decl_size);
        
        decl->memory_location = mem;
        decl->relative_virtual_address = make_relative_virtual_address(rw_segment_start, mem);
    }
    
    if(arena_current(arena) != data_section_start){
        fill_section_header(data, SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/4, /*link*/0, /*info*/0, /*entry_size*/0);
    }
    
    smm bss_size = 0;
    {
        push_align(arena, 0x10);
        u8 *bss_section_start = arena_current(arena);
        u64 bss_virtual_address_base = current_relative_virtual_address + (bss_section_start - rw_segment_start);
        
        for_ast_list(uninitialized_declarations){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            
            smm alignment = get_declaration_alignment(decl);
            smm decl_size = get_declaration_size(decl);
            
            if(decl_size) decl_size = 1;
            
            bss_size = align_up(bss_size, alignment);
            decl->relative_virtual_address = bss_virtual_address_base + bss_size;
            bss_size += decl_size;
        }
        
        if(bss_size){
            struct elf_section_header *bss_section_header = section_headers + section_header_at;
            fill_section_header(bss, SHT_NOBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/0x10, /*link*/0, /*info*/0, /*entry_size*/0);
            
            bss_section_header->offset = 0;
            bss_section_header->size = bss_size;
        }
    }
    
    // 
    // @warninig: We assume this is the last segment.
    // 
    
    if((arena_current(arena) != rw_segment_start) || bss_size){
        struct elf_program_header *rw_program_header = program_headers + program_header_at;
        fill_program_header(rw, 0, PF_READ | PF_WRITE, 0x1000);
        
        u64 segment_size = arena_current(arena) - rw_segment_start;
        
        rw_program_header->type = PT_LOAD;
        rw_program_header->file_size = segment_size;
        rw_program_header->memory_size = segment_size + bss_size;
        
        current_segment_start = arena_current(arena);
        current_relative_virtual_address += align_up(segment_size + bss_size, 0x1000);
    }
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for(struct patch_node *patch = thread_context->local_patch_list.first; patch; patch = patch->next){
            
            enum ir_kind source_kind = patch->source->kind;
            
            u8 *memory_location = patch->dest_declaration->memory_location + patch->location_offset_in_dest_declaration;
            
            if(patch->kind == PATCH_rip_relative){
                assert(patch->dest_declaration->kind == IR_function);
                assert(patch->rip_at >= 0);
                
                smm dest_location = patch->dest_declaration->relative_virtual_address;
                smm rip_at = dest_location + patch->rip_at;
                
                if(source_kind == IR_function || source_kind == IR_declaration){
                    struct ast_declaration *source_declaration = (struct ast_declaration *)patch->source;
                    
                    smm source_location = source_declaration->relative_virtual_address + patch->location_offset_in_source_declaration;
                    
                    *(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
                }else if(source_kind == IR_emitted_float_literal){
                    struct ir_emitted_float_literal *f = (struct ir_emitted_float_literal *)patch->source;
                    assert(f->relative_virtual_address);
                    
                    smm source_location = f->relative_virtual_address;
                    *(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
                }else if(source_kind == IR_string_literal){
                    struct ir_string_literal *lit = (struct ir_string_literal *)patch->source;
                    
                    smm source_location = lit->relative_virtual_address + patch->location_offset_in_source_declaration;
                    *(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
                }else invalid_code_path;
            }else if(patch->kind == PATCH_absolute){
                assert(patch->dest_declaration->kind == IR_declaration);
                
                smm source_location = virtual_image_base + patch->location_offset_in_source_declaration;
                if(source_kind == IR_function || source_kind == IR_declaration){
                    struct ast_declaration *decl = (struct ast_declaration *)patch->source;
                    
                    if(decl->flags & DECLARATION_FLAGS_is_dllimport){
                        not_implemented;
                    }else{
                        source_location += decl->relative_virtual_address;
                    }
                }else if(source_kind == IR_string_literal){
                    struct ir_string_literal *string_literal = (struct ir_string_literal *)patch->source;
                    source_location += string_literal->relative_virtual_address;
                }
                
                *(smm *)memory_location = source_location;
                
                if(!globals.cli_options.no_dynamic_base){
                    // @incomplete: currently not implemented.
                }
            }else not_implemented;
        }
    }
    
    struct string shstrtab = string_list_flatten(section_name_string_table, arena);
    replace_characters(shstrtab, "_", '.');
    u8 *shstrtab_section_start = shstrtab.data;
    arena->current -= 1;
    push_zero_terminated_string_copy(arena, string(".shstrtab"));
    
    fill_section_header(shstrtab, SHT_STRTAB, /*flags*/0, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
    
    struct elf_program_header *gnu_stack_program_header = program_headers + program_header_at++;
    gnu_stack_program_header->type = /*PT_GNU_STACK*/0x6474e551;
    gnu_stack_program_header->flags = PF_READ | PF_WRITE;
    gnu_stack_program_header->alignment = 0x10;
    
    // 
    // We are done with filling in the sections.
    // Now fill in all the values we left out in the beginning and write out the file.
    // 
    
    elf_header->section_name_section_table_index = (u16)(section_header_at-1);
    elf_header->section_header_table_entry_count = (u16)section_header_at;
    elf_header->program_header_table_entry_count = (u16)program_header_at;
    
    u64 elf_and_program_header_size = (u8 *)(program_headers + program_header_at) - elf_base;
    elf_header_program_header->file_size = elf_and_program_header_size;
    elf_header_program_header->memory_size = elf_and_program_header_size;
    
    u64 program_header_segment_size = (u8 *)(program_headers + program_header_at) - program_header_segment_start;
    program_header_program_header->file_size = program_header_segment_size;
    program_header_program_header->memory_size = program_header_segment_size;
    
    if(globals.entry_point){
        elf_header->entry_point = virtual_image_base + globals.entry_point->relative_virtual_address;
    }
    
    u64 elf_size = arena_current(arena) - elf_base;
    
    if(!globals.cli_options.dont_print_the_files){
        char *elf_name = push_cstring_from_string(arena, output_file_path);
        
        HANDLE file_handle = os_open_file(elf_name, OS_OPEN_write);
        
        int success = os_file_write(file_handle, elf_base, elf_size);
        
        os_close_handle(file_handle);
        
        if(success){
            if(!globals.cli_options.quiet) print("Wrote file: '%s'\n", elf_name);
        }else{
            print("Error: Unable to write file '%s'.\n", elf_name);
            globals.an_error_has_occurred = true;
        }
    }
    
#undef fill_section_header
#undef fill_program_header
#undef make_relative_virtual_address
}


u64 read_uleb(u8 *data, u64 *inout_offset){
    
    u64 offset = *inout_offset;
    u64 result = 0;
    u64 shift = 0;
    
    u8 byte;
    do{
        byte = data[offset++];
        result |= (byte & 0x7f) << shift;
        shift += 7;
    }while((byte & 0x80) != 0);
    
    *inout_offset = offset;
    return result;
}

s64 read_sleb(u8 *data, u64 *inout_offset){
    
    u64 offset = *inout_offset;
    u64 result = 0;
    u64 shift = 0;
    
    u8 byte;
    do{
        byte = data[offset++];
        result |= (byte & 0x7f) << shift;
        shift += 7;
    }while((byte & 0x80) != 0);
    
    if(byte & 0x40){
        result |= (~0ull << shift);
    }
    
    *inout_offset = offset;
    return result;
}

int dump_elf(char *cfile_name, struct memory_arena *arena){
    
    struct os_file file = load_file_into_arena(cfile_name, arena);
    struct string file_name = string_from_cstring(cfile_name);
    
    struct elf_header{
        u32 magic;
        u8 size_value;
        u8 endianess_value;
        u8 elf_header_version;
        u8 abi;
        u8 abi_version;
        u8 padding[7];
        u16 object_file_type;
        u16 machine_type;
        u32 elf_version;
        u64 entry_point;
        u64 program_header_table_offset;
        u64 section_header_table_offset;
        u32 flags;
        u16 elf_header_size;
        
        // The entry_size members can be 0xffff and 0 and the real values are in the initial entry of the section table.
        // Also similarly for `section_name_section_table_index`.
        
        u16 program_header_table_entry_size;
        u16 program_header_table_entry_count;
        u16 section_header_table_entry_size;
        u16 section_header_table_entry_count;
        u16 section_name_section_table_index;
    } *elf_header = (void *)file.data;
    
    if(file.size < sizeof(*elf_header)){
        return 1;
    }
    
    print("{#}\n", *elf_header);
    
    u64 program_header_table_offset = elf_header->program_header_table_offset;
    u64 program_header_table_entry_count = elf_header->program_header_table_entry_count;
    u64 program_header_table_entry_size  = elf_header->program_header_table_entry_size;
    u64 program_header_table_size = program_header_table_entry_count * program_header_table_entry_size;
    u64 program_header_table_end = program_header_table_offset + program_header_table_size;
    
    if(program_header_table_offset > file.size || program_header_table_end > file.size){
        print("Error: Shared Object '%.*s' has invalid section header.\n", file_name.size, file_name.data);
        return 1;
    }
    
    print("\n\nProgram headers (%.16x - %.16x):\n\n", program_header_table_offset, program_header_table_end);
    
    //     00000006 (PT_PHDR)         4   (r) 0000000000000040 0000000000400040 0000000000400040 00000000000001f8 00000000000001f8 0000000000000008
    print("      type                 flags       offset       virtual address  physical address    file size       memory size       alignment\n");
    
    struct elf_program_header{
        u32 type;
        u32 flags;
        u64 offset;
        u64 virtual_address;
        u64 physical_address;
        u64 file_size;
        u64 memory_size;
        u64 alignment;
    };
    
    if(program_header_table_size < sizeof(struct elf_program_header)){
        print("Error: Shared Object '%.*s' has invalid program header size.\n", file_name.size, file_name.data);
        return 1;
    }
    
    u64 section_header_table_offset = elf_header->section_header_table_offset;
    u64 section_header_table_entry_count = elf_header->section_header_table_entry_count;
    u64 section_header_table_entry_size  = elf_header->section_header_table_entry_size;
    u64 section_header_table_size = section_header_table_entry_count * section_header_table_entry_size;
    u64 section_header_table_end = section_header_table_offset + section_header_table_size;
    
    if(section_header_table_offset > file.size || section_header_table_end > file.size){
        print("Error: Shared Object '%.*s' has invalid section header.\n", file_name.size, file_name.data);
        return 1;
    }
    
    u16 section_name_section_table_index = elf_header->section_name_section_table_index;
    if(section_name_section_table_index >= section_header_table_entry_count){
        print("Error: Invalid shstrndx\n");
        return 1;
    }
    
    struct elf_section_header{
        u32 name_offset;
        u32 type;
        u64 flags;
        u64 address;
        u64 offset;
        u64 size;
        u32 linked_section;
        u32 info;
        u64 alignment;
        u64 entry_size;
    } *section_name_string_table_section_header = (void *)(file.data + section_header_table_offset + section_name_section_table_index * section_header_table_entry_size);
    
    u64 section_name_string_table_section_offset = section_name_string_table_section_header->offset;
    u64 section_name_string_table_section_size   = section_name_string_table_section_header->size;
    u64 section_name_string_table_section_end    = section_name_string_table_section_offset + section_name_string_table_section_size;
    u8 *section_name_string_table = file.data + section_name_string_table_section_offset;
    
    if(section_name_string_table_section_offset > file.size || section_name_string_table_section_end > file.size || section_name_string_table[section_name_string_table_section_size-1] != 0){
        print("Error: Invalid section name string table section header.\n");
        return 1;
    }
    
    for(u64 program_header_offset = program_header_table_offset, program_header_index = 0; program_header_offset < program_header_table_end;  program_header_offset += program_header_table_entry_size, program_header_index++){
        struct elf_program_header *program_header = (void *)(file.data + program_header_offset);
        
        static struct{
            u32 value;
            char *name;
        } program_header_types[] = {
            {0x00000000, "(PT_NULL)"    }, // Program header table entry unused.
            {0x00000001, "(PT_LOAD)"    }, // Loadable segment.
            {0x00000002, "(PT_DYNAMIC)" }, // Dynamic linking information.
            {0x00000003, "(PT_INTERP)"  }, // Interpreter information.
            {0x00000004, "(PT_NOTE)"    }, // Auxiliary information.
            {0x00000005, "(PT_SHLIB)"   }, // Reserved.
            {0x00000006, "(PT_PHDR)"    }, // Segment containing program header table itself.
            {0x00000007, "(PT_TLS)"     }, // Thread-Local Storage template.
            
            // https://refspecs.linuxbase.org/LSB_3.1.1/LSB-Core-generic/LSB-Core-generic/progheader.html
            {0x6474e550, "(PT_GNU_EH_FRAME)"}, // The array element specifies the location and size of the exception handling information as defined by the .eh_frame_hdr section.
            {0x6474e551, "(PT_GNU_STACK)"}, // The p_flags member specifies the permissions on the segment containing the stack and is used to indicate wether the stack should be executable. The absense of this header indicates that the stack will be executable.
            {0x6474e552, "(PT_GNU_RELRO)"}, // The array element specifies the location and size of a segment which may be made read-only after relocation shave been processed.
            {0x6474e553, "(PT_GNU_PROPERTY)"},
        };
        
        char *type_name = "(???)";
        for(u32 index = 0; index < array_count(program_header_types); index++){
            if(program_header_types[index].value == program_header->type){
                type_name = program_header_types[index].name;
                break;
            }
        }
        
        u8 flags[5] = {0};
        int flags_at = 0;
        if(program_header->flags){
            flags[flags_at++] = '(';
            if(program_header->flags & 4) flags[flags_at++] = 'r';
            if(program_header->flags & 2) flags[flags_at++] = 'w';
            if(program_header->flags & 1) flags[flags_at++] = 'x';
            flags[flags_at++] = ')';
        }
        
        print("[%2u] %.8x %-17s %x %5s %.16x %.16x %.16x %.16x %.16x %.16x\n", program_header_index, program_header->type, type_name, program_header->flags, flags, program_header->offset, program_header->virtual_address, program_header->physical_address, program_header->file_size, program_header->memory_size, program_header->alignment);
    }
    
    print("\n\nSection headers (%.16x - %.16x):\n\n", section_header_table_offset, section_header_table_end);
    
    //                     .interp (0000001b) 00000001      (SHT_PROGBITS) 00000002 0000000000400238 0000000000000238 000000000000000f 00000000 00000000 0000000000000001 0000000000000000
    print("                  section name              section type             flags  virtual address       offset          size           link     info      alignment        entry size\n");
    for(u64 offset = section_header_table_offset, section_index = 0; offset < section_header_table_end; offset += section_header_table_entry_size, section_index++){
        struct elf_section_header *section_header = (void *)(file.data + offset);
        
        if(section_header->name_offset >= section_name_string_table_section_size){
            print("Invalid section header name offset\n");
            return 1;
        }
        
        u8 *section_name = section_name_string_table + section_header->name_offset;
        
        u64 section_offset = section_header->offset;
        u64 section_size   = section_header->size;
        u64 section_end    = section_offset + section_size;
        
        if(section_header->type != /*SHT_NOBITS*/8){
            if(section_offset > file.size || section_end > file.size){
                print("Error: Invalid section bounds for section %s.\n", section_name);
                return 1;
            }
        }
        
        static char *section_type_strings[] = {
            [0x0] = "(SHT_NULL)", // Section header table entry unused
            [0x1] = "(SHT_PROGBITS)", // Program data
            [0x2] = "(SHT_SYMTAB)", // Symbol table
            [0x3] = "(SHT_STRTAB)", // String table
            [0x4] = "(SHT_RELA)", // Relocation entries with addends
            [0x5] = "(SHT_HASH)", // Symbol hash table
            [0x6] = "(SHT_DYNAMIC)", // Dynamic linking information
            [0x7] = "(SHT_NOTE)", // Notes
            [0x8] = "(SHT_NOBITS)", // Program space with no data (bss)
            [0x9] = "(SHT_REL)", // Relocation entries, no addends
            [0x0A] = "(SHT_SHLIB)", // Reserved
            [0x0B] = "(SHT_DYNSYM)", // Dynamic linker symbol table
            [0x0E] = "(SHT_INIT_ARRAY)", // Array of constructors
            [0x0F] = "(SHT_FINI_ARRAY)", // Array of destructors
            [0x10] = "(SHT_PREINIT_ARRAY)", // Array of pre-constructors
            [0x11] = "(SHT_GROUP)", // Section group
            [0x12] = "(SHT_SYMTAB_SHNDX)", // Extended section indices
            [0x13] = "(SHT_NUM)", // Number of defined types.
        };
        
        char *type_string = (section_header->type < array_count(section_type_strings)) ? section_type_strings[section_header->type] : "(???)";
        
        // For symbol table sections, the sh_info member is the index of the first non-local symbol.
        
        print("[%.2u] %20s (%.8x) %.8x %19s %.8x %.16x %.16x %.16x %.8x %.8x %.16x %.16x\n", section_index, section_name, section_header->name_offset, section_header->type, type_string, section_header->flags, section_header->address, section_header->offset, section_header->size, section_header->linked_section, section_header->info, section_header->alignment, section_header->entry_size);
    }
    
    print("\n");
    print("Mapping:\n");
    
    for(u64 program_header_offset = program_header_table_offset, program_header_index = 0; program_header_offset < program_header_table_end;  program_header_offset += program_header_table_entry_size, program_header_index++){
        struct elf_program_header *program_header = (void *)(file.data + program_header_offset);
        if(program_header->type != /*PT_LOAD*/0x00000001) continue;
        
        u8 flags[3] = {0};
        int flags_at = 0;
        if(program_header->flags){
            if(program_header->flags & 4) flags[flags_at++] = 'r';
            if(program_header->flags & 2) flags[flags_at++] = 'w';
            if(program_header->flags & 1) flags[flags_at++] = 'x';
        }
        
        print("[%2u] %3s %p-%p: ", program_header_index, flags, program_header->virtual_address, program_header->virtual_address + program_header->memory_size);
        
        for(u64 section_header_offset = section_header_table_offset, section_index = 0; section_header_offset < section_header_table_end; section_header_offset += section_header_table_entry_size, section_index++){
            struct elf_section_header *section_header = (void *)(file.data + section_header_offset);
            
            if(section_header->name_offset >= section_name_string_table_section_size){
                print("Invalid section header name offset\n");
                return 1;
            }
            
            u8 *section_name = section_name_string_table + section_header->name_offset;
            
            u64 section_address = section_header->address;
            
            if(program_header->virtual_address <= section_address && section_address < program_header->virtual_address + program_header->memory_size){
                print("%s ", section_name);
            }
        }
        print("\n");
    }
    
    
    print("\n");
    
    for(u64 section_header_offset = section_header_table_offset; section_header_offset < section_header_table_end; section_header_offset += section_header_table_entry_size){
        struct elf_section_header *section_header = (void *)(file.data + section_header_offset);
        
        struct string section_name = string_from_cstring((char *)section_name_string_table + section_header->name_offset);
        if(section_name.size == 0) continue;
        
        u64 section_offset = section_header->offset;
        u64 section_size   = section_header->size;
        
        u8 *section_data = file.data + section_offset;
        
        print("%.*s (%p-%p)\n", section_name.size, section_name.data, section_header->address, section_header->address + section_header->size);
        
        if(string_match(section_name, string(".interp"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".note.gnu.property"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".note.gnu.build-id"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".note.ABI-tag"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".hash"))){
            struct elf_section_header *dynsym_header = (void *)(file.data + section_header_table_offset + section_header->linked_section * section_header_table_entry_size);
            struct elf_section_header *dynstr_header = (void *)(file.data + section_header_table_offset + dynsym_header->linked_section * section_header_table_entry_size);
            
            u8 *dynsym = file.data + dynsym_header->offset;
            u8 *dynstr = file.data + dynstr_header->offset;
            
            u32 bucket_count = *(u32 *)(section_data + 0);
            u32 chain_count  = *(u32 *)(section_data + 4);
            print("bucket_count = 0x%x\n", bucket_count);
            print("chain_count = 0x%x\n", chain_count);
            
            u32 *buckets = (u32 *)(section_data + 8);
            u32 *chain   = buckets + bucket_count;
            
            print("Buckets:\n");
            for(u32 index = 0; index < bucket_count; index++){
                u32 symbol_index = buckets[index];
                struct elf_symbol *symbol = (void *)(dynsym + dynsym_header->entry_size * symbol_index);
                u8 *symbol_name = dynstr + symbol->name_offset;
                print("   [%3u] = %3u %s\n", index, symbol_index, symbol_name);
            }
            
            print("Chain:\n");
            for(u32 index = 0; index < chain_count; index++){
                u32 symbol_index = chain[index];
                struct elf_symbol *symbol = (void *)(dynsym + dynsym_header->entry_size * symbol_index);
                u8 *symbol_name = dynstr + symbol->name_offset;
                print("   [%3u] = %3u %s\n", index, symbol_index, symbol_name);
            }
        }
        
        
        // .gnu.hash
        
        if(string_match(section_name, string(".dynsym")) || string_match(section_name, string(".symtab"))){
            
            if(section_header->entry_size < sizeof(struct elf_symbol)){
                print(".dynsym has invalid entry size.\n");
                return 1;
            }
            
            struct elf_section_header *dynstr_header = (void *)(file.data + section_header_table_offset + section_header->linked_section * section_header_table_entry_size);
            u8 *dynstr = file.data + dynstr_header->offset;
            
            for(u64 symbol_offset = 0, symbol_index = 0; symbol_offset < section_size; symbol_offset += section_header->entry_size, symbol_index++){
                struct elf_symbol *symbol = (void *)(section_data + symbol_offset);
                
                u8 *name = dynstr + symbol->name_offset;
                u8 info = symbol->info;
                u8 bind = info >> 4;
                u8 type = info & 0xf;
                u8 visibility = symbol->other & 3;
                
                u16 section_index = symbol->section_index;
                u64 value = symbol->value;
                u64 size  = symbol->size;
                
                static char *symbol_bind_strings[] = {
                    "STB_LOCAL",
                    "STB_GLOBAL",
                    "STB_WEAK",
                };
                
                char *bind_string = (bind < array_count(symbol_bind_strings)) ? bind_string = symbol_bind_strings[bind] : "???";
                
                static char *symbol_type_strings[] = {
                    "STT_NOTYPE",
                    "STT_OBJECT",
                    "STT_FUNC",
                    "STT_SECTION",
                    "STT_FILE",
                    "STT_COMMON",
                    "STT_TLS",
                };
                char *type_string = (type < array_count(symbol_type_strings)) ? type_string = symbol_type_strings[type] : "???";
                
                static char *symbol_visibility_strings[] = {
                    "STV_DEFAULT",
                    "STV_INTERNAL",
                    "STV_HIDDEN",
                    "STV_PROTECTED",
                };
                char *visibility_string = (visibility < array_count(symbol_visibility_strings)) ? visibility_string = symbol_visibility_strings[visibility] : "???";
                
                print("[%3u] %.4x %.16x %.16x %.2x %11s %10s %.2x %13s %.8x (%s)\n", symbol_index, section_index, value, size, info, type_string, bind_string, symbol->other, visibility_string, symbol->name_offset, name);
            }
        }
        
        if(string_match(section_name, string(".dynstr")) || string_match(section_name, string(".strtab")) || string_match(section_name, string(".shstrtab"))){
            for(u64 offset = 0; offset < section_size; ){
                char *name = (char *)(section_data + offset);
                print("[%x] %s\n", offset, name);
                offset += cstring_length(name) + 1;
            }
        }
        
        if(string_match(section_name, string(".gnu.version"))){
            struct elf_section_header *dynsym_header = (void *)(file.data + section_header_table_offset + section_header->linked_section * section_header_table_entry_size);
            struct elf_section_header *dynstr_header = (void *)(file.data + section_header_table_offset + dynsym_header->linked_section * section_header_table_entry_size);
            
            u8 *dynsym = file.data + dynsym_header->offset;
            u8 *dynstr = file.data + dynstr_header->offset;
            
            u64 entry_count = (section_size/2);
            
            if(entry_count != dynsym_header->size/dynsym_header->entry_size){
                print(".gnu.version has wrong number of entries.\n");
                return 1;
            }
            
            u16 *versions = (u16 *)section_data;
            
            for(u64 index = 0; index < entry_count; index++){
                struct elf_symbol *symbol = (void *)(dynsym + dynsym_header->entry_size * index);
                u8 *name = dynstr + symbol->name_offset;
                
                u16 version = versions[index];
                char *info = "";
                if(version == 0) info = "(local)";
                if(version == 1) info = "(defined)";
                
                print("[%3u] %30s version %u %s\n", index, name, version, info);
            }
        }
        
        if(string_match(section_name, string(".gnu.version_r"))){
            struct elf_section_header *dynstr_header = (void *)(file.data + section_header_table_offset + section_header->linked_section * section_header_table_entry_size);
            u8 *dynstr = file.data + dynstr_header->offset;
            
            for(u64 offset = 0; offset < section_size;){
                struct elf_version_requirement{
                    u16 version;
                    u16 auxiliary_entry_count;
                    u32 file_name_offset;
                    u32 offset_to_auxiliary_entries;
                    u32 offset_to_next;
                } *version_requirement = (void *)(section_data + offset);
                
                if(offset + sizeof(*version_requirement) > section_size){
                    print("Invalid .gnu.version_r\n");
                    return 1;
                }
                
                u16 version = version_requirement->version;
                u16 count = version_requirement->auxiliary_entry_count;
                u32 file_name_offset = version_requirement->file_name_offset;
                u32 offset_to_aux = version_requirement->offset_to_auxiliary_entries;
                u32 offset_to_next = version_requirement->offset_to_next;
                
                print("Version %u\n", version);
                print("Auxiliary Entry Count %u\n", count);
                print("File Name %s (%x)\n", dynstr + file_name_offset, file_name_offset);
                print("Offset to Auxiliary Entries %x\n", offset_to_aux);
                print("Offset to Next %x\n", offset_to_next);
                
                
                for(u64 aux_offset = offset + offset_to_aux, index = 0; index < count; index++){
                    struct elf_version_requirement_aux_entry{
                        u32 hash;
                        u16 flags;
                        u16 other;
                        u32 name_offset;
                        u32 next_offset;
                    } *auxiliary_entry = (void *)(section_data + aux_offset);
                    
                    u32 hash = auxiliary_entry->hash;
                    u16 flags = auxiliary_entry->flags;
                    u16 other = auxiliary_entry->other;
                    u32 name_offset = auxiliary_entry->name_offset;
                    u32 next = auxiliary_entry->next_offset;
                    
                    print("[%u] offset %x\n", index, aux_offset);
                    print("     hash %x\n", hash);
                    print("     flags %x\n", flags);
                    print("     other %x (Version)\n", other);
                    print("     name %s (%x)\n", dynstr + name_offset, name_offset);
                    print("     next_offset %x\n", next);
                    
                    aux_offset = aux_offset + next;
                }
                
                if(offset_to_next == 0) break;
                offset += offset_to_next;
            }
        }
        
        if(string_match(section_name, string(".rela.dyn")) || string_match(section_name, string(".rela.plt"))){
            
            // "x86_64 only Elf64_Rela is used".
            struct elf_relocation_addend{
                // For ET_REL type binaries, this value denotes an offset within a section. 
                // For ET_EXEC type binaries this value denotes a virtual address.
                u64 offset; 
                u64 info;
                u64 addend;
            };
            
            
            if(section_header->entry_size < sizeof(struct elf_relocation_addend)){
                print("invalid entry size for %.*s\n", section_name.size, section_name.data);
                return 1;
            }
            
            for(u64 offset = 0; offset < section_size; offset += section_header->entry_size){
                struct elf_relocation_addend *relocation = (void *)(section_data + offset);
                
                u64 info   = relocation->info;
                
                u32 symbol_index = info >> 32;
                u32 type = (u32)info;
                
                static char *relocation_type_strings[] = {
                    [0]  = "R_X86_64_NONE",      // None None
                    [1]  = "R_X86_64_64",        // qword S + A
                    [2]  = "R_X86_64_PC32",      // dword S + A - P
                    [3]  = "R_X86_64_GOT32",     // dword G + A
                    [4]  = "R_X86_64_PLT32",     // dword L + A - P
                    [5]  = "R_X86_64_COPY",      // None Value is copied directly from shared object
                    [6]  = "R_X86_64_GLOB_DAT",  // qword S
                    [7]  = "R_X86_64_JUMP_SLOT", // qword S
                    [8]  = "R_X86_64_RELATIVE",  // qword B + A
                    [9]  = "R_X86_64_GOTPCREL",  // dword G + GOT + A - P
                    [10] = "R_X86_64_32",        // dword S + A
                    [11] = "R_X86_64_32S",       // dword S + A
                    [12] = "R_X86_64_16",        // word S + A
                    [13] = "R_X86_64_PC16",      // word S + A - P
                    [14] = "R_X86_64_8",         // word8 S + A
                    [15] = "R_X86_64_PC8",       // word8 S + A - P
                    [24] = "R_X86_64_PC64",      // qword S + A - P
                    [25] = "R_X86_64_GOTOFF64",  // qword S + A - GOT
                    [26] = "R_X86_64_GOTPC32",   // dword GOT + A - P
                    [32] = "R_X86_64_SIZE32",    // dword Z + A
                    [33] = "R_X86_64_SIZE64",    // qword Z + A
                };
                char *relocation_type_string = type < array_count(relocation_type_strings) ? relocation_type_strings[type] : "???";
                
                print("%.16llx %16s %.8x %.16llx %.16llx\n", info, relocation_type_string, symbol_index, relocation->offset, relocation->addend);
            }
        }
        
        if(string_match(section_name, string(".init"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".plt"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".plt.got"))){
            print_byte_range(section_data, section_size);
        }
        
        
        // .text
        
        if(string_match(section_name, string(".fini"))){
            print_byte_range(section_data, section_size);
        }
        
        
        // .rodata
        
        static char *format_string[16] = {
            [0xf] = "omit",
            [1] = "uleb128",
            [2] = "udata2",
            [3] = "udata4",
            [4] = "udata8",
            [9] = "uleb128",
            [0xa] = "sdata2",
            [0xb] = "sdata4",
            [0xc] = "sdata8",
        };
        
        static char *application_strings[16] = {
            [0xf] = "omit",
            [0x0] = "absptr",
            [0x1] = "pcrel",
            [0x3] = "datarel",
        };
        
        if(string_match(section_name, string(".eh_frame_hdr"))){
            struct eh_frame_header{
                u8 version;
                u8 eh_frame_pointer_encoding;
                u8 fde_count_encoding;
                u8 table_encoding;
            } *eh_frame_header = (void *)section_data;
            
            print("   version %u\n", eh_frame_header->version);
            print("   frame pointer encoding %.2x %s %s\n", eh_frame_header->eh_frame_pointer_encoding, format_string[eh_frame_header->eh_frame_pointer_encoding & 0xf], application_strings[eh_frame_header->eh_frame_pointer_encoding >> 4]);
            print("   fde count encoding %.2x %s %s\n", eh_frame_header->fde_count_encoding, format_string[eh_frame_header->fde_count_encoding & 0xf], application_strings[eh_frame_header->fde_count_encoding >> 4]);
            print("   table encoding %.2x %s %s\n", eh_frame_header->table_encoding, format_string[eh_frame_header->table_encoding & 0xf], application_strings[eh_frame_header->table_encoding >> 4]);
            
            if(eh_frame_header->eh_frame_pointer_encoding != /*sdata4 pcrel*/0x1b){
                print("unhandled eh frame pointer encoding.\n");
                return 1;
            }
            
            if(eh_frame_header->fde_count_encoding != /*udata4 absptr*/0x03){
                print("Unhandled fde count encoding.\n");
                return 1;
            }
            
            if(eh_frame_header->table_encoding != /*sdata4 datarel*/0x3b){
                print("Unhandled eh frame header table encoding.\n");
                return 1;
            }
            
            u32 *eh_frame_header_section_at = (u32 *)(eh_frame_header + 1);
            
            u32 frame_pointer = *eh_frame_header_section_at++;
            u32 fde_count = *eh_frame_header_section_at++;
            
            print("   frame_pointer %x\n", frame_pointer);
            print("   fde_count %x\n", fde_count);
            
            if(fde_count * 8 != section_data + section_size - (u8 *)eh_frame_header_section_at){
                print("unexpected fde_count %x\n", fde_count);
                return 1;
            }
            
            for(u32 index = 0; index < fde_count; index++){
                u32 initial_location = *eh_frame_header_section_at++;
                u32 address = *eh_frame_header_section_at++;
                
                print("       [%u] %x %x\n", index, initial_location, address);
            }
        }
        
        if(string_match(section_name, string(".eh_frame"))){
            
            for(u64 offset = 0; offset < section_size; ){
                
                u64 length = *(u32 *)(section_data + offset);
                offset += 4;
                
                u64 root_offset = offset;
                
                print("Length %x - ", length);
                
                // "If Length contains 0, then this CIE shall be considered a terminator and processing shall end."
                if(length == 0) break;
                
                // "If the Length field contains the value 0xffffffff, then the length is contained in the Exetended Length field."
                if(length == 0xffffffff){
                    length = *(u64 *)(section_data + offset);
                    offset += 8;
                    
                    print("Extended Length %llx - \n", length);
                }
                
                u32 cie_offset = *(u32 *)(section_data + offset + 0);
                offset += 4;
                
                if(cie_offset == 0){
                    
                    print("CIE:\n");
                    u8 version = *(section_data + offset);
                    
                    print("    CIE ID %x (0 means this is the cie)\n", cie_offset);
                    print("    Version %x\n", version);
                    
                    offset += 1;
                    
                    struct string augmentation_string = string_from_cstring((char *)(section_data + offset));
                    
                    print("    Augmentation String \"%.*s\"\n", augmentation_string.size, augmentation_string.data);
                    
                    offset += augmentation_string.size + 1;
                    
                    if(string_contains(augmentation_string, string("eh"))){
                        u64 eh_data = *(u64 *)(section_data + offset);
                        print("    EH Data %llx\n", eh_data);
                        offset += 8;
                    }
                    
                    u64 code_alignment_factor = read_uleb(section_data, &offset);
                    s64 data_alignment_factor = read_sleb(section_data, &offset);
                    
                    print("    Code Alignment Factor %llu\n", code_alignment_factor);
                    print("    Data Alignment Factor %lld\n", data_alignment_factor);
                    
                    // somehow this is not documented? What is the format of this?
                    u64 return_address_register = read_uleb(section_data, &offset);
                    
                    print("    Return Address Register %llu\n", return_address_register);
                    
                    if(augmentation_string.size && augmentation_string.data[0] == 'z'){
                        
                        u64 augmentation_length = read_uleb(section_data, &offset);
                        print("    Augmentation Length %llx\n", augmentation_length);
                        
                        for(smm index = 1; index < augmentation_string.size; index++){
                            
                            if(augmentation_string.data[index] == 'R'){
                                u8 address_pointer_encoding = section_data[offset++];
                                print("    Address Pointer Encoding %x %s %s\n", address_pointer_encoding, format_string[address_pointer_encoding & 0xf], application_strings[address_pointer_encoding >> 4]);
                                
                                if(address_pointer_encoding != 0x1b){
                                    print("unhandled Address pointer encoding. Expected sdata4, pcrel\n");
                                    return 1;
                                }
                            }else{
                                print("Unhandled augmentation string thing %c\n", augmentation_string.data[index]);
                                return 1;
                            }
                        }
                    }else{
                        print("Expected Augmentation string to begin with 'z'\n");
                        return 1;
                    }
                    
                    print("Dwarf data:\n");
                    print_byte_range(section_data + offset, length - (offset - root_offset));
                }else{
                    print("FDE:\n");
                    
                    print("    CIE Pointer: %llx\n", cie_offset);
                    s32 pc_begin = *(s32 *)(section_data + offset + 0); // these are s32 rel because of the address pointer encoding.
                    s32 pc_range = *(s32 *)(section_data + offset + 4);
                    offset += 8;
                    print("    pc_begin %x, pc_range %x\n", pc_begin, pc_range);
                    
                    u64 augmentation_length = read_uleb(section_data, &offset);
                    print("    Augmentation Length %llx\n", augmentation_length);
                    if(augmentation_length){
                        print_byte_range(section_data + offset, augmentation_length);
                        offset += augmentation_length;
                    }
                    
                    print("Dwarf data:\n");
                    print_byte_range(section_data + offset, length - (offset - root_offset));
                }
                
                offset = root_offset + length;
            }
        }
        
        if(string_match(section_name, string(".init_array"))){
            print_byte_range(section_data, section_size);
        }
        
        if(string_match(section_name, string(".fini_array"))){
            print_byte_range(section_data, section_size);
        }
        
        // .data.rel.ro
        
        if(string_match(section_name, string(".dynamic"))){
            struct elf_dynamic_section_entry{
                u64 tag;
                u64 value;
            };
            
            if(section_header->entry_size < sizeof(struct elf_dynamic_section_entry)){
                print("invalid entry size for .dynamic\n");
                return 1;
            }
            
            for(u64 offset = 0; offset < section_size; offset += section_header->entry_size){
                
                struct elf_dynamic_section_entry *entry = (void *)(section_data + offset);
                
                static struct{
                    u64 tag;
                    char *string;
                } dynamic_strings[] = {
                    {0,  "DT_NULL"}, /* Marks end of dynamic section */
                    {1,  "DT_NEEDED"}, /* Name of needed library */
                    {2,  "DT_PLTRELSZ"}, /* Size in bytes of PLT relocs */
                    {3,  "DT_PLTGOT"}, /* Processor defined value */
                    {4,  "DT_HASH"}, /* Address of symbol hash table */
                    {5,  "DT_STRTAB"}, /* Address of string table */
                    {6,  "DT_SYMTAB"}, /* Address of symbol table */
                    {7,  "DT_RELA"}, /* Address of Rela relocs */
                    {8,  "DT_RELASZ"}, /* Total size of Rela relocs */
                    {9,  "DT_RELAENT"}, /* Size of one Rela reloc */
                    {10, "DT_STRSZ"}, /* Size of string table */
                    {11, "DT_SYMENT"}, /* Size of one symbol table entry */
                    {12, "DT_INIT"}, /* Address of init function */
                    {13, "DT_FINI"}, /* Address of termination function */
                    {14, "DT_SONAME"}, /* Name of shared object */
                    {15, "DT_RPATH"}, /* Library search path (deprecated) */
                    {16, "DT_SYMBOLIC"}, /* Start symbol search here */
                    {17, "DT_REL"}, /* Address of Rel relocs */
                    {18, "DT_RELSZ"}, /* Total size of Rel relocs */
                    {19, "DT_RELENT"}, /* Size of one Rel reloc */
                    {20, "DT_PLTREL"}, /* Type of reloc in PLT */
                    {21, "DT_DEBUG"}, /* For debugging; unspecified */
                    {22, "DT_TEXTREL"}, /* Reloc might modify .text */
                    {23, "DT_JMPREL"}, /* Address of PLT relocs */
                    {24, "DT_BIND_NOW"}, /* Process relocations of object */
                    {25, "DT_INIT_ARRAY"}, /* Array with addresses of init fct */
                    {26, "DT_FINI_ARRAY"}, /* Array with addresses of fini fct */
                    {27, "DT_INIT_ARRAYSZ"}, /* Size in bytes of DT_INIT_ARRAY */
                    {28, "DT_FINI_ARRAYSZ"}, /* Size in bytes of DT_FINI_ARRAY */
                    {29, "DT_RUNPATH"}, /* Library search path */
                    {30, "DT_FLAGS"}, /* Flags for the object being loaded */
                    {31, "DT_ENCODING"}, /* Start of encoded range */
                    {32, "DT_PREINIT_ARRAY"}, /* Array with addresses of preinit fct*/
                    {33, "DT_PREINIT_ARRAYSZ"}, /* size in bytes of DT_PREINIT_ARRAY */
                    {34, "DT_SYMTAB_SHNDX"}, /* Address of SYMTAB_SHNDX section */
                    
                    {0x6ffffef5, "DT_GNU_HASH"},
                    
                    {0x6ffffff0, "DT_VERSYM"},
                    {0x6ffffff9, "DT_RELACOUNT"},
                    {0x6ffffffa, "DT_RELCOUNT"},
                    {0x6ffffffb, "DT_FLAGS_1"},
                    {0x6ffffffc, "DT_VERDEF"},
                    {0x6ffffffd, "DT_VERDEFNUM"},
                    {0x6ffffffe, "DT_VERNEED"},
                    {0x6fffffff, "DT_VERNEEDNUM"},
                };
                
                char *dynamic_string = "(???)";
                for(u32 index = 0; index < array_count(dynamic_strings); index++){
                    if(dynamic_strings[index].tag == entry->tag){
                        dynamic_string = dynamic_strings[index].string;
                        break;
                    }
                }
                
                print("%.16x %18s %.16x", entry->tag, dynamic_string, entry->value);
                
                if(entry->tag == /*DT_FLAGS*/0x1e){
                    if(entry->value & 1) print(" DF_ORIGIN");
                    if(entry->value & 2) print(" DF_SYMBOLIC");
                    if(entry->value & 4) print(" DF_TEXTREL");
                    if(entry->value & 8) print(" DF_BIND_NOW");
                    if(entry->value & 0x10) print(" DF_STATIC_TLS");
                }
                
                print("\n");
            }
        }
        
        if(string_match(section_name, string(".got"))){
            u64 *entries = (u64 *)section_data;
            for(u64 index = 0; index < section_size/8; index++){
                print("[%u] %p\n", index, entries[index]);
            }
        }
        
        // .got.plt
        if(string_match(section_name, string(".got.plt"))){
            u64 *entries = (u64 *)section_data;
            for(u64 index = 0; index < section_size/8; index++){
                print("[%u] %p\n", index, entries[index]);
            }
        }
        
        
        // .data
        
        // .bss
        
        if(string_match(section_name, string(".comment"))){
            print_byte_range(section_data, section_size);
        }
        
        
        // .debug_info
        
        // .debug_abbrev
        
        // .debug_line
        
        // .debug_str
        
        // .debug_addr
        
        // .debug_line_str
        
        // .debug_str_offsets
        
        print("\n");
    }
    
    return 0;
}
