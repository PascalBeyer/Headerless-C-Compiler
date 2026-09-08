
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

void push_uleb(struct memory_arena *arena, u64 value){
    do{
        u8 byte = value & 0x7f;
        value >>= 7;
        
        if(value) byte |= 0x80;
        
        *push_uninitialized_struct(arena, u8) = byte;
    }while(value != 0);
}


void push_sleb(struct memory_arena *arena, s64 value){
    
    int done;
    do{
        u8 byte = value & 0x7f;
        value >>= 7; // Arithmetic shift
        
        done = (value == 0 && !(byte & 0x40)) || (value == -1 && (byte & 0x40));
        
        if(!done) byte |= 0x80;
        
        *push_uninitialized_struct(arena, u8) = byte;
    }while(!done);
}

enum abbrev_indices{
    ABBREV_compilation_unit = 1,
    
    ABBREV_function,
    ABBREV_void_function,    
    ABBREV_variable,
    ABBREV_lexical_block,
    
    ABBREV_base_type,
    ABBREV_pointer_type,
    ABBREV_void_pointer_type,
    ABBREV_array_type,
    ABBREV_subrange_type, // for arrays, kinda weird.
    ABBREV_subrange_unknown_size_type, // for arrays, kinda weird.
    
    ABBREV_structure_type,
    ABBREV_union_type,
    ABBREV_member,
    ABBREV_bitfield_member,
    
    ABBREV_enumeration_type,
    ABBREV_enumerator,
    
    ABBREV_atomic_type,
    ABBREV_function_type,
    ABBREV_void_function_type,
    
    ABBREV_unresolved_type,
};


void dwarf_register_type(struct memory_arena *arena, struct ast_type *root_type, struct memory_arena *scratch, struct memory_arena *location_arena, u8 *debug_info_base){
    
    if(root_type->dwarf_form_offset != 0) return;
    
    // 
    // Register all types:
    // 
    //     In this system, the types do not have to be in linear order,
    //     this means we can simply register all the types and emit patches.
    //     when we are done, we simply fix up all of the patches.
    //     
    //     Therefore, we can simply add all sub-types to a queue and mark them
    //     as temporary. We mark them as permanent when we have emitted them.
    //     If the sub-type is not yet permanent, we emit a patch, if it is permanent
    //     we can simpy put in the reference, if it is neither, we add a patch and
    //     add it to the queue and mark it as temporary.
    // 
    
    struct{
        struct type_queue_node{
            struct type_queue_node *next;
            struct ast_type *type;
        } *first, *last;
    } subtypes = {0};
    
    struct{
        struct type_patch_node{
            struct type_patch_node *next;
            struct ast_type *type;
            u32 *patch_location;
        } *first, *last;
    } patches = {0};
    
    struct type_queue_node *initial_subtype = push_struct(scratch, struct type_queue_node);
    initial_subtype->type = root_type;
    sll_push_back(subtypes, initial_subtype);
    
    while(subtypes.first){
        struct ast_type *subtype = subtypes.first->type;
        sll_pop_front(subtypes);
        
        subtype->flags |= TYPE_FLAG_pdb_permanent;
        subtype->dwarf_form_offset = (u32)(arena_current(arena) - debug_info_base);
        
#define recurse(recurse_type)                                                                \
if((recurse_type)->flags & TYPE_FLAG_pdb_permanent){                                         \
    *form_ref = (recurse_type)->dwarf_form_offset;                                           \
}else{                                                                                       \
    if(!((recurse_type)->flags & TYPE_FLAG_pdb_temporary)){                                  \
        (recurse_type)->flags |= TYPE_FLAG_pdb_temporary;                                    \
        struct type_queue_node *subtype_node = push_struct(scratch, struct type_queue_node); \
        subtype_node->type = (recurse_type);                                                 \
        sll_push_back(subtypes, subtype_node);                                               \
    }                                                                                        \
    struct type_patch_node *patch_node = push_struct(scratch, struct type_patch_node);       \
    patch_node->type = (recurse_type);                                                       \
    patch_node->patch_location = form_ref;                                                   \
    sll_push_back(patches, patch_node);                                                      \
}                                                                                            \

        switch(subtype->kind){
            
            case AST_struct:
            case AST_union:{
                struct ast_compound_type *compound = (struct ast_compound_type *)subtype;
                
                *push_struct(arena, u8) = /*index*/(subtype->kind == AST_union) ? ABBREV_union_type : ABBREV_structure_type;
                push_zero_terminated_string_copy(arena, token_get_string(compound->identifier));
                *push_struct_unaligned(arena, u64) = compound->base.size;
                
                {
                    struct token_location_information location_information = get_location_for_token(location_arena, compound->compilation_unit, compound->identifier);
                    struct file *file = globals.file_table.data[location_information.file_index];
                    *push_struct_unaligned(arena, u32) = file->file_number;
                    *push_struct_unaligned(arena, u32) = location_information.line;
                    *push_struct_unaligned(arena, u32) = location_information.column;
                }
                
                for(u32 member_index = 0; member_index < compound->amount_of_members; member_index++){
                    struct compound_member *member = &compound->members[member_index];
                    if(member->name == globals.invalid_identifier_token) continue;
                    
                    struct ast_type *member_type = member->type;
                    if(member_type->kind == AST_bitfield_type){
                        struct ast_bitfield_type *bitfield_type = (struct ast_bitfield_type *)member_type;
                        struct ast_type *base_type = bitfield_type->base_type;
                        
                        *push_struct(arena, u8) = /*index*/ABBREV_bitfield_member;
                        push_zero_terminated_string_copy(arena, token_get_string(member->name));
                        
                        struct token_location_information location_information = get_location_for_token(location_arena, compound->compilation_unit, member->name);
                        struct file *file = globals.file_table.data[location_information.file_index];
                        *push_struct_unaligned(arena, u32) = file->file_number;
                        *push_struct_unaligned(arena, u32) = location_information.line;
                        *push_struct_unaligned(arena, u32) = location_information.column;
                        
                        u32 *form_ref = push_struct_unaligned(arena, u32);
                        recurse(base_type);
                        
                        *push_struct(arena, u8) = (u8)bitfield_type->bit_index;
                        *push_struct(arena, u8) = (u8)bitfield_type->width;
                        
                        *push_struct_unaligned(arena, u64) = member->offset_in_type;
                    }else{
                        *push_struct(arena, u8) = /*index*/ABBREV_member;
                        push_zero_terminated_string_copy(arena, token_get_string(member->name));
                        
                        struct token_location_information location_information = get_location_for_token(location_arena, compound->compilation_unit, member->name);
                        struct file *file = globals.file_table.data[location_information.file_index];
                        *push_struct_unaligned(arena, u32) = file->file_number;
                        *push_struct_unaligned(arena, u32) = location_information.line;
                        *push_struct_unaligned(arena, u32) = location_information.column;
                        
                        u32 *form_ref = push_struct_unaligned(arena, u32);
                        recurse(member_type);
                        
                        *push_struct_unaligned(arena, u64) = member->offset_in_type;
                    }
                }
                
                *push_struct(arena, u8) = /*end*/0;
            }break;
            
            case AST_enum:{
                struct ast_compound_type *compound = (struct ast_compound_type *)subtype;
                
                struct token_location_information location_information = get_location_for_token(location_arena, compound->compilation_unit, compound->identifier);
                struct file *file = globals.file_table.data[location_information.file_index];
                
                *push_struct(arena, u8) = ABBREV_enumeration_type;
                push_zero_terminated_string_copy(arena, token_get_string(compound->identifier));
                *push_struct(arena, u8) = /*encoding(signed)*/5;
                *push_struct(arena, u8) = /*byte_size*/(u8)compound->base.size;
                *push_struct_unaligned(arena, u32) = /*type*/globals.typedef_s32.dwarf_form_offset;
                
                *push_struct_unaligned(arena, u32) = file->file_number;
                *push_struct_unaligned(arena, u32) = location_information.line;
                *push_struct_unaligned(arena, u32) = location_information.column;
                
                for(u32 member_index = 0; member_index < compound->amount_of_members; member_index++){
                    struct compound_member *member = &compound->members[member_index];
                    
                    *push_struct(arena, u8) = /*index*/ABBREV_enumerator;
                    push_zero_terminated_string_copy(arena, token_get_string(member->name));
                    *push_struct_unaligned(arena, s32) = (s32)member->enum_value;
                }
                
                *push_struct(arena, u8) = /*end*/0;
            }break;
            
            case AST_pointer_type:{
                struct ast_pointer_type *pointer_type = (struct ast_pointer_type *)subtype;
                
                // 
                // In the case of:
                // 
                //     struct unresolved *pointer;
                //     
                // Where we did not dereference 'pointer', but 'unresolved' gets defined _later_,
                // this pointer is still pointing to an unresolved type.
                // If this is the case we try to patch it here.
                // 
                maybe_resolve_unresolved_type(&pointer_type->pointer_to);
                
                *push_struct(arena, u8) = /*index*/ABBREV_pointer_type;
                *push_struct(arena, u8) = /*byte_size*/8;
                u32 *form_ref = push_struct_unaligned(arena, u32);
                
                struct ast_type *pointee_type = pointer_type->pointer_to;
                recurse(pointee_type);
            }break;
            
            case AST_array_type:{
                struct ast_array_type *array_type = (struct ast_array_type *)subtype;
                
                *push_struct(arena, u8) = /*index*/ABBREV_array_type;
                *push_struct_unaligned(arena, u64) = array_type->base.size;
                
                u32 *form_ref = push_struct_unaligned(arena, u32);
                struct ast_type *element_type = array_type->element_type;
                recurse(element_type);
                
                {
                    int is_of_unknown_size = array_type->is_of_unknown_size;
                    
                    *push_struct(arena, u8) = /*index*/is_of_unknown_size ? ABBREV_subrange_unknown_size_type : ABBREV_subrange_type;
                    *push_struct_unaligned(arena, u32) = /*type*/globals.typedef_u64.dwarf_form_offset;
                    if(!is_of_unknown_size){
                        *push_struct_unaligned(arena, u64) = /*upper_bound*/array_type->amount_of_elements-1;
                    }
                }
                
                *push_struct(arena, u8) = /*end*/0;
            }break;
            
            case AST_function_type:{
                struct ast_function_type *function_type = (struct ast_function_type *)subtype;
                maybe_resolve_unresolved_type(&function_type->return_type);
                
                struct ast_type *return_type = function_type->return_type;
                int return_type_is_void = (return_type == &globals.typedef_void);
                
                if(return_type_is_void){
                    *push_struct(arena, u8) = /*index*/ABBREV_void_function_type;
                }else{
                    *push_struct(arena, u8) = /*index*/ABBREV_function_type;
                    u32 *form_ref = push_struct_unaligned(arena, u32);
                    recurse(return_type);
                }
                
                // @cleanup: argument types?
                
                *push_struct(arena, u8) = /*end*/0;
            }break;
            
            case AST_unresolved_type:{
                struct ast_unresolved_type *unresolved = (struct ast_unresolved_type *)subtype;
                
                *push_struct(arena, u8) = /*index*/ABBREV_unresolved_type;
                push_zero_terminated_string_copy(arena, token_get_string(unresolved->sleeping_on));
            }break;
            
            default:{
                print("Unhandled type kind %u in dwarf_register_type.\n", subtype->kind);
                invalid_code_path;
            }break;
        }
    }
    
    for(struct type_patch_node *patch = patches.first; patch; patch = patch->next){
        struct ast_type *type = patch->type;
        u32 *form_ref = patch->patch_location;
        *form_ref = type->dwarf_form_offset;
    }
}

void dwarf_emit_debug_information_for_function__recursive(struct ast_function *function, struct memory_arena *arena, struct ast_scope *scope, struct memory_arena *scratch, struct memory_arena *location_arena, u64 virtual_image_base, u8 *debug_info_base){
    
    if(scope->amount_of_declarations){
        if(function->scope != scope){
            // 
            // Open the scope.
            // 
            *push_struct(arena, u8) = /*index*/ABBREV_lexical_block;
            *push_struct_unaligned(arena, u64) = virtual_image_base + function->relative_virtual_address + scope->start_offset;
            *push_struct_unaligned(arena, u64) = scope->end_offset - scope->start_offset;
        }
        
        // 
        // Define all the member's of the scope.
        // 
        for(smm declaration_index = 0; declaration_index < scope->current_max_amount_of_declarations; declaration_index++){
            struct ast_declaration *decl = scope->declarations[declaration_index];
            if(!decl) continue;
            
            // @cleanup:
            if(decl->kind == IR_typedef) continue;
            if(decl->kind == IR_function) continue;
            if(decl->flags & DECLARATION_FLAGS_is_local_persist) continue;
            if(decl->flags & DECLARATION_FLAGS_is_enum_member) continue;
            
            struct string identifier = token_get_string(decl->identifier);
            struct token_location_information location = get_location_for_token(location_arena, function->compilation_unit, decl->identifier);
            struct file *file = globals.file_table.data[location.file_index];
            
            if(decl->type->dwarf_form_offset == 0){
                dwarf_register_type(arena, decl->type, scratch, location_arena, debug_info_base); // I think we can simply register the type here.
            }
            
            *push_struct(arena, u8) = /*index*/ABBREV_variable;
            push_zero_terminated_string_copy(arena, identifier);
            
            *push_struct_unaligned(arena, u32) = (u32)file->file_number;
            *push_struct_unaligned(arena, u32) = (u32)location.line;
            *push_struct_unaligned(arena, u32) = (u32)location.column;
            *push_struct_unaligned(arena, u32) = decl->type->dwarf_form_offset;
            u8 *expression_length = push_struct(arena, u8);
            *push_struct(arena, u8) = /*DW_OP_fbreg*/0x91;
            push_sleb(arena, -decl->offset_on_stack - 16); // @note: The -16 come from how we set up the CFA in the .eh_frame section. This is sort of stupid.
            *expression_length = (u8)(arena_current(arena) - (expression_length + 1));
        }
    }
    
    // 
    // Recurse into all subscopes.
    // 
    
    for(struct ast_scope *subscope = scope->subscopes.first; subscope; subscope = subscope->subscopes.next){
        dwarf_emit_debug_information_for_function__recursive(function, arena, subscope, scratch, location_arena, virtual_image_base, debug_info_base);
    }
    
    if(scope->amount_of_declarations){
        if(function->scope != scope){
            // 
            // End the scope.
            // 
            *push_struct(arena, u8) = /*end*/0;
        }
    }
}

void write_elf(struct string output_file_path, struct memory_arena *arena, struct memory_arena *location_arena, struct memory_arena *scratch){
    
    // 
    // Gather the symbols.
    // 
    
    struct ast_list typedefs = zero_struct;
    
    struct ast_list exports = zero_struct;
    struct ast_list imports = zero_struct;
    struct ast_list data_imports = zero_struct;
    
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
                    
                    if(decl->flags & DECLARATION_FLAGS_is_dllimport){
                        assert(decl->import_node);
                        ast_list_append(&data_imports, scratch, &decl->kind);
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
    
#define fill_section_header(section_name, section_string, section_type, section_flags, section_alignment, section_link, section_info, section_entry_size){  \
    struct elf_section_header *section_header = section_headers + section_header_at++;                                                                      \
    u64 name_offset = section_name_string_table.total_size;                                                                                                 \
    string_list_postfix_no_copy(&section_name_string_table, scratch, string("." section_string "\0"));                                                      \
    section_header->name_offset = (u32)name_offset;                                                                                                         \
    section_header->type = (section_type);                                                                                                                  \
    section_header->flags = (section_flags);                                                                                                                \
    section_header->address = virtual_image_base + current_relative_virtual_address + (section_name##_section_start - current_segment_start);               \
    section_header->offset = section_name##_section_start - elf_base;                                                                                       \
    section_header->size = arena_current(arena) - section_name##_section_start;                                                                             \
    section_header->linked_section = (section_link);                                                                                                        \
    section_header->info = (section_info);                                                                                                                  \
    section_header->alignment = (section_alignment);                                                                                                        \
    section_header->entry_size = (section_entry_size);                                                                                                      \
}
    
#define make_relative_virtual_address(segment_start, address) (u32)(current_relative_virtual_address + ((u8 *)(address) - (segment_start)))
    
    // 
    // Start to actually emit the sections.
    // 
    
    u8 *rx_segment_start = arena_current(arena);
    u8 *plt_section_start = 0;
    u32 plt_section_rva = 0;
    
    u64 text_section_index = 0;
    u64 data_section_index = 0;
    u64 bss_section_index = 0;
    
    {
        u8 *text_section_start = rx_segment_start;
        
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            smm function_size = function->byte_size;
            
            u8 *memory_for_function = push_uninitialized_data(arena, u8, function_size);
            memcpy(memory_for_function, function->memory_location, function_size);
            push_align_initialized_to_specific_value(arena, 16, 0xcc);
            
            function->offset_in_text_section   = memory_for_function - text_section_start;
            function->memory_location          = memory_for_function;
            function->relative_virtual_address = make_relative_virtual_address(rx_segment_start, memory_for_function);
        }
        
        if(arena_current(arena) != text_section_start){
            text_section_index = section_header_at;
            fill_section_header(text, "text", SHT_PROGBITS, SHF_ALLOC|SHF_EXECINSTR, /*alignment*/4, /*link*/0, /*info*/0, /*entry_size*/0);
        }
        
        if(imports.count){
            
            push_align_initialized_to_specific_value(arena, 16, 0xcc);
            
            plt_section_start = arena_current(arena);
            plt_section_rva   = make_relative_virtual_address(rx_segment_start, plt_section_start);
            
            u8 *plt = push_data(arena, u8, 0x10 + 0x10 * imports.count);
            
            // We add the got relative virtual address when we allocate the .got.plt
            
            // push qword ptr [.got.plt + 0x08]
            // jmp  qword ptr [.got.ptl + 0x10]
            
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
            
            fill_section_header(plt, "plt", SHT_PROGBITS, SHF_ALLOC|SHF_EXECINSTR, /*alignment*/0x10, /*link*/0, /*info*/0, /*entry_size*/0x10);
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
        fill_section_header(rodata, "rodata", SHT_PROGBITS, SHF_ALLOC, /*alignment*/4, /*link*/0, /*info*/0, /*entry_size*/0);
    }
    
    if(imports.count || data_imports.count){
        u8 *interp_section_start = arena_current(arena);
        u8 *interp_segment_start = interp_section_start;
        push_zero_terminated_string_copy(arena, string("/lib64/ld-linux-x86-64.so.2"));
        fill_section_header(interp, "interp", SHT_PROGBITS, SHF_ALLOC, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
        fill_program_header(interp, PT_INTERP, PF_READ, /*alignment*/1);
    }
    
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
    
    u64 rela_dyn_section_index = 0;
    u32 rela_dyn_section_rva  = 0;
    u64 rela_dyn_section_size = 0;
    
    struct elf_relocation_addend *rela_dyn_relocations = 0;
    
    if(imports.count || data_imports.count){
        dynstr_section_index = section_header_at;
        dynstr_section_start = arena_current(arena);
        dynstr_section_rva = make_relative_virtual_address(ro_segment_start, dynstr_section_start);
        
        push_zero_terminated_string_copy(arena, string("")); // Start with a zero-sized string.
        
        for_ast_list(imports){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            push_zero_terminated_string_copy(arena, atom_get_string(decl->identifier->atom));
        }
        
        for_ast_list(data_imports){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            push_zero_terminated_string_copy(arena, atom_get_string(decl->identifier->atom));
        }
        
        for(struct import_library_node *import_library_node = globals.import_libraries.first; import_library_node; import_library_node = import_library_node->next){
            import_library_node->name = push_zero_terminated_string_copy(arena, strip_file_path(import_library_node->name));
        }
        
        dynstr_section_size = arena_current(arena) - dynstr_section_start;
        
        fill_section_header(dynstr, "dynstr", SHT_STRTAB, SHF_ALLOC, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
        
        push_align(arena, 8);
        u64 dynsym_section_index = section_header_at;
        u8 *dynsym_section_start = arena_current(arena);
        dynsym_section_rva = make_relative_virtual_address(ro_segment_start, dynsym_section_start);
        
        {
            push_struct(arena, struct elf_symbol); // The first one is all zeroes for some reason.
            
            struct elf_symbol *symbols = push_uninitialized_data(arena, struct elf_symbol, imports.count + data_imports.count);
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
            for_ast_list(data_imports){
                struct ast_declaration *decl = (struct ast_declaration *)it->value;
                struct elf_symbol *symbol = symbols + symbol_index++;
                symbol->name_offset = symbol_name_offset; symbol_name_offset += decl->identifier->size + 1;
                symbol->section_index = 0;
                symbol->value = 0;
                symbol->size = 0;
                symbol->info = /*STB_GLOBAL*/(1 << 4)| /*STT_OBJECT*/1;
                symbol->other = /*STV_DEFAULT*/0;
            }
        }
        
        fill_section_header(dynsym, "dynsym", SHT_DYNSYM, SHF_ALLOC, /*alignment*/8, /*link*/(u32)dynstr_section_index, /*info(local_symbol_count)*/1, /*entry_size*/sizeof(struct elf_symbol));
        
        push_align(arena, 8);
        u8 *hash_section_start = arena_current(arena);
        hash_section_rva = make_relative_virtual_address(ro_segment_start, hash_section_start);
        
        {
            u32 *hash_section_header = push_data(arena, u32, 2);
            
            u32 hash_bucket_count = (u32)(2 * (imports.count + data_imports.count));
            u32 hash_chain_count  = (u32)(imports.count + data_imports.count + 1);
            u32 *hash_buckets = push_data(arena, u32, hash_bucket_count);
            u32 *hash_chains  = push_data(arena, u32, hash_chain_count);
            
            hash_section_header[0] = hash_bucket_count;
            hash_section_header[1] = hash_chain_count;
            
            u32 symbol_index = 1;
            
            for(u32 ast_list_index = 0; ast_list_index < 2; ast_list_index++){
                struct ast_list ast_list = ast_list_index ? data_imports : imports;
                for_ast_list(ast_list){
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
        }
        
        fill_section_header(hash, "hash", SHT_HASH, SHF_ALLOC, /*alignment*/8, /*link*/(u32)dynsym_section_index, /*info*/0, /*entry_size*/4);
        
        push_align(arena, 8);
        u8 *rela_plt_section_start = arena_current(arena);
        rela_plt_section_rva = make_relative_virtual_address(ro_segment_start, rela_plt_section_start);
        
        rela_plt_relocations = push_uninitialized_data(arena, struct elf_relocation_addend, imports.count);
        rela_plt_section_size = arena_current(arena) - rela_plt_section_start;
        
        rela_plt_section_index = section_header_at;
        fill_section_header(rela_plt, "rela.plt", SHT_RELA, SHF_ALLOC | SHF_INFO_LINK, /*alignment*/8, /*link*/(u32)dynsym_section_index, /*info(to be filled in)*/0, /*entry_size*/sizeof(struct elf_relocation_addend));
        
        push_align(arena, 8);
        u8 *rela_dyn_section_start = arena_current(arena);
        rela_dyn_section_rva = make_relative_virtual_address(ro_segment_start, rela_dyn_section_start);
        
        rela_dyn_relocations = push_uninitialized_data(arena, struct elf_relocation_addend, data_imports.count);
        rela_dyn_section_size = arena_current(arena) - rela_dyn_section_start;
        
        rela_dyn_section_index = section_header_at;
        fill_section_header(rela_dyn, "rela_dyn", SHT_RELA, SHF_ALLOC | SHF_INFO_LINK, /*alignment*/8, /*link*/(u32)dynsym_section_index, /*info(to be filled in)*/0, /*entry_size*/sizeof(struct elf_relocation_addend));
        
    }
    
    if(defined_functions.count){
        u8 *eh_frame_section_start = arena_current(arena);
        
        // 
        // The eh_frame_section is very simplified for our rbp based stack frames.
        // We have one CIE (I don't know if there is ever more than one) and then
        // one FDE for each function. 
        // 
        // The CIE gives some common parameters, and then contains an initial set of 
        // dwarf cfa instructions that are intended to define the initial dfa at the start of the function.
        // They seem to be always the same. Here they are:
        // 
        //    DW_CFA_def_cfa: r7 (rsp) ofs 8       (define the dfa to be at rsp + 8)
        //    DW_CFA_offset: r16 (rip) at cfa-8    (define rip to be at cfa-8)
        // 
        // 
        // All of the FDEs have the same form:
        //     
        //  frame instructions:
        //     
        //     DW_CFA_advance_loc: 1               (move the location to past the initial push rbp)
        //     DW_CFA_def_cfa_offset: 16           (define the dfa to be rsp + /*rbp rip*/0x10)
        //     DW_CFA_offset: r6 (rbp) at cfa-16   (define rbp to be on the stack at cfa - 0x10)
        //     
        //     DW_CFA_advance_loc: 3               (move the location to past the mov rbp, rsp)
        //     DW_CFA_def_cfa_register: r6 (rbp)   (define the dfa to be contained in rbp)
        //     
        //     DW_CFA_advance_loc<n>: <end-1>      (move the location all the way to the end of the function, just past the pop rbp, but before the ret)
        //     DW_CFA_def_cfa: r7 (rsp) ofs 8      (define the dfa to point to rsp + 8)
        //     
        // For reference, the corresponding assembly:
        // 
        //   function:
        //           0: push rbp
        //           1: mov rbp, rsp
        //              <...>
        //     <end-2>: pop rbp
        //     <end-1>: ret
        // 
        
        struct cie{
            u32 length;
            u32 cie_offset;
            
            u8 version;
            u8 augmentation_string[3];
            
            u8 code_alignment_factor;
            u8 data_alignment_factor;
            u8 return_address_register;
            u8 augmentation_length;
            
            u8 address_pointer_encoding;
            u8 initial_instructions[7];
        } *cie = push_struct(arena, struct cie);
        cie->length = sizeof(struct cie) - sizeof(cie->length);
        cie->cie_offset = 0; // This is the cie!
        cie->version = 1;
        cie->augmentation_string[0] = 'z';
        cie->augmentation_string[1] = 'R';
        cie->augmentation_string[2] = 0;
        cie->code_alignment_factor   = 1;
        cie->data_alignment_factor   = 0x78; // -8
        cie->return_address_register = 0x10; // rip
        cie->augmentation_length = 1;
        cie->address_pointer_encoding = /*sdata4 pcrel*/0x1b;
        
        memcpy(cie->initial_instructions, (u8[]){
                    // DW_CFA_def_cfa reg=7 (rsp) offset=8
                    0x0c, 0x07, 0x08,
                    
                    // DW_CFA_offset reg=16 (rip) offset=1
                    0x90, 0x01,
                    
                    // DW_CFA_nop, DW_CFA_nop
                    0x00, 0x00,
                }, 7);
        
        for_ast_list(defined_functions){
            
            struct ast_function *function = (struct ast_function *)it->value;
            
            u32 *length = push_struct(arena, u32);
            u8 *start = arena_current(arena);
            
            u32 *cie_offset = push_struct(arena, u32);
            *cie_offset = (u32)(start - (u8 *)cie);
            
            s32 *pc_begin = push_struct(arena, s32);
            s32 *pc_range = push_struct(arena, s32);
            
            s32 relative_virtual_address  = make_relative_virtual_address(ro_segment_start, pc_begin);
            smm function_relative_virtual_address = function->relative_virtual_address;
            
            *pc_begin = (s32)(function_relative_virtual_address - relative_virtual_address);
            
            u32 function_size = (u32)function->byte_size;
            *pc_range = function_size;
            
            *push_struct(arena, u8) = 0; // Augmentation Length;
            
            
            static u8 common_instructions[] = {
                0x41, // DW_CFA_advance_loc 1
                0x0e, 0x10, // DW_CFA_def_cfa_offset 10
                0x86, 0x02, // DW_CFA_offset reg=6 (rbp) offset=2
                
                0x43, // DW_CFA_advance_loc 3
                0x0d, 0x06, // DW_CFA_def_cfa_register reg=6 (rbp)
            };
            push_array_copy(arena, u8, common_instructions, array_count(common_instructions));
            
            if(!(function->type->flags & FUNCTION_TYPE_FLAGS_is_noreturn)){
                u32 advance = function_size - /*push rbp, mov rbp, rsp*/4  - /*ret*/1;
                if(advance <= 0x3f){
                    *push_struct(arena, u8) = 0x40 | (u8)advance;
                }else if(advance <= 0xff){
                    *push_struct(arena, u8) = /*DW_CFA_advance_loc1*/0x02;
                    *push_struct(arena, u8) = (u8)advance;
                }else if(advance <= 0xffff){
                    *push_struct(arena, u8) = /*DW_CFA_advance_loc2*/0x03;
                    *push_struct_unaligned(arena, u16) = (u16)advance;
                }else if(advance <= 0xffffffff){
                    *push_struct(arena, u8) = /*DW_CFA_advance_loc4*/0x04;
                    *push_struct_unaligned(arena, u32) = advance;
                }
                
                // DW_CFA_def_cfa reg=7 (rsp) offset=8
                static u8 define_dfa_rsp_8[] = { 0x0c, 0x07, 0x08 };
                push_array_copy(arena, u8, define_dfa_rsp_8, array_count(define_dfa_rsp_8));
            }
            
            push_zero_align(arena, 8);
            
            *length = (u32)(arena_current(arena) - start);
        }
        
        fill_section_header(eh_frame, "eh_frame", 0x70000001, SHF_ALLOC, /*alignment*/8, /*link*/0, /*info*/0, /*entry_size*/0);
    }

    
    if(rodata_section_start != arena_current(arena)){
        fill_program_header(ro, PT_LOAD, PF_READ, 0x1000);
    }
    
    u8 *rw_segment_start = arena_current(arena);
    
    if(imports.count || data_imports.count){
        push_align(arena, 8);
        
        u8 *got_plt_section_start = arena_current(arena);
        u32 got_plt_section_rva = make_relative_virtual_address(rw_segment_start, got_plt_section_start);
        
        if(imports.count){
            u64 *got_plt = push_uninitialized_data(arena, u64, imports.count + 3);
            got_plt[0] = 0; // filled in below. Not sure if this is actually used.
            got_plt[1] = 0; // reserved
            got_plt[2] = 0; // reserved
            
            *(u32 *)(plt_section_start + 2) += got_plt_section_rva;
            *(u32 *)(plt_section_start + 8) += got_plt_section_rva;
            
            {
                smm import_index = 0;
                for_ast_list(imports){
                    u32 plt_entry_offset = (u32)(0x10 + 0x10 * import_index);
                    u32 import_plt_rva = plt_section_rva + plt_entry_offset;
                    
                    struct ast_function *function = (struct ast_function *)it->value;
                    assert(function->kind == IR_function);
                    function->relative_virtual_address = import_plt_rva;
                    
                    got_plt[import_index + 3] = virtual_image_base + import_plt_rva + 6;
                    *(u32 *)(plt_section_start + plt_entry_offset + 2) += got_plt_section_rva;
                    
                    import_index++;
                }
            }
            
            u64 got_plt_section_index = section_header_at;
            fill_section_header(got_plt, "got.plt", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/8, /*link*/0, /*info*/0, /*entry_size*/8);
            
            {
                // Now that we know where the got_plt is, fill out the rela.plt relocations:
                
                for(smm symbol_index = 0; symbol_index < imports.count; symbol_index++){
                    struct elf_relocation_addend *relocation = rela_plt_relocations + symbol_index;
                    relocation->info = ((symbol_index + 1) << 32) | /*R_X86_64_JUMP_SLOT*/7;
                    relocation->offset = virtual_image_base + got_plt_section_rva + 8 * symbol_index + 0x18;
                    relocation->addend = 0;
                }
                
                section_headers[rela_plt_section_index].info = (u32)got_plt_section_index;
            }
        }
        
        if(data_imports.count){
            u8 *got_section_start = arena_current(arena);
            u32 got_section_rva = make_relative_virtual_address(rw_segment_start, got_section_start);
            
            u64 *got = push_data(arena, u64, data_imports.count); // I think these can all be 0.
            
            {
                u64 import_index = 0;
                for_ast_list(data_imports){
                    struct ast_declaration *decl = (struct ast_declaration *)it->value;
                    assert(decl->kind == IR_declaration);
                    
                    decl->relative_virtual_address = make_relative_virtual_address(rw_segment_start, got + import_index);
                    
                    import_index++;
                }
            }
            
            u64 got_section_index = section_header_at;
            fill_section_header(got, "got", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/8, /*link*/0, /*info*/0, /*entry_size*/8);
            
            {
                // Now that we know where the got is, fill out the rela.dyn relocations:
                
                for(smm symbol_index = 0; symbol_index < data_imports.count; symbol_index++){
                    struct elf_relocation_addend *relocation = rela_dyn_relocations + symbol_index;
                    relocation->info = ((imports.count + symbol_index + 1) << 32) | /*R_X86_64_GLOB_DAT*/6;
                    relocation->offset = virtual_image_base + got_section_rva + 8 * symbol_index;
                    relocation->addend = 0;
                }
                
                section_headers[rela_dyn_section_index].info = (u32)got_section_index;
            }
        }
        
        u8 *dynamic_section_start = arena_current(arena);
        u8 *dynamic_segment_start = dynamic_section_start;
        
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
        
        if(imports.count){
            *push_struct(arena, u64) = /*DT_PLTGOT*/3;
            *push_struct(arena, u64) = virtual_image_base + got_plt_section_rva;
            
            *push_struct(arena, u64) = /*DT_PLTRELSZ*/2;
            *push_struct(arena, u64) = rela_plt_section_size;
            
            *push_struct(arena, u64) = /*DT_PLTREL*/20;
            *push_struct(arena, u64) = /*R_X86_64_JUMP_SLOT*/7;
            
            *push_struct(arena, u64) = /*DT_JMPREL*/0x17;
            *push_struct(arena, u64) = virtual_image_base + rela_plt_section_rva;
        }
        
        if(data_imports.count){
            *push_struct(arena, u64) = /*DT_RELA*/7;
            *push_struct(arena, u64) = virtual_image_base + rela_dyn_section_rva;
            
            *push_struct(arena, u64) = /*DT_RELASZ*/8;
            *push_struct(arena, u64) = rela_dyn_section_size;
            
            *push_struct(arena, u64) = /*DT_RELAENT*/9;
            *push_struct(arena, u64) = sizeof(struct elf_relocation_addend);
        }
        
        *push_struct(arena, u64) = /*DT_FLAGS*/0x1e;
        *push_struct(arena, u64) = /*DF_BIND_NOW*/8;
        
        *push_struct(arena, u64) = /*DT_NONE*/0;
        *push_struct(arena, u64) = 0;
        
        fill_section_header(dynamic, "dynamic", SHT_DYNAMIC, SHF_ALLOC | SHF_WRITE, /*alignment*/8, /*link*/(u32)dynstr_section_index, /*info*/0, /*entry_size*/0x10);
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
        data_section_index = section_header_at;
        fill_section_header(data, "data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/4, /*link*/0, /*info*/0, /*entry_size*/0);
    }
    
    if(!globals.cli_options.no_debug){
        
        u8 *symtab_section_start = arena_current(arena);
        
        u32 symbol_name_offset = 1;
        u64 first_non_local_symbol = 0;
        
        struct string_list strtab = zero_struct;
        
        push_struct(arena, struct elf_symbol); // The first one is all zeroes for some reason.
        
        for(s32 locals_done = 0; locals_done < 2; locals_done++){
            if(locals_done){
                first_non_local_symbol = (arena_current(arena) - symtab_section_start)/sizeof(struct elf_symbol);
            }
            
            for_ast_list(defined_functions){
                struct ast_function *function = (struct ast_function *)it->value;
                
                u8 bind = /*STB_GLOBAL*/1;
                if(function->as_decl.flags & DECLARATION_FLAGS_is_static) bind = /*STB_LOCAL*/0;
                
                if(locals_done == (bind == /*STB_LOCAL*/0)) continue;
                
                string_list_postfix_no_copy(&strtab, scratch, token_get_string(function->identifier));
                
                struct elf_symbol *elf_symbol = push_struct(arena, struct elf_symbol);
                elf_symbol->name_offset = symbol_name_offset; symbol_name_offset += function->identifier->size + 1;
                elf_symbol->info = (bind << 4) | /*STT_FUNC*/2;
                elf_symbol->other = 0;
                elf_symbol->section_index = (u16)text_section_index;
                elf_symbol->value = function->relative_virtual_address + virtual_image_base;
                elf_symbol->size  = function->byte_size;
            }
            
            struct ast_list declaration_ast_lists[] = {
                initialized_declarations,
                uninitialized_declarations,
            };
            
            for(u32 declaration_ast_list_index = 0; declaration_ast_list_index < array_count(declaration_ast_lists); declaration_ast_list_index += 1){
                for_ast_list(declaration_ast_lists[declaration_ast_list_index]){
                    struct ast_declaration *declaration = (struct ast_declaration *)it->value;
                    
                    u8 bind = /*STB_GLOBAL*/1;
                    if(declaration->flags & DECLARATION_FLAGS_is_static) bind = /*STB_LOCAL*/0;
                    
                    if(locals_done == (bind == /*STB_LOCAL*/0)) continue;
                    
                    string_list_postfix_no_copy(&strtab, scratch, token_get_string(declaration->identifier));
                    
                    struct elf_symbol *elf_symbol = push_struct(arena, struct elf_symbol);
                    elf_symbol->name_offset = symbol_name_offset; symbol_name_offset += declaration->identifier->size + 1;
                    elf_symbol->info = (bind << 4) | /*STT_OBJECT*/1;
                    elf_symbol->other = 0;
                    elf_symbol->section_index = (u16)(declaration_ast_list_index ? bss_section_index : data_section_index);
                    elf_symbol->value = declaration->relative_virtual_address + virtual_image_base;
                    elf_symbol->size  = get_declaration_size(declaration);
                }
            }
        }
        
        u32 strtab_section_index = (u32)(section_header_at + 1);
        fill_section_header(symtab, "symtab", SHT_SYMTAB, /*flags*/0, /*alignment*/8, /*link*/strtab_section_index, /*info*/(u32)first_non_local_symbol, /*entry_size*/sizeof(struct elf_symbol));
        
        u8 *strtab_section_start = arena_current(arena);
        
        *push_struct(arena, u8) = 0;
        
        for(struct string_list_node *node = strtab.list.first; node; node = node->next){
            push_zero_terminated_string_copy(arena, node->string);
        }
        
        fill_section_header(strtab, "strtab", SHT_STRTAB, /*flags*/0, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
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
            bss_section_index = section_header_at;
            struct elf_section_header *bss_section_header = section_headers + section_header_at;
            fill_section_header(bss, "bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, /*alignment*/0x10, /*link*/0, /*info*/0, /*entry_size*/0);
            
            bss_section_header->offset = 0;
            bss_section_header->size = bss_size;
        }
    }
    
    // 
    // @Warninig: We assume this is the last segment.
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
                    
                    if(source_kind == IR_declaration && decl->flags & DECLARATION_FLAGS_is_dllimport){
                        // On linux, functions all have a stub, so the decl->relative_virtual_address is fine.
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
    
    // 
    // Debug information:
    // 
    
    {
        // .debug_line_str
        //     directory_names
        //     file_names
        
        struct string_list directories = zero_struct;
        struct string_list file_names = zero_struct;
        
        struct string main_file_name = strip_file_path(cstring_to_string(globals.first_compilation_unit->main_file->absolute_file_path));
        
        string_list_pool_add(&directories, scratch, globals.working_directory);
        string_list_pool_add(&file_names, scratch, main_file_name);
        
        struct{
            struct file_index_node{
                struct file_index_node *next;
                u64 directory_index;
                u64 file_name_offset;
            } *first, *last;
            u64 count;
        } file_index_list = zero_struct;
        
        for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
            struct file *node = globals.file_table.data[file_index];
            if(!node) continue;
            
            struct string absolute_file_path = string_from_cstring(node->absolute_file_path);
            struct string name = strip_file_path(absolute_file_path);
            struct string path = {.data = absolute_file_path.data, .size = absolute_file_path.size - name.size - 1};
            
            struct string_list_pool_add_return directory_add_ret = string_list_pool_add(&directories, scratch, path);
            struct string_list_pool_add_return file_name_add_ret = string_list_pool_add(&file_names, scratch, name);
            
            struct file_index_node *index_node = push_struct(scratch, struct file_index_node);
            index_node->directory_index  = directory_add_ret.index;
            index_node->file_name_offset = file_name_add_ret.offset;
            sll_push_back(file_index_list, index_node);
            
            node->file_number = (u32)file_index_list.count;
            file_index_list.count += 1;
        }
        
        u8 *debug_line_str_section_start = arena_current(arena);
        
        string_list_pool_flatten(directories, arena);
        string_list_pool_flatten(file_names, arena);
        
        fill_section_header(debug_line_str, "debug_line_str", SHT_PROGBITS, /*flags(SHF_MERGE|SHF_STRINGS)*/0x30, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/1);
        
        // .debug_line
        //    v5_header
        //    directory table
        //    file_name table
        //    line_number_ir
        
        struct dwarf_debug_line_v5_header{
            u32 initial_length;
            u16 version;
            u8  address_size;
            u8  segment_selector_size;
            u32 header_length;
            u8  minimum_instruction_length;
            u8  maximum_operation_per_instruction;
            u8  default_is_statement;
            s8  line_base;
            u8  line_range;
            u8  opcode_base;
            u8  opcode_args[12];
        } *header = push_struct_(arena, offset_in_type(struct dwarf_debug_line_v5_header, opcode_args) + sizeof(header->opcode_args), 4);
        header->version = 5;
        header->address_size = 8;
        header->segment_selector_size = 0; // ?
        header->minimum_instruction_length = 1;
        header->maximum_operation_per_instruction = 1;
        header->default_is_statement = 1;
        header->line_base  = -5;
        header->line_range = 14;
        header->opcode_base = 13;
        header->opcode_args[1-1] = 0;
        header->opcode_args[2-1] = 1;
        header->opcode_args[3-1] = 1;
        header->opcode_args[4-1] = 1;
        header->opcode_args[5-1] = 1;
        header->opcode_args[6-1] = 0;
        header->opcode_args[7-1] = 0;
        header->opcode_args[8-1] = 0;
        header->opcode_args[9-1] = 1;
        header->opcode_args[10-1] = 0;
        header->opcode_args[11-1] = 0;
        header->opcode_args[12-1] = 1;
        
        *push_struct(arena, u8) = /*directory_entry_format_count*/1;
        *push_struct(arena, u8) = /*content_type = DW_LNCT_path*/1;
        *push_struct(arena, u8) = /*attribute form = DW_FORM_line_strp*/0x1f;
        
        push_uleb(arena, directories.amount_of_strings);
        
        u32 debug_line_string_offset = 0;
        for(struct string_list_node *node = directories.list.first; node; node = node->next){
            *push_struct_unaligned(arena, u32) = debug_line_string_offset;
            debug_line_string_offset += (u32)node->string.size + 1;
        }
        
        *push_struct(arena, u8) = /*file_name_entry_format_count*/2;
        *push_struct(arena, u8) = /*content_type = DW_LNCT_path*/1;
        *push_struct(arena, u8) = /*attribute form = DW_FORM_line_strp*/0x1f;
        *push_struct(arena, u8) = /*content_type = DW_LNCT_directory_index*/2;
        *push_struct(arena, u8) = /*attribute form = DW_FORM_udata*/0xf;
        
        push_uleb(arena, file_index_list.count + (file_index_list.count == 1));
        
        for(struct file_index_node *node = file_index_list.first; node; node = node->next){
            *push_struct_unaligned(arena, u32) = (u32)(debug_line_string_offset + node->file_name_offset);
            *push_struct(arena, u8) = (u8)node->directory_index;
        }
        
        if(file_index_list.count == 1){
            // For whatever reason gdb adds this if there is only one.
            struct file_index_node *node = file_index_list.first;
            *push_struct_unaligned(arena, u32) = (u32)(debug_line_string_offset + node->file_name_offset);
            *push_struct(arena, u8) = (u8)node->directory_index;
        }
        
        // "The number of bytes following the header_length field to the beginning of 24 the first byte of the line number program itself."
        header->header_length = (u32)(arena_current(arena) - (u8 *)(&header->header_length + 1));
        
        // Virtual machine registers:
        u64 address = 0;
        u64 file_number = 1;
        u64 line = 1;
        // u64 column = 0;
        
        u32 is_statement = 1;
        
        for(struct ast_list_node *function_node = defined_functions.first; function_node; function_node = function_node->next){
            struct ast_function *function = (struct ast_function *)function_node->value;
            
            struct token_location_information initial_location = get_location_for_token(location_arena, function->compilation_unit, function->scope->token);
            struct file *file = globals.file_table.data[initial_location.file_index];
            
            if(file_number != file->file_number){
                *push_struct(arena, u8) = /*DW_LNS_set_file*/4;
                push_uleb(arena, file->file_number);
            }
            
            s8 line_base = -5;
            u8 line_range = 14;
            u8 opcode_base = 13;
            
            {
                // 
                // Push the initial location.
                // 
                
                s64 line_delta    = initial_location.line - line;
                s64 address_delta = address - function->relative_virtual_address;
                
                if(line_base <= line_delta && line_delta < line_range && 0 <= address_delta && address_delta <= 255/14){
                    *push_struct(arena, u8) = (u8)((line_delta - line_base) + line_range * address_delta + opcode_base);
                }else{
                    *push_struct(arena, u8) = /*extended opcode*/0;
                    *push_struct(arena, u8) = /*length*/9;
                    *push_struct(arena, u8) = /*DW_LNE_set_address*/2;
                    *push_struct_unaligned(arena, u64) = (u64)(virtual_image_base + function->relative_virtual_address);
                    
                    *push_struct(arena, u8) = /*DW_LNS_advance_line*/3;
                    push_sleb(arena, line_delta);
                    
                    *push_struct(arena, u8) = /*DW_LNS_copy*/1;
                }
            }
            
            *push_struct(arena, u8) = /*DW_LNS_set_prologue_end*/10;
            
            struct function_line_information last = {
                .line = initial_location.line,
                .column = 0,
                .offset = 0,
            };
            
            for(smm index = 0; index < function->line_information.size; index++){
                struct function_line_information info = function->line_information.data[index];
                
                if((info.flags & /*is_statement*/1) != is_statement){
                    *push_struct(arena, u8) = /*DW_LNS_negate_stmt*/6;
                    is_statement = !is_statement;
                }
                
                s64 line_delta    = (s64)info.line   - (s64)last.line;
                s64 address_delta = (s64)info.offset - (s64)last.offset;
                
                *push_struct(arena, u8) = /*DW_LNS_set_column*/5;
                push_uleb(arena, info.column);
                
                if(line_base <= line_delta && line_delta < line_range && 0 <= address_delta && address_delta <= 255/14){
                    *push_struct(arena, u8) = (u8)((line_delta - line_base) + line_range * address_delta + opcode_base);
                }else{
                    *push_struct(arena, u8) = /*extended opcode*/0;
                    *push_struct(arena, u8) = /*length*/9;
                    *push_struct(arena, u8) = /*DW_LNE_set_address*/2;
                    *push_struct_unaligned(arena, u64) = (u64)(virtual_image_base + function->relative_virtual_address + info.offset);
                    
                    *push_struct(arena, u8) = /*DW_LNS_advance_line*/3;
                    push_sleb(arena, line_delta);
                    
                    *push_struct(arena, u8) = /*DW_LNS_copy*/1;
                }
                
                last = info;
            }
            
            file_number = file->file_number;
            line = last.line;
            address = function->relative_virtual_address + last.offset;
        }
        
        *push_struct(arena, u8) = /*extended opcode*/0;
        *push_struct(arena, u8) = /*length*/1;
        *push_struct(arena, u8) = /*DW_LNE_end_sequence*/1;
        
        header->initial_length = (u32)(arena_current(arena) - ((u8 *)header + 4));
        
        u8 *debug_line_section_start = (u8 *)header;
        fill_section_header(debug_line, "debug_line", SHT_PROGBITS, /*flags*/0, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
        
        // 
        // .debug_abbrev
        // 
        //     DW_TAG_compile_unit
        //         DW_AT_producer           DW_FORM_string
        //         DW_AT_language           DW_FORM_data2
        //         DW_AT_name               DW_FORM_string
        //         DW_AT_stmt_list          DW_FORM_sec_offset
        //         DW_AT_comp_dir           DW_FORM_string
        //         DW_AT_low_pc             DW_FORM_data4
        //         DW_AT_high_pc            DW_FORM_data4
        //         
        //         DW_AT_str_offsets_base   DW_FORM_sec_offset
        //         DW_AT_addr_base          DW_FORM_sec_offset
        //         DW_AT_rnglists_base      DW_FORM_sec_offset
        //         DW_AT_loclists_base      DW_FORM_sec_offset
        // 
        
        u8 *debug_abbrev_section_start = arena_current(arena);
        
        *push_struct(arena, u8) = /*index*/ABBREV_compilation_unit;
        *push_struct(arena, u8) = /*DW_TAG_compile_unit*/0x11;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_producer*/0x25;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_language*/0x13;
            *push_struct(arena, u8) = /*DW_FORM_data2*/0x5;
            
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_stmt_list*/0x10;
            *push_struct(arena, u8) = /*DW_FORM_sec_offset*/0x17;
            
            *push_struct(arena, u8) = /*DW_AT_comp_dir*/0x1b;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_low_pc*/0x11;
            *push_struct(arena, u8) = /*DW_FORM_data4*/0x06;
            
            *push_struct(arena, u8) = /*DW_AT_high_pc*/0x12;
            *push_struct(arena, u8) = /*DW_FORM_data4*/0x06;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_subprogram
        //         DW_AT_name               DW_FORM_string
        //         DW_AT_decl_file          DW_FORM_data4
        //         DW_AT_decl_line          DW_FORM_data4
        //         DW_AT_type               DW_FORM_ref4
        //         DW_AT_low_pc             DW_FORM_addr
        //         DW_AT_high_pc            DW_FORM_data8
        //         DW_AT_frame_base         DW_FORM_exprloc
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_function;
        *push_struct(arena, u8) = /*DW_TAG_subprogram*/0x2e;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = /*DW_AT_low_pc*/0x11;
            *push_struct(arena, u8) = /*DW_FORM_addr*/0x1;
            
            *push_struct(arena, u8) = /*DW_AT_high_pc*/0x12;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            *push_struct(arena, u8) = /*DW_AT_frame_base*/0x40;
            *push_struct(arena, u8) = /*DW_FORM_exprloc*/0x18;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_subprogram
        //         DW_AT_name               DW_FORM_string
        //         DW_AT_decl_file          DW_FORM_data4
        //         DW_AT_decl_line          DW_FORM_data4
        //         DW_AT_low_pc             DW_FORM_addr
        //         DW_AT_high_pc            DW_FORM_data8
        //         DW_AT_frame_base         DW_FORM_exprloc
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_void_function;
        *push_struct(arena, u8) = /*DW_TAG_subprogram*/0x2e;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_low_pc*/0x11;
            *push_struct(arena, u8) = /*DW_FORM_addr*/0x1;
            
            *push_struct(arena, u8) = /*DW_AT_high_pc*/0x12;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            *push_struct(arena, u8) = /*DW_AT_frame_base*/0x40;
            *push_struct(arena, u8) = /*DW_FORM_exprloc*/0x18;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_variable
        //         DW_AT_name               DW_FORM_string
        //         DW_AT_decl_file          DW_FORM_data4
        //         DW_AT_decl_line          DW_FORM_data4
        //         DW_AT_decl_column        DW_FORM_data4
        //         DW_AT_type               DW_FORM_ref4
        //         DW_AT_location           DW_FORM_exprloc
        //  
        *push_struct(arena, u8) = /*index*/ABBREV_variable;
        *push_struct(arena, u8) = /*DW_TAG_variable*/0x34;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = /*DW_AT_location*/0x2;
            *push_struct(arena, u8) = /*DW_FORM_exprloc*/0x18;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_lexical_block
        //         DW_AT_low_pc                DW_FORM_addr
        //         DW_AT_high_pc               DW_FORM_data8
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_lexical_block;
        *push_struct(arena, u8) = /*DW_TAG_lexical_block*/0xb;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_low_pc*/0x11;
            *push_struct(arena, u8) = /*DW_FORM_addr*/0x1;
            
            *push_struct(arena, u8) = /*DW_AT_high_pc*/0x12;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            // *push_struct(arena, u8) = /*DW_AT_sibling*/1;
            // *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_base_type
        //         DW_AT_byte_size          DW_FORM_data1
        //         DW_AT_encoding           DW_FORM_data1
        //         DW_AT_name               DW_FORM_string
        //         
        *push_struct(arena, u8) = /*index*/ABBREV_base_type;
        *push_struct(arena, u8) = /*DW_TAG_base_type*/0x24;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_encoding*/0x3e;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_pointer_type
        //         DW_AT_byte_size          DW_FORM_data1
        //         DW_AT_type               DW_FORM_ref4
        //         
        *push_struct(arena, u8) = /*index*/ABBREV_pointer_type;
        *push_struct(arena, u8) = /*DW_TAG_pointer_type*/0xf;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_pointer_type
        //         DW_AT_byte_size          DW_FORM_data1
        //         
        *push_struct(arena, u8) = /*index*/ABBREV_void_pointer_type;
        *push_struct(arena, u8) = /*DW_TAG_pointer_type*/0xf;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_array_type
        //         DW_AT_byte_size          DW_FORM_data8
        //         DW_AT_type               DW_FORM_ref4
        //         
        //     DW_TAG_subrange_type         
        //         DW_AT_type               DW_FORM_ref4
        //         DW_AT_upper_bound        DW_FORM_data8
        //         
        *push_struct(arena, u8) = /*index*/ABBREV_array_type;
        *push_struct(arena, u8) = /*DW_TAG_array_type*/0x1;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        *push_struct(arena, u8) = /*index*/ABBREV_subrange_type;
        *push_struct(arena, u8) = /*DW_TAG_subrange_type*/0x21;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = /*DW_AT_upper_bound*/0x2f;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        *push_struct(arena, u8) = /*index*/ABBREV_subrange_unknown_size_type;
        *push_struct(arena, u8) = /*DW_TAG_subrange_type*/0x21;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_structure_type
        //         DW_AT_name                   DW_FORM_string
        //         DW_AT_byte_size              DW_FORM_data8
        //         DW_AT_decl_file              DW_FORM_data4
        //         DW_AT_decl_line              DW_FORM_data4
        //         DW_AT_decl_column            DW_FORM_data4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_structure_type;
        *push_struct(arena, u8) = /*DW_TAG_structure_type*/0x13;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_union_type
        //         DW_AT_name                   DW_FORM_string
        //         DW_AT_byte_size              DW_FORM_data8
        //         DW_AT_decl_file              DW_FORM_data4
        //         DW_AT_decl_line              DW_FORM_data4
        //         DW_AT_decl_column            DW_FORM_data4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_union_type;
        *push_struct(arena, u8) = /*DW_TAG_union_type*/0x17;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data8*/0x7;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_member
        //         DW_AT_name                   DW_FORM_string
        //         DW_AT_decl_file              DW_FORM_data4
        //         DW_AT_decl_line              DW_FORM_data4
        //         DW_AT_decl_column            DW_FORM_data4
        //         DW_AT_type                   DW_FORM_ref4
        //         DW_AT_data_member_location   DW_FORM_data8
        //         
        *push_struct(arena, u8) = /*index*/ABBREV_member;
        *push_struct(arena, u8) = /*DW_TAG_member*/0xd;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = /*DW_AT_data_member_location*/0x38;
            *push_struct(arena, u8) = /*DW_FORM_data4*/7;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        // Bitfield member:
        // 
        //     DW_TAG_member
        //         DW_AT_name                   DW_FORM_string
        //         DW_AT_decl_file              DW_FORM_data4
        //         DW_AT_decl_line              DW_FORM_data4
        //         DW_AT_decl_column            DW_FORM_data4
        //         DW_AT_type                   DW_FORM_ref4
        //         DW_AT_bit_size               DW_FORM_data1
        //         DW_AT_data_member_location   DW_FORM_data8
        //         
        *push_struct(arena, u8) = /*index*/ABBREV_bitfield_member;
        *push_struct(arena, u8) = /*DW_TAG_member*/0xd;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = /*DW_AT_data_bit_offset*/0x6b;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_bit_size*/0xd;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_data_member_location*/0x38;
            *push_struct(arena, u8) = /*DW_FORM_data4*/7;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_enumeration_type
        //         DW_AT_name                   DW_FORM_string
        //         DW_AT_encoding               DW_FORM_data1
        //         DW_AT_byte_size              DW_FORM_data1
        //         DW_AT_type                   DW_FORM_ref4
        //         DW_AT_decl_file              DW_FORM_data4
        //         DW_AT_decl_line              DW_FORM_data4
        //         DW_AT_decl_column            DW_FORM_data4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_enumeration_type;
        *push_struct(arena, u8) = /*DW_TAG_enumeration_type*/0x4;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_encoding*/0x3e;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_byte_size*/0xb;
            *push_struct(arena, u8) = /*DW_FORM_data1*/0xb;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = /*DW_AT_decl_file*/0x3a;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_line*/0x3b;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = /*DW_AT_decl_column*/0x39;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_enumerator
        //         DW_AT_name                 DW_FORM_string
        //         DW_AT_const_value          DW_FORM_data4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_enumerator;
        *push_struct(arena, u8) = /*DW_TAG_enumerator*/0x28;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_const_value*/0x1c;
            *push_struct(arena, u8) = /*DW_FORM_data4*/6;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_structure_type
        //         DW_AT_name                   DW_FORM_string
        //         DW_AT_declaration            DW_FORM_flag_present
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_unresolved_type;
        *push_struct(arena, u8) = /*DW_TAG_structure_type*/0x13;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_name*/0x3;
            *push_struct(arena, u8) = /*DW_FORM_string*/0x08;
            
            *push_struct(arena, u8) = /*DW_AT_declaration*/0x3c;
            *push_struct(arena, u8) = /*DW_FORM_flag_present*/0x19;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_atomic_type
        //         DW_AT_type                    DW_FORM_ref4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_atomic_type;
        *push_struct(arena, u8) = /*DW_TAG_atomic_type*/0x47;
        *push_struct(arena, u8) = /*have_children*/0;
        {
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_subroutine_type
        //         DW_AT_prototyped              DW_FORM_flag_present
        //         DW_AT_type                    DW_FORM_ref4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_function_type;
        *push_struct(arena, u8) = /*DW_TAG_subroutine_type*/0x15;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_prototyped*/0x27;
            *push_struct(arena, u8) = /*DW_FORM_flag_present*/0x19;
            
            *push_struct(arena, u8) = /*DW_AT_type*/0x49;
            *push_struct(arena, u8) = /*DW_FORM_ref4*/0x13;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        // 
        //     DW_TAG_subroutine_type
        //         DW_AT_prototyped              DW_FORM_flag_present
        //         DW_AT_type                    DW_FORM_ref4
        // 
        *push_struct(arena, u8) = /*index*/ABBREV_void_function_type;
        *push_struct(arena, u8) = /*DW_TAG_subroutine_type*/0x15;
        *push_struct(arena, u8) = /*have_children*/1;
        {
            *push_struct(arena, u8) = /*DW_AT_prototyped*/0x27;
            *push_struct(arena, u8) = /*DW_FORM_flag_present*/0x19;
            
            *push_struct(arena, u8) = 0; *push_struct(arena, u8) = 0; // zero-terminator
        }
        
        *push_struct(arena, u8) = /*zero-terminator*/0;
        
        fill_section_header(debug_abbrev, "debug_abbrev", SHT_PROGBITS, /*flags*/0, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
        
        struct debug_info_header{
            u32 length;
            u16 version;
            u8 unit_type;
            u8 address_size;
            u32 abbrev_offset;
        } *debug_info_header = push_struct(arena, struct debug_info_header);
        debug_info_header->version = 5;
        debug_info_header->unit_type = 1; // ?
        debug_info_header->address_size = 8;
        debug_info_header->abbrev_offset = 0;
        
        u8 *debug_info_base = (u8 *)debug_info_header;
        
        // DW_TAG_COMPILE_UNIT:
        {
            *push_struct(arena, u8) = /*index*/ABBREV_compilation_unit;
            
            push_zero_terminated_string_copy(arena, string("hlc")); // producer
            *push_struct_unaligned(arena, u16) = 29; // language (C11)
            push_zero_terminated_string_copy(arena, main_file_name); // name
            *push_struct_unaligned(arena, u32) = 0; // stmt_list
            push_zero_terminated_string_copy(arena, globals.working_directory); // comp_dir
            
            struct elf_section_header *text_section = section_headers + text_section_index;
            *push_struct_unaligned(arena, u32) = (u32)(text_section->address); // low_pc
            *push_struct_unaligned(arena, u32) = (u32)(text_section->size); // high_pc
        }
        
        // 
        // Register all of the basic types.
        // 
        
        {
            struct ast_type *type = &globals.typedef_void_pointer.base;
            type->flags |= TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_temporary;
            type->dwarf_form_offset = (u32)(arena_current(arena) - debug_info_base);
            *push_struct(arena, u8) = /*index*/ABBREV_void_pointer_type;
            *push_struct(arena, u8) = /*byte_size*/(u8)type->size;
        }
        
        {
            struct ast_type *type = &globals.typedef_Bool;
            type->flags |= TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_temporary;
            type->dwarf_form_offset = (u32)(arena_current(arena) - debug_info_base);
            *push_struct(arena, u8) = /*index*/ABBREV_base_type;
            *push_struct(arena, u8) = /*byte_size*/(u8)type->size;
            *push_struct(arena, u8) = /*encoding(DW_ATE_boolean)*/2;
            push_zero_terminated_string_copy(arena, basic_type_string(type));
        }
        
        for(struct ast_type *type = &globals.typedef_s8; type <= &globals.typedef_u64; type++){
            type->flags |= TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_temporary;
            type->dwarf_form_offset = (u32)(arena_current(arena) - debug_info_base);
            *push_struct(arena, u8) = /*index*/ABBREV_base_type;
            *push_struct(arena, u8) = /*byte_size*/(u8)type->size;
            *push_struct(arena, u8) = (/*encoding*/ type_is_signed(type) ? /*signed*/5 : /*unsigned*/7) + /*char*/(type->size == 1);
            push_zero_terminated_string_copy(arena, basic_type_string(type));
        }
        
        for(struct ast_type *type = &globals.typedef_f32; type <= &globals.typedef_f64; type++){
            type->flags |= TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_temporary;
            type->dwarf_form_offset = (u32)(arena_current(arena) - debug_info_base);
            *push_struct(arena, u8) = /*index*/ABBREV_base_type;
            *push_struct(arena, u8) = /*byte_size*/(u8)type->size;
            *push_struct(arena, u8) = /*encoding(DW_ATE_float)*/4;
            push_zero_terminated_string_copy(arena, basic_type_string(type));
        }
        
        for(struct ast_type *type = &globals.typedef_atomic_bool; type <= &globals.typedef_atomic_u64; type++){
            type->flags |= TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_temporary;
            type->dwarf_form_offset = (u32)(arena_current(arena) - debug_info_base);
            
            // :translate_atomic_to_non_atomic_and_back
            struct ast_type *base_type = type - (&globals.typedef_atomic_bool - &globals.typedef_Bool);
            
            *push_struct(arena, u8) = /*index*/ABBREV_atomic_type;
            *push_struct_unaligned(arena, u32) = base_type->dwarf_form_offset;
        }
        
        // 
        // Register all compound types.
        // 
        
        for(u64 index = 0; index < globals.compound_types.capacity; index++){
            struct ast_node *node = globals.compound_types.nodes + index;
            if(!node->token) continue;
            
            struct ast_type *type = (struct ast_type *)node->ast;
            
            if(type->dwarf_form_offset == 0){
                dwarf_register_type(arena, type, scratch, location_arena, debug_info_base);
            }
        }
        
        // 
        // Register all declarations.
        // 
        
        for(struct compilation_unit *compilation_unit = &globals.hacky_global_compilation_unit; compilation_unit; compilation_unit = compilation_unit->next){
            struct ast_table *table = &compilation_unit->static_declaration_table; // :DeclarationTableLoop
            
            for(u64 table_index = 0; table_index < table->capacity; table_index++){
                enum ast_kind *ast = table->nodes[table_index].ast;
                if(!ast) continue;
                
                struct ast_declaration *decl = (struct ast_declaration *)ast;
                struct ast_type *type = decl->type;
                
                
                struct string identifier = token_get_string(decl->identifier);
                struct token_location_information location = get_location_for_token(location_arena, decl->compilation_unit, decl->identifier);
                struct file *file = globals.file_table.data[location.file_index];
                
                if(decl->kind == IR_typedef){
                    if(type->dwarf_form_offset == 0 && type != &globals.typedef_void){
                        dwarf_register_type(arena, type, scratch, location_arena, debug_info_base);
                    }
                    
                    // @cleanup: Typedefs.
                    
                    continue;
                }
                
                if(*ast == IR_declaration){
                    
                    // For dllimports, the defining dll has the declaration and type information.
                    if(decl->flags & DECLARATION_FLAGS_is_dllimport) continue;
                    
                    if(!(decl->flags & DECLARATION_FLAGS_is_reachable_from_entry)) continue;
                    
                    if(type->dwarf_form_offset == 0){
                        dwarf_register_type(arena, type, scratch, location_arena, debug_info_base);
                    }
                    
                    *push_struct(arena, u8) = /*index*/ABBREV_variable;
                    push_zero_terminated_string_copy(arena, identifier);
                    
                    *push_struct_unaligned(arena, u32) = (u32)file->file_number;
                    *push_struct_unaligned(arena, u32) = (u32)location.line;
                    *push_struct_unaligned(arena, u32) = (u32)location.column;
                    *push_struct_unaligned(arena, u32) = type->dwarf_form_offset;
                    
                    *push_struct(arena, u8) = /*length*/9;
                    *push_struct(arena, u8) = /*DW_OP_addr*/0x03;
                    *push_struct_unaligned(arena, u64) = virtual_image_base + decl->relative_virtual_address;
                    continue;
                }
            }
        }
        
        for(struct ast_list_node *function_node = defined_functions.first; function_node; function_node = function_node->next){
            struct ast_function *function = (struct ast_function *)function_node->value;
            struct ast_scope *root_scope = function->scope;
            
            struct token_location_information initial_location = get_location_for_token(location_arena, function->compilation_unit, root_scope->token);
            struct file *file = globals.file_table.data[initial_location.file_index];
            
            struct ast_type *return_type = function->type->return_type;
            int returns_void = (return_type == &globals.typedef_void);
            
            if(!returns_void && return_type->dwarf_form_offset == 0){
                dwarf_register_type(arena, return_type, scratch, location_arena, debug_info_base);
            }
            
            *push_struct(arena, u8) = /*index*/returns_void ? ABBREV_void_function : ABBREV_function;
            push_zero_terminated_string_copy(arena, token_get_string(function->identifier));
            *push_struct_unaligned(arena, u32) = (u32)file->file_number;
            *push_struct_unaligned(arena, u32) = (u32)initial_location.line;
            *push_struct_unaligned(arena, u32) = (u32)initial_location.column;
            if(!returns_void){
                *push_struct_unaligned(arena, u32) = return_type->dwarf_form_offset;
            }
            *push_struct_unaligned(arena, u64) = virtual_image_base + function->relative_virtual_address;
            *push_struct_unaligned(arena, u64) = function->byte_size;
            *push_struct(arena, u8) = 1;
            *push_struct(arena, u8) = /*DW_OP_call_frame_cfa*/0x9c;
            
            dwarf_emit_debug_information_for_function__recursive(function, arena, root_scope, scratch, location_arena, virtual_image_base, debug_info_base);
            
            *push_struct(arena, u8) = /*end*/0;
        }
        
        *push_struct(arena, u8) = /*end*/0;
        
        debug_info_header->length = (u32)(arena_current(arena) - (u8 *)(&debug_info_header->length + 1));
        
        u8 *debug_info_section_start = (u8 *)debug_info_header;
        fill_section_header(debug_info, "debug_info", SHT_PROGBITS, /*flags*/0, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
    }
    
    struct string shstrtab = string_list_flatten(section_name_string_table, arena);
    u8 *shstrtab_section_start = shstrtab.data;
    arena->current -= 1;
    push_zero_terminated_string_copy(arena, string(".shstrtab"));
    
    fill_section_header(shstrtab, "shstrtab", SHT_STRTAB, /*flags*/0, /*alignment*/1, /*link*/0, /*info*/0, /*entry_size*/0);
    
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
    
    assert((u8 *)(section_headers + section_header_at) <= elf_base + 0x1000);
    
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

enum dwarf_cfa{
    DW_CFA_nop                = 0x00,
    DW_CFA_set_loc            = 0x01,
    DW_CFA_advance_loc1       = 0x02,
    DW_CFA_advance_loc2       = 0x03,
    DW_CFA_advance_loc4       = 0x04,
    DW_CFA_offset_extended    = 0x05,
    DW_CFA_restore_extended   = 0x06,
    DW_CFA_undefined          = 0x07,
    DW_CFA_same_value         = 0x08,
    DW_CFA_register           = 0x09,
    DW_CFA_remember_state     = 0x0a,
    DW_CFA_restore_state      = 0x0b,
    DW_CFA_def_cfa            = 0x0c,
    DW_CFA_def_cfa_register   = 0x0d,
    DW_CFA_def_cfa_offset     = 0x0e,
    DW_CFA_def_cfa_expression = 0x0f,
    DW_CFA_expression         = 0x10,
    DW_CFA_offset_extended_sf = 0x11,
    DW_CFA_def_cfa_sf         = 0x12,
    DW_CFA_def_cfa_offset_sf  = 0x13,
    DW_CFA_val_offset         = 0x14,
    DW_CFA_val_offset_sf      = 0x15,
    DW_CFA_val_expression     = 0x16,
};

void dump_dwarf_cfa(u8 *buffer, u64 size){
    
    u64 offset = 0;
    
    static char *register_names[] = {
        "rax",      // 0
        "rdx",      // 1
        "rcx",      // 2
        "rbx",      // 3
        "rsi",      // 4
        "rdi",      // 5
        "rbp",      // 6
        "rsp",      // 7
        "r8",       // 8
        "r9",       // 9
        "r10",      // 10
        "r11",      // 11
        "r12",      // 12
        "r13",      // 13
        "r14",      // 14
        "r15",      // 15
        "rip",      // 16
        "xmm0",     // 17
        "xmm1",     // 18
        "xmm2",     // 19
        "xmm3",     // 20
        "xmm4",     // 21
        "xmm5",     // 22
        "xmm6",     // 23
        "xmm7",     // 24
        "xmm8",     // 25
        "xmm9",     // 26
        "xmm10",    // 27
        "xmm11",    // 28
        "xmm12",    // 29
        "xmm13",    // 30
        "xmm14",    // 31
        "xmm15",    // 32
        "st0",      // 33
        "st1",
        "st2",
        "st3",
        "st4",
        "st5",
        "st6",
        "st7",
        "mm0",      // 41
        "mm1",
        "mm2",
        "mm3",
        "mm4",
        "mm5",
        "mm6",
        "mm7",
        "rflags",   // 49
        "es",
        "cs",
        "ss",
        "ds",
        "fs",
        "gs",
        "fs.base",
        "gs.base",
        "tr",
        "ldtr",
        "mxcsr",
        "fcw",
        "fsw"
    };
    
    while(offset < size){
        
        print("        %04zx: ", offset);
        
        u64 start_offset = offset;
        
        u8 op = buffer[offset];
        offset += 1;
        
        u8 primary = op & 0xc0;
        u8 operand = op & 0x3f;
        
        int is_primary = 0;
        
        switch(primary){
            
            case /*DW_CFA_advance_loc*/0x40:{
                print("DW_CFA_advance_loc %x\n", operand);
                is_primary = 1;
            }break;
            
            case 0x80:{
                u64 off = read_uleb(buffer, &offset);
                print("DW_CFA_offset reg=%u (%s) offset=%x\n", operand, operand < array_count(register_names) ? register_names[operand] : "", off);
                is_primary = 1;
            }break;
            
            case 0xc0:{
                print("DW_CFA_restore reg=%u (%s)\n", operand, operand < array_count(register_names) ? register_names[operand] : "");
                is_primary = 1;
            }break;
        }
        
        if(!is_primary) switch(op){
            
            case DW_CFA_nop:{
                print("DW_CFA_nop\n");
            }break;
            
            case DW_CFA_set_loc:{
                print("DW_CFA_set_loc %llx\n", *(u64 *)(buffer + offset));
                offset += 8;
            }break;
            
            case DW_CFA_advance_loc1:{
                print("DW_CFA_advance_loc1 %x\n", buffer[offset]);
                offset += 1;
            }break;
            
            case DW_CFA_advance_loc2:{
                u16 v = *(u16 *)(buffer + offset);
                print("DW_CFA_advance_loc2 %x\n", v);
                offset += 2;
            }break;
            
            case DW_CFA_advance_loc4:{
                u32 v = *(u32 *)(buffer + offset);
                offset += 4;
                print("DW_CFA_advance_loc4 %x\n", v);
            }break;
            
            case DW_CFA_offset_extended:{
                u64 reg = read_uleb(buffer, &offset);
                u64 off = read_uleb(buffer, &offset);
                print("DW_CFA_offset_extended reg=%u (%s) offset=%x\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", off);
            }break;
            
            case DW_CFA_restore_extended:{
                u64 reg = read_uleb(buffer, &offset);
                print("DW_CFA_restore_extended reg=%u (%s)\n", reg, reg < array_count(register_names) ? register_names[reg] : "???");
            }break;
            
            case DW_CFA_undefined:{
                u64 reg = read_uleb(buffer, &offset);
                print("DW_CFA_undefined reg=%u (%s)\n", reg, reg < array_count(register_names) ? register_names[reg] : "???");
            }break;
            
            case DW_CFA_same_value:{
                u64 reg = read_uleb(buffer, &offset);
                print("DW_CFA_same_value reg=%u (%s)\n", reg, reg < array_count(register_names) ? register_names[reg] : "???");
            }break;
            
            case DW_CFA_register: {
                u64 r1 = read_uleb(buffer, &offset);
                u64 r2 = read_uleb(buffer, &offset);
                print("DW_CFA_register reg=%u (%s) -> reg=%u (%s)\n", r1, r1 < array_count(register_names) ? register_names[r1] : "???", r2, r2 < array_count(register_names) ? register_names[r2] : "???");
            }break;
            
            case DW_CFA_remember_state:{
                print("DW_CFA_remember_state\n");
            }break;
            
            case DW_CFA_restore_state:{
                print("DW_CFA_restore_state\n");
            }break;
            
            case DW_CFA_def_cfa:{
                u64 reg = read_uleb(buffer, &offset);
                u64 off = read_uleb(buffer, &offset);
                print("DW_CFA_def_cfa reg=%u (%s) offset=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", off);
            }break;
            
            case DW_CFA_def_cfa_register:{
                u64 reg = read_uleb(buffer, &offset);
                print("DW_CFA_def_cfa_register reg=%u (%s)\n", reg, reg < array_count(register_names) ? register_names[reg] : "???");
            }break;
            
            case DW_CFA_def_cfa_offset:{
                u64 off = read_uleb(buffer, &offset);
                print("DW_CFA_def_cfa_offset %x\n", off);
            }break;
            
            case DW_CFA_def_cfa_expression:{
                u64 length = read_uleb(buffer, &offset);
                print("DW_CFA_def_cfa_expression len=%llx\n", length);
                
                print_byte_range(buffer + offset, length);
                
                offset += length;
            }break;
            
            case DW_CFA_expression:{
                u64 reg = read_uleb(buffer, &offset);
                u64 len = read_uleb(buffer, &offset);
                print("DW_CFA_expression reg=%u (%s) len=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", len);
                
                print_byte_range(buffer + offset, len);
                
                offset += len;
            }break;
            
            case DW_CFA_offset_extended_sf:{
                u64 reg = read_uleb(buffer, &offset);
                s64 off = read_sleb(buffer, &offset);
                print("DW_CFA_offset_extended_sf reg=%u (%s) offset=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", off);
            }break;
            
            case DW_CFA_def_cfa_sf:{
                u64 reg = read_uleb(buffer, &offset);
                s64 off = read_sleb(buffer, &offset);
                print("DW_CFA_def_cfa_sf reg=%u (%s) offset=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", off);
            }break;
            
            case DW_CFA_def_cfa_offset_sf:{
                u64 off = read_sleb(buffer, &offset);
                print("DW_CFA_def_cfa_offset_sf %llx\n", off);
            }break;
            
            case DW_CFA_val_offset:{
                u64 reg = read_uleb(buffer, &offset);
                u64 off = read_uleb(buffer, &offset);
                print("DW_CFA_val_offset reg=%u (%s) offset=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", off);
            }break;
            
            case DW_CFA_val_offset_sf:{
                u64 reg = read_uleb(buffer, &offset);
                s64 off = read_sleb(buffer, &offset);
                print("DW_CFA_val_offset_sf reg=%u (%s) offset=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", off);
            }break;
            
            case DW_CFA_val_expression:{
                u64 reg = read_uleb(buffer, &offset);
                u64 len = read_uleb(buffer, &offset);
                print("DW_CFA_val_expression reg=%u (%s) len=%llx\n", reg, reg < array_count(register_names) ? register_names[reg] : "???", len);
                
                print_byte_range(buffer + offset, len);
                
                offset += len;
            }break;
            
            default:{
                print("Unknown CFA opcode 0x%02x\n", op);
            }break;
        }
        
        print("              ");
        for(u64 index = start_offset; index < offset; index++){
            print("%.2x ", buffer[index]);
        }
        print("\n");
    }
}

#define get(a, i) (((i) < array_count(a)) ? (a)[(i)] : 0)


enum {
    DW_OP_addr       = 0x03,
    DW_OP_deref      = 0x06,
    DW_OP_const1u    = 0x08,
    DW_OP_const1s    = 0x09,
    DW_OP_const2u    = 0x0a,
    DW_OP_const2s    = 0x0b,
    DW_OP_const4u    = 0x0c,
    DW_OP_const4s    = 0x0d,
    DW_OP_const8u    = 0x0e,
    DW_OP_const8s    = 0x0f,
    DW_OP_constu     = 0x10,
    DW_OP_consts     = 0x11,
    
    DW_OP_dup        = 0x12,
    DW_OP_drop       = 0x13,
    DW_OP_over       = 0x14,
    DW_OP_pick       = 0x15,
    DW_OP_swap       = 0x16,
    DW_OP_rot        = 0x17,
    DW_OP_xderef     = 0x18,
    
    DW_OP_abs        = 0x19,
    DW_OP_and        = 0x1a,
    DW_OP_div        = 0x1b,
    DW_OP_minus      = 0x1c,
    DW_OP_mod        = 0x1d,
    DW_OP_mul        = 0x1e,
    DW_OP_neg        = 0x1f,
    DW_OP_not        = 0x20,
    DW_OP_or         = 0x21,
    DW_OP_plus       = 0x22,
    DW_OP_plus_uconst = 0x23,
    DW_OP_shl        = 0x24,
    DW_OP_shr        = 0x25,
    DW_OP_shra       = 0x26,
    DW_OP_xor        = 0x27,
    
    DW_OP_bra        = 0x28,
    DW_OP_eq         = 0x29,
    DW_OP_ge         = 0x2a,
    DW_OP_gt         = 0x2b,
    DW_OP_le         = 0x2c,
    DW_OP_lt         = 0x2d,
    DW_OP_ne         = 0x2e,
    
    DW_OP_skip       = 0x2f,
    
    DW_OP_lit0       = 0x30,
    DW_OP_lit31      = 0x4f,
    
    DW_OP_reg0       = 0x50,
    DW_OP_reg31      = 0x6f,
    
    DW_OP_breg0      = 0x70,
    DW_OP_breg31     = 0x8f,
    
    DW_OP_regx       = 0x90,
    DW_OP_fbreg      = 0x91,
    DW_OP_bregx      = 0x92,
    DW_OP_piece      = 0x93,
    DW_OP_deref_size = 0x94,
    DW_OP_xderef_size = 0x95,
    DW_OP_nop        = 0x96,
    
    DW_OP_push_object_address = 0x97,
    DW_OP_call2      = 0x98,
    DW_OP_call4      = 0x99,
    DW_OP_call_ref   = 0x9a,
    DW_OP_form_tls_address = 0x9b,
    DW_OP_call_frame_cfa = 0x9c,
    DW_OP_bit_piece  = 0x9d,
    DW_OP_implicit_value = 0x9e,
    DW_OP_stack_value = 0x9f,
    
    DW_OP_implicit_pointer = 0xa0,
    DW_OP_addrx      = 0xa1,
    DW_OP_constx     = 0xa2,
    DW_OP_entry_value = 0xa3,
    DW_OP_const_type = 0xa4,
    DW_OP_regval_type = 0xa5,
    DW_OP_deref_type = 0xa6,
    DW_OP_xderef_type = 0xa7,
    DW_OP_convert    = 0xa8,
    DW_OP_reinterpret = 0xa9,
    
    DW_OP_lo_user    = 0xe0,
    DW_OP_hi_user    = 0xff,
};

static const char *
drawf_expression_opcode_name(unsigned op)
{
    switch (op) {
#define CASE(x) case x: return #x
        CASE(DW_OP_addr);
        CASE(DW_OP_deref);
        CASE(DW_OP_const1u);
        CASE(DW_OP_const1s);
        CASE(DW_OP_const2u);
        CASE(DW_OP_const2s);
        CASE(DW_OP_const4u);
        CASE(DW_OP_const4s);
        CASE(DW_OP_const8u);
        CASE(DW_OP_const8s);
        CASE(DW_OP_constu);
        CASE(DW_OP_consts);
        CASE(DW_OP_dup);
        CASE(DW_OP_drop);
        CASE(DW_OP_over);
        CASE(DW_OP_pick);
        CASE(DW_OP_swap);
        CASE(DW_OP_rot);
        CASE(DW_OP_xderef);
        CASE(DW_OP_abs);
        CASE(DW_OP_and);
        CASE(DW_OP_div);
        CASE(DW_OP_minus);
        CASE(DW_OP_mod);
        CASE(DW_OP_mul);
        CASE(DW_OP_neg);
        CASE(DW_OP_not);
        CASE(DW_OP_or);
        CASE(DW_OP_plus);
        CASE(DW_OP_plus_uconst);
        CASE(DW_OP_shl);
        CASE(DW_OP_shr);
        CASE(DW_OP_shra);
        CASE(DW_OP_xor);
        CASE(DW_OP_bra);
        CASE(DW_OP_eq);
        CASE(DW_OP_ge);
        CASE(DW_OP_gt);
        CASE(DW_OP_le);
        CASE(DW_OP_lt);
        CASE(DW_OP_ne);
        CASE(DW_OP_skip);
        CASE(DW_OP_regx);
        CASE(DW_OP_fbreg);
        CASE(DW_OP_bregx);
        CASE(DW_OP_piece);
        CASE(DW_OP_deref_size);
        CASE(DW_OP_xderef_size);
        CASE(DW_OP_nop);
        CASE(DW_OP_push_object_address);
        CASE(DW_OP_call2);
        CASE(DW_OP_call4);
        CASE(DW_OP_call_ref);
        CASE(DW_OP_form_tls_address);
        CASE(DW_OP_call_frame_cfa);
        CASE(DW_OP_bit_piece);
        CASE(DW_OP_implicit_value);
        CASE(DW_OP_stack_value);
        CASE(DW_OP_implicit_pointer);
        CASE(DW_OP_addrx);
        CASE(DW_OP_constx);
        CASE(DW_OP_entry_value);
        CASE(DW_OP_const_type);
        CASE(DW_OP_regval_type);
        CASE(DW_OP_deref_type);
        CASE(DW_OP_xderef_type);
        CASE(DW_OP_convert);
        CASE(DW_OP_reinterpret);
#undef CASE
    }
    
    if (op >= DW_OP_lit0 && op <= DW_OP_lit31)
    return "DW_OP_lit*";
    
    if (op >= DW_OP_reg0 && op <= DW_OP_reg31)
    return "DW_OP_reg*";
    
    if (op >= DW_OP_breg0 && op <= DW_OP_breg31)
    return "DW_OP_breg*";
    
    if (op >= DW_OP_lo_user && op <= DW_OP_hi_user)
    return "DW_OP_user";
    
    return "DW_OP_unknown";
}

void dump_dwarf_expression(u8 *data, u64 size){
    
    for(u32 index = 0; index < size; index++){
        print("%.2x ", data[index]);
    }
    
    u64 offset = 0;
    while(offset < size){
        u8 opcode = data[offset++];
        
        print(" %s ", drawf_expression_opcode_name(opcode));
        
        if(opcode >= DW_OP_lit0 && opcode <= DW_OP_lit31){
            print("%d", (opcode - DW_OP_lit0));
            continue;
        }
        
        if(opcode >= DW_OP_reg0 && opcode <= DW_OP_reg31){
            print("r%d", opcode - DW_OP_reg0);
            continue;
        }
        
        if(opcode >= DW_OP_breg0 && opcode <= DW_OP_breg31){
            s64 offset_value = read_sleb(data, &offset);
            
            print("r%u + %lld", opcode - DW_OP_breg0, offset_value);
            continue;
        }
        
        switch(opcode){
            case DW_OP_addr:{
                u64 address = *(u64 *)(data + offset);
                offset += 8;
                
                print(" %llx", address);
            }break;
            
            case DW_OP_const1u:{
                u8 value = data[offset++];
                print(" %x", value);
            }break;
            case DW_OP_const1s:{
                s8 value = data[offset++];
                if(value >= 0){
                    print(" %x", value);
                }else{
                    print(" -%x", -value);
                }
            }break;
            
            case DW_OP_const2u:{
                u16 value = *(u16 *)(data + offset);
                offset += sizeof(u16);
                print(" %x", value);
            }break;
            case DW_OP_const2s:{
                s16 value = *(s16 *)(data + offset);
                offset += sizeof(s16);
                if(value >= 0){
                    print(" %x", value);
                }else{
                    print(" -%x", -value);
                }
            }break;
            case DW_OP_const4u:{
                u32 value = *(u32 *)(data + offset);
                offset += sizeof(u32);
                print(" %x", value);
            }break;
            case DW_OP_const4s:{
                s32 value = *(s32 *)(data + offset);
                offset += sizeof(s32);
                if(value >= 0){
                    print(" %x", value);
                }else{
                    print(" -%x", -value);
                }
            }break;
            
            case DW_OP_const8u:{
                u64 value = *(u64 *)(data + offset);
                offset += sizeof(u64);
                print(" %llX", value);
            }break;
            case DW_OP_const8s:{
                s64 value = *(s64 *)(data + offset);
                offset += sizeof(s64);
                if(value >= 0){
                    print(" %llX", value);
                }else{
                    print(" -%llX", -value);
                }
            }break;
            
            case DW_OP_constu:
            case DW_OP_plus_uconst:
            case DW_OP_piece:
            case DW_OP_pick:
            case DW_OP_addrx:
            case DW_OP_constx:
            case DW_OP_convert:
            case DW_OP_reinterpret:{
                u64 value = read_uleb(data, &offset);
                print(" %llx\n", value);
            }break;
            
            case DW_OP_bra:
            case DW_OP_skip:{
                
                u16 raw = *(u16 *)(data + offset);
                offset += 2;
                
                print(" %x", raw);
            }break;
            
            case DW_OP_regx:{
                u64 value = read_uleb(data, &offset);
                print(" r%lld\n", value);
            }break;
            
            case DW_OP_bregx:{
                u64 reg = read_uleb(data, &offset);
                s64 off = read_sleb(data, &offset);
                
                print("r%llu + %llx\n", reg, off);
            }break;
            
            case DW_OP_deref_size:
            case DW_OP_xderef_size:{
                u8 value = data[offset++];
                print("%x", value);
            }break;
            
            case DW_OP_call_frame_cfa: break;
            
            case DW_OP_fbreg:{
                s64 value = read_sleb(data, &offset);
                if(value > 0){
                    print("+ %llx", value);
                }else{
                    print("- %llx", -value);
                }
            }break;
            
            default:{
                print("Unhandled dwarf opcode %x", opcode);
            }break;
        }
    }
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
        
        if(section_header->type != /*SHT_NOBITS*/8){
            if(section_offset > file.size || section_end > file.size){
                print("Error: Invalid section bounds for section %s.\n", section_name);
                return 1;
            }
        }
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
            
            print("index sect      value             size       info=     type       bind    visibility    name_offset\n");
            
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
                    dump_dwarf_cfa(section_data + offset, length - (offset - root_offset));
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
                    dump_dwarf_cfa(section_data + offset, length - (offset - root_offset));
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
        
        static char *attribute_form_codes[] = {
            [0x01] = "DW_FORM_addr",
            [0x03] = "DW_FORM_block2",
            [0x04] = "DW_FORM_block4",
            [0x05] = "DW_FORM_data2",
            [0x06] = "DW_FORM_data4",
            [0x07] = "DW_FORM_data8",
            [0x08] = "DW_FORM_string",
            [0x09] = "DW_FORM_block",
            [0x0a] = "DW_FORM_block1",
            [0x0b] = "DW_FORM_data1",
            [0x0c] = "DW_FORM_flag",
            [0x0d] = "DW_FORM_sdata",
            [0x0e] = "DW_FORM_strp",
            [0x0f] = "DW_FORM_udata",
            [0x10] = "DW_FORM_ref_addr",
            [0x11] = "DW_FORM_ref1",
            [0x12] = "DW_FORM_ref2",
            [0x13] = "DW_FORM_ref4",
            [0x14] = "DW_FORM_ref8",
            [0x15] = "DW_FORM_ref_udata",
            [0x16] = "DW_FORM_indirect",
            [0x17] = "DW_FORM_sec_offset",
            [0x18] = "DW_FORM_exprloc",
            [0x19] = "DW_FORM_flag_present",
            [0x1a] = "DW_FORM_strx",
            [0x1b] = "DW_FORM_addrx",
            [0x1c] = "DW_FORM_ref_sup4",
            [0x1d] = "DW_FORM_strp_sup",
            [0x1e] = "DW_FORM_data16",
            [0x1f] = "DW_FORM_line_strp",
            [0x20] = "DW_FORM_ref_sig8",
            [0x21] = "DW_FORM_implicit_const",
            [0x22] = "DW_FORM_loclistx",
            [0x23] = "DW_FORM_rnglistx",
            [0x24] = "DW_FORM_ref_sup8",
            [0x25] = "DW_FORM_strx1",
            [0x26] = "DW_FORM_strx2",
            [0x27] = "DW_FORM_strx3",
            [0x28] = "DW_FORM_strx4",
            [0x29] = "DW_FORM_addrx1",
            [0x2a] = "DW_FORM_addrx2",
            [0x2b] = "DW_FORM_addrx3",
            [0x2c] = "DW_FORM_addrx4",
        };
        
        static char *dwarf_tag_string[] = {
            [0x01] = "DW_TAG_array_type",
            [0x02] = "DW_TAG_class_type",
            [0x03] = "DW_TAG_entry_point",
            [0x04] = "DW_TAG_enumeration_type",
            [0x05] = "DW_TAG_formal_parameter",
            [0x08] = "DW_TAG_imported_declaration",
            [0x0a] = "DW_TAG_label",
            [0x0b] = "DW_TAG_lexical_block",
            [0x0d] = "DW_TAG_member",
            [0x0f] = "DW_TAG_pointer_type",
            [0x10] = "DW_TAG_reference_type",
            [0x11] = "DW_TAG_compile_unit",
            [0x12] = "DW_TAG_string_type",
            [0x13] = "DW_TAG_structure_type",
            [0x15] = "DW_TAG_subroutine_type",
            [0x16] = "DW_TAG_typedef",
            [0x17] = "DW_TAG_union_type",
            [0x18] = "DW_TAG_unspecified_parameters",
            [0x19] = "DW_TAG_variant",
            [0x1a] = "DW_TAG_common_block",
            [0x1b] = "DW_TAG_common_inclusion",
            [0x1c] = "DW_TAG_inheritance",
            [0x1d] = "DW_TAG_inlined_subroutine",
            [0x1e] = "DW_TAG_module",
            [0x1f] = "DW_TAG_ptr_to_member_type",
            [0x20] = "DW_TAG_set_type",
            [0x21] = "DW_TAG_subrange_type",
            [0x22] = "DW_TAG_with_stmt",
            [0x23] = "DW_TAG_access_declaration",
            [0x24] = "DW_TAG_base_type",
            [0x25] = "DW_TAG_catch_block",
            [0x26] = "DW_TAG_const_type",
            [0x27] = "DW_TAG_constant",
            [0x28] = "DW_TAG_enumerator",
            [0x29] = "DW_TAG_file_type",
            [0x2a] = "DW_TAG_friend",
            [0x2b] = "DW_TAG_namelist",
            [0x2c] = "DW_TAG_namelist_item",
            [0x2d] = "DW_TAG_packed_type",
            [0x2e] = "DW_TAG_subprogram",
            [0x2f] = "DW_TAG_template_type_parameter",
            [0x30] = "DW_TAG_template_value_parameter",
            [0x31] = "DW_TAG_thrown_type",
            [0x32] = "DW_TAG_try_block",
            [0x33] = "DW_TAG_variant_part",
            [0x34] = "DW_TAG_variable",
            [0x35] = "DW_TAG_volatile_type",
            
            [0x36] = "DW_TAG_dwarf_procedure",
            [0x37] = "DW_TAG_restrict_type",
            [0x38] = "DW_TAG_interface_type",
            [0x39] = "DW_TAG_namespace",
            [0x3a] = "DW_TAG_imported_module",
            [0x3b] = "DW_TAG_unspecified_type",
            [0x3c] = "DW_TAG_partial_unit",
            [0x3d] = "DW_TAG_imported_unit",
            [0x3f] = "DW_TAG_condition",
            [0x40] = "DW_TAG_shared_type",
            
            [0x41] = "DW_TAG_type_unit",
            [0x42] = "DW_TAG_rvalue_reference_type",
            [0x43] = "DW_TAG_template_alias",
            
            [0x44] = "DW_TAG_coarray_type",  /* DWARF5 */
            [0x45] = "DW_TAG_generic_subrange",  /* DWARF5 */
            [0x46] = "DW_TAG_dynamic_type",  /* DWARF5 */
            [0x47] = "DW_TAG_atomic_type",  /* DWARF5 */
            [0x48] = "DW_TAG_call_site",  /* DWARF5 */
            [0x49] = "DW_TAG_call_site_parameter",  /* DWARF5 */
            [0x4a] = "DW_TAG_skeleton_unit",  /* DWARF5 */
            [0x4b] = "DW_TAG_immutable_type",  /* DWARF5 */
            
            
        };
        
        static char *attr_string[] = {
            [0x01] = "DW_AT_sibling", // reference
            [0x02] = "DW_AT_location", // block, loclistptr
            [0x03] = "DW_AT_name", // string
            [0x09] = "DW_AT_ordering", // constant
            [0x0b] = "DW_AT_byte_size", // block, constant, reference
            [0x0c] = "DW_AT_bit_offset", // block, constant, reference
            [0x0d] = "DW_AT_bit_size", // block, constant, reference
            [0x10] = "DW_AT_stmt_list", // lineptr
            [0x11] = "DW_AT_low_pc", // address
            [0x12] = "DW_AT_high_pc", // address
            [0x13] = "DW_AT_language", // constant
            [0x15] = "DW_AT_discr", // reference
            [0x16] = "DW_AT_discr_value", // constant
            [0x17] = "DW_AT_visibility", // constant
            [0x18] = "DW_AT_import", // reference
            [0x19] = "DW_AT_string_length", // block, loclistptr
            [0x1a] = "DW_AT_common_reference", // reference
            [0x1b] = "DW_AT_comp_dir", // string
            [0x1c] = "DW_AT_const_value", // block, constant, string
            [0x1d] = "DW_AT_containing_type", // reference
            [0x1e] = "DW_AT_default_value", // reference
            [0x20] = "DW_AT_inline", // constant
            [0x21] = "DW_AT_is_optional", // flag
            [0x22] = "DW_AT_lower_bound", // block, constant, reference
            [0x25] = "DW_AT_producer", // string
            [0x27] = "DW_AT_prototyped", // flag
            [0x2a] = "DW_AT_return_addr", // block, loclistptr
            [0x2c] = "DW_AT_start_scope", // constant
            [0x2e] = "DW_AT_bit_stride", // constant
            [0x2f] = "DW_AT_upper_bound", // block, constant, reference
            [0x31] = "DW_AT_abstract_origin", // reference
            [0x32] = "DW_AT_accessibility", // constant
            [0x33] = "DW_AT_address_class", // constant
            [0x34] = "DW_AT_artificial", // flag
            [0x35] = "DW_AT_base_types", // reference
            [0x36] = "DW_AT_calling_convention", // constant
            [0x37] = "DW_AT_count", // block, constant, reference
            [0x38] = "DW_AT_data_member_location", // block, constant, loclistptr
            [0x39] = "DW_AT_decl_column", // constant
            [0x3a] = "DW_AT_decl_file", // constant
            [0x3b] = "DW_AT_decl_line", // constant
            [0x3c] = "DW_AT_declaration", // flag
            [0x3d] = "DW_AT_discr_list", // block
            [0x3e] = "DW_AT_encoding", // constant
            [0x3f] = "DW_AT_external", // flag
            [0x40] = "DW_AT_frame_base", // block, loclistptr
            [0x41] = "DW_AT_friend", // reference
            [0x42] = "DW_AT_identifier_case", // constant
            [0x43] = "DW_AT_macro_info", // macptr
            [0x44] = "DW_AT_namelist_item", // block
            [0x45] = "DW_AT_priority", // reference
            [0x46] = "DW_AT_segment", // block, loclistptr
            [0x47] = "DW_AT_specification", // reference
            [0x48] = "DW_AT_static_link", // block, loclistptr
            [0x49] = "DW_AT_type", // reference
            [0x4a] = "DW_AT_use_location", // block, loclistptr
            [0x4b] = "DW_AT_variable_parameter", // flag
            [0x4c] = "DW_AT_virtuality", // constant
            [0x4d] = "DW_AT_vtable_elem_location", // block, loclistptr
	           // Dwarf3
            [0x4e] = "DW_AT_allocated", // block, constant, reference
            [0x4f] = "DW_AT_associated", // block, constant, reference
            [0x50] = "DW_AT_data_location", // block
            [0x51] = "DW_AT_byte_stride", // block, constant, reference
            [0x52] = "DW_AT_entry_pc", // address
            [0x53] = "DW_AT_use_UTF8", // flag
            [0x54] = "DW_AT_extension", // reference
            [0x55] = "DW_AT_ranges", // rangelistptr
            [0x56] = "DW_AT_trampoline", // address, flag, reference, string
            [0x57] = "DW_AT_call_column", // constant
            [0x58] = "DW_AT_call_file", // constant
            [0x59] = "DW_AT_call_line", // constant
            [0x5a] = "DW_AT_description", // string
            [0x5b] = "DW_AT_binary_scale", // constant
            [0x5c] = "DW_AT_decimal_scale", // constant
            [0x5d] = "DW_AT_small", // reference
            [0x5e] = "DW_AT_decimal_sign", // constant
            [0x5f] = "DW_AT_digit_count", // constant
            [0x60] = "DW_AT_picture_string", // string
            [0x61] = "DW_AT_mutable", // flag
            [0x62] = "DW_AT_threads_scaled", // flag
            [0x63] = "DW_AT_explicit", // flag
            [0x64] = "DW_AT_object_pointer", // reference
            [0x65] = "DW_AT_endianity", // constant
            [0x66] = "DW_AT_elemental", // flag
            [0x67] = "DW_AT_pure", // flag
            [0x68] = "DW_AT_recursive", // flag
            
            [0x69] = "DW_AT_signature", // reference
            [0x6a] = "DW_AT_main_subprogram", // flag
            [0x6b] = "DW_AT_data_bit_offset", // constant
            [0x6c] = "DW_AT_const_expr", // flag
            [0x6d] = "DW_AT_enum_class", // flag
            [0x6e] = "DW_AT_linkage_name", // string
            [0x6f] = "DW_AT_string_length_bit_size", //  constant
            [0x70] = "DW_AT_string_length_byte_size", //  constant
            [0x71] = "DW_AT_rank", //  constant, exprloc
            [0x72] = "DW_AT_str_offsets_base", //  stroffsetsptr
            
            [0x73] = "DW_AT_addr_base", // addrptr
            
            [0x73] = "DW_AT_addr_base", // addrptr
            [0x74] = "DW_AT_rnglists_base", // rnglistsptr
            // Reserved 0x75 Unused
            [0x76] = "DW_AT_dwo_name", // string
            [0x77] = "DW_AT_reference", // flag
            [0x78] = "DW_AT_rvalue_reference", // flag
            [0x79] = "DW_AT_macros", // macptr
            [0x7a] = "DW_AT_call_all_calls", // flag
            [0x7b] = "DW_AT_call_all_source_calls", // flag
            [0x7c] = "DW_AT_call_all_tail_calls", // flag
            [0x7d] = "DW_AT_call_return_pc", // address
            [0x7e] = "DW_AT_call_value", // exprloc
            [0x7f] = "DW_AT_call_origin", // exprloc
            [0x80] = "DW_AT_call_parameter", // reference
            [0x81] = "DW_AT_call_pc", // address
            [0x82] = "DW_AT_call_tail_call", // flag
            [0x83] = "DW_AT_call_target", // exprloc
            [0x84] = "DW_AT_call_target_clobbered", // exprloc
            [0x85] = "DW_AT_call_data_location", // exprloc
            [0x86] = "DW_AT_call_data_value", // exprloc
            [0x87] = "DW_AT_noreturn", // flag
            [0x88] = "DW_AT_alignment", // constant
            [0x89] = "DW_AT_export_symbols", // flag
            [0x8a] = "DW_AT_deleted", // flag
            [0x8b] = "DW_AT_defaulted", // constant
            [0x8c] = "DW_AT_loclists_base", // loclistsptr
        };
        
        
        // .debug_info
        if(string_match(section_name, string(".debug_info"))){
            
            struct elf_section_header *debug_abbrev = 0;
            struct elf_section_header *debug_str = 0;
            struct elf_section_header *debug_line_str = 0;
            struct elf_section_header *debug_str_offsets = 0;
            struct elf_section_header *debug_addr = 0;
            
            for(u64 sho = section_header_table_offset; sho < section_header_table_end; sho += section_header_table_entry_size){
                struct elf_section_header *sh = (void *)(file.data + sho);
                
                struct string sn = string_from_cstring((char *)section_name_string_table + sh->name_offset);
                if(sn.size == 0) continue;
                
                if(string_match(sn, string(".debug_abbrev"))){
                    debug_abbrev = sh;
                }
                
                if(string_match(sn, string(".debug_str"))){
                    debug_str = sh;
                }
                
                if(string_match(sn, string(".debug_line_str"))){
                    debug_line_str = sh;
                }
                
                if(string_match(sn, string(".debug_str_offsets"))){
                    debug_str_offsets = sh;
                }
                
                if(string_match(sn, string(".debug_addr"))){
                    debug_addr = sh;
                }
            }
            
            u8 *abbrev = file.data + debug_abbrev->offset;
            u64 abbrev_size = debug_abbrev->size;
            
            u8 *debug_string_table   = file.data + (debug_str ? debug_str->offset : 0);
            u8 *debug_line_string_table   = file.data + (debug_line_str ? debug_line_str->offset : 0);
            u32 *debug_string_offsets = (u32 *)(file.data + (debug_str_offsets ? debug_str_offsets->offset : 0) + 8);
            u64 *debug_address_table  = (u64 *)(file.data + (debug_addr ? debug_addr->offset : 0) + 8); 
            
            // u64 debug_string_table_size = debug_str->size;
            
            u64 offset = 0;
            
            while(offset < section_size){
                
                print("compilation unit at %x\n", offset);
                
                u32 compilation_unit_length = *(u32 *)(section_data + offset);
                offset += 4;
                
                u64 unit_end = offset + compilation_unit_length;
                
                u16 version = *(u16 *)(section_data + offset);
                offset += 2;
                
                u8 unit_type = section_data[offset++];
                u8 address_size = section_data[offset++];
                
                u32 abbrev_base = *(u32 *)(section_data + offset);
                offset += 4;
                
                print("   length %x\n", compilation_unit_length);
                print("   version %u\n", version);
                print("   unit_type %u\n", unit_type);
                print("   address_size %u\n", address_size);
                print("   abbrev_offset %x\n", abbrev_base);
                
                int depth = 0;
                
                while(offset < unit_end){
                    u64 root_offset = offset;
                    u64 abbrev_code = read_uleb(section_data, &offset);
                    
                    if(abbrev_code == 0){
                        print("%*sAbbrevCode == 0 (%x)\n", depth, "", offset);
                        if(depth == 0) break;
                        
                        depth -= 4;
                        continue;
                    }
                    
                    // Search for the abbrev with the abbrev_code
                    u64 abbrev_entry_offset = 0;
                    u64 root_abbrev_entry_offset = 0;
                    
                    for(u64 abbrev_offset = abbrev_base; abbrev_offset < abbrev_size; ){
                        root_abbrev_entry_offset = abbrev_offset;
                        u64 code = read_uleb(abbrev, &abbrev_offset);
                        
                        if(code == 0) break;
                        if(code == abbrev_code){
                            abbrev_entry_offset = abbrev_offset;
                            break;
                        }
                        
                        read_uleb(abbrev, &abbrev_offset); // tag
                        abbrev_offset++; // have_children
                        
                        while(true){
                            u64 attr = read_uleb(abbrev, &abbrev_offset);
                            u64 form = read_uleb(abbrev, &abbrev_offset);
                            if(attr == 0 && form == 0) break;
                            
                            if(form == /*DW_FORM_implicit_const*/0x21){
                                read_sleb(abbrev, &abbrev_offset);
                            }
                        }
                    }
                    
                    u64 tag = read_uleb(abbrev, &abbrev_entry_offset);
                    u8  have_children = abbrev[abbrev_entry_offset++];
                    print("%*s[%llx] -> [%llx (%llx)] %s [%s]\n", depth, "", root_offset, abbrev_code, root_abbrev_entry_offset, get(dwarf_tag_string, tag), have_children ? "has children" : "no children");
                    
                    depth += 2;
                    while(1){
                        u64 attr = read_uleb(abbrev, &abbrev_entry_offset);
                        u64 form = read_uleb(abbrev, &abbrev_entry_offset);
                        if(attr == 0 && form == 0) break;
                        
                        print("%*s%s(%s): ", depth, "", get(attr_string, attr), get(attribute_form_codes, form));
                        
                        int unsupported = 0;
                        
                        switch(form){
                            
                            case /*DW_FORM_addr*/0x01:{
                                u64 address = *(u64 *)(section_data + offset);
                                offset += 8;
                                print("%p\n", address);
                            }break;
                            
                            case /*DW_FORM_block2*/0x03:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_block4*/0x04:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_data2*/0x05:{
                                u16 data = *(u16 *)(section_data + offset);
                                offset += 2;
                                print("0x%x\n", data);
                            }break;
                            case /*DW_FORM_data4*/0x06:{
                                u32 data = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("0x%x\n", data);
                            }break;
                            case /*DW_FORM_data8*/0x07:{
                                u64 data = *(u64 *)(section_data + offset);
                                offset += 8;
                                print("0x%llx\n", data);
                            }break;
                            case /*DW_FORM_string*/0x08:{
                                char *string = (char *)(section_data + offset);
                                offset += cstring_length(string) + 1;
                                print("%s\n", string);
                            }break;
                            case /*DW_FORM_block*/0x09:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_block1*/0x0a:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_data1*/0x0b:{
                                u8 data = section_data[offset++];
                                print("%x\n", data);
                            }break;
                            case /*DW_FORM_flag*/0x0c:{
                                u8 data = section_data[offset++];
                                print("%x\n", data);
                            }break;
                            case /*DW_FORM_sdata*/0x0d:{
                                s64 value = read_sleb(section_data, &offset);
                                print("%lld (%llx)\n", value, value);
                            }break;
                            case /*DW_FORM_strp*/0x0e:{
                                u32 string_offset = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("%x (%s)\n", string_offset, debug_string_table + string_offset);
                            }break;
                            
                            case /*DW_FORM_udata*/0x0f:{
                                u64 value = read_uleb(section_data, &offset);
                                print("%llx\n", value);
                            }break;
                            case /*DW_FORM_ref_addr*/0x10:{
                                u64 address = *(u64 *)(section_data + offset);
                                offset += 8;
                                print("<%llx>\n", address);
                            }break;
                            case /*DW_FORM_ref1*/0x11:{
                                u8 ref = *(u8 *)(section_data + offset);
                                offset += 1;
                                print("<%x>\n", ref);
                            }break;
                            case /*DW_FORM_ref2*/0x12:{
                                u16 ref = *(u16 *)(section_data + offset);
                                offset += 2;
                                print("<%x>\n", ref);
                            }break;
                            case /*DW_FORM_ref4*/0x13:{
                                u32 ref = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("<%x>\n", ref);
                            }break;
                            case /*DW_FORM_ref8*/0x14:{
                                u64 ref = *(u64 *)(section_data + offset);
                                offset += 8;
                                print("<%llx>\n", ref);
                            }break;
                            case /*DW_FORM_ref_udata*/0x15:{
                                u64 value = read_uleb(section_data, &offset);
                                print("<%llx>\n", value);
                            }break;
                            case /*DW_FORM_indirect*/0x16:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_sec_offset*/0x17:{
                                u32 sec_offset = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("%x\n", sec_offset);
                            }break;
                            case /*DW_FORM_exprloc*/0x18:{
                                u64 length = read_uleb(section_data, &offset);
                                print("expr(%llx): ", length);
                                
                                dump_dwarf_expression(section_data + offset, length);
                                
                                print("\n");
                                offset += length;
                            }break;
                            case /*DW_FORM_flag_present*/0x19:{
                                print("true\n");
                            }break;
                            case /*DW_FORM_strx*/0x1a:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_addrx*/0x1b:{
                                u64 value = read_uleb(section_data, &offset);
                                print("%llx (%p)\n", value, debug_address_table[value]);
                            }break;
                            case /*DW_FORM_ref_sup4*/0x1c:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_strp_sup*/0x1d:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_data16*/0x1e:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_line_strp*/0x1f:{
                                u32 string_offset = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("%x (%s)\n", string_offset, debug_line_string_table + string_offset);
                            }break;
                            case /*DW_FORM_ref_sig8*/0x20:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_implicit_const*/0x21:{
                                s64 value = read_sleb(abbrev, &abbrev_entry_offset);
                                
                                // "No value is stored in the .debug_info section."
                                print("%lld\n", value);
                            }break;
                            case /*DW_FORM_loclistx*/0x22:{
                                u64 value = read_uleb(section_data, &offset);
                                print(".debug_loclists[%llx]\n", value);
                            }break;
                            case /*DW_FORM_rnglistx*/0x23:{
                                u64 value = read_uleb(section_data, &offset);
                                print(".debug_rnglists[%llx]\n", value);
                            }break;
                            case /*DW_FORM_ref_sup8*/0x24:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_strx1*/0x25:{
                                u8 string_offset = section_data[offset++];
                                print("%x (%s)\n", string_offset, debug_string_table + debug_string_offsets[string_offset]);
                            }break;
                            case /*DW_FORM_strx2*/0x26:{
                                u16 string_offset = *(u16 *)(section_data + offset);
                                offset += 2;
                                print("%x (%s)\n", string_offset, debug_string_table + debug_string_offsets[string_offset]);
                            }break;
                            case /*DW_FORM_strx3*/0x27:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_strx4*/0x28:{
                                u32 string_offset = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("%x (%s)\n", string_offset, debug_string_table + debug_string_offsets[string_offset]);
                            }break;
                            case /*DW_FORM_addrx1*/0x29:{
                                u8 index = *(u8 *)(section_data + offset);
                                offset += 1;
                                print("%x (%p)\n", index, debug_address_table[index]);
                            }break;
                            case /*DW_FORM_addrx2*/0x2a:{
                                u16 index = *(u16 *)(section_data + offset);
                                offset += 2;
                                print("%x (%p)\n", index, debug_address_table[index]);
                            }break;
                            case /*DW_FORM_addrx3*/0x2b:{
                                unsupported = 1;
                            }break;
                            case /*DW_FORM_addrx4*/0x2c:{
                                u32 index = *(u32 *)(section_data + offset);
                                offset += 4;
                                print("%x (%p)\n", index, debug_address_table[index]);
                            }break;
                            default:{
                                unsupported = 1;
                            }break;
                        }
                        
                        if(unsupported){
                            print("Unsupported form!!!\n");
                            goto double_break;
                        }
                    }
                    depth -= 2;
                    
                    if(have_children) depth += 4;
                }
            }
            double_break:;
        }
        
        // .debug_abbrev
        if(string_match(section_name, string(".debug_abbrev"))){
            
            u64 offset = 0;
            while(offset < section_size){
                print("Table @ 0x%llx:\n", offset);
                
                while(true){
                    u64 root_offset = offset;
                    u64 code = read_uleb(section_data, &offset);
                    if(code == 0){
                        print("    [0] - end of abbreviation table\n");
                        break;
                    }
                    
                    u64 tag = read_uleb(section_data, &offset);
                    
                    u8 have_children = section_data[offset++];
                    
                    print("    [%x (%llx)] %x (%s) children = %u\n", code, root_offset, tag, get(dwarf_tag_string, tag), have_children);
                    
                    while(true){
                        u64 attr = read_uleb(section_data, &offset);
                        u64 form = read_uleb(section_data, &offset);
                        
                        if(attr == 0 && form == 0){
                            print("        attr = 0, form = 0, end of table\n");
                            break;
                        }
                        
                        print("        attr = 0x%x (%s), form = 0x%x (%s)", attr, get(attr_string, attr), form, get(attribute_form_codes, form));
                        
                        if(form == /*DW_FORM_implicit_const*/0x21){
                            s64 value = read_sleb(section_data, &offset);
                            print(" -> value = %lld\n", value);
                        }else{
                            print("\n");
                        }
                    }
                }
            }
        }
        
        // .debug_line
        if(string_match(section_name, string(".debug_line"))){
            
            // for whatever reason, there is no associated section, 
            // so search for the .debug_line_str section manually
            
            struct elf_section_header *debug_line_str = null;
            u8 *section_end = section_data + section_size;
            
            for(u64 sho = section_header_table_offset; sho < section_header_table_end; sho += section_header_table_entry_size){
                struct elf_section_header *sh = (void *)(file.data + sho);
                
                struct string sn = string_from_cstring((char *)section_name_string_table + sh->name_offset);
                if(sn.size == 0) continue;
                
                if(string_match(sn, string(".debug_line_str"))){
                    debug_line_str = sh;
                    break;
                }
            }
            
            u8 *debug_line_string_table = file.data + debug_line_str->offset;
            
            u32 initial_length = *(u32 *)section_data;
            section_data += 4;
            
            u32 offset_size;
            u64 unit_length;
            
            print("initial_length = %x\n", initial_length);
            
            if(initial_length == 0xffffffff){
                offset_size = 8;
                unit_length = *(u64 *)section_data;
            }else{
                offset_size = 4;
                unit_length = initial_length;
            }
            
            u16 version = *(u16 *)section_data;
            section_data += 2;
            
            print("unit_length = %x\n", unit_length);
            print("offset_size = %x\n", offset_size);
            print("version %u\n", version);
            
            if(version >= 5){
                u8 address_size = *section_data++;
                u8 segment_selector_size = *section_data++;
                
                print("address size %x\n", address_size);
                print("segment selector size %x\n", segment_selector_size);
            }
            
            u32 header_length = *(u32 *)section_data;
            section_data += 4;
            
            u8 minimum_instruction_length = *section_data++;
            u8 maximum_operation_per_instruction = *section_data++;
            u8 default_is_statement = *section_data++;
            s8 line_base = *section_data++;
            u8 line_range = *section_data++;
            u8 opcode_base = *section_data++;
            
            print("header length %x\n", header_length);
            print("minimum instruction length %x\n", minimum_instruction_length);
            print("maximum instruction per instruction %x\n", maximum_operation_per_instruction);
            print("default is statement %x\n", default_is_statement);
            print("line base %d\n", line_base);
            print("line range %x\n", line_range);
            print("opcode base %x\n", opcode_base);
            
            for(u32 index = 1; index < opcode_base; index++){
                print("  [%u] %x\n", index, *section_data++);
            }
            
            static char *content_type_code[] = {
                [1] = "DW_LNCT_path",
                "DW_LNCT_directory_index",
                "DW_LNCT_timestamp",
                "DW_LNCT_size",
                "DW_LNCT_MD5",
            };
            
            
            u8 directory_entry_format_count = *section_data++;
            print("directory entry format count %x\n", directory_entry_format_count);
            for(u8 index = 0; index < directory_entry_format_count; index++){
                u64 offset = 0;
                u64 a = read_uleb(section_data, &offset);
                u64 b = read_uleb(section_data, &offset);
                section_data += offset;
                
                print("  [%u] %x (%s) %x (%s)\n", index, a, content_type_code[a], b, attribute_form_codes[b]);
            }
            
            u64 uff_offset = 0;
            u64 number_of_directories = read_uleb(section_data, &uff_offset);
            section_data += uff_offset;
            
            print("Number of directories %u\n", number_of_directories);
            // u32 *directory_offsets = (u32 *)section_data;
            
            for(u32 index = 0; index < number_of_directories; index++){
                u32 offset = *(u32 *)section_data;
                section_data += 4;
                print("  [%u] %x %s\n", index, offset, debug_line_string_table + offset);
            }
            
            u8 file_name_entry_format_count = *section_data++;
            print("File name entry format count %u\n", file_name_entry_format_count);
            
            for(u32 index = 0; index < file_name_entry_format_count; index++){
                u64 offset = 0;
                u64 a = read_uleb(section_data, &offset);
                u64 b = read_uleb(section_data, &offset);
                section_data += offset;
                
                print("  [%u] %x (%s) %x (%s)\n", index, a, content_type_code[a], b, attribute_form_codes[b]);
            }
            
            uff_offset = 0;
            u64 number_of_files = read_uleb(section_data, &uff_offset);
            section_data += uff_offset;
            
            print("Number of files %u\n", number_of_files);
            u8 *file_table = section_data;
            
            for(u32 index = 0; index < number_of_files; index++){
                u32 offset = *(u32 *)section_data;
                section_data += 4;
                
                u8 value = *section_data++;
                print("  [%u] %x %s -> %x\n", index, offset, debug_line_string_table + offset, value);
            }
            
            // 
            // Dump the line program.
            // 
            
            u64 address = 0;
            u64 file_number = 1;
            u64 line = 1;
            u64 column = 0;
            u64 isa = 0;
            u64 discriminator = 0;
            
            int is_statement = default_is_statement;
            int is_basic_block = 0;
            int prologue_end = 0;
            int epilogue_begin = 0;
            int end_sequence = 0;
            
            struct row{
                u64 address;
                u64 file_number;
                u64 line;
                u64 column;
                u64 isa;
                u64 discriminator;
                
                int is_statement;
                int is_basic_block;
                int prologue_end;
                int epilogue_begin;
                int end_sequence;
            } *rows = push_data(arena, struct row, 0);
            
#define push_row()                                            \
struct row *row = push_struct(arena, struct row); \
row->address = address;                                       \
row->file_number = file_number;                               \
row->line = line;                                             \
row->column = column;                                         \
row->isa = isa;                                               \
row->discriminator = discriminator;                           \
row->is_statement = is_statement;                     \
row->is_basic_block = is_basic_block;                         \
row->prologue_end = prologue_end;                             \
row->epilogue_begin = epilogue_begin;                         \
row->end_sequence = end_sequence;                             \
            
            
            int row_index = 0;
            
            while(section_data < section_end){
                u8 opcode = *section_data++;
                
                if(row_index++ == 0x100) break;
                
                if(opcode == 0){
                    // Extended opcode
                    uff_offset = 0;
                    u64 opcode_length = read_uleb(section_data, &uff_offset);
                    section_data += uff_offset;
                    
                    print("Extended(%llx): ", opcode_length);
                    
                    u8 sub_opcode = *section_data++;
                    
                    switch(sub_opcode){
                        case /*DW_LNE_end_sequence*/1:{
                            print("DW_LNE_end_sequence\n");
                            end_sequence = 1;
                            push_row();
                            
                            address  = 0;
                            column = 0;
                            isa = 0;
                            discriminator = 0;
                            is_statement = 0;
                            is_basic_block = 0;
                            prologue_end = 0;
                            epilogue_begin = 0;
                            end_sequence = 0;
                            
                            file_number = 1;
                            line = 1;
                            is_statement = default_is_statement;
                        }break;
                        
                        case /*DW_LNE_set_address*/2:{
                            address = *(u32 *)section_data;
                            print("DW_LNE_set_address %x\n", address);
                        }break;
                        
                        // case /*DW_LNE_define_file*/3:{}break;
                        
                        case /*DW_LNE_set_discriminator*/4:{
                            uff_offset = 0;
                            discriminator = read_uleb(section_data, &uff_offset);
                            section_data += uff_offset;
                        }break;
                        
                        default:{
                            print("    extended opcode %u unhandled\n", sub_opcode);
                            os_panic(1);
                        }break;
                    }
                    
                    section_data += opcode_length-1;
                    continue;
                }
                
                if(opcode < opcode_base){
                    switch(opcode){
                        case /*DW_LNS_copy*/1:{
                            print("DW_LNS_copy\n");
                            
                            push_row();
                            
                            is_basic_block = false;
                            prologue_end = false;
                            epilogue_begin = false;
                            discriminator = 0;
                        }break;
                        
                        case /*DW_LNS_advance_pc*/2:{
                            uff_offset = 0;
                            u64 advance = read_uleb(section_data, &uff_offset);
                            section_data += uff_offset;
                            
                            print("DW_LNS_advance_pc %x\n", advance);
                            
                            address += advance * minimum_instruction_length;
                        }break;
                        
                        case /*DW_LNS_advance_line*/3:{
                            uff_offset = 0;
                            s64 advance = read_sleb(section_data, &uff_offset);
                            section_data += uff_offset;
                            line += advance;
                            
                            print("DW_LNS_advance_line %x\n", advance);
                        }break;
                        
                        case /*DW_LNS_set_file*/4:{
                            uff_offset = 0;
                            file_number = read_uleb(section_data, &uff_offset);
                            section_data += uff_offset;
                            
                            print("DW_LNS_set_file %x\n", file_number);
                        }break;
                        
                        case /*DW_LNS_set_column*/5:{
                            uff_offset = 0;
                            column = read_uleb(section_data, &uff_offset);
                            section_data += uff_offset;
                            
                            print("DW_LNS_set_column %x\n", column);
                        }break;
                        
                        case /*DW_LNS_negate_stmt*/6:{
                            is_statement = !is_statement;
                            print("DW_LNS_negate_stmt\n");
                        }break;
                        
                        case /*DW_LNS_set_basic_block*/7:{
                            is_basic_block = 1;
                            print("DW_LNS_set_basic_block\n");
                        }break;
                        
                        case /*DW_LNS_const_add_pc*/8:{
                            u32 adjusted = 255 - opcode_base;
                            u32 operation_advance = adjusted / line_range;
                            address += operation_advance * minimum_instruction_length;
                            
                            print("DW_LNS_const_add_pc\n");
                        }break;
                        
                        case /*DW_LNS_fixed_advance_pc*/9:{
                            u16 advance = *(u16 *)section_data;
                            address += advance;
                            section_data += 2;
                            
                            print("DW_LNS_fixed_advance_pc %x\n", advance);
                        }break;
                        
                        case /*DW_LNS_set_prologue_end*/10:{
                            print("DW_LNS_set_prologue_end\n");
                            prologue_end = 1;
                        }break;
                        
                        case /*DW_LNS_set_epilogue_begin*/11:{
                            print("DW_LNS_set_epilogue_begin\n");
                            epilogue_begin = 1;
                        }break;
                        
                        case /*DW_LNS_set_isa*/12:{
                            uff_offset = 0;
                            isa = read_uleb(section_data, &uff_offset);
                            section_data += uff_offset;
                            print("DW_LNS_set_isa %x\n", isa);
                        }break;
                        
                        default:{
                            print("    opcode %u unhandled\n", opcode);
                            os_panic(1);
                        }break;
                    }
                    
                    continue;
                }
                
                // Special opcode?
                
                u32 adjusted_opcode   = opcode - opcode_base;
                u32 operation_advance = adjusted_opcode / line_range;
                
                int line_increment = line_base + (adjusted_opcode % line_range);
                
                address += operation_advance * minimum_instruction_length;
                line    += line_increment;
                
                print("Special Opcode %x (address += 0x%x, line += %d)\n", opcode, operation_advance * minimum_instruction_length, line_increment);
                
                push_row();
                
                is_basic_block    = 0;
                prologue_end   = 0;
                epilogue_begin = 0;
                discriminator  = 0;
            }
            
            u64 amount_of_rows = push_data(arena, struct row, 0) - rows;
            
            print("\nExtracted Rows:\n");
            for(u64 index = 0; index < amount_of_rows; index++){
                struct row *row = &rows[index];
                
                print("    %p: %s(%u,%u)\n", row->address, debug_line_string_table + *(u32 *)(file_table + 5 * row->file_number), row->line, row->column);
            }
        }
        
        // .debug_addr
        if(string_match(section_name, string(".debug_addr"))){
            u32 length  = *(u32 *)(section_data + 0);
            u16 version = *(u16 *)(section_data + 4);
            u8 address_size = section_data[6];
            u8 segment_size = section_data[7];
            
            print("length %x\n", length);
            print("version %u\n", version);
            print("address_size %u\n", address_size);
            print("segment_size %u\n", segment_size);
            
            for(u64 offset = 8, index = 0; offset < section_size; offset += 8, index++){
                u64 address = *(u64 *)(section_data + offset);
                print("    [%x] = %llx\n", index, address);
            }
        }
        
        if(string_match(section_name, string(".debug_line_str")) || string_match(section_name, string(".debug_str"))){
            for(u64 offset = 0; offset < section_size;){
                char *string = (char *)(section_data + offset);
                print("    0x%x -> %s\n", offset, string);
                offset += cstring_length(string) + 1;
            }
        }
        
        // .debug_str_offsets
        if(string_match(section_name, string(".debug_str_offsets"))){
            
            struct elf_section_header *debug_str = null;
            
            for(u64 sho = section_header_table_offset; sho < section_header_table_end; sho += section_header_table_entry_size){
                struct elf_section_header *sh = (void *)(file.data + sho);
                
                struct string sn = string_from_cstring((char *)section_name_string_table + sh->name_offset);
                if(sn.size == 0) continue;
                
                if(string_match(sn, string(".debug_str"))){
                    debug_str = sh;
                    break;
                }
            }
            
            u8 *debug_string_table = file.data + debug_str->offset;
            
            u32 length = *(u32 *)(section_data + 0);
            u16 version = *(u16 *)(section_data + 4);
            u16 padding = *(u16 *)(section_data + 6);
            
            print("Length %x\n", length);
            print("version %u\n", version);
            print("padding %u\n", padding);
            
            for(u64 offset = 8, index = 0; offset < section_size; offset += 4, index++){
                u32 string_offset = *(u32 *)(section_data + offset);
                print("    [%x] -> %x (%s)\n", index, string_offset, debug_string_table + string_offset);
            }
        }
        
        print("\n");
    }
    
    return 0;
}
