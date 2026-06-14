
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

struct elf_symbol *elf_lookup_symbol(struct library_node *library_node, struct string identifier){
    
    struct so_library_node *so = &library_node->shared_object;
    
    u64 hash = 0;
    for(smm index = 0; index < identifier.size; index++){
        hash = (hash << 4) + identifier.data[index];
        u64 top_nibble = hash & 0xf0000000;
        if(top_nibble){
            hash ^= (top_nibble >> 24);
            hash &= ~top_nibble;
        }
    }
    
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
