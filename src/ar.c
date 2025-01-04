
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
int ar_parse_file(struct string file_name, struct memory_arena *arena){
    
    struct os_file file = load_file_into_arena((char *)file_name.data, arena);
    
    if(file.file_does_not_exist){
        print("Error: Library '%s' does not exist.\n", (char *)file_name.data);
        return 1;
    }
    
    if(file.size < 8 || memcmp(file.data, "!<arch>\n", 8) != 0){
        print("Error: Library '%s' is not an archive file (.lib, .a, .ar).\n", (char *)file_name.data);
        return 1;
    }
    
    u8 *file_at  = file.data + 8;
    u8 *file_end = file.data + file.size;
    
    if(file_at == file_end){
        print("Warning: Library '%s' is empty.\n", (char *)file_name.data);
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
            print("Error: Failed to parse library '%s'.\n", (char *)file_name.data);
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
        print("Error: Failed to parse library '%s'.\n", (char *)file_name.data);
        return 1;
    }
    
    u32 *symbol_index_at  = (u32 *)little_endian_symbol_index_base;
    u8  *symbol_index_end = (u8 *)(little_endian_symbol_index_base + little_endian_symbol_index_size);
    
    u32 amount_of_members = *symbol_index_at++;
    if(symbol_index_at + amount_of_members + 1 > (u32 *)symbol_index_end){
        print("Error: Failed to parse library '%s'.\n", (char *)file_name.data);
        return 1;
    }
    
    u32 *member_offsets = symbol_index_at;
    symbol_index_at += amount_of_members;
    
    u32 amount_of_symbols = *symbol_index_at++;
    if(amount_of_symbols == 0){
        print("Warning: Library '%s' does not define any symbols.\n", (char *)file_name.data);
        return 0;
    }
    
    u16 *symbol_member_indices = (u16 *)symbol_index_at;
    
    if(symbol_member_indices + amount_of_symbols >= (u16 *)symbol_index_end || symbol_index_end[-1] != 0){ // @note: Equality so the string buffer is not empty.
        print("Error: Failed to parse library '%s'.\n", (char *)file_name.data);
        return 1;
    }
    
    u8 *string_buffer = (u8 *)(symbol_member_indices + amount_of_symbols);
    
    struct library_import_table_node *string_table = push_data(arena, struct library_import_table_node, 0);
    u64 amount_of_strings = 0;
    
    for(u8 *it = string_buffer; it < (u8 *)symbol_index_end;){
        struct string string = cstring_to_string((char *)it);
        it += string.size + 1;
        
        push_struct(arena, struct library_import_table_node)->string = string;
        
        amount_of_strings++;
    }
    
    if(amount_of_strings != (u64)amount_of_symbols){
        print("Error: Failed to parse library '%s'.\n", (char *)file_name.data);
        return 1;
    }
    
    u64 amount_of_import_symbols = push_data(arena, struct library_import_table_node, 0) - string_table;
    if(amount_of_import_symbols == 0){
        print("Warning: Library '%s' does not export any symbols.\n", (char *)file_name.data);
        return 0;
    }
    
    struct library_node *library_node = push_struct(arena, struct library_node);
    library_node->path = file_name;
    library_node->file = file;
    library_node->amount_of_members     = amount_of_members;
    library_node->member_offsets        = member_offsets;
    library_node->amount_of_symbols     = amount_of_symbols;
    library_node->symbol_member_indices = symbol_member_indices;
    library_node->import_symbol_string_table = string_table;
    library_node->amount_of_import_symbols   = amount_of_import_symbols;
    
    // @cleanup: In the future, we could thread this part and then this would need to be atomic.
    //           This will also be true for '#pragma comment(lib, <...>)'.
    sll_push_back(globals.libraries, library_node);
    globals.libraries.amount += 1;
    return 0;
}

// returns '1', if found, else '0'.
struct ar_symbol_lookup{
    
    enum ar_symbol_lookup_result{
        AR_SYMBOL_LOOKUP_failed,
        AR_SYMBOL_LOOKUP_import_header,
        AR_SYMBOL_LOOKUP_object_file,
    } lookup_result;
    
    union{
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
        } *ar_import_header;
        struct coff_file_header{
            u16 machine;
            u16 number_of_sections;
            u32 time_date_stamp;
            u32 pointer_to_symbol_table;
            u32 number_of_symbols;
            u16 size_of_optional_header;
            u16 characteristics;
        } *coff_file_header;
    };
    u64 file_size;
} ar_lookup_symbol(struct library_node *library_node, struct string identifier){
    
    struct ar_symbol_lookup ret = {
        .lookup_result = AR_SYMBOL_LOOKUP_failed,
    };
    
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
        if(import_symbol_index == -1) return ret;
        
        // Check if we have looked this up previously.
        // struct dll_import_node *dll_import_node = library_node->import_symbol_string_table[import_symbol_index].import_node;
        // if(dll_import_node) return dll_import_node;
        
        symbol_index = (u32)import_symbol_index;
    }
    
    assert(symbol_index < library_node->amount_of_symbols);
    // @note: These indices are one based for some stupid reason.
    u16 member_index = library_node->symbol_member_indices[symbol_index];
    if(member_index == 0 || member_index > library_node->amount_of_members){
        print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return ret;
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
        print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return ret;
    }
    
    struct ar_file_header *file_header = (void *)(file.data + member_offset);
    u8 *file_data = (u8 *)(file_header + 1);
    
    struct string file_size_string = string_strip_whitespace((struct string){.data = file_header->file_size_in_bytes, .size = sizeof(file_header->file_size_in_bytes)});
    int parse_size_success = 1;
    u64 file_size = string_to_u64(file_size_string, &parse_size_success);
    if(!parse_size_success || file_size > file.size || member_offset + file_size > file.size){
        print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return ret;
    }
    
    struct ar_import_header *import_header = (void *)(file_header + 1);
    
    // @note: The size of an import header matches the size of an _IMAGE_FILE_HEADER.
    
    if(file_size < sizeof(*import_header)){
        print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
        return ret;
    }
    
    if((import_header->signature_1 == 0) && (import_header->signature_2 == 0xffff)){
        // 
        // This is an import header.
        // 
        
        if((file_size <= sizeof(*import_header) + identifier.size + 1) || file_data[file_size-1] != 0){
            print("Warning: A parse error occurred while looking up '%.*s' in library '%.*s'.\n", identifier.size, identifier.data, library_node->path.size, library_node->path.data);
            return ret;
        }
        ret.ar_import_header = import_header;
        ret.file_size = file_size;
        ret.lookup_result = AR_SYMBOL_LOOKUP_import_header;
        return ret;
    }
    
    ret.lookup_result = AR_SYMBOL_LOOKUP_object_file;
    ret.file_size = file_size;
    ret.coff_file_header = (struct coff_file_header *)import_header;
    return ret;
}

