
static void read_pdb(struct memory_arena *scratch, struct os_file pdb_file);

//////////////////////////////////////////////////////
// PE helpers                                       //
//////////////////////////////////////////////////////

//_____________________________________________________________________________________________________________________
// PDB helpers

// For reference see `HashPbCb` in `microsoft-pdb/PDB/include/misc.h`.
u32 pdb_hash_index(u8 *bytes, size_t size, u32 modulus){
    u32 hash = 0;
    
    // Xor the bytes by dword lanes.
    for(u32 index = 0; index < size/sizeof(u32); index++){
        hash ^= ((u32 *)bytes)[index];
    }
    
    // Xor remaining bytes in.
    if(size & 2) hash ^= *(u16 *)(bytes + (size & ~3));
    if(size & 1) hash ^= *(u8 *) (bytes + (size -  1));
    
    // Make sure the hash is case insensitive.
    hash |= 0x20202020;
    
    // Mix the lanes.
    hash ^= (hash >> 11);
    hash ^= (hash >> 16);
    
    // Take the modulus and return the hash.
    return (hash % modulus);
}


// based on hashStringV1 in llvm, which is based on Hasher::lhashPbCb
func u32 pdb_string_hash(struct string string){
    u32 ret = 0;
    
    u32 *string_as_u32 = cast(u32 *)string.data;
    smm truncated_amount_of_u32 = (string.length / sizeof(u32));
    
    for(u32 i = 0; i < truncated_amount_of_u32; i++){
        ret ^= string_as_u32[i];
    }
    
    u32 remaining = string.length & 3;
    smm at = truncated_amount_of_u32 * sizeof(u32);
    
    if(remaining >= 2){
        ret ^= *(u16 *)(string.data + at);
        remaining -= 2;
        at += 2;
    }
    
    if(remaining){
        ret ^= *(u8 *)(string.data + at);
    }
    
    u32 to_lower = 0x20202020;
    ret |= to_lower;
    ret ^= (ret >> 11);
    ret ^= (ret >> 16);
    
    return ret;
}

struct codeview_type_record_header{
    u16 length;
    u16 kind;
};

// returns -1 on failiure.
int pdb_numeric_leaf_size_or_error(u16 numeric_leaf){
    
    if(!(numeric_leaf & 0x8000)) return 2;
    
    //
    // @cleanup: implement this more correctly
    //
    
    switch(numeric_leaf){
        case 0x8000:{ // LF_CHAR
            return 2 + 1;
        }break;
        case 0x8001:  // LF_SHORT
        case 0x8002:{ // LF_USHORT
            return 2 + 2;
        }break;
        case 0x8005: // LF_REAL32
        case 0x8003: // LF_LONG
        case 0x8004:{ // LF_ULONG
            return 2 + 4;
        }break;
        
        case 0x8009: // LF_QUADWORD
        case 0x800a: // LF_UQUADWORD
        case 0x8006:{ // LF_REAL64
            return 2 + 8;
        }break;
        
        case 0x8008: // LF_REAL128
        
        // case 0x8007: // LF_REAL80
        // case 0x800b: // LF_REAL48
        // case 0x800c: // LF_COMPLEX32
        // case 0x800d: // LF_COMPLEX64
        // case 0x800e: // LF_COMPLEX80
        // case 0x800f: // LF_COMPLEX128
        // case 0x8010: // LF_VARSTRING
        
        case 0x8017: // LF_OCTWORD
        case 0x8018:{ // LF_UOCTWORD
            return 2 + 16;
        }break;
        
        // case 0x8019: // LF_DECIMAL
        // case 0x801a: // LF_DATE
        // case 0x801b: // LF_UTF8STRING
        // case 0x801c: // LF_REAL16
        default:{
            return -1;
        }break;
    }
    
    // unreachable!
    // return -1;
}

char *pdb_type_record__get_name(u8 *type_record){
    
    struct codeview_type_header{
        u16 length;
        u16 kind;
    } *type_header = (void *)type_record;
    
    char *type_data = (char *)(type_header + 1);
    switch(type_header->kind){
        
        case /*LF_CLASS2*/0x1608:
        case /*LF_INTERFACE2*/0x160b:
        case /*LF_STRUCTURE2*/0x1609:{
            type_data += 0x10;
            type_data += pdb_numeric_leaf_size_or_error(*(u16 *)type_data); // count
            type_data += pdb_numeric_leaf_size_or_error(*(u16 *)type_data); // size
            return type_data;
        }break;
        
        case /*LF_STRUCTURE*/0x1505:
        case /*LF_CLASS*/0x1504:
        case /*LF_INTERFACE*/0x1519:{
            return type_data + 0x10 + pdb_numeric_leaf_size_or_error(*(u16 *)(type_data + 0x10));
        }break;
        
        case /*LF_UNION2*/0x160a:{
            type_data += 8;
            type_data += pdb_numeric_leaf_size_or_error(*(u16 *)type_data); // count
            type_data += pdb_numeric_leaf_size_or_error(*(u16 *)type_data); // size
            return type_data;
        }break;
        
        case /*LF_UNION*/0x1506:{
            return type_data + 8 + pdb_numeric_leaf_size_or_error(*(u16 *)(type_data + 8));
        }break;
        
        case /*LF_ENUM*/0x1507:{
            return type_data + 12;
        }break;
        
        case /*LF_ALIAS*/0x150a:{
            return type_data + 4;
        }break;
        
        default: return "";
    }
}

u32 tpi_hash_table_index_for_record(struct codeview_type_record_header *type_record_header, u32 number_of_hash_buckets){
    
    u8 *type_data = (u8 *)(type_record_header + 1);
    
    char *name = 0;
    size_t length = 0;
    
    switch(type_record_header->kind){
        case /*LF_ALIAS*/0x150a:{
            name = (char *)(type_data + 4);
        }break;
        
        case /*LF_CLASS2*/0x1608:
        case /*LF_INTERFACE2*/0x160b:
        case /*LF_STRUCTURE2*/0x1609: // @note: These get rid of the 'count' member to get 32-bits of 'properties' but stay the same size.
        case /*LF_UNION2*/0x160a: // @note: These get rid of the 'count' member to get 32-bits of 'properties' but stay the same size.
        
        case /*LF_UNION*/0x1506:
        case /*LF_ENUM*/0x1507:
        case /*LF_CLASS*/0x1504:
        case /*LF_STRUCTURE*/0x1505:
        case /*LF_INTERFACE*/0x1519:{
            
            u32 properties;
            if(type_record_header->kind < 0x1600){
                // @note: All of these have the 'properies' field at the same offset.
                properties = *(u16 *)(type_data + 2);
            }else{
                // @note: These dropped the 'count' for 32-bits more of properties.
                properties = *(u32 *)type_data;
            }
            
            u16 forward_ref = (properties & (1 << 7));
            u16 scoped      = (properties & (1 << 8));
            u16 has_unique_name = (properties & (1 << 9));
            
            char *tag_name = pdb_type_record__get_name((u8 *)type_record_header);
            
            // @note: This only works for c. for c++ they also search for 'foo::<unnamed-tag>' stuff.
            int anonymous = cstring_match(tag_name, "<unnamed-tag>") || cstring_match(tag_name, "__unnamed");
            
            if(!forward_ref && !anonymous){
                if(!scoped){
                    name = tag_name;
                }else if(has_unique_name){
                    name = tag_name + cstring_length(tag_name) + 1;
                }
            }
        }break;
        
        case /*LF_UDT_SRC_LINE*/0x1606:
        case /*LF_UDT_MOD_SRC_LINE*/0x1607:{
            name   = (char *)type_data;
            length = sizeof(u32);
        }break;
    }
    
    u32 hash_index;
    if(name){
        if(!length) length = cstring_length(name);
        hash_index = pdb_hash_index((u8 *)name, length, number_of_hash_buckets);
    }else{
        hash_index = crc32(/*initial_crc*/0, (u8 *)type_record_header, type_record_header->length + sizeof(type_record_header->length)) % number_of_hash_buckets;
    }
    
    return hash_index;
}


struct pdb_header{
    u8 signature[32];
    u32 page_size;
    u32 free_page_map;
    u32 number_of_pages;
    u32 directory_stream_size;
    u32 reserved;
    u32 page_number_of_directory_stream_number_list; // i.e some relative pointer to an array
};

static const u8 pdb_signature[] = {
    0x4d, 0x69, 0x63, 0x72, 0x6f, 0x73, 0x6f, 0x66, 0x74, 0x20, 0x43, 0x2f, 0x43, 0x2b, 0x2b, 0x20,
    0x4d, 0x53, 0x46, 0x20, 0x37, 0x2e, 0x30, 0x30, 0x0d, 0x0a, 0x1a, 0x44, 0x53, 0x00, 0x00, 0x00,
};
static_assert(sizeof(pdb_signature) == 32);

struct tpi_stream{
    u32 version;            // always 20040203
    u32 header_size;        // sizeof(tpi_header_size)
    u32 minimal_type_index; // always 0x1000
    u32 maximal_type_index; // -> maximal_type_index - minimal_type_index = amount
    u32 amount_of_bytes_of_type_record_data_following_the_header;
    
    u16 hash_stream_index;
    s16 hash_aux_stream_index; // unknown, can be -1
    
    u32 hash_key_size;         // usually 4 bytes
    u32 number_of_hash_buckets;
    
    // the length and offset of a buffer of hash values whithin the TPI hash stream (tpi->hash_stream_index)
    // this buffer should be of size (maximal_type_index - minimal_type_index) * hash_key_size
    s32 hash_value_buffer_offset;
    u32 hash_value_buffer_length; // = (maximal_type_index - minimal_type_index) * hash_key_size
    
    // the length and offset of a buffer of (type_index, offset of the type in the type record data)
    // again in the TPI hash stream (pairs of u32's)
    s32 index_offset_buffer_offset;
    u32 index_offset_buffer_length;
    
    // a serialized hash table in the TPI hash stream
    s32 incremental_linking_hash_table_offset; // mapping hashes to type indices
    u32 incremental_linking_hash_table_length;
};

struct dbi_stream{
    u32 version_signature;                             // always -1
    u32 version;                                       // always 19990903
    u32 amount_of_times_the_pdb_has_been_written;      // same as in pdb_stream
    u16 index_of_the_global_symbol_stream;
    u16 toolchain_version; // u16 major_version: 8, minor_version : 7, is_new_version_format : 1;
    u16 index_of_the_public_symbol_stream;             // what are these indices? are they in the stream array?
    u16 version_number_of_mspdb;                       // we dont use this?
    u16 index_of_the_symbol_record_stream;
    u16 PdbDllRbld;                                    // unknown
    u32 module_info_substream_byte_size;           // substream 0
    u32 section_contribution_substream_byte_size;  // substream 1
    u32 section_map_substream_byte_size;           // substream 2
    u32 source_info_substream_byte_size;           // substream 3
    u32 type_server_map_substream_byte_size;       // substream 4
    u32 offset_of_the_MFC_type_server_in_the_type_server_map_substream; // unknown what this is for
    u32 optional_debug_header_substream_byte_size; // substream 6
    u32 edit_and_continue_substream_byte_size;     // substream 5
    // unknown what the last flag does /DEBUG:CTYPES link flag
    u16 flags; // u16 incrementally_linked :1, private_symbols_stripped :1, has_conflict_types       :1; 
    u16 machine;                                        // for us always 0x8664 (x86_64)
    u32 padding;
};

struct section_contribution_entry{
    s16 section_id;
    // this seems to be one based and can be used to identify the section from some other data
    char Padding1[2];
    s32 offset; // offset of the contribution in the section. can be computed contrib.rva - section.rva
    s32 size;   // size of the contribution
    u32 characteristics;
    s16 module_index;    // the module, that is responsible for the contribution
    char Padding2[2];
    u32 data_crc;         // CRC-32 check sums
    u32 reloc_crc;        // CRC-32 check sums
};

struct dbi_module_info{
    u32 pad1; // currently open module in the source code???
    struct section_contribution_entry first_section_contribution_entry; // the modules first section_contribution_entry
    u16 flags;
    // {was_written_since_dbi_was_opened : 1,unused :7, index_into_TSM_list_for_this_mods_server;}
    u16 module_symbol_stream_index;
    // these 3 correspond to the sizes of Symbols C11LineInfo and C13LineInfo
    u32 byte_size_of_symbol_information;
    u32 byte_size_of_c11_line_information; // not understood assumed to be 0
    u32 byte_size_of_c13_line_information;
    u16 amount_of_source_files;
    u16 pad2;
    u32 pad3;
    u32 offset_in_module_name;  // these are offsets into the buffers below, they are always 0
    u32 offset_in_obj_file_name; // these are offsets into the buffers below, they are always 0
    char module_name[];
    //char ObjFileName[];
};

struct dbi_source_info {
    u16 amount_of_modules;
    u16 ignored_amount_of_source_files; // ignored as this would limit the amount of source files
    
    //uint16_t module_indices[NumModules];           // present but "does not appear to be useful"
    //uint16_t module_to_source_file_count_map[NumModules];
    //uint32_t FileNameOffsets[NumSourceFiles];  // mapping each source file to an offset to a name in the names buffer
    //char NamesBuffer[][NumSourceFiles];
};


enum stream_index{
    STREAM_old_directory       = 0, // done :old_directory_hack
    STREAM_PDB                 = 1, // done
    STREAM_TPI                 = 2, // done
    STREAM_DBI                 = 3, // done
    STREAM_IPI                 = 4, // done
    STREAM_names               = 5, // done
    STREAM_TPI_hash            = 6, // done
    STREAM_IPI_hash            = 7, // done
    STREAM_symbol_records      = 8, // done
    STREAM_global_symbol_hash  = 9, // done (stubbed)
    STREAM_public_symbol_hash  = 10, // done (stubbed)
    STREAM_section_header_dump = 11, // done
    
    // after this come the module streams
    STREAM_module_zero,
};

struct pdb_location{
    struct page_list_node *page;
    u16 offset;
    u32 size;
};

struct pdb_line_info{
    smm offset_in_function;
    smm line_number;
};

/////////////////////////////////////////////////////////////////////////////////////////////

// @note @maybe_cleanup @hardcoded we always use 0x1000 here when we need page size, maybe that should be a macro or something

struct page_list_node{
    struct page_list_node *next;
    u16 page_index;
    u16 offset;
    u32 offset_in_stream;
};

struct pdb_write_context{
    struct memory_arena *arena;
    struct memory_arena *scratch;
    
    struct pdb_header *pdb_header;
    struct{
        struct page_list_node *first;
        struct page_list_node *last;
    } free_page_maps;
    
    struct{
        struct page_list_node *first;
        struct page_list_node *last;
    } unused_free_page_maps;
    
    // address in memory while writing you can get a page_index by doint (page - pdb_base) >> 12
    u8 *pdb_base;
    u8 *pdb_end;
    smm pdb_size;
    
    // @hmm: Right now we do the whole pdb thing, where pages could be all over the place...
    // I actually don't really see a reason for that. Maybe it would be faster if this was just
    // u16 starting_page_index; u16 ending_page_index; and we just commit to writing stuff out sequentially
    // @hmm: actually if we have to write these multi threaded (:PDBFunctionSize) there is good reason
    struct page_list{
        struct page_list_node *first;
        struct page_list_node *last;
        
        u32 amount_of_pages;
        u32 symbol_at;
    } page_list[0x1000];
    
    // we let this be stateful. call set_current_stream, to change what stream you are writing to.
    enum stream_index active_stream;
    struct page_list *active_page_list;
    
    struct pdb_location active_symbol;
    b32 in_symbol;
    
    // TPI stream
    struct ast_type **type_stack;
    smm type_stack_at;
    smm type_stack_size;
    struct pdb_type_info{
        u32 pointer_type_index;
        struct ast_type *type;
    }*type_index_to_type_info;
    smm maximal_amount_of_type_indices;
    smm amount_of_type_indices;
    
    // for emitting type info: (TPI stream/IPI stream)
    // @WARNING: this means that TPI < TPI_hash < IPI < IPI_hash;
    struct pdb_location type_record_data_begin;
    struct pdb_index_offset_buffer_entry{
        u32 type_index;
        u32 offset;
    } *index_offset_buffer;
    u32 index_offset_buffer_size;
    u32 index_offset_buffer_at;
    smm index_offset_buffer_boundary;
    
    // MODULE stream
    smm pdb_line_at; // used to track the line number when emitting line information
    smm pdb_offset_at;
    smm pdb_amount_of_lines;
    
    struct pdb_location module_stream_begin;
    smm current_block32_offset_in_stream;
    
    s16 text_section_id;
};

func struct pdb_location get_current_pdb_location(struct pdb_write_context *context){
    struct page_list *list = context->active_page_list;
    
    struct pdb_location ret;
    ret.page   = list->last;
    ret.offset = ret.page->offset;
    ret.size   = 0;
    
    return ret;
}

// calculates (a - b), where a and be are assumed to be in the same stream
func u32 pdb_location_diff(struct pdb_location a, struct pdb_location b){
    if(a.page == b.page){
        assert(a.offset >= b.offset);
        return a.offset - b.offset;
    }
    
    u32 a_offset = a.page->offset_in_stream + a.offset;
    u32 b_offset = b.page->offset_in_stream + b.offset;
    
    assert(a_offset >= b.offset);
    return a_offset - b_offset;
}

func u32 pdb_current_offset_from_location(struct pdb_write_context *context, struct pdb_location loc){
    return pdb_location_diff(get_current_pdb_location(context), loc);
}


func u8 *pdb_page_from_index(struct pdb_write_context *context, smm index){
    assert(0 <= index && index <= max_u16);
    return context->pdb_base + index * 0x1000;
}

func void stream_push_page(struct pdb_write_context *context, struct page_list *list){
    retry:;
    // @cleanup: is uninitialized correct here?
    u8 *page = push_uninitialized_data(context->arena, u8, 0x1000);
    
    struct page_list_node *node = push_struct(context->scratch, struct page_list_node);
    
    smm page_offset = page - context->pdb_base;
    assert(!(page_offset & (0x1000 - 1)));
    node->page_index = to_u16(page_offset / 0x1000);
    node->offset     = 0;
    node->offset_in_stream = list->amount_of_pages * 0x1000;
    
    // :free_page_map
    // we implicitly push free page maps when we have to.
    // The pdb format has free page maps every 0x1000 blocks and they cover
    // the first '8 * 0x1000' blocks, so there are like way to many of them... but that's just life
    if((node->page_index & (0x1000 - 1)) == 1){
        sll_push_back(context->free_page_maps, node);
        goto retry;
    }
    
    if((node->page_index & (0x1000 - 1)) == 2){
        sll_push_back(context->unused_free_page_maps, node);
        goto retry;
    }
    
    list->amount_of_pages++;
    sll_push_back(*list, node);
}


func void set_current_stream(struct pdb_write_context *context, enum stream_index stream_index){
    assert(!context->in_symbol);
    assert(stream_index < array_count(context->page_list));
    context->active_stream = stream_index;
    
    // init if not inited
    struct page_list *list = &context->page_list[stream_index];
    if(!list->last){
        stream_push_page(context, list);
        list->symbol_at = 0x1000; // first symbol
    }
    
    context->active_page_list = list;
}

func struct pdb_location stream_allocate_bytes(struct pdb_write_context *context, smm size){
    struct page_list *list = context->active_page_list;
    assert(list->last);
    
    struct pdb_location ret;
    ret.page   = list->last;
    ret.offset = ret.page->offset;
    ret.size   = to_u32(size);
    
    assert(ret.offset <= 0x1000);
    assert(ret.page->next == null);
    
    struct page_list_node *current_page = list->last;
    smm remaining_size = 0x1000 - current_page->offset;
    if(remaining_size <= size){
        current_page->offset = 0x1000; // page is now _full_
        
        smm size_to_allocate  = size - remaining_size;
        smm pages_to_allocate = size_to_allocate >> 12;
        
        // @cleanup: dumb loop this could be way faster, but we probably never do more then one iteration...
        for(smm page = 0; page < pages_to_allocate; page++){
            stream_push_page(context, list);
            list->last->offset = 0x1000; // these lists are all _full_
        }
        
        smm rest_size_to_allocate = size_to_allocate & (0x1000 - 1);
        
        // allocate a new page even if rest_size_to_allocate == 0
        stream_push_page(context, list);
        list->last->offset = to_u16(rest_size_to_allocate);
    }else{
        current_page->offset += to_u16(size);
    }
    
    return ret;
}

func void stream_write_bytes(struct pdb_write_context *context, struct pdb_location *dest, void *_source, smm size){
    assert(size <= dest->size);
    smm save_size = size;
    u8 *source = _source;
    
    while(size){
        u8 *page = pdb_page_from_index(context, dest->page->page_index);
        u8 *at = page + dest->offset;
        assert(dest->offset <= 0x1000);
        
        smm remaining_size_in_page = 0x1000 - dest->offset;
        if(remaining_size_in_page < size){
            memcpy(at, source, remaining_size_in_page);
            
            size   -= remaining_size_in_page;
            source += remaining_size_in_page;
            
            dest->offset = 0;
            assert(dest->page->next); // this should always be here, as you can only write, after you allocated.
            dest->page = dest->page->next;
        }else{
            assert(size <= 0x1000);
            memcpy(at, source, size);
            dest->offset += to_u16(size);
            break;
        }
    }
    
    dest->size -= to_u32(save_size);
}

func void stream_emit_struct(struct pdb_write_context *context, void *memory, smm size){
    struct pdb_location dest = stream_allocate_bytes(context, size);
    stream_write_bytes(context, &dest, memory, size);
}

func void stream_emit_int(struct pdb_write_context *context, s64 integer, smm size){
    assert(size <= 8);
    stream_emit_struct(context, &integer, size);
}

func void stream_align(struct pdb_write_context *context, u16 align_to, b32 f3f2f1){
    struct page_list *list = context->active_page_list;
    assert(list->last);
    assert(is_power_of_two(align_to));
    u16 offset = list->last->offset;
    u16 up = align_up(offset, align_to);
    if(up > offset){
        u8 *page = pdb_page_from_index(context, list->last->page_index);
        u8 *dest = page + list->last->offset;
        if(f3f2f1){
            for(s32 i = offset; i < up; i++){
                page[i] = 0xf0 + to_u8(up - i);
            }
        }else{
            memset(dest, 0, up - offset);
        }
        list->last->offset = up;
    }
}

func u32 stream_begin_symbol(struct pdb_write_context *context, u16 symbol_kind){
    assert(!context->in_symbol);
    context->in_symbol = true;
    u32 ret = context->active_page_list->symbol_at++;
    
    if(context->active_stream == STREAM_TPI || context->active_stream == STREAM_IPI){
        u32 offset = pdb_current_offset_from_location(context, context->type_record_data_begin);
        if(offset > context->index_offset_buffer_boundary){
            u32 index = context->index_offset_buffer_at++;
            if(index >= context->index_offset_buffer_size){
                struct pdb_index_offset_buffer_entry *new_entries = push_uninitialized_data(context->scratch, struct pdb_index_offset_buffer_entry, context->index_offset_buffer_size * 2);
                memcpy(new_entries, context->index_offset_buffer, context->index_offset_buffer_size * sizeof(context->index_offset_buffer[0]));
                context->index_offset_buffer_size *= 2;
                context->index_offset_buffer = new_entries;
            }
            context->index_offset_buffer[index].type_index = ret;
            context->index_offset_buffer[index].offset = offset;
            context->index_offset_buffer_boundary += kilo_bytes(8);
        }
    }
    
    // size of the symbol is filled in stream_end_symbol
    context->active_symbol = stream_allocate_bytes(context, sizeof(u16));
    stream_emit_struct(context, &symbol_kind, sizeof(u16));
    
    return ret;
}

func void stream_end_symbol(struct pdb_write_context *context){
    stream_align(context, sizeof(u32), false);
    
    struct pdb_location current_loc = get_current_pdb_location(context);
    
    // -2 because the length field excludes the 'size' field on the symbol record
    smm size = pdb_location_diff(current_loc, context->active_symbol) - 2;
    if(size > max_u16){
        // :ERROR :pdb_symbol_overflow
        print("Internal Compiler Error:\n");
        print("A symbol in the '.pdb' has a size of %lld but the biggest size allowed is 65535.\n", size);
        print("This might probably leads to a coruppt '.pdb'. Sorry!\n");
        // os_panic(1);
    }
    u16 size_u16 = to_u16(size);
    stream_write_bytes(context, &context->active_symbol, &size_u16, sizeof(u16));
    context->in_symbol = false;
}


///////////////////////////////////////////////////////////////////////////////


#define out_struct(data) stream_emit_struct(context, &data, sizeof(data))
#define out_string(string) {stream_emit_struct(context, (string).data, (string).length); out_int(0, u8);}

#define out_int(integer, type) ((void)(type)3, stream_emit_int(context, integer, sizeof(type)))
#define out_align(align_to) stream_align(context, align_to, false)
#define out_f3f2f1_align(align_to) stream_align(context, align_to, true)
#define begin_symbol(type) stream_begin_symbol(context, type)
#define end_symbol() stream_end_symbol(context)

func void stream_emit_size_and_name(struct pdb_write_context *context, smm value, b32 is_signed, struct string string){
    if(is_signed){
        smm val = value;
        if(val >= 0){
            if(val < 0x8000){
                out_int(val, u16);
            }else if(val <= s16_max){
                out_int(0x8001, u16); // LF_SHORT
                out_int(val, u16);
            }else if(val <= s32_max){
                out_int(0x8003, u16); // LF_LONG
                out_int(val, u32);
            }else{
                out_int(0x8009, u16); // LF_QUADWORD
                out_int(val, u64);
            }
        }else{
            if(val >= s8_min){
                out_int(0x8000, u16); // LF_CHAR
                out_int(val, u8);
            }else if(val >= s16_min){
                out_int(0x8001, u16); // LF_SHORT
                out_int(val, u16);
            }else if(val >= s32_min){
                out_int(0x8003, u16); // LF_LONG
                out_int(val, u32);
            }else{
                out_int(0x8009, u16); // LF_QUADWORD
                out_int(val, u64);
            }
        }
    }else{
        u64 val = value;
        if(val < 0x8000){
            out_int(val, u16);
        }else if(val <= u16_max){
            out_int(0x8002, u16); // LF_USHORT
            out_int(val, u16);
        }else if(val <= u32_max){
            out_int(0x8004, u16); // LF_ULONG
            out_int(val, u32);
        }else{
            out_int(0x800a, u16); // LF_UQUADWORD
            out_int(val, u64);
        }
    }
    
    out_string(string);
    out_f3f2f1_align(sizeof(u32));
}


#define IPHR_HASH 4096
struct gsi_hash_bucket{
    struct gsi_hash_bucket *next;
    struct string name;
    u32 hash;
    u32 symbol_offset;
    u32 rva;
};

func b32 global_symbol_stream_hash_table_add(struct gsi_hash_bucket **hash_table, struct memory_arena *scratch, u32 ref_offset, smm rva, struct string name){
    
    u32 hash = pdb_string_hash(name);
    struct gsi_hash_bucket *new_bucket = push_struct(scratch, struct gsi_hash_bucket);
    new_bucket->hash = hash;
    
    new_bucket->symbol_offset = ref_offset;
    new_bucket->name = name;
    new_bucket->rva  = (u32)rva;
    
    struct gsi_hash_bucket *start = hash_table[hash % IPHR_HASH];
    if(!start){
        hash_table[hash % IPHR_HASH] = new_bucket;
        return 1;
    }else{
        // put them in sorted, this is n^2 but who cares @speed
        struct gsi_hash_bucket *prev = null;
        for(struct gsi_hash_bucket *bucket = start; bucket; prev = bucket, bucket = bucket->next){
            if(bucket->next){
                // this should be sort of the same as 'gsiRecordLess'
                b32 should_insert = false;
                if(name.length < bucket->name.length){
                    should_insert = true;
                }else if(name.length == bucket->name.length){
                    for(smm i = 0; i < name.length; i++){
                        if(name.data[i] != bucket->name.data[i]){
                            should_insert = (name.data[i] < bucket->name.data[i]);
                            break;
                        }
                    }
                }
                
                if(should_insert){
                    new_bucket->next = bucket;
                    if(prev){
                        prev->next = new_bucket;
                    }else{
                        hash_table[hash % IPHR_HASH] = new_bucket;
                    }
                    break;
                }
            }else{
                bucket->next = new_bucket;
                break;
            }
        }
        return 0;
    }
}

//_____________________________________________________________________________________________________________________

func void tpi_emit_type_index_or_predecl_type_index(struct pdb_write_context *context, struct ast_type *type, enum ast_kind *defined_type){
    if(defined_type && *defined_type == AST_enum){
        struct ast_type *ast_enum = cast(struct ast_type *)defined_type;
        if(ast_enum->flags & TYPE_FLAG_pdb_permanent){
            out_int(ast_enum->pdb_type_index, u32);
            return;
        }
    }
    
    if(type->flags & TYPE_FLAG_pdb_permanent){
        out_int(type->pdb_type_index, u32);
    }else{
        assert(type->pdb_predecl_type_index);
        out_int(type->pdb_predecl_type_index, u32);
    }
}


func void pdb_emit_regrels_for_scope(struct pdb_write_context *context, struct ast_scope *scope){
    // @note: it seems, that all declarations need to be at the beginning of the scope.
    //        so we loop here over all of them instead of doing it recursively
    for(smm i = 0; i < scope->current_max_amount_of_declarations; i++){
        struct ast_declaration *decl = scope->declarations[i];
        if(!decl) continue;
        
        // Skip typedefs.
        if(decl->kind != IR_declaration) continue;
        
        // Enums don't get 'S_REGREL32'... @cleanup: maybe they get constants?
        if((decl->flags & DECLARATION_FLAGS_is_enum_member)) continue;
        
        if(decl->flags & DECLARATION_FLAGS_is_local_persist){
            // @incomplete: emit a LDATA32
            continue;
        }
        s32 offset_of_rbp = to_s32(-decl->offset_on_stack);
        
        begin_symbol(0x1111);{ // S_REGREL32
            out_int(offset_of_rbp, u32);
            tpi_emit_type_index_or_predecl_type_index(context, decl->type, decl->defined_type);
            out_int(334, u16); // CV_AMD64_RBP (cvconst.h)
            out_string(decl->identifier->string);
        }end_symbol();
    }
    
}

func struct pdb_location pdb_begin_scope(struct pdb_write_context *context, struct ast_function *function, struct ast_scope *scope){
    
    struct pdb_location pointer_to_end_loc;
    smm scope_size = 0;
    smm offset_in_text_section = function->offset_in_text_section;
    
    if(scope->start_line_index < function->line_information.size){ // Only false for empty functions.
        
        u32 start_offset = function->line_information.data[scope->start_line_index].offset;
        u32 end_offset   = scope->end_line_index < function->line_information.size ? function->line_information.data[scope->end_line_index].offset : (u32)function->byte_size_without_prolog;
        
        // @cleanup: Does this correctly include function->size_of_prologue?
        scope_size = end_offset - start_offset;
        offset_in_text_section = function->offset_in_text_section + function->size_of_prolog + start_offset; // relocated by relocation.
    }
    
    smm scope_offset_in_symbol_stream = pdb_current_offset_from_location(context, context->module_stream_begin);
    begin_symbol(0x1103);{// S_BLOCK32
        out_int(context->current_block32_offset_in_stream, u32);      // pointer to parent
        pointer_to_end_loc = stream_allocate_bytes(context, sizeof(u32));
        out_int(scope_size, u32);                                     // size of the scope in bytes
        out_int(offset_in_text_section, u32);                         // offset in segment
        out_int(context->text_section_id, u16);                                // segment
    }end_symbol();
    context->current_block32_offset_in_stream = scope_offset_in_symbol_stream;
    return pointer_to_end_loc;
}

func void emit_debug_info_for_scope__recursive(struct pdb_write_context *context, struct ast_function *function, struct ast_scope *scope){
    
    // @cleanup: skip if there are no declarations in the scope.
    
    
    struct pdb_location pointer_to_end_loc = zero_struct;
    smm old_offset = context->current_block32_offset_in_stream;
    
    if(scope != function->scope){
        // @note: do not emit a scope for the initial scope as it is implied by the frameproc
        pointer_to_end_loc = pdb_begin_scope(context, function, scope);
    }
    
    pdb_emit_regrels_for_scope(context, scope);
    
    for(struct ast_scope *subscope = scope->subscopes.first; subscope; subscope = subscope->subscopes.next){
        emit_debug_info_for_scope__recursive(context, function, subscope);
    }
    
    if(scope != function->scope){
        u32 diff = pdb_current_offset_from_location(context, context->module_stream_begin);
        stream_write_bytes(context, &pointer_to_end_loc, &diff, sizeof(u32));
        begin_symbol(0x6);{ // S_END
        }end_symbol();
    }
    context->current_block32_offset_in_stream = old_offset; // @cleanup: should maybe be called pointer
}


func void emit_debug_info_for_function(struct pdb_write_context *context, struct ast_function *function){
    // @cleanup: is this needed somewhere else?
    context->current_block32_offset_in_stream = function->debug_symbol_offset;
    emit_debug_info_for_scope__recursive(context, function, function->scope);
}

//_____________________________________________________________________________________________________________________
// 

func void emit_one_pdb_line_info(struct pdb_write_context *context){
    // @cleanup: line should only have lower bits set.. not sure?
    u32 line   = save_truncate_smm_to_u32(context->pdb_line_at);
    u32 offset = save_truncate_smm_to_u32(context->pdb_offset_at);
    
    out_int(offset, u32);
    u32 is_statement = 0x80000000;
    out_int(line | is_statement, u32);
    context->pdb_amount_of_lines += 1;
}

func void emit_pdb_line_info_for_function(struct pdb_write_context *context, struct ast_function *function){
    
#if 0
    context->pdb_amount_of_lines = 0;
    context->pdb_offset_at = 0;
    context->pdb_line_at   = function->scope->token->line;
    emit_one_pdb_line_info(context); // emit an initial one and then we always emit _after_ updating
    
    emit_pdb_line_info_for_ast__recursive(context, function, function->scope);
#else   
    
    const u32 is_statement = 0x80000000;
    
    struct ast_scope *scope = function->scope;
    
    // 
    // Emit an initial line for the prologue.
    // 
    out_int(0, u32);
    out_int(scope->token->line | is_statement, u32);
    
    for(smm index = 0; index < function->line_information.size; index++){
        struct function_line_information line = function->line_information.data[index];
        
        u32 offset = (u32)(line.offset + function->size_of_prolog);
        
        out_int(offset, u32);
        out_int(line.line | is_statement, u32);
    }
    context->pdb_amount_of_lines = function->line_information.size + 1;
    
#endif
    
}

//_____________________________________________________________________________________________________________________


func void print_coff(struct string output_file_path, struct memory_arena *arena, struct memory_arena *scratch){
    
    
    // 
    // Gather Symbols.
    // 
    
    struct ast_list typedefs = zero_struct;
    
    struct ast_list dllexports = zero_struct;
    struct ast_list dll_function_stubs = zero_struct;
    struct ast_list dll_imports = zero_struct;
    
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
                
                case IR_typedef:{
                    if(!(decl->flags & DECLARATION_FLAGS_is_reachable_from_entry)) continue;
                    
                    ast_list_append(&typedefs, arena, ast);
                }break;
                
                case IR_function:{
                    struct ast_function *function = (struct ast_function *)ast;
                    
                    if(!(function->as_decl.flags & DECLARATION_FLAGS_is_reachable_from_entry)) continue;
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_intrinsic)  continue;
                    if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                    
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
                        assert(function->dll_import_node);
                        ast_list_append(&dll_imports, scratch, &function->kind);
                        if(function->as_decl.flags & DECLARATION_FLAGS_need_dllimport_stub_function) ast_list_append(&dll_function_stubs, scratch, &function->kind);
                        continue;
                    }
                    
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                        ast_list_append(&dllexports, scratch, &function->kind);
                    }
                    
                    ast_list_append(&defined_functions, scratch, &function->kind);
                    
                    for_ast_list(function->static_variables){
                        struct ast_declaration *static_decl = (struct ast_declaration *)it->value;
                        if(static_decl->assign_expr){
                            ast_list_append(&initialized_declarations, scratch, &static_decl->kind);
                        }else{
                            ast_list_append(&uninitialized_declarations, scratch, &static_decl->kind);
                        }
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
                assert(function->dll_import_node);
                ast_list_append(&dll_imports, scratch, &function->kind);
            }
        }
    }
    
    if(tls_declarations.count){
        // 
        // If we have a tls_declaration, we need to reference the tls_index_declaration.
        // 
        ast_list_append(&uninitialized_declarations, scratch, &globals.tls_index_declaration->kind);
    }
    
    
    assert(globals.output_file_type == OUTPUT_FILE_exe || globals.output_file_type == OUTPUT_FILE_dll);
    
    struct temporary_memory temporary_memory = begin_temporary_memory(scratch);
    
    struct string root_file_name = strip_file_extension(output_file_path);
    struct string exe_full_path = push_format_string(arena, "%.*s",  output_file_path.size, output_file_path.data);
    struct string pdb_full_path = push_format_string(arena, "%.*s.pdb", root_file_name.size, root_file_name.data);
    replace_characters(pdb_full_path, "/", '\\');
    replace_characters(exe_full_path, "/", '\\');
    
    /////////////////////////////////////////////////////////////////////////////////////
    //                              start writing the exe                              //
    /////////////////////////////////////////////////////////////////////////////////////
    
    
    /////////////////////////////////////////////////////////
    // EXE layout:                                         //
    // DOS_STUB (location of the pe header at offset 0x3c) //
    // PE header (specifies amount of sections)            //
    // "optional" header                                   //
    // section header 1                                    //
    // ...                                                 //
    // section header n                                    //
    // data of section 1 (0x200 aligned)                   //
    // ...                                                 //
    // data of section n (0x200 aligned)                   //
    /////////////////////////////////////////////////////////
    
    push_align(arena, 0x1000);
    
    static const u8 DOS_STUB[] = {
        0x4d, 0x5a, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
        0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd0, 0x00, 0x00, 0x00,
        0x0e, 0x1f, 0xba, 0x0e, 0x00, 0xb4, 0x09, 0xcd, 0x21, 0xb8, 0x01, 0x4c, 0xcd, 0x21, 0x54, 0x68,
        0x69, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f,
        0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6e, 0x20, 0x69, 0x6e, 0x20, 0x44, 0x4f, 0x53, 0x20,
        0x6d, 0x6f, 0x64, 0x65, 0x2e, 0x0d, 0x0d, 0x0a, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xfd, 0xa2, 0x09, 0x47, 0xb9, 0xc3, 0x67, 0x14, 0xb9, 0xc3, 0x67, 0x14, 0xb9, 0xc3, 0x67, 0x14,
        0x9b, 0xa3, 0x66, 0x15, 0xba, 0xc3, 0x67, 0x14, 0xb9, 0xc3, 0x66, 0x14, 0xb6, 0xc3, 0x67, 0x14,
        0x1b, 0xa0, 0x63, 0x15, 0xb8, 0xc3, 0x67, 0x14, 0x1b, 0xa0, 0x65, 0x15, 0xb8, 0xc3, 0x67, 0x14,
        0x52, 0x69, 0x63, 0x68, 0xb9, 0xc3, 0x67, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4e, 0x44, 0x47,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };
    static_assert(sizeof(DOS_STUB) >= 0x3c);
    
    u8 *exe_base_address = push_uninitialized_data(arena, u8, sizeof(DOS_STUB));
    memcpy(exe_base_address, DOS_STUB, sizeof(DOS_STUB));
    
    // @note: this is just hardcoded in the dos stub
    //*(u32 *)(base_address + 0x3c) = sizeof(DOS_STUB);
    
    *push_struct(arena, u8) = 'P';
    *push_struct(arena, u8) = 'E';
    *push_struct(arena, u8) = '\0';
    *push_struct(arena, u8) = '\0';
    
    struct coff_file_header{
        // 
        // The type of machine this object is intended for. In our case 0x8664.
        // 
        u16 machine;
        
        // 
        // The number of sections contained in the object file.
        // 
        u16 amount_of_sections;
        
        // 
        // The number of sections since 00:00 January 1st 1970,
        // when the object file was created.
        // 
        u32 time_date_stamp;
        
        // 
        // The file offset of the COFF symbol table if present.
        // 
        u32 pointer_to_symbol_table;
        
        // 
        // The amount of symbols in the COFF symbol table.
        // 
        u32 amount_of_symbols;
        
        // 
        // Size of the optional header, should be zero for object files.
        // As they don't have an optional header.
        // 
        u16 size_of_optional_header;
        
        // 
        // Characteristics of the file.
        // Seems to be zero for object files.
        // 
        u16 file_characteristics;
    } *coff_file_header = push_struct(arena, struct coff_file_header);
    
    // Everything else is zero for now and will be filled in incrementally.
    coff_file_header->machine = 0x8664; // x64
    coff_file_header->time_date_stamp = get_unix_time();
    coff_file_header->file_characteristics = /*EXECUTABLE_IMAGE*/2 | /*LARGE_ADDRESS_AWARE*/0x20;
    
    if(globals.cli_options.no_dynamic_base) coff_file_header->file_characteristics |= /*RELOCATIONS_STRIPPED*/1;
    if(globals.output_file_type == OUTPUT_FILE_dll) coff_file_header->file_characteristics |= /*IS_DLL*/0x2000;
    
    struct image_optional_header64{
        // 
        // The type of image file, for us 0x20B = PE32+.
        // 
        u16 magic;
        
        // 
        // Major and minor linker version, these usually get filled in by link.exe.
        // 
        u8 major_linker_version;
        u8 minor_linker_version;
        
        // 
        // The sum of the sizes of all code sections.
        // 
        u32 size_of_code;
        
        // 
        // The sum of the sizes of all initialized data sections.
        // 
        u32 size_of_initialized_data;
        
        // 
        // The sum of the sizes of all .bss-type sections.
        // 
        u32 size_of_uninitialized_data;
        
        // 
        // A relative virtual address to the entry point of the program.
        // 
        u32 address_of_entry_point;
        
        // 
        // A relative virtual address to the start of the code section.
        // 
        u32 base_of_code;
        
        // From here on out are the "Windows-Specific Fields"
        
        // 
        // The base address of the executable. If the image is not relocatable, this is used as the base.
        // Must be 64k aligned.
        // 
        u64 image_base;
        
        // 
        // The alignment (in bytes) of sections when they are loaded.
        // Must be bigger than the file alignment.
        // 
        u32 section_alignment;
        
        // 
        // The alignment of sections in the file.
        // Must be smaller than the section aligment and between 512 and 64k.
        // 
        u32 file_alignment;
        
        // 
        // Operating system version. Not sure why this is 6.
        // 
        u16 major_operating_system_version;
        u16 minor_operating_system_version;
        
        // 
        // Version number for the image file format.
        // 
        u16 major_image_version;
        u16 minor_image_version;
        
        // 
        // Version number for the subsystem.
        // 
        u16 major_subsystem_version;
        u16 minor_subsystem_version;
        
        // 
        // Win32 version. Must be zero.
        // 
        u32 Win32_version_value;
        
        // 
        // The size of the image after it was loaded into memory.
        // 
        u32 size_of_image;
        
        // 
        // The size of everything up to and including the section headers rounded up to the file alignment.
        // 
        u32 size_of_headers;
        
        // 
        // Usually 0. Only checked for critical windows components.
        // 
        u32 checksum;
        
        // 
        // The subsystem used to run this image, like "console", "windows" or "kernel".
        // 
        u16 subsystem;
        
        // 
        // Characteristics of the dll/executable.
        // 
        u16 dll_characteristics;
        
        // 
        // The commit and reserve size for stack and heap.
        // 
        u64 size_of_stack_reserve;
        u64 size_of_stack_commit;
        u64 size_of_heap_reserve;
        u64 size_of_heap_commit;
        
        // 
        // Reserved must be 0.
        // 
        u32 loader_flags;
        
        // 
        // The amount of data-directory entries that make up the rest of the optional header.
        // 
        u32 number_of_rva_and_sizes;
        
        struct image_data_directory{
            u32 rva;
            u32 size;
        } data_directory[16];
    } *image_optional_header = push_struct(arena, struct image_optional_header64);
    
    coff_file_header->size_of_optional_header = sizeof(struct image_optional_header64);
    
    image_optional_header->magic = 0x20b;
    image_optional_header->major_linker_version = 14;
    image_optional_header->minor_linker_version = 11;
    
    image_optional_header->image_base = 0x140000000;
    if(globals.output_file_type == OUTPUT_FILE_dll) image_optional_header->image_base = 0x180000000;
    if(globals.cli_options.image_base_specified) image_optional_header->image_base = globals.cli_options.image_base;
    
#define SECTION_ALIGNMENT 0x1000
#define FILE_ALIGNMENT 0x200
    
    image_optional_header->section_alignment = SECTION_ALIGNMENT;
    image_optional_header->file_alignment = FILE_ALIGNMENT;
    
    image_optional_header->major_operating_system_version = 6;
    image_optional_header->major_subsystem_version = 6;    
    image_optional_header->subsystem = globals.subsystem;
    
    image_optional_header->dll_characteristics = /*DLL_HIGH_ENTROPY_VA*/0x20 | /*DLL_NX*/0x0100;
    if(globals.output_file_type != OUTPUT_FILE_dll) image_optional_header->dll_characteristics |= /*DLL_TERMINAL_SERVER_AWARE*/0x8000; // Not sure what this field means.
    if(!globals.cli_options.no_dynamic_base) image_optional_header->dll_characteristics |= /*DLL_DYNAMIC_BASE*/0x0040;
    
    image_optional_header->size_of_stack_reserve = mega_bytes(1);
    image_optional_header->size_of_stack_commit  = mega_bytes(1);
    image_optional_header->number_of_rva_and_sizes = array_count(image_optional_header->data_directory);
    
    // We currently have at most 6 sections. In the future, when we allow user sections this needs to be variable sized.
    struct coff_section_header *image_sections = push_data(arena, struct coff_section_header, 8);
    u32 image_section_at = 0;
    
    push_align(arena, FILE_ALIGNMENT);
    image_optional_header->size_of_headers = save_truncate_smm_to_u32(arena_current(arena) - exe_base_address);
    
    u64 section_virtual_address_at = align_up(image_optional_header->size_of_headers, SECTION_ALIGNMENT);
    
    // 
    // Warning: The order in which we allocate the section headers should match the order of the sections in the file.
    // 
    
    // section table:
#define SECTION_read                  0x40000000
#define SECTION_execute               0x20000000
#define SECTION_code                  0x00000020
#define SECTION_initialized_data      0x00000040
#define SECTION_uninitialized_data    0x00000080
#define SECTION_write                 0x80000000
#define SECTION_discardable           0x02000000
    
#define fill_section_header(section, section_name, permissions) {                                            \
    section = image_sections + image_section_at++;                                                           \
    u32 actual_size = save_truncate_smm_to_u32(arena_current(arena) - section ## _section_start);            \
    memcpy(section->name, section_name "\0\0\0\0\0\0\0", 8);                                                 \
    section->virtual_address = (u32)section_virtual_address_at;                                              \
    section->virtual_size = actual_size;                                                                     \
    push_align(arena, FILE_ALIGNMENT);                                                                       \
    section->pointer_to_raw_data = (u32)(section ## _section_start - exe_base_address);                      \
    section->size_of_raw_data = (u32)(arena_current(arena) - section ## _section_start);                     \
    section->characteristics = (permissions);                                                                \
    section_virtual_address_at = (u32)align_up(section_virtual_address_at + actual_size, SECTION_ALIGNMENT); \
}
    
#define make_relative_virtual_address(section_start, address) (u32)(section_virtual_address_at + ((u8 *)(address) - (section_start)))
    
    // Crappy guid
    u8 pdb_guid[16];{
        //
        // Unique enough!
        //
        assert(sizeof(SYSTEMTIME) == sizeof(pdb_guid));
        GetSystemTime((PSYSTEMTIME)pdb_guid);
        ((u64 *)pdb_guid)[0] ^= __rdtsc();
        ((u64 *)pdb_guid)[1] ^= GetTickCount64();
    }
    
    
    //
    // Write out the '.text' section
    //
    struct coff_section_header *text = null;
    {
        u8 *text_section_start = arena_current(arena);
        
        for_ast_list(defined_functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            smm function_size = function->size_of_prolog + function->byte_size_without_prolog;
            
            u8 *memory_for_function = push_uninitialized_data(arena, u8, function_size);
            
            u8 *at = memory_for_function;
            memcpy(at, function->base_of_prolog, function->size_of_prolog);
            at += function->size_of_prolog;
            
            memcpy(at, function->base_of_main_function, function->byte_size_without_prolog);
            function->base_of_main_function = at;
            at += function->byte_size_without_prolog;
            
            push_align_initialized_to_specific_value(arena, 16, 0xcc);
            
            function->offset_in_text_section   = memory_for_function - text_section_start;
            function->memory_location          = memory_for_function;
            function->relative_virtual_address = make_relative_virtual_address(text_section_start, memory_for_function);
        }
        
        for_ast_list(dll_function_stubs){
            
            // Emit a stub for every dllimport that needs it.
            struct ast_function *function = cast(struct ast_function *)it->value;
            struct dll_import_node *dll_import_node = function->dll_import_node;
            
            u8 *memory_for_stub = push_uninitialized_data(arena, u8, 6);
            
            memory_for_stub[0] = 0xff;
            memory_for_stub[1] = 0x25; // jmp [rip + offset_32bit]
            dll_import_node->stub_relative_virtual_address = make_relative_virtual_address(text_section_start, memory_for_stub);
            dll_import_node->stub_memory_location = memory_for_stub;
        }
        
        if(arena_current(arena) != text_section_start){
            fill_section_header(text, ".text", SECTION_read | SECTION_execute | SECTION_code);
            
            // @cleanup: If we have more than one code section (which we currently not support), this could be in another code section.
            //           We should just move this to the end.
            if(globals.entry_point){
                image_optional_header->address_of_entry_point = (u32)globals.entry_point->relative_virtual_address;
            }
        }
    }
    
    
    struct coff_section_header *bss = null;
    {   //
        // Write out the '.bss' section
        //
        u8 *bss_section_start = arena_current(arena);
        
        smm size = 0;
        for_ast_list(uninitialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            
            smm alignment = get_declaration_alignment(decl);
            smm decl_size = get_declaration_size(decl);
            
            if(decl_size == 0) decl_size = 1; // Ensure even zero-sized declarations have unique addresses.
            
            size = align_up(size, alignment);
            decl->relative_virtual_address = section_virtual_address_at + size;
            size += decl_size;
        }
        
        // @incomplete: what to do when this gets bigger than 0xFFFFFFFF
        u32 bss_size = to_u32(align_up(size, 0x1000));
        
        if(bss_size){
            fill_section_header(bss, ".bss", SECTION_read | SECTION_write | SECTION_uninitialized_data);
            
            bss->virtual_size = bss_size;
            bss->pointer_to_raw_data = 0;
            
            section_virtual_address_at += bss_size;
            image_optional_header->size_of_uninitialized_data += bss_size;
        }
    }
    
    u32 tls_data_rva = 0;
    
    struct coff_section_header *rdata = null;
    {   //
        // Write out the '.rdata' section
        //
        u8 *rdata_section_start = arena_current(arena);
        
        if(!sll_is_empty(globals.dlls)){ // import directory table
            u8 *import_begin = arena_current(arena);
            
            // Layout:
            //    import directory tables
            //    null import directory table
            //    
            //    for each dll:
            //        name rva ("kernel32.dll")
            //        import lookup table
            //        import address table
            //        name hint table
            //    
            //    There is only supposed to be one name hint table maybe...
            // 
            
            
            struct image_import_descriptor{
                
                // 
                // The relative virtual address of the import lookup table.
                // This table contains the address or ordinal of each import.
                // 
                u32 import_lookup_table_rva;
                
                // 
                // Time date stamp is zero until the image is bound.
                // 
                u32 time_date_stamp;
                
                // 
                // The index of the first forwarder reference. (Not sure what that is.)
                // 
                u32 forwarder_chain;
                
                // 
                // The relative virtual address to the name of the dll we are importing from.
                // 
                u32 name_rva;
                
                // 
                // The relative virtual address of the import address table.
                // Until the image is bound, this table is identically to the import lookup table.
                // 
                u32 import_address_table_rva;
            } *import_descriptors = push_data(arena, struct image_import_descriptor, globals.dlls.amount + 1);
            
            u8 *import_end = arena_current(arena);
            
            // Import data directory
            image_optional_header->data_directory[1].rva  = make_relative_virtual_address(rdata_section_start, import_begin);
            image_optional_header->data_directory[1].size = save_truncate_smm_to_u32(import_end - import_begin);
            
            u32 dll_import_index = 0;
            for(struct dll_node *dll_node = globals.dlls.first; dll_node; dll_node = dll_node->next, dll_import_index++){
                struct image_import_descriptor *import = import_descriptors + dll_import_index;
                
                char *string = push_cstring_from_string(arena, dll_node->name);
                import->name_rva = make_relative_virtual_address(rdata_section_start, string);
               
                // 
                // "The last entry is set to zero (NULL) to indicate the end of the table"
                // 
                u64 *import_address_table = push_uninitialized_data(arena, u64, dll_node->import_list.count + 1);
                import_address_table[dll_node->import_list.count] = 0;
                
                u32 import_address_table_index = 0;
                for(struct dll_import_node *import_node = dll_node->import_list.first; import_node; import_node = import_node->next){
                    
                    if(import_node->import_by_ordinal){
                        import_address_table[import_address_table_index] = /*import by ordinal*/0x8000000000000000 | import_node->ordinal_hint;
                    }else{
                        u16 *hint = push_struct(arena, u16);
                        *hint = import_node->ordinal_hint;
                        push_cstring_from_string(arena, import_node->import_name);
                        
                        import_address_table[import_address_table_index] = make_relative_virtual_address(rdata_section_start, hint);
                    }
                    
                    import_node->memory_location = (u8*)&import_address_table[import_address_table_index];
                    import_address_table_index++;
                }
                assert(import_address_table_index == dll_node->import_list.count);
                
                // "The RVA of the import address table. The contents of this table are identical to the contents of the import lookup table until the image is bound"
                u64 *import_lookup_table = push_array_copy(arena, u64, import_address_table, dll_node->import_list.count + 1);
                
                import->import_address_table_rva = make_relative_virtual_address(rdata_section_start, import_address_table);
                import->import_lookup_table_rva  = make_relative_virtual_address(rdata_section_start, import_lookup_table);
            }
            assert(dll_import_index == globals.dlls.amount);
            
            for_ast_list(dll_imports){
                struct ast_function *function = cast(struct ast_function *)it->value;
                struct dll_import_node *dll_import_node = function->dll_import_node;
                
                u8 *memory_location = dll_import_node->memory_location;
                smm relative_virtual_address = make_relative_virtual_address(rdata_section_start, memory_location);
                
                // the other functions are relative to
                function->memory_location = memory_location;
                function->relative_virtual_address = relative_virtual_address;
            }
            
            for_ast_list(dll_function_stubs){
                struct ast_function *function = cast(struct ast_function *)it->value;
                struct dll_import_node *dll_import_node = function->dll_import_node;
                
                u32 import_address_table_entry    = (u32)function->relative_virtual_address;
                u32 stub_relative_virtual_address = dll_import_node->stub_relative_virtual_address;
                s32 relative_offset = (s32)(import_address_table_entry - (stub_relative_virtual_address + 6));
                memcpy(dll_import_node->stub_memory_location + /*ff 25*/2, &relative_offset, sizeof(relative_offset));
            }
        }
        
        if(!globals.cli_options.no_debug){
            // setup the 'debug directory' which maps the exe to the pdb
            push_align(arena, 8);
            push_struct(arena, u32); // we want the guid to be 8 byte aligned I think, so we align to mod 8 = 4.
            // this is at least what link.exe seems to do.
            
            u8 *debug_info_begin = arena_current(arena);
            {
                
                // +0h   dword        "RSDS" signature
                // +4h   GUID         16-byte Globally Unique Identifier
                // +14h  dword        "age"
                // +18h  byte string  zero terminated UTF8 path and file name
                
                u8 *rsds = push_uninitialized_data(arena, u8, 4);
                rsds[0] = 'R'; rsds[1] = 'S'; rsds[2] = 'D'; rsds[3] = 'S';
                u8 *guid = push_uninitialized_data(arena, u8, 16);
                memcpy(guid, pdb_guid, sizeof(pdb_guid));
                
                u32 *age = push_struct(arena, u32);
                *age = 1;
                push_cstring_from_string(arena, pdb_full_path);
            }
            u8 *debug_info_end = arena_current(arena);
            
            push_align(arena, 4);
            
            
            struct image_debug_directory{
                // 
                // Reserved, must be zero. 
                // 
                u32 characteristics;
                
                // 
                // Time-date-stamp of the debug data, seems to be zero in practice.
                // 
                u32 time_date_stamp; 
                
                // 
                // Major and minor version of the debug data format.
                // 
                u16 major_version;
                u16 minor_version;
                
                // 
                // Type of the debug data, for us only codeview.
                // 
                u32 type;
                
                // 
                // Size of the debug data (not including the directory).
                // 
                u32 size_of_data;
                
                // 
                // Relative virtual address to the data.
                // 
                u32 address_of_raw_data;
                
                // 
                // File pointer the the data.
                // 
                u32 pointer_to_raw_data;
                
            } *debug = push_struct(arena, struct image_debug_directory);
            debug->characteristics = 0; // "reserved and must be zero"
            debug->time_date_stamp = 0;
            debug->major_version = 0; // @note: version seems to be 0.0
            debug->minor_version = 0;

            debug->type = /*IMAGE_DEBUG_TYPE_CODEVIEW*/2;
            debug->size_of_data = to_u32(debug_info_end - debug_info_begin);
            debug->address_of_raw_data = make_relative_virtual_address(rdata_section_start, debug_info_begin);
            debug->pointer_to_raw_data = to_u32(debug_info_begin - exe_base_address);
            
            // Debug directory
            image_optional_header->data_directory[6].rva  = make_relative_virtual_address(rdata_section_start, debug);
            image_optional_header->data_directory[6].size = sizeof(struct image_debug_directory);
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
                            
                            lit->relative_virtual_address = make_relative_virtual_address(rdata_section_start, base);
                            
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
                        
                        lit->relative_virtual_address = make_relative_virtual_address(rdata_section_start, string_table[index].data);
                        break;
                    }
                }
            }
            
            end_temporary_memory(temp);
        }
        
        
        // @note: float literals are loaded rip realtive so emit them here in rdata
        for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
            struct context *thread_context = globals.thread_infos[thread_index].context;
            
            for(struct ir_emitted_float_literal *lit = thread_context->emitted_float_literals.first; lit; lit = lit->next){
                if(lit->literal.type == &globals.typedef_f32){
                    f32 *_float = push_struct(arena, f32);
                    *_float = (f32)lit->literal._f32;
                    lit->relative_virtual_address = make_relative_virtual_address(rdata_section_start, _float);
                }else{
                    assert(lit->literal.type == &globals.typedef_f64);
                    f64 *_float = push_struct(arena, f64);
                    *_float = lit->literal._f64;
                    lit->relative_virtual_address = make_relative_virtual_address(rdata_section_start, _float);
                }
            }
        }
        
        if(dllexports.count){
            u32 *export_address_table = push_data(arena, u32, dllexports.count);
            u32 *name_pointer_table   = push_data(arena, u32, dllexports.count);
            u16 *ordinal_table        = push_data(arena, u16, dllexports.count);
            
            // @cleanup: what is this for exe's?
            char *dll_name = push_cstring_from_string(arena, exe_full_path);
            
            push_align(arena, sizeof(u32));
            u8 *edata_start = arena_current(arena);
            
            u32 ordinal_base = 1;
            
            *push_struct(arena, u32) = 0; // exports flags (reserved, must be zero).
            *push_struct(arena, u32) = 0xffffffff; // time date stamp
            *push_struct(arena, u16) = 0; // major version
            *push_struct(arena, u16) = 0; // minor version
            
            // dll name rva (rva of a string that contains the name of the dll)
            *push_struct(arena, u32) = make_relative_virtual_address(rdata_section_start, dll_name);
            *push_struct(arena, u32) = ordinal_base; // starting ordinal number
            
            // @note: these seem to be the same as far as I can tell
            
            // number of entries in the "export address table"
            *push_struct(arena, u32) = to_u32(dllexports.count);
            // number of entries in the "name pointer table" and "ordinal table"
            *push_struct(arena, u32) = to_u32(dllexports.count);
            
            // rva of the "export address table"
            *push_struct(arena, u32) = make_relative_virtual_address(rdata_section_start, export_address_table);
            
            // rva of the "name pointer table"
            *push_struct(arena, u32) = make_relative_virtual_address(rdata_section_start, name_pointer_table);
            
            // rva of the "ordinal table"
            *push_struct(arena, u32) = make_relative_virtual_address(rdata_section_start, ordinal_table);
            
            image_optional_header->data_directory[0].rva  = make_relative_virtual_address(rdata_section_start, edata_start);
            image_optional_header->data_directory[0].size = to_u32(arena_current(arena) - edata_start);
            
#define function_node_smaller(a, b) \
            string_lexically_smaller( \
                    ((struct ast_function *)a->value)->identifier->string, \
                    ((struct ast_function *)b->value)->identifier->string)
            
            sll_sort(dllexports, scratch, function_node_smaller);
#undef function_node_smaller
            
            // emit all the names
            
            u32 i = 0;
            for_ast_list(dllexports){
                assert(*it->value == IR_function);
                struct ast_function *function = cast(struct ast_function *)it->value;
                assert(function->relative_virtual_address); // The function better be emitted!
                
                
                char *function_name = push_cstring_from_string(arena, function->identifier->string);
                name_pointer_table[i] = make_relative_virtual_address(rdata_section_start, function_name);
                export_address_table[i] = to_u32(function->relative_virtual_address);
                ordinal_table[i] = to_u16((smm)i);
                i++;
            }
        }
        
        if(tls_declarations.count){
            {
                // Calculate the start of the tls section.
                struct ast_declaration *first_tls_decl = (struct ast_declaration *)tls_declarations.first->value;
                smm alignment = get_declaration_alignment(first_tls_decl);
                push_align(arena, (u32)alignment); // @cleanup: why does push align take a u32?
                tls_data_rva = make_relative_virtual_address(rdata_section_start, arena_current(arena));
            }
            
            for_ast_list(tls_declarations){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                
                smm alignment = get_declaration_alignment(decl);
                smm decl_size = get_declaration_size(decl);
                
                push_zero_align(arena, alignment);
                
                if(decl_size == 0) decl_size = 1; // Ensure even zero-sized declarations have unique addresses.
                
                u8 *mem = push_uninitialized_data(arena, u8, decl_size);
                
                if(decl->memory_location){
                    memcpy(mem, decl->memory_location, decl_size);
                }else{
                    memset(mem, 0, decl_size);
                }
                
                decl->memory_location = mem;
                decl->relative_virtual_address = make_relative_virtual_address(rdata_section_start, mem);
            }
            
            u32 tls_data_end_rva = make_relative_virtual_address(rdata_section_start, arena_current(arena));
            
            u64 *tls_callbacks = push_struct(arena, u64); // zero-terminated
            
            assert(globals.tls_index_declaration->relative_virtual_address >= 0); // This thing is in .bss
            
            push_align(arena, 0x40); // Align the tls_directory so it is guarateed to be on one page, for the base relocation hack.
            struct tls_directory{
                u64 raw_data_start;
                u64 raw_data_end;
                u64 address_of_index;
                u64 address_of_callbacks;
                u32 size_of_zero_fill;
                u32 characteristics; // @cleanup: alignment.
            } *tls_directory = push_struct(arena, struct tls_directory);
            tls_directory->raw_data_start = image_optional_header->image_base + tls_data_rva;
            tls_directory->raw_data_end   = image_optional_header->image_base + tls_data_end_rva;
            tls_directory->address_of_callbacks = image_optional_header->image_base + make_relative_virtual_address(rdata_section_start, tls_callbacks);
            tls_directory->address_of_index     = image_optional_header->image_base + globals.tls_index_declaration->relative_virtual_address;
            tls_directory->characteristics = /*IMAGE_SCN_ALIGN_4096BYTES*/0x00D00000;
            
            // Fill in the .tls directory.
            image_optional_header->data_directory[9].rva = make_relative_virtual_address(rdata_section_start, tls_directory);
            image_optional_header->data_directory[9].size = sizeof(*tls_directory);
        }
        
        
        if(arena_current(arena) != rdata_section_start){
            fill_section_header(rdata, ".rdata", SECTION_read | SECTION_initialized_data);
            image_optional_header->size_of_initialized_data += rdata->size_of_raw_data;
        }
    }
    
    struct coff_section_header *data = null;
    {   //
        // Write out the '.data' section
        //
        u8 *data_section_start = arena_current(arena);
        
        for_ast_list(initialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            
            smm alignment = get_declaration_alignment(decl);
            smm decl_size = get_declaration_size(decl);
            
            push_zero_align(arena, alignment);
            
            assert(decl->memory_location);
            
            if(decl_size == 0) decl_size = 1; // Ensure even zero-sized declarations have unique addresses.
            
            u8 *mem = push_uninitialized_data(arena, u8, decl_size);
            memcpy(mem, decl->memory_location, decl_size);
            
            decl->memory_location = mem;
            decl->relative_virtual_address = make_relative_virtual_address(data_section_start, mem);
        }
        
        if(arena_current(arena) != data_section_start){
            fill_section_header(data, ".data", SECTION_read | SECTION_write | SECTION_initialized_data);
            image_optional_header->size_of_initialized_data += data->size_of_raw_data;
        }
    }
    
    // From this point all functions and all declarations have their relative virtual address set.
    // thus we can patch now.
    
    struct relocation_node{
        struct relocation_node *next;
        smm offset_in_page;
    };
    
    struct base_relocation_block{
        struct base_relocation_block *next;
        smm page_rva;
        struct{
            struct relocation_node *first;
            struct relocation_node *last;
            smm count;
        } relocations;
    };
    
    struct{
        struct base_relocation_block *first;
        struct base_relocation_block *last;
        smm count;
    } relocation_blocks = zero_struct;
    
    // @hack: We manually add the relocation block for the tls section here.
    //        Once we re-write this whole backend, we can think about how to do this properly.
    if(tls_declarations.count){
        u32 tls_base_rva = image_optional_header->data_directory[9].rva;
        
        smm page_rva = tls_base_rva & ~((1ull << 12) - 1);
        smm offset   = tls_base_rva &  ((1ull << 12) - 1);
        
        struct base_relocation_block *block = push_struct(scratch, struct base_relocation_block);
        block->page_rva = page_rva;
        sll_push_back(relocation_blocks, block);
        relocation_blocks.count++;
        
        // Push base relocations for `raw_data_start`, `raw_data_end`, `address_of_index`, `address_of_callbacks`.
        for(u32 index = 0; index < 4; index++){
            struct relocation_node *node = push_struct(scratch, struct relocation_node);
            node->offset_in_page = offset + index * 8;
            sll_push_back(block->relocations, node);
            block->relocations.count++;
        }
    }
    
    // :patch :patches
    begin_counter(timing, patch);
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for(struct patch_node *patch = thread_context->local_patch_list.first; patch; patch = patch->next){
            
            enum ir_kind source_kind = patch->source->kind;
            
            // The destination has to have memory associated with it so 'memory_location'.
            assert(patch->dest_declaration->memory_location);
            if(source_kind == IR_declaration || source_kind == IR_function){
                // The source has to have memory when loaded, i.e. a 'relative virtual address'.
                struct ast_declaration *decl = (struct ast_declaration *)patch->source;
                assert(decl->relative_virtual_address > 0);
            }
            
            u8 *memory_location = patch->dest_declaration->memory_location + patch->location_offset_in_dest_declaration;
            
            // @hack... this is ugly.. we emit first the body then the prolog, so everything is actually
            //          relative to 'function->memory_location + function->size_of_prolog'
            // @cleanup: could we do everything relative to 'emit_pool.base'?
            if(patch->dest_declaration->kind == IR_function){
                struct ast_function *function = cast(struct ast_function *)patch->dest_declaration;
                
                memory_location += function->size_of_prolog;
                patch->rip_at   += function->size_of_prolog;
            }
            
            if(patch->kind == PATCH_rip_relative){
                assert(patch->dest_declaration->kind == IR_function);
                assert(patch->rip_at >= 0);
                
                if(source_kind == IR_function || source_kind == IR_declaration){
                    struct ast_declaration *source_declaration = cast(struct ast_declaration *)patch->source;
                    
                    // :patches_are_32_bit all functions that might need a patch are from us thus 32 bit are enough,
                    //                     this is now not true anymore, as we have to patch 64 bit locations for memory accesses
                    smm source_location = source_declaration->relative_virtual_address;
                    smm dest_location   = patch->dest_declaration->relative_virtual_address;
                    
                    // @note: this is used to access a member of a struct or a member of an array.
                    source_location += patch->location_offset_in_source_declaration;
                    smm rip_at = dest_location + patch->rip_at;
                    *cast(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
                }else if(source_kind == IR_emitted_float_literal){
                    struct ir_emitted_float_literal *f = (struct ir_emitted_float_literal *)patch->source;
                    assert(f->relative_virtual_address);
                    
                    smm dest_location = patch->dest_declaration->relative_virtual_address;
                    smm rip_at = dest_location + patch->rip_at;
                    smm source_location = f->relative_virtual_address;
                    *cast(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
                }else{
                    if(source_kind != IR_string_literal){
                        report_internal_compiler_error(null, "Not a string literal, but %d\n", *patch->source);
                        continue;
                    }
                    
                    struct ir_string_literal *lit = cast(struct ir_string_literal *)patch->source;
                    
                    smm dest_location = patch->dest_declaration->relative_virtual_address;
                    smm source_location = lit->relative_virtual_address + patch->location_offset_in_source_declaration;
                    
                    smm rip_at = dest_location + patch->rip_at;
                    *cast(s32 *)memory_location = save_truncate_smm_to_s32(source_location - rip_at);
                }
            }else if(patch->kind == PATCH_absolute){
                assert(patch->dest_declaration->kind == IR_declaration);
                
                smm source_location;
                if(source_kind == IR_function || source_kind == IR_declaration){
                    struct ast_declaration *decl = cast(struct ast_declaration *)patch->source;
                    
                    if(decl->flags & DECLARATION_FLAGS_is_dllimport){
                        assert(decl->kind == IR_function);
                        struct ast_function *function = (struct ast_function *)decl;
                        struct dll_import_node *import_node = function->dll_import_node;
                        source_location = import_node->stub_relative_virtual_address + image_optional_header->image_base;
                        source_location += patch->location_offset_in_source_declaration;
                    }else{
                        assert(decl->relative_virtual_address);
                        source_location = decl->relative_virtual_address + image_optional_header->image_base;
                        source_location += patch->location_offset_in_source_declaration;
                    }
                }else{
                    assert(source_kind == IR_string_literal);
                    struct ir_string_literal *lit = (struct ir_string_literal *)patch->source;
                    
                    source_location = (smm)lit->relative_virtual_address + image_optional_header->image_base;
                }
                
                *cast(smm *)memory_location = source_location;
                
                if(!globals.cli_options.no_dynamic_base){
                    smm rva = patch->dest_declaration->relative_virtual_address + patch->location_offset_in_dest_declaration;
                    smm page_rva = rva & ~((1ull << 12) - 1);
                    smm offset   = rva &  ((1ull << 12) - 1);
                    
                    struct base_relocation_block *block = null;
                    
                    // @cleanup: linear search...
                    for(struct base_relocation_block *it = relocation_blocks.first; it; it = it->next){
                        if(it->page_rva == page_rva){
                            block = it;
                            break;
                        }
                    }
                    
                    if(!block){
                        block = push_struct(scratch, struct base_relocation_block);
                        block->page_rva = page_rva;
                        sll_push_back(relocation_blocks, block);
                        relocation_blocks.count++;
                    }
                    
                    struct relocation_node *node = push_struct(scratch, struct relocation_node);
                    node->offset_in_page = offset;
                    sll_push_back(block->relocations, node);
                    block->relocations.count++;
                }
            }else{
                assert(patch->kind == PATCH_section_offset);
                struct ast_declaration *source_declaration = cast(struct ast_declaration *)patch->source;
                assert(source_declaration->flags & DECLARATION_FLAGS_is_thread_local);
                
                u32 source_location = (u32)(source_declaration->relative_virtual_address - tls_data_rva);
                
                *cast(u32 *)memory_location = source_location;
            }
        }
    }
    end_counter(timing, patch);
    
    struct coff_section_header *reloc = null;
    {   //
        // Fill in the .reloc section.
        // 
        u8 *reloc_section_start = arena_current(arena);
        
        for(struct base_relocation_block *block = relocation_blocks.first; block; block = block->next){
            *push_struct(arena, u32) = to_u32(block->page_rva);
            
            smm count = block->relocations.count;
            if(count & 1) count++;
            
            *push_struct(arena, u32) = to_u32((smm)(count * sizeof(u16) + 2 * sizeof(u32)));
            
            for(struct relocation_node *node = block->relocations.first; node; node = node->next){
                // four bits of type (10 = IMAGE_REL_BASED_DIR64) and then the offset in the page
                *push_struct(arena, u16) = to_u16((10 << 12) | node->offset_in_page);
            }
            
            // block must be aligned on a 32bit boundary so push pad in case its not
            // 0 is pad
            if(block->relocations.count & 1) push_struct(arena, u16);
        }
        
        if(arena_current(arena) != reloc_section_start){
            
            fill_section_header(reloc, ".reloc", SECTION_read | SECTION_initialized_data | SECTION_discardable);
            
            // Reloc data directory
            image_optional_header->data_directory[5].rva  = reloc->virtual_address;
            image_optional_header->data_directory[5].size = reloc->virtual_size;
            // @note: this needs to be the actual size (which is reloc->Misc.VirtualSize)
            // and cant be 'reloc->SizeOfRawData', because then it just silently fails.
        }
    }
    
    struct coff_section_header *pdata = null;
    {   // 
        // Fill in the .pdata section.
        // 
        u8 *pdata_section_start = arena_current(arena);
        
        // @note: @WARNING: These have to be sorted by address, right now we just sort the
        //                  functions after gathering them!
        
        struct coff_runtime_function{
            // 
            // Relative virtual addresses to the start and end of the function.
            // 
            u32 begin_address;
            u32 end_address;
            
            // 
            // A relative virtual address to the unwind information for the function.
            // 
            u32 unwind_info_rva;
        } *runtime_functions = push_data(arena, struct coff_runtime_function, defined_functions.count);
        
        
        // Exception data directory
        image_optional_header->data_directory[3].rva  = (u32)section_virtual_address_at;
        image_optional_header->data_directory[3].size = to_u32(sizeof(struct coff_runtime_function) * defined_functions.count);
        
        u32 runtime_function_index = 0;
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            struct coff_runtime_function *runtime_function = runtime_functions + runtime_function_index;
            
            runtime_function->begin_address = save_truncate_smm_to_u32(function->relative_virtual_address);
            runtime_function->end_address   = save_truncate_smm_to_u32(function->relative_virtual_address + function->byte_size);
            
            void *unwind_info = push_unwind_information_for_function(arena, function);
            
            runtime_function->unwind_info_rva = make_relative_virtual_address(pdata_section_start, unwind_info);
            runtime_function_index++;
        }
        
        if(arena_current(arena) != pdata_section_start){
            fill_section_header(pdata, ".pdata", SECTION_read | SECTION_initialized_data);
        }
    }
    
    
    image_optional_header->size_of_image = (u32)section_virtual_address_at;
    
    // @note: set the base of the code to be the first section, its zero sized anyway, but that is what
    //        link.exe seems to do!
    image_optional_header->base_of_code = text ? text->virtual_address : 0x1000;
    
    coff_file_header->amount_of_sections = (u16)image_section_at;
    
    push_zero_align(arena, 0x200);
    u8 *exe_end_address = arena_current(arena);
    
    
    if(!globals.cli_options.dont_print_the_files){
        
        // 
        // Write exe file to disk.
        // 
        
        char *exe_name = push_cstring_from_string(arena, exe_full_path);
        smm size = exe_end_address - exe_base_address;
        
        b32 success;
        {
            u8 *buffer = exe_base_address;
            smm buffer_size = size;
            
            //u32 FILE_FLAG_NO_BUFFERING = 0x20000000;
            u32 FILE_FLAG_NO_BUFFERING = 0;
            
            HANDLE file_handle = CreateFileA(exe_name, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_FLAG_NO_BUFFERING, NULL);
            
            // ignore FILE_ALREADY_EXISTS
            if(GetLastError() == 183) SetLastError(0);
            //if(GetLastError()) print("Warning: GetLastError in create file: %d\n", GetLastError());
            
            DWORD bytes_written;
            
            success = WriteFile(file_handle, buffer, save_truncate_smm_to_s32(buffer_size), &bytes_written, 0);
            
            success = success && (bytes_written == buffer_size);
            
            begin_counter(context, virus_scanner);
            // @note: this apparently invokes the anti virus scanner or something thus it takes 200ms...
            CloseHandle(file_handle);
            end_counter(context, virus_scanner);
        }
        
        if(success){
            if(!globals.cli_options.quiet) print("Wrote file: '%s'\n", exe_name);
        }else{
            print("Error: Unable to write file '%s'.\n", exe_name);
            globals.an_error_has_occurred = true;
        }
    }
    
    if(globals.cli_options.no_debug || /*we failed to write the .exe*/globals.an_error_has_occurred){
        end_temporary_memory(temporary_memory);
        return;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //                                  PDB                                      //
    ///////////////////////////////////////////////////////////////////////////////
    
    begin_counter(timing, print_pdb);
    
    u16 text_section_id  = text  ? (u16)((text  - image_sections) + 1) : 0;
    u16 data_section_id  = data  ? (u16)((data  - image_sections) + 1) : 0;
    u16 bss_section_id   = bss   ? (u16)((bss   - image_sections) + 1) : 0;
    u16 rdata_section_id = rdata ? (u16)((rdata - image_sections) + 1) : 0;
    
    struct pdb_write_context *context = &(struct pdb_write_context)zero_struct;
    
    context->arena = arena;
    context->scratch = scratch;
    
    // the pdb format is formated in pages: first 3 pages are
    // 1) The PDB header
    // 2) The first free block map
    // 3) The second free block map
    
    // everything after that is data, organized is so called streams.
    // each stream has an entry in the _stream directory_.
    // The stream directory is a map of _stream indices_ to a list of pages.
    // the data associated to the stream is then serial in the pages associated to the stream.
    
    // the first 5 streams are fixed and given as follows:
    // stream 0: Old Directory
    // stream 1: PDB Stream
    // stream 2: TPI Stream
    // stream 3: DBI Stream (DBI DeBugInfo)
    // stream 4: IPI Stream
    
    
    // From the github: tho it seems wrong... this is what they think
    //    1        Pdb (header)                 Version information, and information to connect this PDB to the EXE
    //    2        Tpi (Type manager)           All the types used in the executable.
    //    3        Dbi (Debug information)      Holds section contributions, and list of Mods
    //    4        NameMap                      Holds a hashed string table
    //    4-(n+4)  n Mods (Module information) Each Mod stream holds symbols and line numbers for one compiland
    //    n+4      Global symbol hash           An index that allows searching in global symbols by name
    //    n+5      Public symbol hash           An index that allows searching in public symbols by addresses
    //    n+6      Symbol records               Actual symbol records of global and public symbols
    //    n+7      Type hash                    Hash used by the TPI stream.
    
    
    // the layout we choose corresponds to the 'stream_index' enum above.
    
    // the stream directory will in our case not be more than a page, but there is a double
    // indirection in case it would be.
    // pdb_header->page_number_of_directory_stream_number_list points to an array of pages.
    
    // this gives us the directory stream which follows the layout:
    // directory_stream:
    // u32 amount_of_streams;
    // u32 stream_sizes[amount_of_streams];
    // u32 stream_one_pages[];
    // u32 stream_two_pages[];
    // ...
    
    // Differance between TPI and IPI is only what appears in them. See this table:
    // TPI Stream    IPI Stream
    // LF_POINTER    LF_FUNC_ID
    // LF_MODIFIER   LF_MFUNC_ID
    // LF_PROCEDURE  LF_BUILDINFO
    // LF_MFUNCTION  LF_SUBSTR_LIST
    // LF_LABEL      LF_STRING_ID
    // LF_ARGLIST    LF_UDT_SRC_LINE
    // LF_FIELDLIST  LF_UDT_MOD_SRC_LINE
    // LF_ARRAY
    // LF_CLASS
    // LF_STRUCTURE
    // LF_INTERFACE
    // LF_UNION
    // LF_ENUM
    // LF_TYPESERVER2
    // LF_VFTABLE
    // LF_VTSHAPE
    // LF_BITFIELD
    // LF_METHODLIST
    // LF_PRECOMP
    // LF_ENDPRECOMP
    
    const u32 page_size = 0x1000;
    push_align(arena, page_size);
    context->pdb_base = arena_current(arena);
    
    struct pdb_header *pdb_header = push_struct(arena, struct pdb_header);
    context->pdb_header = pdb_header;
    {
        memcpy(pdb_header->signature, pdb_signature, sizeof(pdb_signature));
        pdb_header->page_size = page_size;
        pdb_header->free_page_map = 1; // has to be 1 or 2
        //pdb_header->number_of_file_pages;
        pdb_header->reserved = 0;
        //pdb_header->page_number_of_directory_stream_page_list = ???;
    }
    
    push_align(arena, page_size);
    
    struct memory_arena pdb_information_stream = create_memory_arena(mega_bytes(1), 2.0f, 0);
    {   // 
        // stream 1: PDB stream :pdb_stream
        // 
        
        struct pdb_information_stream_header{
            u32 version;
            u32 time_date_stamp;
            u32 age;
            u8 pdb_guid[16];
        } *pdb_information_stream_header = push_struct(&pdb_information_stream, struct pdb_information_stream_header);
        pdb_information_stream_header->version = 20000404;
        pdb_information_stream_header->time_date_stamp = coff_file_header->time_date_stamp;
        pdb_information_stream_header->age = 1;
        memcpy(pdb_information_stream_header->pdb_guid, pdb_guid, sizeof(pdb_guid));
        
        static struct {
            struct string stream_name;
            u32   stream_index;
        } named_streams[] = {
            { const_string("/names"), STREAM_names },
        };
        
        
        u32 named_stream_table_capacity = 2 * array_count(named_streams);
        struct named_stream_table_entry{
            u32 key;
            u32 value;
        } *named_stream_table_entries = push_data(scratch, struct named_stream_table_entry, named_stream_table_capacity);
        
        u32 *string_buffer_size = push_struct(&pdb_information_stream, u32);
        u8 *string_buffer = push_data(&pdb_information_stream, u8, 0);
        
        for(u32 named_stream_index = 0; named_stream_index < array_count(named_streams); named_stream_index++){
            
            struct string stream_name  = named_streams[named_stream_index].stream_name;
            u32   stream_index = named_streams[named_stream_index].stream_index;
            
            u16 hash = (u16)pdb_hash_index(stream_name.data, stream_name.size, (u32)-1);
            
            for(u32 hash_index = 0; hash_index < named_stream_table_capacity; hash_index++){
                
                u32 index = (hash_index + hash) % named_stream_table_capacity;
                
                // @note: Currently there are no deleted named steams. 
                //        So we don't have to care about tombstones!
                if(named_stream_table_entries[index].value == /*empty_slot*/0){
                    // We have found an empty slot.
                    
                    // Allocate the `stream_name` into the `string_buffer`.
                    u8 *stream_name_in_string_buffer = push_data(&pdb_information_stream, u8, stream_name.size + 1);
                    memcpy(stream_name_in_string_buffer, stream_name.data, stream_name.size + 1);
                    
                    named_stream_table_entries[index].key   = (u32)(stream_name_in_string_buffer - string_buffer);
                    named_stream_table_entries[index].value = stream_index;
                    break;
                }
            }
        }
        
        *string_buffer_size = (u32)(push_data(&pdb_information_stream, u8, 0) - string_buffer);
        
        // 
        // @WARNING: "Importantly, after the string table, the rest of the stream
        //            does not have any defined alignment anymore."
        // 
        
        /*amount_of_entries*/*push_struct_unaligned(&pdb_information_stream, u32) = array_count(named_streams);
        /*capacity         */*push_struct_unaligned(&pdb_information_stream, u32) = named_stream_table_capacity;
        
        u32 present_bits_word_count = ((named_stream_table_capacity + 31) & ~31)/32;
        
        /*present_bits.word_count*/*push_struct_unaligned(&pdb_information_stream, u32) = present_bits_word_count;
        
        u32 *present_bits_words = push_data_unaligned(&pdb_information_stream, u32, present_bits_word_count);
        for(u32 named_stream_table_index = 0; named_stream_table_index < named_stream_table_capacity; named_stream_table_index++){
            u32 word_index = named_stream_table_index / (sizeof(u32) * 8);
            u32 bit_index  = named_stream_table_index % (sizeof(u32) * 8);
            
            if(named_stream_table_entries[named_stream_table_index].value != 0){
                present_bits_words[word_index] |= (1u << bit_index);
            }
        }
        
        /*deleted_bits.word_count*/*push_struct_unaligned(&pdb_information_stream, u32) = 0;
        
        // struct { u32 key; u32 value; } entries[amount_of_entries];
        for(u32 named_stream_table_index = 0; named_stream_table_index < named_stream_table_capacity; named_stream_table_index++){
            if(named_stream_table_entries[named_stream_table_index].value != 0){
                *push_struct_unaligned(&pdb_information_stream, struct named_stream_table_entry) = named_stream_table_entries[named_stream_table_index];
            }
        }
        
        /*unused*/*push_struct_unaligned(&pdb_information_stream, u32) = 0;
        
        // Feature code: 
        *push_struct_unaligned(&pdb_information_stream, u32) = /*impvVC140*/20140508;
        
        set_current_stream(context, STREAM_PDB);
        stream_emit_struct(context, pdb_information_stream.base, arena_current(&pdb_information_stream) - pdb_information_stream.base);
    }
    
    context->maximal_amount_of_type_indices = max_of(0x1000, globals.compound_types.capacity);
    context->amount_of_type_indices = 0;
    context->type_index_to_type_info = push_data(scratch, struct pdb_type_info, context->maximal_amount_of_type_indices);
    
    context->type_stack_size = 0x100;
    context->type_stack = push_uninitialized_data(scratch, struct ast_type *, context->type_stack_size);
    
#define pdb_init_basic_type_index(type_name)\
{\
    globals.typedef_##type_name.pdb_type_index = CV_##type_name;\
    globals.typedef_##type_name.flags |= TYPE_FLAG_pdb_permanent;\
    context->type_index_to_type_info[CV_##type_name].type = &globals.typedef_##type_name;\
}
    pdb_init_basic_type_index(void);
    pdb_init_basic_type_index(Bool);
    pdb_init_basic_type_index(s8);
    pdb_init_basic_type_index(s16);
    pdb_init_basic_type_index(s32);
    pdb_init_basic_type_index(s64);
    pdb_init_basic_type_index(u8);
    pdb_init_basic_type_index(u16);
    pdb_init_basic_type_index(u32);
    pdb_init_basic_type_index(u64);
    pdb_init_basic_type_index(f32);
    pdb_init_basic_type_index(f64);
    
    globals.typedef_s8_pointer->pdb_type_index = CV_s8_pointer;
    globals.typedef_u8_pointer->pdb_type_index = CV_u8_pointer;
    context->type_index_to_type_info[CV_s8_pointer].type = globals.typedef_s8_pointer;
    context->type_index_to_type_info[CV_u8_pointer].type = globals.typedef_u8_pointer;
    globals.typedef_s8_pointer->flags |= TYPE_FLAG_pdb_permanent;
    globals.typedef_u8_pointer->flags |= TYPE_FLAG_pdb_permanent;
    
    
    context->type_index_to_type_info[CV_s8].pointer_type_index = CV_s8_pointer;
    context->type_index_to_type_info[CV_u8].pointer_type_index = CV_u8_pointer;
    
#undef pdb_init_basic_type_index
    
    context->index_offset_buffer_size = 0x100;
    context->index_offset_buffer = push_uninitialized_data(scratch, struct pdb_index_offset_buffer_entry, context->index_offset_buffer_size);
    context->index_offset_buffer_at = 0;
    context->index_offset_buffer_boundary = -1; // initialized to -1 so we always allocate one immediately
    
    begin_counter(timing, tpi_stream);
    
    struct index_stream_header{
        // We expect the version to be '20040203'.
        u32 version;
        
        // The size of this header.
        u32 header_size;
        
        //
        // The range of type indices present in this stream.
        //
        u32 minimal_type_index;
        u32 one_past_last_type_index;
        
        u32 byte_count_of_type_record_data_following_the_header;
        
        //
        // The stream index for the TPI/IPI hash stream.
        // The auxiliary stream seems to be unused.
        //
        u16 stream_index_of_hash_stream;
        u16 stream_index_of_auxiliary_hash_stream;
        
        //
        // The hash key size and the number of buckets used for the incremental linking table below.
        //
        u32 hash_key_size;
        u32 number_of_hash_buckets;
        
        //
        // The 'hash key buffer' is contained within the TPI/IPI hash stream.
        // The size of the buffer should be '(maximal_type_index - minimal_type_index) * hash_key_size'.
        // These hash keys are used in the incremental linking hash table below.
        //
        u32 hash_table_index_buffer_offset;
        u32 hash_table_index_buffer_length;
        
        //
        // The 'index offset buffer' is an array of 'struct { u32 type_index; u32 offset_in_stream; }'.
        // The offset of each entry increases by about 8 kb each entry.
        // This buffer is intended for binary searching by type index, to get a rough (8kb accurate) offset
        // to the type, and from there one can search linearly to find the type record.
        //
        u32 index_offset_buffer_offset;
        u32 index_offset_buffer_length;
        
        // 
        // The 'udt_order_adjust_table' is used to adjust the order of entries inside 
        // of a collision chain of the hash table above. This is useful for types
        // which have been altered, but then the change was reverted.
        // 
        u32 udt_order_adjust_table_offset;
        u32 udt_order_adjust_table_length;
    } *tpi_stream_header = null, *ipi_stream_header = null;
    
    
#define TPI_NUMBER_OF_HASH_BUCKETS (0x40000 - 1)
    
    struct memory_arena stack_arena = create_memory_arena(giga_bytes(8), 2.0f, kilo_bytes(10));
    
    struct memory_arena tpi_stream = create_memory_arena(giga_bytes(4), 2.0f, 0);
    struct memory_arena tpi_hash_stream = create_memory_arena(giga_bytes(4), 2.0f, 0);
    {   // 
        // stream 2: TPI stream - The TPI stream describes type information.  :tpi_stream
        // 
        
        tpi_stream_header = push_struct(&tpi_stream, struct index_stream_header);
        tpi_stream_header->version = 20040203;
        tpi_stream_header->header_size = sizeof(*tpi_stream_header);
        tpi_stream_header->minimal_type_index = 0x1000;
        tpi_stream_header->one_past_last_type_index = register_all_types(&tpi_stream, scratch, &stack_arena, defined_functions);
        tpi_stream_header->byte_count_of_type_record_data_following_the_header = (u32)(arena_current(&tpi_stream) - (u8 *)(tpi_stream_header + 1));
        
        tpi_stream_header->stream_index_of_auxiliary_hash_stream = (u16)-1;
        tpi_stream_header->stream_index_of_hash_stream = STREAM_TPI_hash;
        
        tpi_stream_header->hash_key_size = sizeof(u32);
        tpi_stream_header->number_of_hash_buckets = TPI_NUMBER_OF_HASH_BUCKETS;
        
        {   //
            // stream 7: TPI hash   :tpi_hash_stream
            // 
            
            u8 *type_record_start = (u8 *)(tpi_stream_header + 1);
            
            u32 amount_of_type_indices = tpi_stream_header->one_past_last_type_index - tpi_stream_header->minimal_type_index;
            u32 *type_index_buffer = push_uninitialized_data(&tpi_hash_stream, u32, amount_of_type_indices);
            
            tpi_stream_header->hash_table_index_buffer_offset = 0;
            tpi_stream_header->hash_table_index_buffer_length = amount_of_type_indices * sizeof(u32);
            
            struct index_offset_buffer_entry{
                u32 type_index;
                u32 offset_in_record_data;
            } *index_offset_buffer = push_struct(&tpi_hash_stream, struct index_offset_buffer_entry);
            index_offset_buffer->type_index = tpi_stream_header->minimal_type_index;
            
            struct index_offset_buffer_entry *last_entry = index_offset_buffer;
            
            for(u32 unbiased_type_index = 0, type_record_offset = 0; unbiased_type_index < amount_of_type_indices; unbiased_type_index += 1){
                
                // hashing a type_record: (LF_STRUCTURE, LF_UNION, LF_ENUM):
                // if(!forward_ref && !is_anonymous){
                //    if(!scoped || has_unique_name){
                //        return pdb_string_hash(name);
                //    }
                // }
                // return string CRC32(serialized);
                
                assert((type_record_offset & 3) == 0);
                struct codeview_type_record_header *tpi_record_header = (void *)(type_record_start + type_record_offset);
                u32 type_record_size = tpi_record_header->length + sizeof(tpi_record_header->length);
                
                type_index_buffer[unbiased_type_index] = tpi_hash_table_index_for_record(tpi_record_header, tpi_stream_header->number_of_hash_buckets);
                
                if(type_record_offset + type_record_size >= last_entry->offset_in_record_data + 8 * 0x1000){
                    last_entry = push_struct(&tpi_hash_stream, struct index_offset_buffer_entry);
                    last_entry->offset_in_record_data = type_record_offset;
                    last_entry->type_index = unbiased_type_index + tpi_stream_header->minimal_type_index;
                }
                
                type_record_offset += type_record_size;
            }
            
            tpi_stream_header->index_offset_buffer_offset = tpi_stream_header->hash_table_index_buffer_length;
            tpi_stream_header->index_offset_buffer_length = (u32)((u8 *)(last_entry + 1) - (u8 *)index_offset_buffer);
            
            set_current_stream(context, STREAM_TPI_hash);
            stream_emit_struct(context, tpi_hash_stream.base, arena_current(&tpi_hash_stream) - tpi_hash_stream.base);
        }
        
        tpi_stream_header->udt_order_adjust_table_offset = tpi_stream_header->index_offset_buffer_offset + tpi_stream_header->index_offset_buffer_length;
        tpi_stream_header->udt_order_adjust_table_length = 0;
        
        set_current_stream(context, STREAM_TPI);
        stream_emit_struct(context, tpi_stream.base, arena_current(&tpi_stream) - tpi_stream.base);
    }
    
    struct memory_arena names_stream = create_memory_arena(giga_bytes(4), 2.0f, 0);
    
    // stream 5: Names
    { // :names_stream /names
        
        struct names_stream_header{
            u32 signature;
            u32 hash_version;
            u32 string_buffer_byte_size;
            char string_buffer[];
        } *names_stream_header = push_struct(&names_stream, struct names_stream_header);
        names_stream_header->signature = 0xEFFEEFFE;
        names_stream_header->hash_version = 1;
        
        u8 *string_buffer_start = arena_current(&names_stream);
        
        // "The first string inside the string buffer always has to be the zero-sized string, 
        //  as a zero offset is also used as an invalid offset in the hash table."
        push_struct(&names_stream, u8); // zero-sized string!
        
        smm string_count = 0;
        
        for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
            struct file *node = globals.file_table.data[file_index];
            if(!node) continue;
            
            // Patch up the string to have windows slashes.
            // This is needed for WinDbg to work correctly.
            smm size = 0;
            for(char *c = node->absolute_file_path; *c; c++){
                if(*c == '/') *c = '\\';
                size += 1;
            }
            
            // 
            // Push the string to the string buffer.
            // 
            string_count++;
            node->offset_in_names = (u32)(arena_current(&names_stream) - string_buffer_start);
            
            u8 *name = push_data(&names_stream, u8, size);
            memcpy(name, node->absolute_file_path, size);
            *push_struct(&names_stream, u8) = 0;
        }
        
        u8 *string_buffer_end = arena_current(&names_stream);
        names_stream_header->string_buffer_byte_size = (u32)(string_buffer_end - string_buffer_start);
        
        // 
        // After the string buffer we have a serialized hash table:
        //     string_hash -> offset in the string buffer
        //     
        u32 bucket_count = to_u32(2 * string_count);
        *push_struct_unaligned(&names_stream, u32) = bucket_count; // bucket count
        
        // Write in the buckets.
        u32 *buckets = push_data_unaligned(&names_stream, u32, bucket_count);
        
        for(u8 *string = string_buffer_start + 1; string < string_buffer_end; ){
            struct string s = string_from_cstring((char *)string);
            
            u32 hash = pdb_string_hash(s);
            for(u32 table_index = 0; table_index < bucket_count; table_index++){
                u32 hash_index = (hash + table_index) % bucket_count;
                if(buckets[hash_index] != 0) continue;
                
                buckets[hash_index] = (u32)(string - string_buffer_start);
                break;
            }
            
            string += s.size + 1;
        }
        
        // at the end is the amount of strings
        *push_struct_unaligned(&names_stream, u32) = (u32)string_count;
        
        set_current_stream(context, STREAM_names);
        stream_emit_struct(context, names_stream.base, arena_current(&names_stream) - names_stream.base);
    }
    
    
    // reset the 'context->index_offset_buffer'
    context->index_offset_buffer_boundary = -1;
    context->index_offset_buffer_at = 0;
    
    u32 build_info_symbol;
    // stream 4: IPI stream
    
    struct memory_arena ipi_stream = create_memory_arena(giga_bytes(4), 2.0f, 0);
    struct memory_arena ipi_hash_stream = create_memory_arena(giga_bytes(4), 2.0f, 0);
    
    { // :ipi_stream
        // the IPI Stream describes _id_ information
        
        // @note: For more detail see the tpi-Stream, which is structured exactly the same, the only difference
        //        is what kind of symbols they contain.
        
        ipi_stream_header = push_struct(&ipi_stream, struct index_stream_header);
        ipi_stream_header->version = 20040203;
        ipi_stream_header->header_size = sizeof(*ipi_stream_header);
        ipi_stream_header->minimal_type_index = 0x1000;
        ipi_stream_header->stream_index_of_auxiliary_hash_stream = (u16)-1;
        ipi_stream_header->stream_index_of_hash_stream = STREAM_IPI_hash;
        
        ipi_stream_header->hash_key_size = sizeof(u32);
        ipi_stream_header->number_of_hash_buckets = TPI_NUMBER_OF_HASH_BUCKETS;
        
        // 
        // @cleanup: Should we share code here with the obj_writer?
        // 
        
        // 
        // After the header there is a list of variable sized id records.
        // 
        // For source files: LF_STRING_ID
        // For functions: LF_FUNC_ID
        // For compound types: LF_UDT_MOD_SRC_LINE
        // For typedefs: LF_UDT_MOD_SRC_LINE
        // 
        // And then a build info structure.
        // 
        
        
        // Convenience macros to handle type index creation
#define begin_id_record(record_kind) { current_id_record = push_struct(&ipi_stream, struct codeview_type_record_header); current_id_record->kind = (record_kind); }
#define end_id_record() { id_index_at++; push_f3f2f1_align(&ipi_stream, sizeof(u32)); current_id_record->length = to_u16(arena_current(&ipi_stream) - (u8 *)&current_id_record->kind); }
        
        struct codeview_type_record_header *current_id_record;
        u32 id_index_at = 0x1000;
        
        // 
        // One LF_STRING_ID per source file.
        // 
        
        u32 main_file_ipi = 0;
        
        for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
            struct file *node = globals.file_table.data[file_index];
            if(!node) continue;
            
            if(node == globals.compilation_units.first->main_file) main_file_ipi = id_index_at;
            
            begin_id_record(0x1605); // LF_STRING_ID
            
            *push_struct(&ipi_stream, u32) = 0; // "ID to list of sub-string IDs"
            push_zero_terminated_string_copy(&ipi_stream, string_from_cstring(node->absolute_file_path));
            
            end_id_record();
        }
        
        // 
        // One LF_UDT_MOD_SRC_LINE for every compound type.
        // 
        struct codeview_udt_mod_src_line{
            u32 type_index;
            u32 file_name_offset_in_names;
            u32 line_number;
            u16 module_index;
        };
        
        for(u64 table_index = 0; table_index < globals.compound_types.capacity; table_index++){
            struct ast_node *node = globals.compound_types.nodes + table_index;
            if(!node->token) continue;
            struct ast_compound_type *type = (struct ast_compound_type *)node->ast;
            
            assert(type->identifier->file_index != -1); // In the future maybe there will be predefined compounds, not sure.
            struct file *file = globals.file_table.data[type->identifier->file_index];
            
            begin_id_record(0x1607);  // LF_UDT_MOD_SRC_LINE
            
            struct codeview_udt_mod_src_line *udt_mod_src_line = push_struct_(&ipi_stream, sizeof(struct codeview_udt_mod_src_line) - 2, 1);
            udt_mod_src_line->type_index = type->base.pdb_type_index;
            udt_mod_src_line->file_name_offset_in_names = file->offset_in_names;
            udt_mod_src_line->line_number = type->identifier->line;
            udt_mod_src_line->module_index = 1; // type->compilation_unit->index + 1;
            
            end_id_record();  // LF_UDT_MOD_SRC_LINE
        }
        
        // 
        // One LF_UDT_MOD_SRC_LINE for every typedef.
        // 
        for_ast_list(typedefs){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            struct ast_type *type = decl->type;
            
            struct file *file = globals.file_table.data[decl->identifier->file_index];
            
            begin_id_record(0x1607);  // LF_UDT_MOD_SRC_LINE
            
            struct codeview_udt_mod_src_line *udt_mod_src_line = push_struct_(&ipi_stream, sizeof(struct codeview_udt_mod_src_line) - 2, 1);
            udt_mod_src_line->type_index = type->pdb_type_index;
            udt_mod_src_line->file_name_offset_in_names = file->offset_in_names;
            udt_mod_src_line->line_number = decl->identifier->line;
            udt_mod_src_line->module_index = 1; // decl->compilation_unit->index + 1;
            
            end_id_record();  // LF_UDT_MOD_SRC_LINE
        }
        
        // 
        // One LF_FUNC_ID for every defined function.
        // 
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            begin_id_record(0x1601); // LF_FUNC_ID
            
            *push_struct(&ipi_stream, u32) = /*parent scope*/0; // @incomplete: look at a language with local procedures.
            *push_struct(&ipi_stream, u32) = function->type->base.pdb_type_index;
            push_zero_terminated_string_copy(&ipi_stream, function->identifier->string);
            
            end_id_record();
        }
        
        // 
        // One LF_BUILDINFO containing a bunch of information.
        // 
        
        {
            u32 working_directiory_symbol = id_index_at;
            {
                begin_id_record(0x1605); // LF_STRING_ID
                
                *push_struct(&ipi_stream, u32) = 0; // "ID to list of sub-string IDs"
                
                // GetCurrentDirectory with 0 returns the size of the buffer including the null terminator
                smm CurrentWorkingDirectoryLength = GetCurrentDirectoryA(0, null); // @cleanup: What about utf-8?
                u8 *CurrentWorkingDirectory = push_uninitialized_data(&ipi_stream, u8, CurrentWorkingDirectoryLength);
                GetCurrentDirectoryA((DWORD)CurrentWorkingDirectoryLength, CurrentWorkingDirectory);
                
                end_id_record();
            }
            
            u32 compiler_name_symbol = id_index_at;
            {
                begin_id_record(0x1605); // LF_STRING_ID
                
                *push_struct(&ipi_stream, u32) = 0; // "ID to list of sub-string IDs"
                
                char *ModuleFileName = push_uninitialized_data(&ipi_stream, char, MAX_PATH + 1);
                smm ModuleFileNameLength = GetModuleFileNameA(null, ModuleFileName, MAX_PATH + 1); // @cleanup: What about utf-8?
                ModuleFileName[ModuleFileNameLength] = 0;
                ipi_stream.current -= (MAX_PATH + 1) - ModuleFileNameLength;
                
                end_id_record();
            }
            
            u32 pdb_symbol = id_index_at;
            {
                begin_id_record(0x1605); // LF_STRING_ID
                
                *push_struct(&ipi_stream, u32) = 0; // "ID to list of sub-string IDs"
                push_zero_terminated_string_copy(&ipi_stream, string("vc140.pdb")); // Not sure why this is here.
                
                end_id_record();
            }
            
            u32 command_line_symbol = id_index_at;
            {
                begin_id_record(0x1605); // LF_STRING_ID
                
                *push_struct(&ipi_stream, u32) = 0; // "ID to list of sub-string IDs"
                push_zero_terminated_string_copy(&ipi_stream, string_from_cstring(GetCommandLineA())); // @cleanup: What about utf-8?
                
                end_id_record();
            }
            
            build_info_symbol = id_index_at;
            {
                begin_id_record(0x1603); // LF_BUILDINFO
                
                *push_struct(&ipi_stream, u16) = 5; // Count
                *push_struct_unaligned(&ipi_stream, u32) = working_directiory_symbol;
                *push_struct_unaligned(&ipi_stream, u32) = compiler_name_symbol;
                *push_struct_unaligned(&ipi_stream, u32) = main_file_ipi;
                *push_struct_unaligned(&ipi_stream, u32) = pdb_symbol;
                *push_struct_unaligned(&ipi_stream, u32) = command_line_symbol;
                
                end_id_record();
            }
        }
        
#undef begin_id_record
#undef end_id_record
        
        ipi_stream_header->byte_count_of_type_record_data_following_the_header = (u32)(arena_current(&ipi_stream) - (u8 *)(ipi_stream_header + 1));
        ipi_stream_header->one_past_last_type_index = id_index_at;
        
        {   //
            // stream 7: IPI hash   :ipi_hash_stream
            // 
            
            u8 *id_record_start = (u8 *)(ipi_stream_header + 1);
            
            u32 amount_of_id_indices = ipi_stream_header->one_past_last_type_index - ipi_stream_header->minimal_type_index;
            u32 *type_index_buffer = push_uninitialized_data(&ipi_hash_stream, u32, amount_of_id_indices);
            
            ipi_stream_header->hash_table_index_buffer_offset = 0;
            ipi_stream_header->hash_table_index_buffer_length = amount_of_id_indices * sizeof(u32);
            
            struct index_offset_buffer_entry{
                u32 type_index;
                u32 offset_in_record_data;
            } *index_offset_buffer = push_struct(&ipi_hash_stream, struct index_offset_buffer_entry);
            index_offset_buffer->type_index = ipi_stream_header->minimal_type_index;
            
            struct index_offset_buffer_entry *last_entry = index_offset_buffer;
            
            for(u32 unbiased_id_index = 0, id_record_offset = 0; unbiased_id_index < amount_of_id_indices; unbiased_id_index += 1){
                
                // hashing a type_record: (LF_STRUCTURE, LF_UNION, LF_ENUM):
                // if(!forward_ref && !is_anonymous){
                //    if(!scoped || has_unique_name){
                //        return pdb_string_hash(name);
                //    }
                // }
                // return string CRC32(serialized);
                
                assert((id_record_offset & 3) == 0);
                struct codeview_type_record_header *ipi_record_header = (void *)(id_record_start + id_record_offset);
                u32 id_record_size = ipi_record_header->length + sizeof(ipi_record_header->length);
                
                type_index_buffer[unbiased_id_index] = tpi_hash_table_index_for_record(ipi_record_header, ipi_stream_header->number_of_hash_buckets);
                
                if(id_record_offset + id_record_size >= last_entry->offset_in_record_data + 8 * 0x1000){
                    last_entry = push_struct(&ipi_hash_stream, struct index_offset_buffer_entry);
                    last_entry->offset_in_record_data = id_record_offset;
                    last_entry->type_index = unbiased_id_index + ipi_stream_header->minimal_type_index;
                }
                
                id_record_offset += id_record_size;
            }
            
            ipi_stream_header->index_offset_buffer_offset = ipi_stream_header->hash_table_index_buffer_length;
            ipi_stream_header->index_offset_buffer_length = (u32)((u8 *)(last_entry + 1) - (u8 *)index_offset_buffer);
            
            set_current_stream(context, STREAM_IPI_hash);
            stream_emit_struct(context, ipi_hash_stream.base, arena_current(&ipi_hash_stream) - ipi_hash_stream.base);
        }
        
        set_current_stream(context, STREAM_IPI);
        stream_emit_struct(context, ipi_stream.base, arena_current(&ipi_stream) - ipi_stream.base);
    }
    
    // 
    // We for now just have a single module stream.
    // I don't really see a reason to have multiple.
    // And we don't have to duplicate things like file tables if we only have one.
    // 
    
    u32 module_stream_symbol_size;
    u32 module_stream_line_info_size;
    
    struct string module_object_name_full_path = push_format_string(scratch, "%.*s.obj", root_file_name.size, root_file_name.data);
    replace_characters(module_object_name_full_path, "/", '\\');
    
    struct memory_arena module_stream = create_memory_arena(giga_bytes(4), 2.0f, 0);
    
    {   // Module streams :module_streams
        // 
        // Module stream layout:
        // 
        //     u32 signature;
        //     u8  symbols[symbol_byte_size - 4];
        //     u8  line_information[line_info_byte_size];
        //     u32 global_refs_byte_size;
        //     u8  global_refs[global_refs_byte_size];
        //     
        
        *push_struct(&module_stream, u32) = /*CV_SIGNATURE_C13*/4;
        
#define begin_symbol_record(record_kind) { current_symbol_record = push_struct(&module_stream, struct codeview_type_record_header); current_symbol_record->kind = (record_kind); }
#define end_symbol_record() { push_f3f2f1_align(&module_stream, sizeof(u32)); current_symbol_record->length = to_u16(arena_current(&module_stream) - (u8 *)&current_symbol_record->kind); }
        
        struct codeview_type_record_header *current_symbol_record;
        
        // 
        // First thing is always the S_OBJNAME, then the S_COMPILE3.
        // 
        begin_symbol_record(0x1101); // S_OBJNAME
        *push_struct(&module_stream, u32) = 0; // Signature (?)
        push_zero_terminated_string_copy(&module_stream, module_object_name_full_path);
        end_symbol_record();
        
        
        begin_symbol_record(0x113c); // S_COMPILE3
        
        struct codeview_compile3{
            u32 flags;
            u16 machine;
            
            u16 front_end_major_version;
            u16 front_end_minor_version;
            u16 front_end_build_version;
            u16 front_end_QFE_version;
            
            u16 back_end_major_version;
            u16 back_end_minor_version;
            u16 back_end_build_version;
            u16 back_end_QFE_version;
            
            char compiler_version_string[];
        } *compile3 = push_struct(&module_stream, struct codeview_compile3);
        compile3->flags   = 0;    // 0 means C
        compile3->machine = 0xd0; // machine = x64 <- This is used by cvdump.
        
        // @cleanup: Not sure what to do about these, but I am also not sure that anyone cares.
        compile3->front_end_major_version = 19;
        compile3->front_end_minor_version = 11;
        compile3->front_end_build_version = 25506;
        compile3->front_end_QFE_version = 0;
        
        compile3->back_end_major_version = 19;
        compile3->back_end_minor_version = 11;
        compile3->back_end_build_version = 25506;
        compile3->back_end_QFE_version = 0;
        
        push_zero_terminated_string_copy(&module_stream, string("hlc")); // @cleanup: When we have a version system thing, we should probably put it in here.
        
        end_symbol_record();
        
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            function->debug_symbol_offset = (u32)(arena_current(&module_stream) - module_stream.base);
            
            u16 function_symbol_kind = (function->decl_flags & DECLARATION_FLAGS_is_static) ? /* LPROC32 */ 0x110f: /* GPROC32 */ 0x1110;
            
            begin_symbol_record(function_symbol_kind);
            
            struct codeview_proc{
                u32 pointer_to_parent;
                u32 pointer_to_end;
                u32 pointer_to_next;
                u32 procedure_length;
                u32 debug_start_offset;
                u32 debug_end_offset;
                u32 type_index;
                u32 offset_in_section;
                u16 section_id;
                u8 procedure_flags;
                u8 procedure_name[];
            } *proc_symbol = push_struct_(&module_stream, offset_in_type(struct codeview_proc, procedure_name) + (function->identifier->length + 1), 4);
            proc_symbol->pointer_to_parent  = 0;
            proc_symbol->pointer_to_end     = 0;
            proc_symbol->pointer_to_next    = 0;
            proc_symbol->procedure_length   = (u32)function->byte_size;
            proc_symbol->debug_start_offset = (u32)function->size_of_prolog;
            proc_symbol->debug_end_offset   = (u32)function->byte_size;
            proc_symbol->type_index         = function->type->base.pdb_type_index;
            proc_symbol->offset_in_section  = (u32)function->offset_in_text_section;
            proc_symbol->section_id         = text_section_id;
            proc_symbol->procedure_flags    = 0; // Flags, somehow never present.
            memcpy(proc_symbol->procedure_name, function->identifier->data, function->identifier->length);
            proc_symbol->procedure_name[function->identifier->length] = 0;
            
            end_symbol_record();
            
            // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
            
            codeview_emit_debug_info_for_function(function, &module_stream, /*relocation_arena (used by obj_writer.c)*/null, module_stream.base, text_section_id);
            
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            
            proc_symbol->pointer_to_end = (u32)(arena_current(&module_stream) - module_stream.base);
            
            begin_symbol_record(0x6); end_symbol_record(); // S_END
        }
        
        begin_symbol_record(0x114c); // S_BUILDINFO
        *push_struct(&module_stream, u32) = build_info_symbol; // Item Id: type_index of a LF_BUILDINFO in the IPI stream
        end_symbol_record();
        
        module_stream_symbol_size = (u32)(arena_current(&module_stream) - module_stream.base);
        
        u8 *line_information_start = arena_current(&module_stream);
        
        codeview_push_debug_s_file_checksums(&module_stream);
        
        // now comes the line info
        for_ast_list(defined_functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            codeview_push_debug_s_lines(function, &module_stream, /*relocation_arena (used by obj_writer.c)*/null, /*debug_symbol_base (used by obj_writer.c)*/null, text_section_id);
        }
        
        
        module_stream_line_info_size = (u32)(arena_current(&module_stream) - line_information_start);
        
        *push_struct(&module_stream, u32) = 0; // Amount of global references.
        
        set_current_stream(context, STREAM_module_zero);
        stream_emit_struct(context, module_stream.base, arena_current(&module_stream) - module_stream.base);
    }
    end_counter(timing, module_stream);
    
    begin_counter(timing, dbi_stream);
    // Stream 3: Debug Info stream
    { // :dbi_stream
        set_current_stream(context, STREAM_DBI);
        struct dbi_stream dbi = zero_struct;
        dbi.version = 19990903;
        dbi.version_signature = (u32)-1;
        dbi.amount_of_times_the_pdb_has_been_written = 1;
        
        dbi.index_of_the_global_symbol_stream = STREAM_global_symbol_hash;
        dbi.index_of_the_public_symbol_stream = STREAM_public_symbol_hash;
        dbi.index_of_the_symbol_record_stream = STREAM_symbol_records;
        
        dbi.toolchain_version = (11 << 0) | (14 << 8) | (1 << 15);
        //dbi.toolchain_version.major_version = 11;
        //dbi.toolchain_version.minor_version = 14;
        //dbi.toolchain_version.is_new_version_format = 1;
        
        // @cleanup:
        dbi.version_number_of_mspdb = 25506; // I don't know. copied from a pdb
        dbi.PdbDllRbld = 0;
        dbi.flags = 0;
        
        dbi.machine = 0x8664;
        
        struct pdb_location header_location = stream_allocate_bytes(context, sizeof(dbi));
        
        struct pdb_location module_info_begin = get_current_pdb_location(context);
        
        { // module info substream
            // @incomplete: these are just _filled in_ for now
            
            // @cleanup: this should not be invalid, or at least is not in the reference pdb's
            
            struct dbi_module_info module = zero_struct;
            module.module_symbol_stream_index = STREAM_module_zero;
            module.amount_of_source_files = (u16)globals.file_table.size;
            
            const struct section_contribution_entry invalid_section_contribution_entry = {
                .section_id = -1,
                .offset = 0,
                .size = -1,
                .characteristics = 0x0,
                .module_index = -1,
                .data_crc = 0,
                .reloc_crc = 0,
            };
            
            if(text){
                struct section_contribution_entry text_entry = {
                    .section_id = text_section_id,
                    .offset = 0,
                    .size = text->virtual_size,
                    .module_index = 0,
                    .characteristics = text->characteristics,
                    .data_crc = 0, // @cleanup:
                    .reloc_crc = 0,
                };
                module.first_section_contribution_entry = text_entry;
            }else{
                module.first_section_contribution_entry = invalid_section_contribution_entry;
            }
            module.byte_size_of_symbol_information = module_stream_symbol_size;
            module.byte_size_of_c13_line_information = module_stream_line_info_size;
            out_struct(module);
            
            // @incomplete:
            out_struct("l:\\l++\\build\\test.obj");
            out_struct("l:\\l++\\build\\test.obj");
            out_align(sizeof(u32));
        }
        
        struct pdb_location section_contribution_begin = get_current_pdb_location(context);
        
        {// section contribution substream
            out_int(0xeffe0000 + 19970605, u32);
            for(u32 i = 0; i < coff_file_header->amount_of_sections; i++){
                struct coff_section_header *header = &image_sections[i];
                
                // @cleanup: right now we just make one contribution per section and say it was module 0
                struct section_contribution_entry entry = {
                    .section_id = (s16)(i + 1),
                    .offset = 0,
                    .size = header->virtual_size,
                    .module_index = 0,
                    .characteristics = header->characteristics,
                    .data_crc = 0, // @cleanup:
                    .reloc_crc = 0,
                };
                
                stream_emit_struct(context, &entry, sizeof(entry));
            }
        }
        
        struct pdb_location section_map_begin = get_current_pdb_location(context);
        {  // section map substream
            
            // @note: I will note that I have no idear why this is here or what it does.
            // referance: DbiStreamBuilder::createSectionMap
            
            // write the header @note: these are always the same in llvm
            struct pdb_location number_of_segment_descriptors = stream_allocate_bytes(context, sizeof(u16));
            struct pdb_location number_of_logical_segment_descriptors = stream_allocate_bytes(context, sizeof(u16));
            
            // write a section map entry for each section.
            u32 frame_index = 1;
            for(u32 i = 0; i < coff_file_header->amount_of_sections; i++){
                struct coff_section_header *header = &image_sections[i];
                
                u32 characteristics = header->characteristics;
                u16 flags = (1 << 3) | (1 << 8);
                if(characteristics & SECTION_read)    flags |= (1 << 0);
                if(characteristics & SECTION_write)   flags |= (1 << 1);
                if(characteristics & SECTION_execute) flags |= (1 << 2);
                // @note: llvm checked reserved flag IMGAE_SCN_MEM_16BIT here
                // and only added (1 << 3) if it was not set.. I don't know the flag, so I probably wont set it
                
                out_int(flags,         u16);
                out_int(0,             u16); // Ovl   ??
                out_int(0,             u16); // group ??
                out_int(frame_index++, u16); // Frame ??
                out_int(u16_max,       u16); // SecName   (Meaning Unknown)
                out_int(u16_max,       u16); // ClassName (Meaning Unknown)
                out_int(0,             u32); // offset
                out_int(header->virtual_size, u32);
            }
            
            // finally write one dummy entry that "is for absolute symbols" what ever that means
            // (1 << 9) is apperantly for IsAbsoluteAddress
            u16 flags = (1 << 3) | (1 << 9);
            out_int(flags, u16);
            out_int(0, u16);           // Ovl
            out_int(0, u16);           // group
            out_int(frame_index, u16); // Frame
            out_int(u16_max, u16);     // SecName   (Meaning Unknown)
            out_int(u16_max, u16);     // ClassName (Meaning Unknown)
            out_int(0, u32);           // offset
            out_int(u32_max, u32);
            
            // @cleanup: why are these initialized to the same value?
            stream_write_bytes(context, &number_of_segment_descriptors,         &frame_index, sizeof(u16));
            stream_write_bytes(context, &number_of_logical_segment_descriptors, &frame_index, sizeof(u16));
        }
        
        
        struct pdb_location source_info_begin = get_current_pdb_location(context);
        { // source info substream
            
            // the layout is as follows:
            //     u16 amount_of_modules;
            //     u16 ignored_amount_of_source_files;
            //     u16 module_indices[amount_of_modules];
            //     u16 module_to_source_file_count_map[amount_of_modules];
            //     u32 source_file_name_offset_in_the_name_buffer[amount_of_source_files];
            //     char name_buffer[][NumSourceFiles];
            
            // the "ignored_amount_of_source_files" is ignored as this would limit the amount of source files to max_u16, but we should still try to approximate it the best
            // the actuall amount_of_source_files is the sum over module_to_source_file_count_map.
            // module indices seem to be  [0, ... , amount_of_modules - 1]
            
            u16 amount_of_modules = 1; // @cleanup: Right now there are only two... not sure
            out_int(amount_of_modules, u16);
            u16 amount_of_source_files_u16 = (u16)min_of(globals.file_table.size, u16_max);
            out_int(amount_of_source_files_u16, u16);
            
            // @cleanup: right now we know that "amount_of_modules" is 2 so we will just write this stuff out
            //           later we want "module_infos" to be in an array, so we can iterate over them
            
            out_int(0, u16); // index of module 0
            
            out_int(globals.file_table.size, u16); // amount_of_source_files for module 0
            
            smm at = 0;
            for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
                struct file *node = globals.file_table.data[file_index];
                if(!node) continue;
                
                out_int(at, u32); // offset of the one and only source file
                at += cstring_length(node->absolute_file_path) + 1;
            }
            for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
                struct file *node = globals.file_table.data[file_index];
                if(!node) continue;
                
                out_string(string_from_cstring(node->absolute_file_path));
            }
            
            out_align(sizeof(u32));
        }
        
        //struct pdb_location type_server_begin = get_current_pdb_location(context);
        dbi.type_server_map_substream_byte_size = 0;
        {// type server substream (0 bytes)
        }
        
        struct pdb_location edit_and_continue_begin = get_current_pdb_location(context);
        #if 0
        {// edit and continue substream
            // this stream has the same layout as the /names stream, but only has the pdb name in it
            out_int(0xEFFEEFFE, u32); // signature
            out_int(1, u32);          // hash version
            struct pdb_location string_buffer_size = stream_allocate_bytes(context, sizeof(u32));
            
            struct pdb_location string_buffer_start = get_current_pdb_location(context);
            {
                out_int(0, u8);
                // @incomplete: for source files write them out.
                // @cleanup: at least make this dependent on the actual file
                out_string(pdb_full_path);
            }
            struct pdb_location string_buffer_end = get_current_pdb_location(context);
            u32 size = pdb_location_diff(string_buffer_end, string_buffer_start);
            stream_write_bytes(context, &string_buffer_size, &size, sizeof(u32));
            
            // @note this whole thing is copy and pastes from the /names string
            // see that string for more information
            out_int(1, u32); // bucket count
            out_int(1, u32); // offset
            out_int(1, u32); // amount of strings
        }
        #endif
        
        struct pdb_location optional_debug_header_begin = get_current_pdb_location(context);
        { // optional debug header substream
            // @note: I have no idea what any of these are... These meanings are mearly copied
            out_int(-1, s16); // no FPO data
            out_int(-1, s16); // no Exception data
            out_int(-1, s16); // no Fixup data
            out_int(-1, s16); // no Omap to source data
            out_int(-1, s16); // no Omap from source data
            out_int(STREAM_section_header_dump, s16); // section header dump stream
            out_int(-1, s16); // no Token / RID map
            out_int(-1, s16); // no xdata dump
            out_int(-1, s16); // no pdata dump
            out_int(-1, s16); // no new fpo data
            out_int(-1, s16); // no original section header data
        }
        
        struct pdb_location dbi_end = get_current_pdb_location(context);
        
        // substream 0
        dbi.module_info_substream_byte_size = pdb_location_diff(section_contribution_begin, module_info_begin);
        // substream 1
        dbi.section_contribution_substream_byte_size = pdb_location_diff(section_map_begin, section_contribution_begin);
        // substream 2
        dbi.section_map_substream_byte_size = pdb_location_diff(source_info_begin, section_map_begin);
        // substream 3
        dbi.source_info_substream_byte_size = pdb_location_diff(edit_and_continue_begin, source_info_begin);
        // substream 4
        // empty dealt with above
        // substream 5
        dbi.edit_and_continue_substream_byte_size = pdb_location_diff(optional_debug_header_begin, edit_and_continue_begin);
        // substream 6
        dbi.optional_debug_header_substream_byte_size = pdb_location_diff(dbi_end, optional_debug_header_begin);
        
        stream_write_bytes(context, &header_location, &dbi, sizeof(dbi));
    }
    
    // :gsi_hash_table
    struct gsi_hash_bucket **gsi_hash_table = push_data(scratch, struct gsi_hash_bucket *, IPHR_HASH + 1);
    smm gsi_amount_of_bucket_offsets = 0;
    smm gsi_amount_of_global_symbols = 0;
    
    // :psi_hash_table
    struct gsi_hash_bucket **psi_hash_table = push_data(scratch, struct gsi_hash_bucket *, IPHR_HASH + 1);
    smm psi_amount_of_bucket_offsets = 0;
    smm psi_amount_of_global_symbols = 0;
    
    // stream 9: Symbol record stream
    { // :symbol_records :symbol_stream
        set_current_stream(context, STREAM_symbol_records);
        
        struct pdb_location symbol_record_start = get_current_pdb_location(context);
        
        // Members generated by the linker:
        //     S_PUB32    < associate a name (marker) to specific location (section, offset), does not give any type info
        //     S_LPROCREF < static procedures, tells you where the LPROC32 is.
        //     S_PROCREF  < export procedures, tells you where the GPROC32 is.
        // 
        // Members coming from codeview:
        //     S_CONSTANT < For constants, tells you the name, type index, and value
        //     S_UDT      < typedefs, associates a name to a type index
        //     S_GDATA32  < export globals, tells you about the location (section, offset), and the type of some data.
        //     S_LDATA32  < static globals, tells you about the location (section, offset), and the type of some data.
        
        // emit a 'LPROCREF' or 'PROCREF' for each function
        // @cleanup: does this break for local functions of the same name?
        for_ast_list(defined_functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            assert(!(function->decl_flags & DECLARATION_FLAGS_is_dllimport));
            
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            
            u16 symbol = (function->as_decl.flags & DECLARATION_FLAGS_is_static) ? /* LPROCREF */0x1127 : /* PROCREF */0x1125;
            begin_symbol(symbol);{
                out_int(0, u32);                             // sumName < this appears to always be 0
                out_int(function->debug_symbol_offset, u32); // offset in the module symbol stream
                out_int(1, u16);                             // module index of the stream containing the symbol
                out_string(function->identifier->string);            // name (not sure if this is mangled or not)
            }end_symbol();
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, scratch, ref_offset, function->relative_virtual_address, function->identifier->string);
            gsi_amount_of_global_symbols += 1;
            
            if(!(function->decl_flags & DECLARATION_FLAGS_is_static)){
                
                ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
                
                begin_symbol(0x110e);{                              // PUB32
                    out_int(2, u32);                                // flags (is_function)
                    out_int(function->offset_in_text_section, u32); // offset in segment
                    out_int(text_section_id, u16);                  // segment
                    out_string(function->identifier->string);               // name
                }end_symbol();
                
                psi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(psi_hash_table, scratch, ref_offset, function->relative_virtual_address, function->identifier->string);
                psi_amount_of_global_symbols += 1;
            }
        }
        
        // Emit a 'S_PUB32' for every dllimport.
        for_ast_list(dll_imports){
            struct ast_function *function = (struct ast_function *)it->value;
            
            struct string name = push_format_string(scratch, "__imp_%.*s", function->identifier->size, function->identifier->data);
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            
            smm rva;
            begin_symbol(0x110e);{                              // PUB32
                out_int(0, u32);                                // flags
                
                out_int(function->relative_virtual_address - rdata->virtual_address, u32); // offset in segment
                rva = function->relative_virtual_address;
                
                out_int(rdata_section_id, u16);                 // segment
                out_string(name);                               // name
            }end_symbol();
            
            psi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(psi_hash_table, scratch, ref_offset, rva, name);
            psi_amount_of_global_symbols += 1;
        }
        
        // Emit a 'S_PUB32' for every dllimport stub.
        for_ast_list(dll_function_stubs){
            struct ast_function *function = (struct ast_function *)it->value;
            struct dll_import_node *dll_import_node = function->dll_import_node;
            
            struct string name = function->identifier->string;
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            
            smm rva;
            begin_symbol(0x110e);{                              // PUB32
                out_int(0, u32);                                // flags
                
                out_int(dll_import_node->stub_relative_virtual_address - text->virtual_address, u32); // offset in segment
                rva = dll_import_node->stub_relative_virtual_address;
                
                out_int(text_section_id, u16);                 // segment
                out_string(name);                               // name
            }end_symbol();
            
            psi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(psi_hash_table, scratch, ref_offset, rva, name);
            psi_amount_of_global_symbols += 1;
        }
        
        
        // emit a 'S_CONSTANT' for every enum member @cleanup: dumb loop, but maybe okay
        for(u64 i = 0; i < globals.compound_types.capacity; i++){
            struct ast_node *node = globals.compound_types.nodes + i;
            if(!node->token) continue;
            struct ast_compound_type *ast_enum = cast(struct ast_compound_type *)node->ast;
            if(ast_enum->base.kind != AST_enum) continue;
            
            for(u32 member_index = 0; member_index < ast_enum->amount_of_members; member_index++){
                struct compound_member *member = &ast_enum->members[member_index];
                
                u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
                s32 value = (s32)member->enum_value;
                begin_symbol(0x1107);{ // S_CONSTANT
                    out_int(ast_enum->base.pdb_type_index, u32);
                    stream_emit_size_and_name(context, value, true, member->name->string);
                }end_symbol();
                
                gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, scratch, ref_offset, 0, member->name->string);
                gsi_amount_of_global_symbols += 1;
            }
        }
        
        // @cleanup: pub32 for non static data definitions.
        
        // emit 'S_GDATA32' or 'S_LDATA32' for every global
        for_ast_list(uninitialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            
            u16 symbol_kind = (decl->flags & DECLARATION_FLAGS_is_static) ? /* S_LDATA32 */ 0x110c : /* S_GDATA32 */ 0x110d;
            
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            begin_symbol(symbol_kind);{
                out_int(decl->type->pdb_type_index, u32); // type_index
                out_int(to_u32(decl->relative_virtual_address - bss->virtual_address), u32); // offset in section
                out_int(bss_section_id, u16);             // section id
                out_string(decl->identifier->string);
            } end_symbol();
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, scratch, ref_offset, decl->relative_virtual_address, decl->identifier->string);
            gsi_amount_of_global_symbols += 1;
        }
        
        for_ast_list(initialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            
            if(decl->flags & DECLARATION_FLAGS_is_unnamed) continue;
            
            u16 symbol_kind = (decl->flags & DECLARATION_FLAGS_is_static) ? /* S_LDATA32 */ 0x110c : /* S_GDATA32 */ 0x110d;
            
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            begin_symbol(symbol_kind);{
                out_int(decl->type->pdb_type_index, u32); // type_index
                out_int(to_u32(decl->relative_virtual_address - data->virtual_address), u32); // offset in section
                out_int(data_section_id, u16);             // section id
                out_string(decl->identifier->string);
            } end_symbol();
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, scratch, ref_offset, decl->relative_virtual_address, decl->identifier->string);
            gsi_amount_of_global_symbols += 1;
        }
        
        // emit 'S_UDT' for every typedef
        for_ast_list(typedefs){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            assert(decl->type->pdb_type_index > 0);
            
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            begin_symbol(0x1108);{ // S_UDT
                out_int(decl->type->pdb_type_index, u32);
                out_string(decl->identifier->string);
            } end_symbol();
            
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, scratch, ref_offset, 0, decl->identifier->string);
            gsi_amount_of_global_symbols += 1;
        }
        
    }
    end_counter(timing, dbi_stream);
    
    // header dump stream
    { // :header_dump
        set_current_stream(context, STREAM_section_header_dump);
        for(u32 i = 0; i < coff_file_header->amount_of_sections; i++){
            struct coff_section_header *header = &image_sections[i];
            
            out_struct(*header);
        }
    }
    
    begin_counter(timing, gsi_stream);
    // global symbol hash stream
    { // :global_symbol_hash_stream
        set_current_stream(context, STREAM_global_symbol_hash);
        
        // global_symbol_stream layout:
        // u32 unclear = -1;
        // u32 signature;
        // u32 byte_size_of_hash_records;
        // u32 size_of_buckets_and_bitmap;
        // hash_records[amount_of_global_symbols];
        // hash_bitmap[(IPHR_HASH + 32)/32];
        // hash_bucket_offsets[];
        
        // The reader is supposed to reconstruct a hash_table:
        // pdb_string_hash(symbol_name) -> {offset, reference_count}
        // this hash_table has (IPHR_HASH + 1) buckets and chains collisions.
        // We serialize all non empty buckets in hash table order with the chains being in sorted order.
        // Finally to see the chain order there is a 'hash_bucket_offsets' array, that specifies
        // the offset in bytes of the n'th present bucket.
        
        // :gsi_hash_table
        // We have constructed this hash table iteratively while adding the symbols to the symbol_stream
        
        // gsi header
        out_int(-1, u32); // unclear
        out_int(0xeffe0000 + 19990810, u32); // signature
        
        out_int(gsi_amount_of_global_symbols * 2 * sizeof(u32), u32); // size of hash records in bytes
        
        u32 bitmap_size = (IPHR_HASH + 32)/32;
        out_int(bitmap_size * 4 + gsi_amount_of_bucket_offsets * 4, u32); // size of buckets and bitmap in bytes
        
        struct pdb_location records = stream_allocate_bytes(context, gsi_amount_of_global_symbols * 2 *sizeof(u32));
        struct pdb_location bitmap  = stream_allocate_bytes(context, bitmap_size * sizeof(u32));
        struct pdb_location offsets = stream_allocate_bytes(context, gsi_amount_of_bucket_offsets * sizeof(u32));
        
        u32 bit = 0;
        u32 bit_index = 0;
        u32 offset_at = 0;
        for(u32 i = 0; i < IPHR_HASH + 1; i++){
            
            if(gsi_hash_table[i]) bit |= (1u << bit_index);
            bit_index += 1;
            if(bit_index == 32){
                stream_write_bytes(context, &bitmap, &bit, sizeof(bit));
                bit = 0;
                bit_index = 0;
            }
            if(!gsi_hash_table[i]) continue;
            
            u32 amount_in_bucket = 0;
            u32 one = 1;
            for(struct gsi_hash_bucket *it = gsi_hash_table[i]; it; it = it->next){
                amount_in_bucket++;
                // @note: +1 for some stupid reason
                // this offset is the offset in the symbol record stream, so no module index.
                u32 ref_offset = it->symbol_offset + 1;
                stream_write_bytes(context, &records, &ref_offset, sizeof(u32));
                stream_write_bytes(context, &records, &one, sizeof(u32));
            }
            
            // 12 is the size of a 'gsi_hash_bucket' on a 32-bit system...
            stream_write_bytes(context, &offsets, &offset_at, sizeof(u32));
            offset_at += 12 * amount_in_bucket;
        }
        stream_write_bytes(context, &bitmap, &bit, sizeof(bit));
        assert(records.size == 0);
        assert(bitmap.size == 0);
        assert(offsets.size == 0);
        
    }
    end_counter(timing, gsi_stream);
    
    begin_counter(timing, psi_stream);
    // public symbol hash stream
    { // :public_symbol_hash_stream
        set_current_stream(context, STREAM_public_symbol_hash);
        
        // public symbol stream header
        struct pdb_location size = stream_allocate_bytes(context, sizeof(u32));// symbol hash byte size
        out_int(psi_amount_of_global_symbols * sizeof(u32), u32); // address map byte size
        out_int(0, u32); // number of thunks
        out_int(0, u32); // thunk byte size
        out_int(0, u16); // thunk table section index
        out_int(0, u16); // pad
        out_int(0, u32); // thunk table index
        out_int(0, u32); // number of sections
        
        
        struct pdb_location before_gsi = get_current_pdb_location(context);
        // now comes a copy of the gsi stream, but for public symbols
        out_int(-1, u32); // unclear
        out_int(0xeffe0000 + 19990810, u32); // signature
        
        out_int(psi_amount_of_global_symbols * 2 * sizeof(u32), u32); // size of hash records in bytes
        
        u32 bitmap_size = (IPHR_HASH + 32)/32;
        out_int(bitmap_size * 4 + psi_amount_of_bucket_offsets * 4, u32); // size of buckets and bitmap in bytes
        
        struct pdb_location records = stream_allocate_bytes(context, psi_amount_of_global_symbols * 2 * sizeof(u32));
        struct pdb_location bitmap  = stream_allocate_bytes(context, bitmap_size * sizeof(u32));
        struct pdb_location offsets = stream_allocate_bytes(context, psi_amount_of_bucket_offsets * sizeof(u32));
        
        u32 bit = 0;
        u32 bit_index = 0;
        u32 offset_at = 0;
        for(u32 bucket_index = 0; bucket_index < IPHR_HASH + 1; bucket_index++){
            
            if(psi_hash_table[bucket_index]) bit |= (1u << bit_index);
            bit_index += 1;
            if(bit_index == 32){
                stream_write_bytes(context, &bitmap, &bit, sizeof(bit));
                bit = 0;
                bit_index = 0;
            }
            if(!psi_hash_table[bucket_index]) continue;
            
            u32 amount_in_bucket = 0;
            u32 one = 1;
            for(struct gsi_hash_bucket *it = psi_hash_table[bucket_index]; it; it = it->next){
                amount_in_bucket++;
                // @note: +1 for some stupid reason
                // this offset is the offset in the symbol record stream, so no module index.
                u32 ref_offset = it->symbol_offset + 1;
                stream_write_bytes(context, &records, &ref_offset, sizeof(u32));
                stream_write_bytes(context, &records, &one, sizeof(u32));
            }
            
            // 12 is the size of a 'psi_hash_bucket' on a 32-bit system...
            stream_write_bytes(context, &offsets, &offset_at, sizeof(u32));
            offset_at += 12 * amount_in_bucket;
        }
        stream_write_bytes(context, &bitmap, &bit, sizeof(bit));
        assert(records.size == 0);
        assert(bitmap.size == 0);
        assert(offsets.size == 0);
        
        u32 diff = pdb_current_offset_from_location(context, before_gsi);
        stream_write_bytes(context, &size, &diff, sizeof(u32));
        
        // "The address map is an array of symbol offsets sorted so that it can be binary searched by address."
        // after the gsi stream copy thing there is an address map mapping "addresses" to symbol offsets
        {
            u32 *symbol_offsets             = push_uninitialized_data(scratch, u32, psi_amount_of_global_symbols);
            u32 *relative_virtual_addresses = push_uninitialized_data(scratch, u32, psi_amount_of_global_symbols);
            
            smm out = 0;
            for(u32 bucket_index = 0; bucket_index < IPHR_HASH + 1; bucket_index++){
                for(struct gsi_hash_bucket *bucket = psi_hash_table[bucket_index]; bucket; bucket = bucket->next){
                    smm index = out++;
                    symbol_offsets[index]             = bucket->symbol_offset;
                    relative_virtual_addresses[index] = bucket->rva;
                }
            }
            assert(out == psi_amount_of_global_symbols);
            
            if(psi_amount_of_global_symbols > 1){
                smm amount = psi_amount_of_global_symbols;
                
                //
                // Quicksort the 'symbol_offsets' by the 'relative_virtual_addresses'.
                //
                smm *sort_stack = push_uninitialized_data(scratch, smm, amount);
                
                smm left = 0;
                smm right = amount - 1;
                
                smm sort_stack_at = 0;
                while(true){
                    
                    if(left < right){
                        smm piveot = relative_virtual_addresses[left];
                        
                        smm right_it = right;
                        smm left_it  = left;
                        
                        while(left_it < right_it){
                            while(relative_virtual_addresses[left_it] <= piveot && left_it < right_it) left_it++;
                            
                            while(relative_virtual_addresses[right_it] > piveot) right_it--;
                            
                            if(left_it < right_it){
                                {
                                    //
                                    // swap the relative_virtual_addresses
                                    //
                                    u32 temp = relative_virtual_addresses[left_it];
                                    relative_virtual_addresses[left_it] = relative_virtual_addresses[right_it];
                                    relative_virtual_addresses[right_it] = temp;
                                }
                                {
                                    //
                                    // swap the symbol_offsets
                                    //
                                    u32 temp = symbol_offsets[left_it];
                                    symbol_offsets[left_it] = symbol_offsets[right_it];
                                    symbol_offsets[right_it] = temp;
                                }
                            }
                        }
                        
                        //
                        // Swap the piveot element to the middle!
                        //
                        {
                            u32 temp = relative_virtual_addresses[left];
                            relative_virtual_addresses[left] = relative_virtual_addresses[right_it];
                            relative_virtual_addresses[right_it] = temp;
                        }
                        
                        {
                            u32 temp = symbol_offsets[left];
                            symbol_offsets[left] = symbol_offsets[right_it];
                            symbol_offsets[right_it] = temp;
                        }
                        
                        //
                        // At this point we know:
                        //    1) everything after  'right_it' is >  piveot
                        //    2) everything before 'left_it'  is <= piveot
                        //    3) relative_virtual_addresses[right_it] == piveot
                        
                        // We need to sort:
                        //    1) [left, left_it - 1]   = [left, right_it]
                        //    2) [right_it + 1, right] = [left_it, right]
                        //     3) but actually 'relative_virtual_addresses[right_it]' is known to be correct
                        
                        if(left_it < right){
                            sort_stack[sort_stack_at++] = right;
                        }
                        
                        if(left < right_it - 1){
                            sort_stack[sort_stack_at++] = right_it - 1;
                        }else{
                            left++;
                        }
                    }
                    
                    if(sort_stack_at == 0) break;
                    
                    right = sort_stack[--sort_stack_at];
                }
                
            }
            
            stream_emit_struct(context, symbol_offsets, psi_amount_of_global_symbols * sizeof(u32));
        }
        
    }
    end_counter(timing, psi_stream);
    
    begin_counter(timing, directory_stream);
    {// :directory_stream
        // directory stream layout:
        // u32 amount_of_streams;
        // u32 stream_sizes[amount_of_streams]
        // u32 stream_one_pages[]
        // u32 stream_two_pages[]
        // ...
        
        // :old_directory_hack
        set_current_stream(context, STREAM_old_directory);
        u32 amount_of_streams = 0;
        for(u32 i = 0; i < array_count(context->page_list); i++){
            struct page_list *list = context->page_list + i;
            if(!list->first) break;
            amount_of_streams++;
        }
        out_int(amount_of_streams, u32);
        
        u32 *sizes = push_uninitialized_data(scratch, u32, amount_of_streams);
        struct pdb_location stream_sizes = stream_allocate_bytes(context, amount_of_streams * sizeof(u32));
        for(u32 i = 0; i < amount_of_streams; i++){
            smm size = 0;
            struct page_list *list = context->page_list + i;
            for(struct page_list_node *it = list->first; it; it = it->next){
                //os_console_print("stream %d page %d\n", i, it->page_index);
                
                // @hmm, right now we emit a page on 'set_current_stream' if the stream then turns out to
                // be empty we would still emit the page, this is why this 'if' is here...
                
                if(it->offset) out_int(it->page_index, u32);
                size += it->offset;
            }
            sizes[i] = to_u32(size);
            
        }
        stream_write_bytes(context, &stream_sizes, sizes, amount_of_streams * sizeof(u32));
    }
    end_counter(timing, directory_stream);
    
    
    {
        u8 *directory_page_list_page = push_uninitialized_data(arena, u8, 0x1000);
        
        u32 page_number = to_u32((directory_page_list_page - context->pdb_base) >> 12);
        pdb_header->page_number_of_directory_stream_number_list = page_number;
        
        // :old_directory_hack @note: right now we use old directory to do the current directory
        u32 *directory_page_list_entry = (u32 *)directory_page_list_page;
        u32 directory_stream_size = 0;
        u32 i = 0;
        for(struct page_list_node *it = context->page_list[STREAM_old_directory].first; it; it = it->next){
            directory_stream_size += it->offset;
            directory_page_list_entry[i++] = it->page_index;
        }
        
        pdb_header->directory_stream_size = directory_stream_size;
    }
    
    
    u8 *end_address = arena_current(arena);
    pdb_header->number_of_pages = to_u32((end_address - context->pdb_base) >> 12);
    context->pdb_end = end_address;
    context->pdb_size = context->pdb_end - context->pdb_base;
    
    // free page maps :free_page_map
    begin_counter(timing, free_page_map);
    {
        // the free page map has a bit set for every page that is free.
        // @note: every page is used until 'pdb_header->number_of_pages' and all other pages are unused.
        // @note: the first free page map covers the first 8 'intervals' i.e. the first 0x8000 pages.
        u32 first_non_zero_page_index_in_fpm = pdb_header->number_of_pages / 0x8000;
        struct page_list_node *first_non_zero_page = context->free_page_maps.first;
        for(u32 i = 0; i < first_non_zero_page_index_in_fpm; i++){
            u8 *page = pdb_page_from_index(context, first_non_zero_page->page_index);
            memset(page, 0, page_size);
            first_non_zero_page = first_non_zero_page->next;
        }
        for(struct page_list_node *node = first_non_zero_page; node; node = node->next){
            u8 *page = pdb_page_from_index(context, node->page_index);
            memset(page, 0xff, page_size);
        }
        
        u32 remainder = pdb_header->number_of_pages - first_non_zero_page_index_in_fpm * 0x8000;
        u8 *page = pdb_page_from_index(context, first_non_zero_page->page_index);
        for(u32 i = 0; i < remainder; i++){
            page[i / 8] &= ~(1 << (i % 8));
        }
    }
    end_counter(timing, free_page_map);
    
    end_temporary_memory(temporary_memory);
    
#if READ_PDB_AFTER_EMITING_IT
    struct os_file pdb_file = {
        .memory = context->pdb_base,
        .size = context->pdb_size,
    };
    
    read_pdb(scratch, pdb_file);
#endif
    
    end_counter(timing, print_pdb);
    
    if(!globals.cli_options.dont_print_the_files){
        begin_counter(context, write_pdb);
        char *pdb_name = (char *)pdb_full_path.data;
        smm success = os_write_file(pdb_name, context->pdb_base, context->pdb_size);
        end_counter(context, write_pdb);
        
        if(success){
            if(!globals.cli_options.quiet) print("Wrote file: '%s'\n", pdb_name);
        }else{
            print("Error: Unable to write file '%s'.\n", pdb_name);
            globals.an_error_has_occurred = true;
        }
    }
    
    return;
}

#undef out_struct
#undef out_int
#undef out_align
#undef begin_symbol
#undef end_symbol

