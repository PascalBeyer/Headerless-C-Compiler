
static void read_pdb(struct memory_arena *scratch, struct os_file pdb_file);

//////////////////////////////////////////////////////
// PE helpers
//////////////////////////////////////////////////////
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

typedef struct _IMAGE_FILE_HEADER {
    WORD Machine;
    WORD NumberOfSections;
    DWORD TimeDateStamp;
    DWORD PointerToSymbolTable;
    DWORD NumberOfSymbols;
    WORD SizeOfOptionalHeader;
    WORD Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;

typedef struct _IMAGE_DATA_DIRECTORY {
    DWORD VirtualAddress;
    DWORD Size;
} IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;

typedef struct _IMAGE_SECTION_HEADER {
    BYTE Name[8];
    union {
        DWORD PhysicalAddress;
        DWORD VirtualSize;
    } Misc;
    DWORD VirtualAddress;
    DWORD SizeOfRawData;
    DWORD PointerToRawData;
    DWORD PointerToRelocations;
    DWORD PointerToLinenumbers;
    WORD NumberOfRelocations;
    WORD NumberOfLinenumbers;
    DWORD Characteristics;
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;

typedef struct _IMAGE_OPTIONAL_HEADER64 {
    WORD Magic;
    BYTE MajorLinkerVersion;
    BYTE MinorLinkerVersion;
    DWORD SizeOfCode;
    DWORD SizeOfInitializedData;
    DWORD SizeOfUninitializedData;
    DWORD AddressOfEntryPoint;
    DWORD BaseOfCode;
    ULONGLONG ImageBase;
    DWORD SectionAlignment;
    DWORD FileAlignment;
    WORD MajorOperatingSystemVersion;
    WORD MinorOperatingSystemVersion;
    WORD MajorImageVersion;
    WORD MinorImageVersion;
    WORD MajorSubsystemVersion;
    WORD MinorSubsystemVersion;
    DWORD Win32VersionValue;
    DWORD SizeOfImage;
    DWORD SizeOfHeaders;
    DWORD CheckSum;
    WORD Subsystem;
    WORD DllCharacteristics;
    ULONGLONG SizeOfStackReserve;
    ULONGLONG SizeOfStackCommit;
    ULONGLONG SizeOfHeapReserve;
    ULONGLONG SizeOfHeapCommit;
    DWORD LoaderFlags;
    DWORD NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[16];
} IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;

typedef struct _IMAGE_NT_HEADERS64 {
    DWORD Signature;
    IMAGE_FILE_HEADER FileHeader;
    IMAGE_OPTIONAL_HEADER64 OptionalHeader;
} IMAGE_NT_HEADERS64, *PIMAGE_NT_HEADERS64;
typedef PIMAGE_NT_HEADERS64 PIMAGE_NT_HEADERS;

typedef struct _IMAGE_DEBUG_DIRECTORY {
    DWORD Characteristics;
    DWORD TimeDateStamp;
    WORD MajorVersion;
    WORD MinorVersion;
    DWORD Type;
    DWORD SizeOfData;
    DWORD AddressOfRawData;
    DWORD PointerToRawData;
} IMAGE_DEBUG_DIRECTORY, *PIMAGE_DEBUG_DIRECTORY;

typedef struct _IMAGE_IMPORT_DESCRIPTOR {
    DWORD ImportLookupRVA;
    DWORD TimeDateStamp; 
    
    DWORD ForwarderChain; 
    DWORD Name;
    DWORD ImportAddressRVA;
} IMAGE_IMPORT_DESCRIPTOR;
typedef IMAGE_IMPORT_DESCRIPTOR __unaligned *PIMAGE_IMPORT_DESCRIPTOR;

typedef enum _UNWIND_OP_CODES {
    UWOP_PUSH_NONVOL = 0, /* info == register number */
    UWOP_ALLOC_LARGE,     /* no info, alloc size in next 2 slots */
    UWOP_ALLOC_SMALL,     /* info == size of allocation / 8 - 1 */
    UWOP_SET_FPREG,       /* no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 */
    UWOP_SAVE_NONVOL,     /* info == register number, offset in next slot */
    UWOP_SAVE_NONVOL_FAR, /* info == register number, offset in next 2 slots */
    UWOP_SAVE_XMM128 = 8, /* info == XMM reg number, offset in next slot */
    UWOP_SAVE_XMM128_FAR, /* info == XMM reg number, offset in next 2 slots */
    UWOP_PUSH_MACHFRAME   /* info == 0: no error-code, 1: error-code */
} UNWIND_CODE_OPS;
typedef union _UNWIND_CODE {
    struct{
        UBYTE CodeOffset;
        UBYTE UnwindOp; // UBYTE UnwindOp : 4; UBYTE OpInfo : 4;
    };
    USHORT FrameOffset;
} UNWIND_CODE, *PUNWIND_CODE;

typedef struct _UNWIND_INFO {
    UBYTE VersionAndFlags;   //UBYTE Version: 3; UBYTE Flags: 5;
    UBYTE SizeOfProlog;
    UBYTE CountOfCodes;
    UBYTE FrameInfo;    //UBYTE FrameRegister : 4;    UBYTE FrameOffset   : 4;
    //UNWIND_CODE UnwindCode[1];
    /*  UNWIND_CODE MoreUnwindCode[((CountOfCodes + 1) & ~1) - 1];
    *   union {
    *       OPTIONAL ULONG ExceptionHandler;
    *       OPTIONAL ULONG FunctionEntry;
    *   };
    *   OPTIONAL ULONG ExceptionData[]; */
} UNWIND_INFO, *PUNWIND_INFO;

func UNWIND_INFO *function_fill_in_unwind_info(struct memory_arena *emit_arena, struct ast_function *ast_function){
    UNWIND_INFO *unwind = push_struct(emit_arena, UNWIND_INFO);
    //unwind->Version = 1;
    unwind->VersionAndFlags = 1;
    unwind->SizeOfProlog = save_truncate_smm_to_u8(ast_function->size_of_prolog);
    unwind->CountOfCodes = 0; // this gets incrementally increased below
    unwind->FrameInfo = 5; // RBP
    //unwind->FrameOffset = 0;
    
    // @note: the order on these seems to be reversed for some stupid reason
    // "The array is sorted by descending order of offset in the prolog."
    
    assert(ast_function->stack_space_needed >= 0);
    assert(ast_function->stack_space_needed < giga_bytes(4));
    
    smm memory = (ast_function->stack_space_needed) / 8 - 1;
    if(memory < (1 << 4)){
        unwind->CountOfCodes += 1;
        UNWIND_CODE *code = push_struct(emit_arena, UNWIND_CODE);
        code->CodeOffset  = save_truncate_smm_to_u8(ast_function->rsp_subtract_offset);
        code->UnwindOp    = UWOP_ALLOC_SMALL | to_u8(memory << 4);
    }else if(memory < (1 << 16)){
        unwind->CountOfCodes += 2;
        UNWIND_CODE *code = push_struct(emit_arena, UNWIND_CODE);
        code->CodeOffset  = save_truncate_smm_to_u8(ast_function->rsp_subtract_offset);
        code->UnwindOp = UWOP_ALLOC_LARGE;
        *cast(u16 *)push_struct_(emit_arena, sizeof(u16), 1) = save_truncate_smm_to_u16(memory);
    }else{
        unwind->CountOfCodes += 3;
        assert(ast_function->stack_space_needed <= 0xFFFFFFF8);
        UNWIND_CODE *code = push_struct(emit_arena, UNWIND_CODE);
        code->CodeOffset  = save_truncate_smm_to_u8(ast_function->rsp_subtract_offset);
        code->UnwindOp = UWOP_ALLOC_LARGE | (1 << 4);
        *cast(u32 *)push_struct_(emit_arena, sizeof(u32), 1) = save_truncate_smm_to_u32(ast_function->stack_space_needed);
    }
    
    { // code for establishing the Frame Pointer 
        unwind->CountOfCodes += 1;
        UNWIND_CODE *code = push_struct(emit_arena, UNWIND_CODE);
        code->CodeOffset = 4; // after push rbp and mov rbp, rsp
        code->UnwindOp = UWOP_SET_FPREG;
    }
    
    {// code for pushing RBP, later we have to do the same for other things we push @incomplete:
        unwind->CountOfCodes += 1;
        UNWIND_CODE *code = push_struct(emit_arena, UNWIND_CODE);
        code->CodeOffset = 1; // after push rbp, which is '55'
        code->UnwindOp = UWOP_PUSH_NONVOL | (5 << 4);        
        //code->OpInfo   = 5; // RBP
    }
    
    if(unwind->CountOfCodes & 1){
        *cast(u16 *)push_struct_(emit_arena, sizeof(u16), 1) = 0; // pad need to be aligned to 32
    }
    
    // not sure if this has to be here?
    *push_struct(emit_arena, u32) = 0;
    //*cast(u32 *)push_struct_(emit_arena, sizeof(u32), 1) = 0;
    
    
    return unwind;
}


// @WARNING: this has to match the union in 'exe_write_context' below
enum section_id{
    SECTION_ID_none,
    
    SECTION_ID_text,
    SECTION_ID_bss, 
    SECTION_ID_rdata,
    SECTION_ID_data,
    SECTION_ID_pdata,
    
    SECTION_ID_count,
};

struct exe_write_context{
    u8 *base_address;
    u8 *end_address;
    u8 *begin_of_text_section;
    
    IMAGE_FILE_HEADER *coff;
    IMAGE_OPTIONAL_HEADER64 *header;
    
    union{
        IMAGE_SECTION_HEADER *section_headers[SECTION_ID_count];
        struct{
            // @WARNING: this hash to match the enum 'section_id' above
            IMAGE_SECTION_HEADER *none; // this should be null
            IMAGE_SECTION_HEADER *text;
            IMAGE_SECTION_HEADER *bss;
            IMAGE_SECTION_HEADER *rdata;
            IMAGE_SECTION_HEADER *data;
            IMAGE_SECTION_HEADER *pdata;
        };
    };
};

func IMAGE_SECTION_HEADER *write_section_header(struct exe_write_context *ctx, struct memory_arena *arena, char *name, u32 characteristics){
    IMAGE_SECTION_HEADER *section = push_struct(arena, IMAGE_SECTION_HEADER);
    for(u32 i = 0; i < 8; i++){
        section->Name[i] = *name++;
        if(!*name)break;
    }
    section->Characteristics = characteristics;
    ctx->coff->NumberOfSections++; 
    
    return section;
}

struct section_write_context{
    u8 *base_address;
    struct memory_arena *arena;
    u32 virtual_address;
    
    // per section data
    u8 *section_memory_location;
    IMAGE_SECTION_HEADER *section;
};

func u32 make_relative_virtual_address(struct section_write_context *ctx, void *pointer){
    return save_truncate_smm_to_u32(cast(u8 *)pointer - ctx->section_memory_location + ctx->section->VirtualAddress);
}

func void begin_section(struct section_write_context *context, IMAGE_SECTION_HEADER *section){
    push_zero_align(context->arena, 0x200); // file alignment
    context->section = section;
    
    section->VirtualAddress = context->virtual_address;
    section->PointerToRawData = save_truncate_smm_to_u32(arena_current(context->arena) - context->base_address);
    
    context->section_memory_location = arena_current(context->arena);
}

func void end_section(struct section_write_context *context){
    u32 actual_size = save_truncate_smm_to_u32(arena_current(context->arena)
                                               - context->section_memory_location);
    context->section->SizeOfRawData = align_up(actual_size, 0x200); // file_alignment
    context->section->Misc.VirtualSize = actual_size;
    context->virtual_address = align_up(context->virtual_address + actual_size, 0x1000);
}

/////////////////////////////////////////////////////////////////////
// PDB helpers
/////////////////////////////////////////////////////////////////////

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
        ret ^= *(string.data + at);
    }
    
    const u32 to_lower = 0x20202020;
    ret |= to_lower;
    ret ^= (ret >> 11);
    ret ^= (ret >> 16);
    
    return ret;
}


struct pdb_header{
    u8 signiture[32];
    u32 page_size;
    u32 free_page_map;
    u32 number_of_pages;
    u32 directory_stream_size;
    u32 reserved;
    u32 page_number_of_directory_stream_number_list; // i.e some relative pointer to an array
};

static const u8 pdb_signiture[] = {
    0x4d, 0x69, 0x63, 0x72, 0x6f, 0x73, 0x6f, 0x66, 0x74, 0x20, 0x43, 0x2f, 0x43, 0x2b, 0x2b, 0x20, 
    0x4d, 0x53, 0x46, 0x20, 0x37, 0x2e, 0x30, 0x30, 0x0d, 0x0a, 0x1a, 0x44, 0x53, 0x00, 0x00, 0x00,
};
static_assert(sizeof(pdb_signiture) == 32);

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

enum CV_basic_type{
    CV_none = 0x0,
    CV_void = 0x3,
    
    CV_s8  = 0x68,
    CV_u8  = 0x69,
    CV_s16 = 0x72,
    CV_u16 = 0x73,
    CV_s32 = 0x74,
    CV_u32 = 0x75,
    CV_s64 = 0x76,
    CV_u64 = 0x77,
    
    CV_f32 = 0x40,
    CV_f64 = 0x41,
    
    
    //CV_s8_pointer = 0x0168,
    //CV_u8_pointer = 0x0169,
    
    // char *
    CV_s8_pointer = 0x0620,
    
    // unsigned char *
    CV_u8_pointer = 0x0670,
};

struct dbi_stream{
    u32 version_signiture;                             // always -1
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
    //unkown what the last flag does /DEBUG:CTYPES link flag
    u16 flags; // u16 incrementally_linked :1, private_symbols_stripped :1, has_conflict_types       :1; // 
    u16 machine;                                        // for us always 0x8664 (x86_64)
    u32 padding;
};

// i do not understand what this is for....
struct section_contribution_entry{
    s16 section_id;
    // this seems to be one based and can be used to identify the section from some other data
    char Padding1[2];
    int32_t offset; // offset of the contribution in the section. can be computed contrib.rva - section.rva
    int32_t size;   // size of the contribution
    uint32_t characteristics; 
    s16 module_index;    // the module, that is responsible for the contribution
    char Padding2[2];
    uint32_t data_crc;         // CRC-32 check sums
    uint32_t reloc_crc;        // CRC-32 check sums
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
    STREAM_link_info           = 6, // done
    STREAM_TPI_hash            = 7, // done
    STREAM_IPI_hash            = 8, // done
    STREAM_symbol_records      = 9, // done
    STREAM_global_symbol_hash  = 10, // done (stubbed)
    STREAM_public_symbol_hash  = 11, // done (stubbed)
    STREAM_linker_module       = 12, // done modulo coffgroup
    STREAM_section_header_dump = 13, // done
    
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
    smm pdb_line_at; // used to track the line number when emiting line information
    smm pdb_offset_at;
    smm pdb_amount_of_lines;
    
    struct pdb_location module_stream_begin;
    smm current_block32_offset_in_stream;
    
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
    u8 *page = push_data(context->arena, u8, 0x1000);
    
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
                struct pdb_index_offset_buffer_entry *new_entries = push_data(context->scratch, struct pdb_index_offset_buffer_entry, context->index_offset_buffer_size * 2);
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
    u16 size_u16 = to_u16(size);
    stream_write_bytes(context, &context->active_symbol, &size_u16, sizeof(u16));
    context->in_symbol = false;
}


struct coff_context{
    struct exe_write_context exe;
    struct pdb_write_context pdb;
};

///////////////////////////////////////////////////////////////////////////////


#define out_struct(data) stream_emit_struct(context, &data, sizeof(data))
#define out_string(string) {stream_emit_struct(context, (string).data, (string).length); out_int(0, u8);}

#define out_int(integer, type) ((void)(type)3, stream_emit_int(context, integer, sizeof(type)))
#define out_align(align_to) stream_align(context, align_to, false)
#define out_f3f2f1_align(align_to) stream_align(context, align_to, true)
#define begin_symbol(type) stream_begin_symbol(context, type)
#define end_symbol() stream_end_symbol(context)

func void stream_emit_size_and_name(struct pdb_write_context *context, smm value, b32 is_signed, struct string *string){
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
    
    out_string(*string);
    out_f3f2f1_align(sizeof(u32));
}

func u32 tpi_emit_predecl(struct pdb_write_context *context, struct ast_type *type){
    assert(type->kind == AST_struct || type->kind == AST_union);
    struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
    u32 ret = begin_symbol(0x1505);{
        out_int(0, u16);        // count (0 for forward ref)
        out_int((1 << 7), u16); // properties: forward_ref flag
        out_int(0, u32);        // the LF_FIELDLIST (0 for forward ref)
        if(type->kind == AST_struct){
            out_int(0, u32);    // derived
            out_int(0, u32);    // vshape
        }
        stream_emit_size_and_name(context, 0, false, compound->identifier);
    }end_symbol();
    return ret;
}


#define IPHR_HASH 4096
struct gsi_hash_bucket{
    struct gsi_hash_bucket *next;
    struct string name;
    u32 hash;
    u32 symbol_offset;
};

func b32 global_symbol_stream_hash_table_add(struct gsi_hash_bucket **hash_table, u32 ref_offset, struct memory_arena *scratch, struct string name){
    
    u32 hash = pdb_string_hash(name);
    struct gsi_hash_bucket *new_bucket = push_struct(scratch, struct gsi_hash_bucket);
    new_bucket->hash = hash;
    // @note: +1 for some stupid reason
    // this offset is the offset in the symbol record stream, so no module index.
    new_bucket->symbol_offset = ref_offset + 1;
    new_bucket->name = name;
    
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

func void tpi_push_to_type_stack(struct pdb_write_context *context, struct ast_type *type){
    // @incomplete: grow the stack
    assert(context->type_stack_at < context->type_stack_size);
    context->type_stack[context->type_stack_at++] = type;
}

func u32 tpi_maybe_emit_predecl(struct pdb_write_context *context, struct ast_type *type){
    u32 type_index;
    if(type->flags & TYPE_FLAG_pdb_permanent){
        assert(type->pdb_type_index);
        type_index = type->pdb_type_index;
    }else{
        assert(type->flags & TYPE_FLAG_pdb_tempoary);
        assert(!type->pdb_type_index);
        if(!type->pdb_predecl_type_index){
            type->pdb_predecl_type_index = tpi_emit_predecl(context, type); 
        }
        type_index = type->pdb_predecl_type_index;
    }
    return type_index;
}

func void tpi_emit_type_index_or_predecl_type_index(struct pdb_write_context *context, struct ast_type *type, struct ast *defined_type){
    if(defined_type && defined_type->kind == AST_enum){
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

func void tpi_register_type(struct pdb_write_context *context, struct ast_type *initial_type){
    if(initial_type->flags & TYPE_FLAG_pdb_permanent){
        assert(initial_type->pdb_type_index);
        return; // already emited this type
    }
    
    // reset the type_stack
    context->type_stack_at = 0;
    tpi_push_to_type_stack(context, initial_type);
    
    u32 type_index;
    while(context->type_stack_at){
        struct ast_type *type = context->type_stack[context->type_stack_at - 1];
        type->flags |= TYPE_FLAG_pdb_tempoary;
        
        // maybe recurse, if we are in a compound_type
        if(type->kind == AST_struct || type->kind == AST_union){
            struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
            b32 should_recurse = false;
            for_ast_list(compound->declarations){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                // @cleanup: defined type for typedefs
                
                if(decl->type->flags & (TYPE_FLAG_pdb_tempoary | TYPE_FLAG_pdb_permanent)){
                    // these are fine, either we emit a predecl below, or we have already emited it
                }else{
                    tpi_push_to_type_stack(context, decl->type);
                    should_recurse = true;
                    break;
                }
            }
            if(should_recurse) continue;
            
        }
        
        switch(type->kind){
            case AST_enum:{
                struct ast_compound_type *ast_enum = cast(struct ast_compound_type *)type;
                
                u32 fieldlist = begin_symbol(0x1203); // LF_FIELDLIST
                for_ast_list(ast_enum->declarations){
                    assert(it->value->kind == AST_declaration);
                    struct ast_declaration *decl = cast(struct ast_declaration *) it->value;
                    s32 value = integer_literal_as_s32(decl->assign_expr);
                    
                    out_int(0x1502, u16); // LF_ENUMERATE
                    out_int(3, u16);      // @cleanup: attributes?
                    stream_emit_size_and_name(context, value, true, decl->identifier);
                }
                end_symbol();
                
                type_index = begin_symbol(0x1507);{ // LF_ENUM
                    out_int(ast_enum->declarations.count, u16);
                    out_int(0, u16);    // @incomplete: properties
                    out_int(CV_s32, u32); // underlying type
                    out_int(fieldlist, u32);
                    out_string(*ast_enum->identifier); // no size for enums
                }end_symbol();
            }break;
            case AST_struct: case AST_union:{
                struct ast_compound_type *compound = cast(struct ast_compound_type *)type;
                
                // emit predeclarations if we need to
                for_ast_list(compound->declarations){
                    struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                    tpi_maybe_emit_predecl(context, decl->type);
                }
                
                u32 fieldlist = begin_symbol(0x1203); // LF_FIELDLIST
                for_ast_list(compound->declarations){
                    struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                    assert(decl->type->flags & (TYPE_FLAG_pdb_tempoary | TYPE_FLAG_pdb_permanent));
                    // @note: right now we are not doing LF_NESTTYPE... is that one necessary?
                    //        it seems to be used when we have a unnamed type with no ident
                    out_int(0x150d, u16); // LF_MEMBER
                    out_int(0, u16); // attributes @cleanup:
                    tpi_emit_type_index_or_predecl_type_index(context, decl->type, decl->defined_type);
                    stream_emit_size_and_name(context, decl->offset_in_type, false, decl->identifier);              
                } end_symbol();
                
                
                // LF_STRUCTURE or LF_UNION
                u16 lf_kind = type->kind == AST_struct ? 0x1505 : 0x1506;
                type_index = begin_symbol(lf_kind);{
                    out_int(compound->declarations.count, u16);
                    out_int(0, u16);         // @incomplete: properties?
                    out_int(fieldlist, u32); 
                    if(type->kind == AST_struct){
                        out_int(0, u32);         // derived (c++)
                        out_int(0, u32);         // vshape (c++)
                    }
                    stream_emit_size_and_name(context, type->size, false, compound->identifier);
                }end_symbol();
                
            }break;
            case AST_pointer_type:{
                struct ast_pointer_type *pointer = cast(struct ast_pointer_type *)type;
                struct ast_type *pointer_to = pointer->pointer_to;
                
                u32 pointer_to_type_index;
                if(pointer_to->flags & (TYPE_FLAG_pdb_permanent|TYPE_FLAG_pdb_tempoary)){
                    pointer_to_type_index = tpi_maybe_emit_predecl(context, pointer_to);
                }else{
                    tpi_push_to_type_stack(context, pointer_to);
                    continue;
                }
                
                // check if we already have this pointer
                struct pdb_type_info *info = &context->type_index_to_type_info[pointer_to_type_index];
                if(info->pointer_type_index){
                    type_index = info->pointer_type_index;
                }else{
                    type_index = begin_symbol(0x1002);{ // LF_POINTER
                        tpi_emit_type_index_or_predecl_type_index(context, pointer_to, pointer->pointer_to_defined_type); // underlying_type
                        out_int(0x1000c, u32); // modifiers (size = 8 + kind = __ptr64)
                    }end_symbol();
                    info->pointer_type_index = type_index;
                }
                
            }break;
            case AST_array_type:{
                struct ast_array_type *array = cast(struct ast_array_type *)type;
                struct ast_type *element_type = array->element_type;
                
                if(element_type->flags & (TYPE_FLAG_pdb_permanent|TYPE_FLAG_pdb_tempoary)){
                    tpi_maybe_emit_predecl(context, element_type);
                }else{
                    tpi_push_to_type_stack(context, element_type);
                    continue;
                }
                
                type_index = begin_symbol(0x1503);{ // LF_ARRAY
                    tpi_emit_type_index_or_predecl_type_index(context, element_type, array->element_type_defined_type); // underlying_type
                    out_int(CV_s64, u32); // index type
                    stream_emit_size_and_name(context, array->base.size, false, &string("")); // I think this always has an empty name.
                }end_symbol();
                
            }break;
            case AST_function_type:{
                struct ast_function_type *function_type = cast(struct ast_function_type *)type;
                
                if(function_type->return_type->flags & (TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_tempoary)){
                    // @cleanup: I think these can be 'self_referantial' if you have 
                    //     struct comp { struct comp *(*asd)(); }
                    // that should make a predecl for struct comp @cleanup: maybe test this
                    
                    tpi_maybe_emit_predecl(context, function_type->return_type);
                }else{
                    tpi_push_to_type_stack(context, function_type->return_type);
                    continue;
                }
                
                b32 should_recurse = false;
                for_ast_list(function_type->argument_list){
                    struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                    
                    if(decl->type->flags & (TYPE_FLAG_pdb_permanent | TYPE_FLAG_pdb_tempoary)){    
                        tpi_maybe_emit_predecl(context, decl->type);
                    }else{
                        tpi_push_to_type_stack(context, decl->type);
                        should_recurse = true;
                        break;
                    }
                }
                if(should_recurse) continue;
                
                u32 arglist = begin_symbol(0x1201);{ // LF_ARGLIST
                    out_int(to_u32(function_type->argument_list.count), u32);
                    for_ast_list(function_type->argument_list){
                        struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                        tpi_emit_type_index_or_predecl_type_index(context, decl->type, decl->defined_type);
                    }
                }end_symbol();
                
                type_index = begin_symbol(0x1008);{ // LF_PROCEDURE
                    tpi_emit_type_index_or_predecl_type_index(context, function_type->return_type, function_type->return_type_defined_type);
                    out_int(0, u8); // calling convention (always zero ?)
                    out_int(0, u8); // function attributes, c++ crazyness
                    out_int(to_u16(function_type->argument_list.count), u16);
                    out_int(arglist, u32);
                }end_symbol();
                
            }break;
            case AST_unresolved_type:{
                // @ugh, we would want to put T_NOTYPE here probably but that is '0' and we use 0 as an invalid 
                //       value. @cleanup:
                type_index = CV_void;
            }break;
            invalid_default_case(type_index = 0);
        }
        
        type->flags |= TYPE_FLAG_pdb_permanent;
        type->flags &= ~TYPE_FLAG_pdb_tempoary;
        type->pdb_type_index = type_index;
        if(type_index >= context->maximal_amount_of_type_indices){
            smm new_max = context->maximal_amount_of_type_indices << 1;
            struct pdb_type_info *new_mem = push_zero_data(context->scratch, struct pdb_type_info, new_max);
            memcpy(new_mem, context->type_index_to_type_info, context->maximal_amount_of_type_indices * sizeof(new_mem[0]));
            context->type_index_to_type_info = new_mem;
            context->maximal_amount_of_type_indices = new_max;
        }
        context->type_index_to_type_info[type->pdb_type_index].type = type;
        
        context->type_stack_at -= 1;
        context->amount_of_type_indices = max_of(context->amount_of_type_indices, type_index);
    }
}

func void tpi_register_all_types_in_ast__recursive(struct pdb_write_context *context, struct ast *ast){
    switch(ast->kind){
        case AST_declaration:{
            struct ast_declaration *decl = cast(struct ast_declaration *)ast;
            tpi_register_type(context, decl->type);
        }break;
        case AST_declaration_list:{
            struct ast_declaration_list *list = cast(struct ast_declaration_list *)ast;
            for(struct declaration_node *node = list->list.first; node; node = node->next){
                tpi_register_type(context, node->decl->type);
            }
        }break;
        case AST_if:{
            struct ast_if *ast_if = cast(struct ast_if *)ast;
            tpi_register_all_types_in_ast__recursive(context, ast_if->statement);
            if(ast_if->else_statement){
                tpi_register_all_types_in_ast__recursive(context, ast_if->else_statement);
            }
        }break;
        case AST_do_while:
        case AST_for:{
            struct ast_for *ast_for = cast(struct ast_for *)ast;
            // @yuck @ugly @hack: ast_for->decl is any statement right now
            if(ast_for->decl) tpi_register_all_types_in_ast__recursive(context, ast_for->decl);
            tpi_register_all_types_in_ast__recursive(context, &ast_for->scope_for_decl->base);
            tpi_register_all_types_in_ast__recursive(context, ast_for->body);
        }break;
        case AST_scope:{
            struct ast_scope *scope = cast(struct ast_scope *)ast;
            for_ast_list(scope->statement_list){
                tpi_register_all_types_in_ast__recursive(context, it->value);
            }
        }break;
        case AST_switch:{
            struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
            tpi_register_all_types_in_ast__recursive(context, ast_switch->statement);
        }break;
        default:{
            // everything else is fine.
        }break;
    }
}

/////////////////////////////////////////////////////////////////////

// @sigh: we have these two helper routines purely because for statements don't really have a place to put their
//        declaration.

func void pdb_emit_regrels_for_scope(struct pdb_write_context *context, struct ast_scope *scope){
    // @note: it seems, that all declarations need to be at the beginning of the scope.
    //        so we loop here over all of them instead of doing it recursively
    for(smm i = 0; i < scope->amount_of_declarations; i++){
        struct ast_declaration *decl = scope->declarations[i];
        // enums don't get 'S_REGREL32'... @cleanup: maybe they get constants?
        if(decl->base.kind != AST_declaration) continue;
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
            out_string(*decl->identifier);
        }end_symbol();
    }
    
}

func struct pdb_location pdb_begin_scope(struct pdb_write_context *context, struct ast_function *function, struct ast_scope *scope){
    
    struct pdb_location pointer_to_end_loc;
    smm scope_size = scope->scope_end_byte_offset_in_function - scope->base.byte_offset_in_function;
    smm offset_in_text_section = function->offset_in_text_section + scope->base.byte_offset_in_function + function->size_of_prolog;
    
    smm scope_offset_in_symbol_stream = pdb_current_offset_from_location(context, context->module_stream_begin);
    begin_symbol(0x1103);{// S_BLOCK32
        out_int(context->current_block32_offset_in_stream, u32);      // pointer to parent
        pointer_to_end_loc = stream_allocate_bytes(context, sizeof(u32));
        out_int(scope_size, u32);                                     // size of the scope in bytes
        out_int(offset_in_text_section, u32);                         // offset in segment
        out_int(SECTION_ID_text, u16);                                // segment
    }end_symbol();
    context->current_block32_offset_in_stream = scope_offset_in_symbol_stream;
    return pointer_to_end_loc;
}

func void emit_debug_info_for_ast__recursive(struct pdb_write_context *context, struct ast_function *function, struct ast *ast){
    assert(ast->byte_offset_in_function >= 0);
    switch(ast->kind){
        case AST_scope:{
            // @cleanup: skip if there are no declarations in the scope, 
            //           also maybe skip if function->scope == scope
            struct ast_scope *scope = cast(struct ast_scope *)ast;
            smm old_offset = context->current_block32_offset_in_stream;
            struct pdb_location pointer_to_end_loc = pdb_begin_scope(context, function, scope);
            
            pdb_emit_regrels_for_scope(context, scope);
            
            for_ast_list(scope->statement_list){
                emit_debug_info_for_ast__recursive(context, function, it->value);
            }
            
            u32 diff = pdb_current_offset_from_location(context, context->module_stream_begin);
            stream_write_bytes(context, &pointer_to_end_loc, &diff, sizeof(u32));
            begin_symbol(0x6);{ // S_END
            }end_symbol();
            context->current_block32_offset_in_stream = old_offset; // @cleanup: should maybe be called pointer
            
        }break;
        case AST_if:{
            struct ast_if *ast_if = cast(struct ast_if *)ast;
            emit_debug_info_for_ast__recursive(context, function, ast_if->statement);
            if(ast_if->else_statement){
                emit_debug_info_for_ast__recursive(context, function, ast_if->else_statement);
            }
        }break;
        case AST_do_while:
        case AST_for:{
            struct ast_for *ast_for = cast(struct ast_for *)ast;
            // @note: this does NOT work as this scope is empty 
            //            emit_debug_info_for_ast__recursive(context, function, &ast_for->scope_for_decl->base);
            //        so we have to do it manually
            
            smm old_offset = context->current_block32_offset_in_stream;
            struct pdb_location pointer_to_end_loc = pdb_begin_scope(context, function, ast_for->scope_for_decl);
            pdb_emit_regrels_for_scope(context, ast_for->scope_for_decl);
            
            if(ast_for->decl){
                emit_debug_info_for_ast__recursive(context, function, ast_for->decl);
            }
            emit_debug_info_for_ast__recursive(context, function, ast_for->body);
            
            // end the scope for the _for_ declaration.
            u32 diff = pdb_current_offset_from_location(context, context->module_stream_begin);
            stream_write_bytes(context, &pointer_to_end_loc, &diff, sizeof(u32));
            begin_symbol(0x6);{ // S_END
            }end_symbol();
            context->current_block32_offset_in_stream = old_offset; // @cleanup: should maybe be called pointer
        }break;
        case AST_switch:{
            struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
            emit_debug_info_for_ast__recursive(context, function, ast_switch->statement);
        }break;
    }
}


func void emit_debug_info_for_function(struct pdb_write_context *context, struct ast_function *function){
    begin_counter(emit_debug_for_function);
    // @cleanup: is this needed somewhere else?
    context->current_block32_offset_in_stream = function->debug_symbol_offset; 
    emit_debug_info_for_ast__recursive(context, function, function->scope);
    
    end_counter(emit_debug_for_function);
}

//////////////////////////////////////////////////////////////////////////////
func void emit_one_pdb_line_info(struct pdb_write_context *context){
    // @cleanup: line should only have lower bits set.. not sure?
    u32 line   = save_truncate_smm_to_u32(context->pdb_line_at);
    u32 offset = save_truncate_smm_to_u32(context->pdb_offset_at);
    
    out_int(offset, u32);
    u32 is_statement = 0x80000000;
    out_int(line | is_statement, u32);
    context->pdb_amount_of_lines += 1;
}

func void emit_pdb_line_info_for_ast__recursive(struct pdb_write_context *context, struct ast_function *function, struct ast *ast){
    if(ast->token->line != context->pdb_line_at){
        assert(ast->token->line > context->pdb_line_at);
        context->pdb_offset_at = ast->byte_offset_in_function + function->size_of_prolog;
        context->pdb_line_at   = ast->token->line;
        emit_one_pdb_line_info(context);
    }
    
    switch(ast->kind){
        // @cleanup: maybe recurse into struct literals...
        case AST_if:{
            struct ast_if *ast_if = cast(struct ast_if *)ast;
            emit_pdb_line_info_for_ast__recursive(context, function, ast_if->statement);
            if(ast_if->else_statement){
                emit_pdb_line_info_for_ast__recursive(context, function, ast_if->else_statement);
            }
        }break;
        case AST_do_while:{
            struct ast_for *do_while = cast(struct ast_for *)ast;
            emit_pdb_line_info_for_ast__recursive(context, function, do_while->body);
            
            if(do_while->condition){
                emit_pdb_line_info_for_ast__recursive(context, function, do_while->condition);
            }
        }break;
        case AST_for:{
            struct ast_for *ast_for = cast(struct ast_for *)ast;
            if(ast_for->decl){
                emit_pdb_line_info_for_ast__recursive(context, function, ast_for->decl);
            }
            
            if(ast_for->condition){
                emit_pdb_line_info_for_ast__recursive(context, function, ast_for->condition);
            }
            
            if(ast_for->increment){
                emit_pdb_line_info_for_ast__recursive(context, function, ast_for->increment);
            }
            
            emit_pdb_line_info_for_ast__recursive(context, function, ast_for->body);
        }break;
        case AST_scope:{
            struct ast_scope *scope = cast(struct ast_scope *)ast;
            for_ast_list(scope->statement_list){
                emit_pdb_line_info_for_ast__recursive(context, function, it->value);
            }
        }break;
        case AST_switch:{
            struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
            emit_pdb_line_info_for_ast__recursive(context, function, ast_switch->switch_on);
            emit_pdb_line_info_for_ast__recursive(context, function, ast_switch->statement);
        }break;
    }
}

func void emit_pdb_line_info_for_function(struct pdb_write_context *context, struct ast_function *function){
    assert(function == function->is_defined);
    
    context->pdb_amount_of_lines = 0;
    context->pdb_offset_at = 0;
    context->pdb_line_at   = function->base.token->line;
    emit_one_pdb_line_info(context); // emit an initial one and then we always emit _after_ updating
    
    emit_pdb_line_info_for_ast__recursive(context, function, function->scope);
}


//////////////////////////////////////////////////////////////////////////////

func struct coff_context print_coff(struct ast_function *entry_point, struct memory_arena *arena, struct memory_arena *scratch){
    
    begin_counter(print_exe);
    
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
    
    struct coff_context ret = zero_struct;
    struct exe_write_context *exe = &ret.exe;
    struct tempoary_memory temp = begin_tempoary_memory(scratch);
    
    push_align(arena, 0x1000);
    
    u8 *base_address = push_data(arena, u8, sizeof(DOS_STUB));
    memcpy(base_address, DOS_STUB, sizeof(DOS_STUB));
    
    // @note: this is just hardcoded in the dos stub
    //*(u32 *)(base_address + 0x3c) = sizeof(DOS_STUB);
    exe->base_address = base_address;
    
    //u8 *pe32_start = arena->current;
    // @cleanup: this should be aligned?
    *push_struct(arena, u8) = 'P';
    *push_struct(arena, u8) = 'E';
    *push_struct(arena, u8) = '\0';
    *push_struct(arena, u8) = '\0';
    { // coff header
        IMAGE_FILE_HEADER *coff = push_struct(arena, IMAGE_FILE_HEADER);
        coff->Machine = 0x8664; // x64
        coff->NumberOfSections = 0; // filled dynamically in write_section_header
        coff->TimeDateStamp = 0x5DA82834; // @cleanup: this is 17.10.19
        coff->SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64);
        coff->Characteristics = 0x1 | 0x2 | 0x20; 
        
        exe->coff = coff;
    }
    
    { // "optional" header
        IMAGE_OPTIONAL_HEADER64 *header = push_struct(arena, IMAGE_OPTIONAL_HEADER64);
        header->Magic = 0x20b;
        header->MajorLinkerVersion = 14; // @note: copied from lld
        header->MinorLinkerVersion = 11; 
        
        //header->SizeOfCode = ???;
        //header->SizeOfInitializedData = ????;
        //header->SizeOfUninitializedData = 0; // filled in bss 
        //header->AddressOfEntryPoint = ???;
        //header->BaseOfCode = ???;
        
        // copied from a dumpbin
        header->ImageBase = 0x140000000;
        header->SectionAlignment = 0x1000;
        header->FileAlignment    = 0x200;
        header->MajorOperatingSystemVersion = 6;
        header->MinorOperatingSystemVersion = 0;
        header->MajorImageVersion = 0;
        header->MinorImageVersion = 0;
        header->MajorSubsystemVersion = 6;
        header->MinorSubsystemVersion = 0;
        header->Win32VersionValue = 0;
        
        //header->SizeOfImage = ???;
        //header->SizeOfHeaders = ???;
        //header->CheckSum = ???; // this is left 0 by msvc
        header->Subsystem = 3; // this is windows or console, 3 is console
        header->DllCharacteristics = 0x8120; // this was weird stuff.
        header->SizeOfStackReserve = mega_bytes(1);
        header->SizeOfStackCommit  = mega_bytes(1);
        header->SizeOfHeapReserve  = 0;
        header->SizeOfHeapCommit   = 0;
        header->LoaderFlags = 0;
        header->NumberOfRvaAndSizes = array_count(header->DataDirectory);
        
        exe->header = header;
    }
    
    
    // gather global symbols
    struct ast_list *initialized_declarations = &(struct ast_list)zero_struct;
    struct ast_list *uninitialized_declarations = &(struct ast_list)zero_struct;
    
    struct ast_list *functions = &(struct ast_list)zero_struct;
    //struct ast_list *dll_functions = &(struct ast_list)zero_struct;
    
    for(u64 i = 0; i < globals.global_declarations.capacity; i++){
        struct ast_node *node = globals.global_declarations.nodes + i;
        if(!node->ast) continue;
        if(node->ast->kind == AST_typedef) continue;
        if(node->ast->kind != AST_function){
            assert(node->ast->kind == AST_declaration);
            struct ast_declaration *decl = cast(struct ast_declaration *)node->ast;
            if(decl->flags & DECLARATION_FLAGS_is_enum_member) continue;
            if(decl->assign_expr){
                ast_list_append(initialized_declarations, scratch, node->ast);
            }else{
                ast_list_append(uninitialized_declarations, scratch, node->ast);
            }
            
            continue;
        }
        struct ast_function *function = cast(struct ast_function *)node->ast;
        if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
        if(function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport) {
            //ast_list_append(dll_functions, scratch, node->ast);
            if(function->as_decl.flags & DECLARATION_FLAGS_is_referanced){
                ast_list_append(&function->dll_import_node->functions, scratch, &function->base);
            }
        }else{
            if(!function->is_defined){
                // we allow non-referanced non-defined functions
                assert(!(function->as_decl.flags & DECLARATION_FLAGS_is_referanced));
                continue;
            }
            function = function->is_defined;
            ast_list_append(functions, scratch, &function->base);
            for_ast_list(function->static_variables){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                if(decl->assign_expr){
                    ast_list_append(initialized_declarations, scratch, &decl->base);
                }else{
                    ast_list_append(uninitialized_declarations, scratch, &decl->base);
                }
            }
        }
    }
    // @hmm: code duplication
    for_ast_list(globals.local_functions){
        struct ast_function *function = cast(struct ast_function *)it->value;
        if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic) continue;
        if(function->type->flags & FUNCTION_TYPE_FLAGS_is_dllimport){
            //ast_list_append(dll_functions, scratch, it->value);
            if(function->as_decl.flags & DECLARATION_FLAGS_is_referanced){
                ast_list_append(&function->dll_import_node->functions, scratch, &function->base);
            }
        }else{
            if(!function->is_defined){
                // we allow non-referanced non-defined functions
                assert(!(function->as_decl.flags & DECLARATION_FLAGS_is_referanced));
                continue;
            }
            function = function->is_defined;
            ast_list_append(functions, scratch, &function->base);
            for(struct ast_list_node *decl_node = function->static_variables.first; decl_node; decl_node = decl_node->next){
                struct ast_declaration *decl = cast(struct ast_declaration *)decl_node->value;
                if(decl->assign_expr){
                    ast_list_append(initialized_declarations, scratch, &decl->base);
                }else{
                    ast_list_append(uninitialized_declarations, scratch, &decl->base);
                }
            }
        }
    }
    
    // Sort the functions by address, this seems to be what they want usually 
    // needed for pdata and maybe later in the pdb
    begin_counter(sort_functions);
    if(functions->count > 1){
        struct sort_function_node{ 
            struct sort_function_node *next;
            struct ast_list list;
            smm min;
            smm max;
        };
        
        struct sort_function_stack_node{
            struct sort_function_stack_node *next;
            struct sort_function_node node;
        };
        
        struct{
            struct sort_function_stack_node *first;
            struct sort_function_stack_node *last;
        } stack = zero_struct;
        
        struct sort_function_stack_node *initial = push_struct(scratch, struct sort_function_stack_node);
        sll_push_front(stack, initial);
        
        initial->node.min = 0;
        initial->node.max = globals.text_section_offset_allocator;
        initial->node.list = *functions;
        
        while(!sll_is_empty(stack)){
            struct sort_function_stack_node *stack_node = stack.first;
            sll_pop_front(stack);
            
            struct sort_function_node *node = &stack_node->node;
            smm piveot = ((node->max - node->min) >> 1) + node->min;
            
            assert(node->list.count > 1);
            
            struct ast_list smaller = zero_struct;
            struct ast_list bigger  = zero_struct;
            
            for(struct ast_list_node *it = node->list.first; it; ){
                struct ast_list_node *next = it->next;
                
                struct ast_function *function = (struct ast_function *)it->value;
                assert(function->is_defined == function);
                
                // non dll functions should better have been emited.
                assert(function->offset_in_text_section >= 0);
                if(function->offset_in_text_section > piveot){
                    sll_push_back(bigger, it);
                    bigger.count++;
                }else{
                    sll_push_back(smaller, it);
                    smaller.count++;
                }
                it = next;
            }
            
            if(bigger.count && smaller.count){
                struct sort_function_stack_node *new_stack_node = push_struct(scratch, struct sort_function_stack_node);
                struct sort_function_node *new_node = &new_stack_node->node;
                new_node->list = bigger;
                new_node->min  = piveot;
                new_node->max  = node->max;
                
                node->list = smaller;
                node->max  = piveot;
                
                new_node->next = node->next;
                node->next = new_node;
                
                if(node->list.count > 1){
                    sll_push_front(stack, stack_node);
                }
                
                if(new_node->list.count > 1){
                    sll_push_front(stack, new_stack_node);
                }
            }else{
                if(smaller.count){
                    node->max  = piveot;
                    sll_push_front(stack, stack_node);
                }else{
                    assert(bigger.count);
                    node->list = bigger;
                    node->min = piveot;
                    sll_push_front(stack, stack_node);
                }
            }
        }
        
        struct ast_list sorted = zero_struct;
        for(struct sort_function_node *it = &initial->node; it; it = it->next){
            sll_push_back_list(sorted, it->list);
            sorted.count += it->list.count;
        }
        
        *functions = sorted;
    }
    end_counter(sort_functions);
    
    /////////////////////////////////////////////////////////////////////////////////////
    //                              start writing the exe                              //
    /////////////////////////////////////////////////////////////////////////////////////
    
    // section table:
#define SECTION_read                  0x40000000
#define SECTION_execute               0x20000000
#define SECTION_code                  0x00000020
#define SECTION_initalized_data       0x00000040
#define SECTION_uninitialized_data    0x00000080
#define SECTION_write                 0x80000000
    
    assert(functions->count); // @cleanup: can this fail?
    
    // @note: apperantly the order of these and the order of the sections below must match
    // @note: @Warning: we also use this as the order for 'enum section_id'
    
    IMAGE_SECTION_HEADER *text = 
        write_section_header(exe, arena, ".text", SECTION_read | SECTION_execute | SECTION_code);
    exe->text = text;
    
    IMAGE_SECTION_HEADER *bss = null;
    if(uninitialized_declarations->count) bss =
        write_section_header(exe, arena, ".bss", SECTION_read | SECTION_write | SECTION_uninitialized_data);
    exe->bss = bss;
    
    // @note: for now we allways have rdata, what happens if this is empty?
    IMAGE_SECTION_HEADER *rdata =
        write_section_header(exe, arena, ".rdata", SECTION_read | SECTION_initalized_data);
    exe->rdata = rdata;
    
    IMAGE_SECTION_HEADER *data = null;
    if(initialized_declarations->count) data =
        write_section_header(exe, arena, ".data", SECTION_read | SECTION_write | SECTION_initalized_data);
    exe->data = data;
    
    IMAGE_SECTION_HEADER *pdata = 
        write_section_header(exe, arena, ".pdata", SECTION_read | SECTION_initalized_data);
    exe->pdata = pdata;
    
    // :End of section table
    u32 actual_header_size = save_truncate_smm_to_u32(arena_current(arena) - base_address);
    exe->header->SizeOfHeaders = align_up(actual_header_size, 0x200);
    
    struct section_write_context *section_writer = &(struct section_write_context)zero_struct;
    section_writer->virtual_address = align_up(actual_header_size, 0x1000);
    section_writer->base_address = exe->base_address;
    section_writer->arena = arena;
    
    
    {   // .text section
        begin_section(section_writer, text);
        
        u8 *memory_for_functions = push_data(arena, u8, globals.text_section_offset_allocator);
        for_ast_list(*functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            u8 *function_memory = memory_for_functions + function->offset_in_text_section;
            
            u8 *at = function_memory;
            memcpy(at, function->base_of_prolog, function->size_of_prolog);
            at += function->size_of_prolog;
            memcpy(at, function->base_of_main_function, function->byte_size_without_prolog);
            function->base_of_main_function = at;
            at += function->byte_size_without_prolog;
            memset(at, 0xcc, function->byte_size - (at - function_memory));
            
            function->memory_location = function_memory;
            function->relative_virtual_address = make_relative_virtual_address(section_writer, function_memory);
            // @cleanup: this should go into its own variable. and probably set atomically in emit_code_for_ast, so we don't have to patch everything
        }
        
        end_section(section_writer);
        
        exe->header->SizeOfCode = exe->text->SizeOfRawData;
        exe->header->BaseOfCode = exe->text->VirtualAddress;
        
        exe->header->AddressOfEntryPoint = cast(DWORD)entry_point->relative_virtual_address;
    }
    
    if(bss){
        begin_section(section_writer, bss);
        end_section(section_writer);
        
        smm size = 0;
        for_ast_list(*uninitialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            size = align_up(size, decl->type->alignment);
            decl->relative_virtual_address = bss->VirtualAddress + size;
            size += decl->type->size;
        }
        
        // @incomplete: what to do when this gets bigger then 0xFFFFFFFF
        u32 bss_size = to_u32(align_up(size, 0x1000));
        section_writer->virtual_address += bss_size;
        bss->Misc.VirtualSize = bss_size;
        bss->PointerToRawData = 0;
        exe->header->SizeOfUninitializedData += bss_size;
    }
    
    
    if(rdata){ // .rdata
        begin_section(section_writer, rdata);
        { // import directory table
            u8 *import_begin = arena_current(arena);
            
            // First an array of 'IMAGE_IMPORT_DESCRIPTOR' one for every dll.
            // @note: +1 as zero_terminated
            IMAGE_IMPORT_DESCRIPTOR *import_descriptors = push_zero_data(arena, IMAGE_IMPORT_DESCRIPTOR, globals.dll_imports.amount + 1);
            u8 *import_end = arena_current(arena);
            
            {            
                u32 dll_import_index = 0;
                for(struct dll_import_node *node = globals.dll_imports.first; node; node = node->next, dll_import_index++){
                    IMAGE_IMPORT_DESCRIPTOR *import = import_descriptors + dll_import_index;                    
                    import->TimeDateStamp = 0;
                    import->ForwarderChain = 0;
                    //import->ImportLookupRVA = ???;
                    //import->name = ????;
                    //import->ImportAddressRVA = ???;
                    
                    char *string = push_cstring_from_string(arena, node->name);
                    import->Name = make_relative_virtual_address(section_writer, string);
                    
                    u64 *import_address_table = push_data(arena, u64, node->functions.count + 1);        
                    // "The last entry is set to zero (NULL) to indicate the end of the table"
                    import_address_table[node->functions.count] = 0; 
                    
                    {
                        u32 i = 0;
                        for_ast_list(node->functions){
                            struct ast_function *function = cast(struct ast_function *)it->value;
                            u16 *hint = push_struct(arena, u16); // we do not care right now @incomplete:
                            push_cstring_from_string(arena, *function->identifier);
                            //push_zero_align(arena, 0x16);
                            
                            // the other functions are relative to 
                            function->memory_location = (u8 *)(import_address_table + i);
                            function->relative_virtual_address = make_relative_virtual_address(section_writer, import_address_table + i);
                            import_address_table[i] = make_relative_virtual_address(section_writer, hint);
                            i++;
                        }
                    }
                    
                    // "The RVA of the import address table. The contents of this table are identical to the contents of the import lookup table until the image is bound"
                    u64 *import_lookup_table = push_array_copy(arena, u64, import_address_table, node->functions.count + 1);
                    
                    
                    
                    import->ImportAddressRVA = make_relative_virtual_address(section_writer, import_address_table);
                    import->ImportLookupRVA  = make_relative_virtual_address(section_writer, import_lookup_table);
                    
                }
                assert(dll_import_index == globals.dll_imports.amount);
            }
            
            // Import data directory
            exe->header->DataDirectory[1].VirtualAddress = make_relative_virtual_address(section_writer, import_begin);
            exe->header->DataDirectory[1].Size = save_truncate_smm_to_u32(import_end - import_begin);
            
            if(globals.want_pdb){
                // setup the 'debug directory' which maps the exe to the pdb
                u8 *debug_info_begin = arena_current(arena);
                {
                    /*
                    +0h   dword        "RSDS" signature
                    +4h   GUID         16-byte Globally Unique Identifier
                    +14h  dword        "age"
                    +18h  byte string  zero terminated UTF8 path and file name
                    */
                    u8 *rsds = push_data(arena, u8, 4);
                    rsds[0] = 'R'; rsds[1] = 'S'; rsds[2] = 'D'; rsds[3] = 'S'; 
                    u32 *guid = push_data(arena, u32, 4);
                    
                    // @cleanup: md5
                    guid[0] = 0x13371337;
                    guid[1] = 0x13371337;
                    guid[2] = 0x13371337;
                    guid[3] = 0x13371337;
                    u32 *age = push_struct(arena, u32);
                    *age = 1;
                    push_cstring_from_string(arena, globals.pdb_full_path);
                }
                u8 *debug_info_end = arena_current(arena);
                
                IMAGE_DEBUG_DIRECTORY *debug = push_struct(arena, IMAGE_DEBUG_DIRECTORY);
                debug->Characteristics = 0; // "reserved and must be zero"
                debug->TimeDateStamp = 0;
                debug->MajorVersion = 11; // @cleanup: find the real numbers for these!
                debug->MinorVersion = 0;
#define IMAGE_DEBUG_TYPE_CODEVIEW 2
                debug->Type = IMAGE_DEBUG_TYPE_CODEVIEW;
                debug->SizeOfData = to_u32(debug_info_end - debug_info_begin);
                debug->AddressOfRawData = make_relative_virtual_address(section_writer, debug_info_begin); 
                debug->PointerToRawData = to_u32(debug_info_begin - exe->base_address);
                
                // Debug directory
                exe->header->DataDirectory[6].VirtualAddress = make_relative_virtual_address(section_writer, debug);
                exe->header->DataDirectory[6].Size = sizeof(IMAGE_DEBUG_DIRECTORY);
                
            }
            
            for(s64 i = 0; i < globals.string_table.capacity; i++){
                struct string_table_entry *val = globals.string_table.entries + i;
                if(!val->value) continue;
                // :string_literal_location
                // @hack?: we copy the data pointer here, so it is valid for patching
                // I cant decide between 'this is a gross hack' and 'this is really smart'...
                
                char* cstring = push_cstring_from_string(arena, *val->value);
                smm addr = make_relative_virtual_address(section_writer, cstring) + exe->header->ImageBase;
                val->value->data = cast(u8 *)(addr);
            }
            
            
            // @note: float literals are loaded rip realtive so emit them here in rdata
            for_ast_list(*functions){
                struct ast_function *function = cast(struct ast_function *)it->value;
                for(struct ast_float_literal *lit = function->float_literals.first; lit; lit = lit->next){
                    
                    if(lit->base.resolved_type == &globals.typedef_f32){
                        f32 *_float = push_struct(arena, f32);
                        *_float = (f32)lit->value;
                        lit->relative_virtual_address = make_relative_virtual_address(section_writer, _float);
                    }else{
                        assert(lit->base.resolved_type == &globals.typedef_f64);
                        f64 *_float = push_struct(arena, f64);
                        *_float = lit->value;
                        lit->relative_virtual_address = make_relative_virtual_address(section_writer, _float);
                    }
                }
            }
        }
        end_section(section_writer);
        
        exe->header->SizeOfInitializedData += exe->rdata->SizeOfRawData;
    }
    
    if(data){ // .data
        // @note: the data section cannot be empty apperantly
        begin_section(section_writer, data);
        
        for_ast_list(*initialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            push_zero_align(arena, decl->type->alignment);
            
            assert(decl->memory_location);
            
            u8 *mem = push_data(arena, u8, decl->type->size);
            u8 *initializer = cast(u8 *)decl->memory_location;
            memcpy(mem, initializer, decl->type->size);
            
            decl->memory_location = mem;
            decl->relative_virtual_address = make_relative_virtual_address(section_writer, mem);
        }
        
        end_section(section_writer);
        exe->header->SizeOfInitializedData += exe->data->SizeOfRawData;
    }
    
    { // .pdata
        begin_section(section_writer, pdata);
        
        // @note: @WARNING: These have to be sorted by address, right now we just sort the 
        //                  functions after gathering them!
        RUNTIME_FUNCTION *rtfs = push_zero_data(arena, RUNTIME_FUNCTION, functions->count);
        
        u32 i = 0;
        for_ast_list(*functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            RUNTIME_FUNCTION *rtf = rtfs + i;
            
            rtf->BeginAddress = save_truncate_smm_to_u32(function->relative_virtual_address);
            // @ugh @cleanup:
            rtf->EndAddress = save_truncate_smm_to_u32(function->relative_virtual_address + function->byte_size);
            
            UNWIND_INFO *unwind = function_fill_in_unwind_info(arena, function);
            rtf->UnwindInfoAddress = make_relative_virtual_address(section_writer, unwind);
            i++;
        }
        end_section(section_writer);
        
        // Exception data directory
        exe->header->DataDirectory[3].VirtualAddress = make_relative_virtual_address(section_writer, rtfs);
        exe->header->DataDirectory[3].Size = to_u32((smm)sizeof(RUNTIME_FUNCTION) * functions->count);
    }
    
    
    exe->header->SizeOfImage = section_writer->virtual_address;
    push_zero_align(arena, 0x200);
    exe->end_address = arena_current(arena);
    
    end_counter(print_exe);
    
    if (!globals.want_pdb) {
        end_tempoary_memory(temp);
        return ret;
    }
    
    begin_counter(print_pdb);
    
    ///////////////////////////////////////////////////////////////////////////////
    //                                  PDB                                      //
    ///////////////////////////////////////////////////////////////////////////////
    
    
    // @note: do not do end temporary memory here, tho it seems tempting.. We need the list we created above.
    //        e.g functions, dll_functions, ...
    
    
    struct string pdb_full_path = push_string_copy(scratch, globals.pdb_full_path);
    replace_characters(pdb_full_path, "/", '\\');
    struct string exe_full_path = push_string_copy(scratch, globals.exe_full_path);
    replace_characters(exe_full_path, "/", '\\');
    
    
    struct pdb_write_context *context = &ret.pdb;
    context->arena = arena;
    context->scratch = scratch;
    
    // the pdb format is formated in pages: first 3 pages are 
    // 1) The PDB header
    // 2) The first free block map
    // 3) The second free block map
    
    // everthing after that is data, organized is so called streams.
    // each stream has an entry in the _stream directory_.
    // The stream directory is a map of _stream indices_ to a list of pages.
    // the data associated to the stream is then serial in the pages associated to the stream.
    
    // the first 5 streams are fixed and given as follows:
    // stream 0: Old Directory
    // stream 1: PDB Stream
    // stream 2: TPI Stream
    // stream 3: DBI Stream (DBI DeBugInfo)
    // stream 4: IPI Stream
    
    /* From the github: tho it seems wrong... this is what they think
    1	    Pdb (header)	             Version information, and information to connect this PDB to the EXE
    2	    Tpi (Type manager)	       All the types used in the executable.
    3	    Dbi (Debug information)	  Holds section contributions, and list of ‘Mods’
    4	    NameMap	                  Holds a hashed string table
    4-(n+4)  n Mod’s (Module information) Each Mod stream holds symbols and line numbers for one compiland
    n+4	  Global symbol hash	       An index that allows searching in global symbols by name
    n+5	  Public symbol hash	       An index that allows searching in public symbols by addresses
    n+6	  Symbol records	           Actual symbol records of global and public symbols
    n+7	  Type hash	                Hash used by the TPI stream.
          */
    
    // the layout we choose corresponds to the 'stream_index' enum above.
    
    // the stream directory will in our case not be more then a page, but there is a double
    // indirection in case it would be.
    // pdb_header->page_number_of_directory_stream_number_list points to an array of pages.
    
    // this gives us the directiory stream which follows the layout:
    // directory_stream:
    // u32 amount_of_streams;
    // u32 stream_sizes[amount_of_streams]
    // u32 stream_one_pages[]
    // u32 stream_two_pages[]
    // ...
    
    // Differance between TPI and IPI is only what appears in them. See this table:
    // TPI Stream	IPI Stream
    // LF_POINTER	LF_FUNC_ID
    // LF_MODIFIER   LF_MFUNC_ID
    // LF_PROCEDURE  LF_BUILDINFO
    // LF_MFUNCTION  LF_SUBSTR_LIST
    // LF_LABEL	  LF_STRING_ID
    // LF_ARGLIST	LF_UDT_SRC_LINE
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
        memcpy(pdb_header->signiture, pdb_signiture, sizeof(pdb_signiture));
        pdb_header->page_size = page_size;
        pdb_header->free_page_map = 1; // has to be 1 or 2
        //pdb_header->number_of_file_pages;
        pdb_header->reserved = 0;
        //pdb_header->page_number_of_directory_stream_page_list = ???;
    }
    
    push_align(arena, page_size);
    
    // @note: free page_maps get pushed automatically by the system.
    // but here would be the free page map 1 and 2 :free_page_maps
    
    
    // From this point on we use the macros instread of working on arena directly 
    
    // stream 0: Previous stream directory 
    // we have to figure out of we can leave this out....
    
    begin_counter(pdb_stream);
    // stream 1: PDB stream
    { // :pdb_stream
        set_current_stream(context, STREAM_PDB);
        struct pdb_stream{
            u32 version;
            u32 time_in_seconds_since_1970;
            u32 amount_of_times_the_pdb_has_been_written;
            u32 guid1;
            u32 guid2;
            u32 guid3;
            u32 guid4;
            u32 length_of_the_string_buffer;
            char buffer_of_named_stream_names[];
        };
        
        struct pdb_stream pdb = zero_struct;
        
        pdb.version = 20000404;
        pdb.time_in_seconds_since_1970 = 0x5DA82834; // @cleanup: this is 17.10.19
        pdb.amount_of_times_the_pdb_has_been_written = 1;
        // who cares!
        //int _rdrand32_step (unsigned int* val);
        //_rdrand32_step(&pdb.guid1);
        //_rdrand32_step(&pdb.guid2);
        //_rdrand32_step(&pdb.guid3);
        //_rdrand32_step(&pdb.guid4);
        pdb.guid1 = 0x13371337;
        pdb.guid2 = 0x13371337;
        pdb.guid3 = 0x13371337;
        pdb.guid4 = 0x13371337;
        
        char string_table[] = "/LinkInfo\0/names";
        
        pdb.length_of_the_string_buffer = sizeof(string_table); 
        
        out_struct(pdb);
        out_struct(string_table);
        
        // following the pdb_stream_header there is a serialized hash table, that maps 
        // offsets of strings in the string_table above to stream_indices or in other words
        // stream names to named streams
        // for us it seems that the only named streams are a "/LinkInfo" and "/names", but "/LinkInfo" is empty
        out_int(2, u32); // size
        out_int(4, u32); // capacity
        
        // now come two bit vectors one for the present_hash_buckets and then a subset of which are deleted
        // struct bit_vector{ u32 word_count; u32 words[]; };
        
        out_int(1, u32); // one word present
        out_int(6, u32); // second and third hash bucket present
        
        out_int(0, u32); // no deleted buckets
        
        // no comes an array of all _present_ hash buckets so in our case 2 buckets.
        // these hash buckets are key value pairs, the key is the offset in the string_table and the 
        // value is the stream_index
        
        // @note: @cleanup: I do not actually know the hash algorithm, tho llvm has it implemented.
        //                  But it seems that we never have to extend this table... so its fine
        
        // key value pair 1
        out_int(sizeof("/LinkInfo"), u32); // offset
        out_int(STREAM_names, u32);        // stream_index
        
        // key value pair 2
        out_int(0, u32);                   // offset
        out_int(STREAM_link_info, u32);    // stream_index
        
        // for some reason they put a 0 here
        // @cleanup: find the referance fot that 
        out_int(0, u32); 
        // the flags, or version or something 
        out_int(20140508, u32);
    }
    end_counter(pdb_stream);
    
    context->maximal_amount_of_type_indices = max_of(0x1000, globals.compound_types.capacity);
    context->amount_of_type_indices = 0;
    context->type_index_to_type_info = push_zero_data(scratch, struct pdb_type_info, context->maximal_amount_of_type_indices);
    
    context->type_stack_size = 0x100;
    context->type_stack = push_data(scratch, struct ast_type *, context->type_stack_size);
    
#define pdb_init_basic_type_index(type_name){\
        globals.typedef_##type_name.pdb_type_index = CV_##type_name;\
        globals.typedef_##type_name.flags |= TYPE_FLAG_pdb_permanent;\
        context->type_index_to_type_info[CV_##type_name].type = &globals.typedef_##type_name;\
    }
    pdb_init_basic_type_index(void);
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
    context->index_offset_buffer = push_data(scratch, struct pdb_index_offset_buffer_entry, context->index_offset_buffer_size);
    context->index_offset_buffer_at = 0;
    context->index_offset_buffer_boundary = -1; // initialized to -1 so we always allocate one immediately
    
    begin_counter(tpi_stream);
    // stream 2: TPI stream
    struct tpi_stream tpi = zero_struct;
    { // :tpi_stream
        // the TPI stream describes type information
        
        set_current_stream(context, STREAM_TPI);
        
        tpi.version = 20040203;
        tpi.header_size = sizeof(struct tpi_stream);
        tpi.minimal_type_index = 0x1000;
        
        tpi.hash_aux_stream_index = -1;
        tpi.hash_stream_index = STREAM_TPI_hash; 
        tpi.hash_key_size = sizeof(u32);
        
        struct pdb_location header_location = stream_allocate_bytes(context, sizeof(tpi));
        struct pdb_location type_record_data_begin = get_current_pdb_location(context);
        context->type_record_data_begin = type_record_data_begin;
        
        // after the header comes a list of variable sized type records    
        {   // write in all the type records, this has to be done in a topological order.

            // register all named compound_types
            for(u64 i = 0; i < globals.compound_types.capacity; i++){
                struct ast_node *node = globals.compound_types.nodes + i;
                if(!node->hash) continue;
                struct ast_type *initial_type = cast(struct ast_type *)node->ast;
                tpi_register_type(context, initial_type);
            } 
            
            // register types for all declaration, this has to be done, as they might be anonymous types...
            for_ast_list(*initialized_declarations){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                tpi_register_type(context, decl->type);
            }
            
            // register types for all declaration, this has to be done, as they might be anonymous types...
            for_ast_list(*uninitialized_declarations){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                tpi_register_type(context, decl->type);
            }
            
            // register all function types
            for_ast_list(*functions){
                struct ast_function *function = cast(struct ast_function *)it->value;
                tpi_register_type(context, &function->type->base);
            }
            
            // register types for all declarations in functions... these should probably be 'scoped'?
            // @incomplete: 
            for_ast_list(*functions){
                struct ast_function *function = cast(struct ast_function *)it->value;
                tpi_register_all_types_in_ast__recursive(context, function->scope);
            }
            
        } // end of emitting all the types
        
        struct pdb_location end_location = get_current_pdb_location(context);
        tpi.amount_of_bytes_of_type_record_data_following_the_header = (u32)pdb_location_diff(end_location, type_record_data_begin);
        
        tpi.maximal_type_index = context->active_page_list->symbol_at;
        
        tpi.hash_value_buffer_offset = 0;
#define IMPLEMENT_HASH_VALUE_BUFFER 0
#if IMPLEMENT_HASH_VALUE_BUFFER
        tpi.hash_value_buffer_length = (tpi.maximal_type_index - tpi.minimal_type_index) * tpi.hash_key_size;
#else
        tpi.hash_value_buffer_length = 0;
#endif
        
#define TPI_NUMBER_OF_HASH_BUCKETS (0x40000 - 1)
        tpi.number_of_hash_buckets = TPI_NUMBER_OF_HASH_BUCKETS;
        
        tpi.index_offset_buffer_offset = tpi.hash_value_buffer_length;
        
        // @note: This is not correct because a symbol could span the next 8kb boundary, but no symbol would
        // begin after that boundary. 
        //    u32 entries = 1 + (tpi.amount_of_bytes_of_type_record_data_following_the_header / kilo_bytes(8));
        // but we have the actual number, wo why not use that.
        tpi.index_offset_buffer_length = context->index_offset_buffer_at * sizeof(context->index_offset_buffer[0]); 
        
        // incremental linking not supported
        tpi.incremental_linking_hash_table_offset = tpi.index_offset_buffer_offset + tpi.index_offset_buffer_length;
        tpi.incremental_linking_hash_table_length = 0; 
        
        
        stream_write_bytes(context, &header_location, &tpi, sizeof(tpi));
    }
    end_counter(tpi_stream);
    
    begin_counter(names_stream);
    // stream 5: Names
    { // :names_stream /names
        set_current_stream(context, STREAM_names);
        out_int(0xEFFEEFFE, u32); // signature
        out_int(1, u32);          // hash version
        struct pdb_location string_buffer_size = stream_allocate_bytes(context, sizeof(u32));
        
        struct pdb_location string_buffer_start = get_current_pdb_location(context);
        {
            // first string is the zero_string? referance: DebugStringTableSubsection::commit
            // also this is _invalid_ I think, see PDBLinker in PDB.cpp of llvm
            out_int(0, u8);
            
            for(struct file_stack_node *node = globals.file_list.first; node; node = node->next){
                node->offset_in_names = pdb_current_offset_from_location(context, string_buffer_start);
                out_string(node->absolute_file_path);
            }
        }
        struct pdb_location string_buffer_end = get_current_pdb_location(context);
        u32 size = pdb_location_diff(string_buffer_end, string_buffer_start);
        stream_write_bytes(context, &string_buffer_size, &size, sizeof(u32));
        
        // @cleanup: lazy as fuck should probably round to the next power of two
        smm smm_bucket_count = 2 * globals.file_list.amount;
        u32 bucket_count = to_u32(smm_bucket_count);
        
        // after the string buffer we have a serialized hash table 
        //     string_hash -> offset in the string buffer
        out_int(bucket_count, u32); // bucket count
        
        // write in the buckets
        u32 *buckets = push_zero_data(scratch, u32, bucket_count);
        for(struct file_stack_node *node = globals.file_list.first; node; node = node->next){
            u32 hash = pdb_string_hash(node->absolute_file_path);
            for(u32 i = 0; bucket_count; i++){
                u32 index = (hash + i) % bucket_count;
                if(buckets[index] != 0) continue;
                buckets[index] = node->offset_in_names;
                break;
            }
        }
        stream_emit_struct(context, buckets, sizeof(u32) * bucket_count);
        
        // at the end is the amount of strings
        out_int(globals.file_list.amount, u32);
    }
    end_counter(names_stream);
    
    begin_counter(tpi_hash_stream);
    // stream 7: TPI hash
    { // :tpi_hash_stream
        set_current_stream(context, STREAM_TPI_hash);
        // tpi_hash_stream layout: (this depends on how we choose it in the tpi stream)
        // u32 tpi_type_hashes[amount_of_type_indices];
        // struct {u32 type_index; u32 offset;} index_offsets[amount_of_type_indices/kilo_bytes(8) + 1];
        // incremental hash table (size 0);
        
        
#if IMPLEMENT_HASH_VALUE_BUFFER
        // a map 'type_index' -> type_hash
        for(u32 i = 0; i < tpi.hash_value_buffer_length; i+= tpi.hash_key_size){
            // hashing a type_record: (LF_STRUCTURE, LF_UNION, LF_ENUM):
            // if(!forward_ref && !is_anonymous){
            //    if(!scoped || hash_unique_name){
            //        return pdb_string_hash(name);
            //    }
            // }
            // return string CRC32(serialized);
            out_int(0, u32);
            //out_int(hash % TPI_NUMBER_OF_HASH_BUCKETS, u32); // could make this tpi.hash_key_size
        }
#endif
        
        // index_offset_buffer:
        // An array of type_index offset pairs, which are used _chunk_ the type_records into 8KB chunks.
        // This gives O(log(n)) acccess time. (binary search over this buffer + linear search of 8KB).
        stream_emit_struct(context, context->index_offset_buffer, tpi.index_offset_buffer_length);
    }
    end_counter(tpi_hash_stream);
    
    begin_counter(ipi_stream);
    
    // reset the 'context->index_offset_buffer'
    context->index_offset_buffer_boundary = -1;
    context->index_offset_buffer_at = 0;
    
    u32 build_info_symbol;
    // stream 4: IPI stream
    struct tpi_stream ipi = zero_struct;
    { // :ipi_stream
        // the IPI Stream describes _id_ information
        
        // @note: For more detail see the tpi-Stream, which is structured exacly the same, the only differance
        //        is what kind of symbols they contain.
        
        set_current_stream(context, STREAM_IPI);
        
        ipi.version = 20040203;
        ipi.header_size = sizeof(struct tpi_stream);
        
        ipi.minimal_type_index = 0x1000;
        
        ipi.hash_aux_stream_index = -1;
        ipi.hash_stream_index = STREAM_IPI_hash;
        ipi.hash_key_size = sizeof(u32);
        ipi.number_of_hash_buckets = TPI_NUMBER_OF_HASH_BUCKETS;
        
        struct pdb_location header_location = stream_allocate_bytes(context, sizeof(ipi));
        struct pdb_location type_record_data_begin = get_current_pdb_location(context);
        context->type_record_data_begin = type_record_data_begin;
        
        // after the header is a list of variable sized type records
        
        // we associate to any function and any type definition a source line
        // for functions: 
        //      LF_FUNC_ID
        // for types: 
        //      LF_UDT_MOD_SRC_LINE
        
        // UDT stands for user defined type
        
        
        // LF_STRING_ID for every used source file
        for(struct file_stack_node *node = globals.file_list.first; node; node = node->next){
            node->ipi = begin_symbol(0x1605);{ // LF_STRING_ID
                out_int(0, u32); // @cleanup: what was this?
                out_string(node->absolute_file_path);
            }end_symbol();
        }
        
        // a LF_UDT_MOD_SRC_LINE for every global compound type @cleanup: typedefs @cleanup: local types
        for(u64 i = 0; i < globals.compound_types.capacity; i++){
            struct ast_node *node = globals.compound_types.nodes + i;
            if(!node->hash) continue;
            struct ast_type *type = cast(struct ast_type *)node->ast;
            assert(type->kind == AST_enum || type->kind == AST_union || type->kind == AST_struct);
            assert(type->pdb_type_index);
            begin_symbol(0x1607);{ // LF_UDT_MOD_SRC_LINE
                out_int(type->pdb_type_index,               u32); // type_index
                out_int(type->token->file->offset_in_names, u32); // file_name    (offset in /names)
                out_int(type->token->line,                  u32); // line_number
                out_int(1,                                  u32); // module_index (@hardcoded one counted)
            }end_symbol();
        }
        
        for_ast_list(*functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            assert(function->type->base.flags & TYPE_FLAG_pdb_permanent);
            begin_symbol(0x1601);{ // LF_FUNC_ID
                // @cleanup: can functions in C be non-global?
                out_int(0, u32); // parent scope id or 0 if its global @incomplete: maybe local functions
                out_int(function->type->base.pdb_type_index, u32); // type index of the type
                out_string(*function->identifier);
            }end_symbol();
        } 
        
        
        // @note: @incomplete: I think this exists for every compilation unit?
        {// build the BUILDINFO
            u32 working_directiory_symbol = begin_symbol(0x1605);{ // LF_STRING_ID
                out_int(0, u32); // id to the list of sub string Id's???????
                out_struct("l:\\l++\\tests");
            }end_symbol();
            
            u32 compiler_name_symbol = begin_symbol(0x1605);{ // LF_STRING_ID
                out_int(0, u32); // id to the list of sub string Id's???????
                out_struct("l:\\l++\\build\\out.exe");
            }end_symbol();
            
            u32 pdb_symbol = begin_symbol(0x1605);{ // LF_STRING_ID
                
                out_int(0, u32); // id to the list of sub string Id's???????
                out_struct("l:\\l++\\tests\\vc140.pdb");// @cleanup why is this here?
            }end_symbol();
            
            u32 command_line_symbol = begin_symbol(0x1605);{ // LF_STRING_ID
                
                out_int(0, u32); // id to the list of sub string Id's???????
                out_struct("-command -line -arguments");
            }end_symbol();
            
            build_info_symbol = begin_symbol(0x1603);{ // BUILDINFO
                out_int(5, u16); // count
                
                //  a list of code item id's
                out_int(working_directiory_symbol,    u32); // current directory
                out_int(compiler_name_symbol,         u32); // build tool (cl.exe)
                out_int(globals.file_list.first->ipi, u32); // source file (foo.cpp)
                out_int(pdb_symbol,                   u32); // pdb file (foo.pdb)
                out_int(command_line_symbol,          u32); // command arguments (-I etc)
            }end_symbol();
        }
        
        struct pdb_location end_location = get_current_pdb_location(context);
        ipi.amount_of_bytes_of_type_record_data_following_the_header = (u32)pdb_location_diff(end_location, type_record_data_begin);
        
        ipi.maximal_type_index = context->active_page_list->symbol_at;
        
        ipi.hash_value_buffer_offset = 0;
#if IMPLEMENT_HASH_VALUE_BUFFER
        ipi.hash_value_buffer_length = (ipi.maximal_type_index - ipi.minimal_type_index) * ipi.hash_key_size;
#else
        ipi.hash_value_buffer_length = 0;
#endif
        
        ipi.index_offset_buffer_offset = ipi.hash_value_buffer_length + ipi.hash_value_buffer_offset;
        ipi.index_offset_buffer_length = context->index_offset_buffer_at * sizeof(context->index_offset_buffer[0]); 
        
        ipi.incremental_linking_hash_table_offset = ipi.index_offset_buffer_offset + ipi.index_offset_buffer_length;
        ipi.incremental_linking_hash_table_length = 0;
        
        stream_write_bytes(context, &header_location, &ipi, sizeof(ipi));
    }
    end_counter(ipi_stream);
    
    begin_counter(ipi_hash_stream);
    // stream 8: IPI hash stream
    { // :ipi_hash_stream
        // same as  :ipi_hash_stream
        set_current_stream(context, STREAM_IPI_hash);
        
        // @cleanup: these are like 
        //    hash_entry = hash % (hash_buffer_size)
        
        // look at hashTypeRecord
        // the hashes are computed via a CRC. Do we acutally have to use their hash algorithm?
        // and crc32.h
#if IMPLEMENT_HASH_VALUE_BUFFER
        for(u32 i = 0; i < ipi.hash_value_buffer_length; i+= ipi.hash_key_size){
            out_int(1, u32); // could make this ipi.hash_key_size
        }
#endif
        
        // index_offset_buffer:
        // An array of type_index offset pairs, which are used _chunk_ the type_records into 8KB chunks.
        // This gives O(log(n)) acccess time. (binary search over this buffer + linear search of 8KB).
        stream_emit_struct(context, context->index_offset_buffer, ipi.index_offset_buffer_length);
    }
    end_counter(ipi_hash_stream);
    
    // stream 5: Link Info stream
    { // :link_info_stream
        set_current_stream(context, STREAM_link_info);
        // yes, this appears to be empty... not sure if we need it?
    }
    
    u32 module_stream_symbol_size;
    u32 module_stream_line_info_size;
    
    begin_counter(module_stream);
    // stream > 12: Module streams
    { // :module_streams
        set_current_stream(context, STREAM_module_zero);
        // module layout: 
        // u32 signature; always 4
        // u8  symbols[symbol_byte_size - 4];
        // u8  line_information[line_info_byte_size]
        // u32 global_refs_byte_size;
        // u8  global_refs[global_refs_byte_size]
        
        struct pdb_location module_stream_begin = get_current_pdb_location(context);
        context->module_stream_begin = module_stream_begin;
        
        out_int(4, u32); // signature
        
        // first thing is always the OBJNAME, then the COMPILE3
        
        begin_symbol(0x1101);{ // OBJNAME
            out_int(0, u32); // signature
            char module_string[] = "l:\\l++\\build\\test.obj";
            out_struct(module_string); // obj name
        }end_symbol();

        begin_symbol(0x113c);{ // S_COMPILE3
        
            out_int(0, u32); // flags, the first byte is for the _language index_ 0 means C
            out_int(0xd0, u16); // mashine
            
            // @note: this is copied from the dump
            out_int(19,    u16); // front end major version
            out_int(11,    u16); // front end minor version
            out_int(25506, u16); // front end build version
            out_int(0,     u16); // front end QFE version ????
            out_int(19,    u16); // back end major version
            out_int(11,    u16); // back end minor version
            out_int(25506, u16); // back end build version
            out_int(0,     u16); // back end QFE version ????
            char version_string[] = "ccup";
            out_struct(version_string);
        }end_symbol();

        
        begin_counter(module_function_info);
        for_ast_list(*functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            function->debug_symbol_offset = pdb_current_offset_from_location(context, module_stream_begin);
            
            struct pdb_location function_pointer_to_end;
            u16 GPROC32 = 0x1110;
            u16 LPROC32 = 0x110f;
            //  @incomplete: @hack
            u16 symbol = (function == entry_point) ? GPROC32 : LPROC32;
            begin_symbol(symbol);{ // GPROC32 (global procdure start)
                out_int(0, u32); // pointer to parent (what is a pointer ?)
                function_pointer_to_end = stream_allocate_bytes(context, sizeof(u32)); // pointer to end
                out_int(0, u32); // pointer to next symbol? why is this allways 0?
                out_int(function->byte_size, u32); // length of the procedure
                out_int(function->size_of_prolog, u32); // offset in the function where debuging makes sense
                out_int(function->byte_size, u32); // end of the section where it makes sense to debug @cleanup
                out_int(function->type->base.pdb_type_index, u32); // typeindex 
                out_int(function->offset_in_text_section, u32); // offset in segment
                out_int(SECTION_ID_text, u16); // segment
                out_int(0, u8); // flags: Frame pointer, @cleanup: custom calling convention, no_return
                // these are somehow never present???
                out_string(*function->identifier);
            }end_symbol();
            
            begin_symbol(0x1012);{ // FRAMEPROC
                out_int(function->stack_space_needed + 8, u32); // frame size 
                out_int(0, u32); // pad size @cleanup: search up what this was again
                out_int(0, u32); // offset of pad
                out_int(0, u32); // callee saved register byte size (not sure)
                out_int(0, u32); // offset of exception handler
                out_int(0, u16); // section id of exception handler
                
                u32 flags = 0;
                flags |= (2 << 16); // local base pointer (2 = rbp) (1 = rsp)
                flags |= (2 << 14); // param base pointer (2 = rbp) (1 = rsp)
                out_int(flags, u32); // asynceh invalid_pgo_counts opt_for_speed Local=rsp Param=rsp ???
            }end_symbol();
            
            // @cleanup: we don't have to emit blocks, that do not have variables
            //           and also we do not have to emit the _initial_ block as it is implicit, 
            //           but these might be things to do in the part that creates debug_members
            // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
            emit_debug_info_for_function(context, function);
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            
            
            u32 diff = pdb_current_offset_from_location(context, module_stream_begin);
            stream_write_bytes(context, &function_pointer_to_end, &diff, sizeof(u32));
            begin_symbol(0x6);{ // S_END
            }end_symbol();
        }
        end_counter(module_function_info);
        
        begin_symbol(0x114c);{ // S_BUILDINFO 
            out_int(build_info_symbol, u32); // Item Id: type_index of a LF_BUILDINFO in the IPI stream
        }end_symbol();
        
        struct pdb_location symbols_end = get_current_pdb_location(context);
        module_stream_symbol_size = pdb_location_diff(symbols_end, module_stream_begin);
        
        begin_counter(module_file_info);
        // here come the md5's
        {
            out_int(0xf4, u32); // DEBUG_S_FILECHKSUM or something like that
            struct pdb_location size_loc = stream_allocate_bytes(context, sizeof(u32));
            struct pdb_location begin_loc = get_current_pdb_location(context);
            for(struct file_stack_node *node = globals.file_list.first; node; node = node->next){
                node->offset_in_f4 = pdb_current_offset_from_location(context, begin_loc);
                out_int(node->offset_in_names, u32);   // offset in /names
                out_int(0x110, u16); // kind of hash function (md5)
                m128 md5 = node->md5;
                stream_emit_struct(context, md5._u8, 16);
                out_align(sizeof(u32)); // align up to 32.. there are two bytes of pad
            }
            struct pdb_location end_loc = get_current_pdb_location(context);
            smm size = pdb_location_diff(end_loc, begin_loc);
            stream_write_bytes(context, &size_loc, &size, sizeof(u32));
        }
        
        end_counter(module_file_info);
        
        begin_counter(module_line_info);
        // now comes the line info
        for_ast_list(*functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
            
            out_int(0xf2, u32); // DEBUG_S_LINES
            struct pdb_location size_loc = stream_allocate_bytes(context, sizeof(u32));
            struct pdb_location begin_loc = get_current_pdb_location(context);
            
            // DEBUG_S_LINES header
            out_int(function->offset_in_text_section, u32);   // offset in the section contribution
            out_int(SECTION_ID_text, u16);                    // section id (segment ?)
            out_int(0, u16);                                  // flags (0x1 = have columns)
            out_int(function->byte_size, u32);                // the size of the contribution
            
            struct pdb_location block_begin = get_current_pdb_location(context);
            
            // offset of the file info in the 0xf4 DEBUG_S_SECTION
            out_int(function->base.token->file->offset_in_f4, u32); 
            struct pdb_location amount_of_lines_loc = stream_allocate_bytes(context, sizeof(u32));
            struct pdb_location block_write = stream_allocate_bytes(context, sizeof(u32));
            
            emit_pdb_line_info_for_function(context, function);
            u32 amount_of_lines = save_truncate_smm_to_u32(context->pdb_amount_of_lines);
            stream_write_bytes(context, &amount_of_lines_loc, &amount_of_lines, sizeof(u32));
            
            struct pdb_location end_loc = get_current_pdb_location(context);
            smm block_size = pdb_location_diff(end_loc, block_begin);
            stream_write_bytes(context, &block_write, &block_size, sizeof(u32));
            
            smm size = pdb_location_diff(end_loc, begin_loc);
            stream_write_bytes(context, &size_loc, &size, sizeof(u32));
        }
        end_counter(module_line_info);
        
        struct pdb_location module_stream_end = get_current_pdb_location(context);
        module_stream_line_info_size = pdb_location_diff(module_stream_end, symbols_end);
        
        out_int(0, u32); // amount_of global referances
    }
    end_counter(module_stream);
    
    u32 linker_stream_symbol_size;
    // stream 12: the * Linker * module
    { // * Linker * module stream
        set_current_stream(context, STREAM_linker_module);
        
        struct pdb_location stream_start = get_current_pdb_location(context);
        // same layout as :module_streams
        out_int(4, u32); // signature
        
        begin_symbol(0x1101); // OBJNAME
        {
            out_int(0, u32); // signature
            char module_string[] = "* Linker *";
            out_struct(module_string); // obj name
        }
        end_symbol();
        
        begin_symbol(0x113c); // S_COMPILE3
        {
            out_int(7, u32); // flags, the first byte is for the _language index_ 7 means LINK
            out_int(0xd0, u16); // mashine
            
            // @note: this is copied from the dump
            out_int(0, u16); // front end major version
            out_int(0, u16); // front end minor version
            out_int(0, u16); // front end build version
            out_int(0, u16); // front end QFE version ????
            out_int(14, u16); // back end major version
            out_int(11, u16); // back end minor version
            out_int(25506, u16); // back end build version
            out_int(0, u16); // back end QFE version ????
            char version_string[] = "ccup";
            out_struct(version_string);
        }
        end_symbol();
        
        begin_symbol(0x113d);
        {
            out_int(0, u8); // reserved
            // cwd - compile working directory?
            // exe - path to program that linked
            // cmd - command line
            
            out_struct("cwd");
            out_struct("l:\\l++\\tests");
            out_struct("exe");
            out_struct("l:\\l++\\build\\out.exe");
            out_struct("pdb");
            out_string(pdb_full_path);
            out_struct("cmd");
            out_struct("-dunno");
        }
        end_symbol();
        
        
        for(u32 i = 0; i < SECTION_ID_count; i++){
            IMAGE_SECTION_HEADER *header = exe->section_headers[i];
            if(!header) continue;
            
            begin_symbol(0x1136);                      // SECTION (a section in the PE executable file)
            {
                out_int(i, u16);                       // I think this _defines_ the section number
                out_int(0xc, u8);                      // 0xc = 12 = log2(0x1000) alignment of the section
                out_int(0, u8);                        // reserved
                out_int(header->VirtualAddress, u32);  // relative virtual address
                out_int(header->SizeOfRawData, u32);   // size in bytes
                out_int(header->Characteristics, u32); // characteristics
                out_struct(header->Name);              // name of the section
            }
            end_symbol();
        }
        
        
        // @cleanup: are these COFF group things neccesarry?
        // they seem to be a more fine grained section information, i.e .pdata is inside .rdata
        
        struct pdb_location stream_end = get_current_pdb_location(context);
        linker_stream_symbol_size = pdb_location_diff(stream_end, stream_start);
        
        out_int(0, u32); // amount_of global referances
    }
    
    begin_counter(dbi_stream);
    // Stream 3: Debug Info stream
    { // :dbi_stream
        set_current_stream(context, STREAM_DBI);
        struct dbi_stream dbi = zero_struct;
        dbi.version = 19990903;
        dbi.version_signiture = (u32)-1;
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
            
            // @cleanup: this should not be invalid, or at least is not in the referance pdb's
            
            struct dbi_module_info module = zero_struct;
            module.module_symbol_stream_index = STREAM_module_zero;
            module.amount_of_source_files = (u16)globals.file_list.amount;
            
            struct section_contribution_entry text_entry = {
                .section_id = SECTION_ID_text,
                .offset = 0,
                .size = text->Misc.VirtualSize,
                .module_index = 0,
                .characteristics = text->Characteristics,
                .data_crc = 0, // @cleanup:
                .reloc_crc = 0,
            };
            
            module.first_section_contribution_entry = text_entry;
            module.byte_size_of_symbol_information = module_stream_symbol_size;
            module.byte_size_of_c13_line_information = module_stream_line_info_size;
            out_struct(module);
            
            out_struct("l:\\l++\\build\\test.obj");
            out_struct("l:\\l++\\build\\test.obj");
            out_align(sizeof(u32));
            
            struct dbi_module_info linker_module = zero_struct;
            linker_module.module_symbol_stream_index = STREAM_linker_module;
            
            const struct section_contribution_entry invalid_section_contribution_entry = {
                .section_id = -1,
                .offset = 0,
                .size = -1,
                .characteristics = 0x0,
                .module_index = -1,
                .data_crc = 0,
                .reloc_crc = 0,
            };
            linker_module.first_section_contribution_entry = invalid_section_contribution_entry;
            linker_module.byte_size_of_symbol_information = linker_stream_symbol_size;
            out_struct(linker_module);
            
            out_struct("* Linker *");
            out_struct("");
            out_align(sizeof(u32));
        }
        
        struct pdb_location section_contribution_begin = get_current_pdb_location(context);
        
        {// section contribution substream
            out_int(0xeffe0000 + 19970605, u32);
            for(s16 i = 0; i < SECTION_ID_count; i++){
                IMAGE_SECTION_HEADER *header = exe->section_headers[i];
                if(!header) continue;
                
                // @cleanup: right now we just make one contribution per section and say it was module 0
                struct section_contribution_entry entry = {
                    .section_id = i,
                    .offset = 0,
                    .size = header->Misc.VirtualSize,
                    .module_index = 0,
                    .characteristics = header->Characteristics,
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
            for(s16 i = 0; i < SECTION_ID_count; i++){
                IMAGE_SECTION_HEADER *header = exe->section_headers[i];
                if(!header) continue;
                
                u32 characteristics = header->Characteristics;
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
                out_int(header->Misc.VirtualSize, u32);
            }
            
            // finally write one dummy entry that "is for absolute symbols" what ever that means
            // (1 << 9) is apperantly for IsAbsoluteAddress
            u16 flags = (1 << 3) | (1 << 9);
            out_int(flags, u16);
            out_int(0, u16);             // Ovl   
            out_int(0, u16);             // group 
            out_int(frame_index, u16); // Frame 
            out_int(u16_max, u16);       // SecName   (Meaning Unknown)
            out_int(u16_max, u16);       // ClassName (Meaning Unknown)
            out_int(0, u32);             // offset
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
            
            u16 amount_of_modules = 2; // @cleanup: Right now there are only two... not sure
            out_int(amount_of_modules, u16);
            u16 amount_of_source_files_u16 = (u16)min_of(globals.file_list.amount, u16_max);
            out_int(amount_of_source_files_u16, u16);
            
            // @cleanup: right now we know that "amount_of_modules" is 2 so we will just write this stuff out
            //           later we want "module_infos" to be in an array, so we can iterate over them
            
            out_int(0, u16); // index of module 0
            out_int(1, u16); // index of module 1
            
            out_int(globals.file_list.amount, u16); // amount_of_source_files for module 0
            out_int(0, u16); // amount_of_source_files for module 1 (* Linker *)
            
            smm at = 0;
            for(struct file_stack_node *node = globals.file_list.first; node; node = node->next){
                out_int(at, u32); // offset of the one and only source file
                at += node->absolute_file_path.amount + 1;
            }
            for(struct file_stack_node *node = globals.file_list.first; node; node = node->next){
                out_string(node->absolute_file_path);
            }
            
            out_align(sizeof(u32));
        }
        
        //struct pdb_location type_server_begin = get_current_pdb_location(context);
        dbi.type_server_map_substream_byte_size = 0;
        {// type server substream (0 bytes)
        }
        
        struct pdb_location edit_and_continue_begin = get_current_pdb_location(context);
        {// edit and continue substream
            // this stream has the same layout as the /names stream, but only has the pdb name in it
            out_int(0xEFFEEFFE, u32); // signature
            out_int(1, u32);          // hash version
            struct pdb_location string_buffer_size = stream_allocate_bytes(context, sizeof(u32));       
            
            struct pdb_location string_buffer_start = get_current_pdb_location(context);
            {
                out_int(0, u8);
                // @incomplete: for source files write them out.
                // @cleanup: at least make this dependent on the acutal file
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
        
        struct pdb_location optional_debug_header_begin = get_current_pdb_location(context);
        { // optional debug header substream
            // @note: I have no idear what any of these are... These meanings are mearly copied
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
    struct gsi_hash_bucket **gsi_hash_table = push_zero_data(scratch, struct gsi_hash_bucket *, IPHR_HASH + 1);
    smm gsi_amount_of_bucket_offsets = 0;
    smm gsi_amount_of_global_symbols = 0;
    
    // stream 9: Symbol record stream
    { // :symbol_records :symbol_stream
        set_current_stream(context, STREAM_symbol_records);
        
        struct pdb_location symbol_record_start = get_current_pdb_location(context);
        
        // PUB32    < exports imports
        // LPROCREF < static procedures
        // PROCREF  < export procedures
        // CONSTANT < enums, not sure what else
        // UDT      < typedefs
        // DATA32   < export globals
        // LDATA32  < static globals
        
        // @cleanup: this should for now be the main symbol and eventually all 'dll_exports'
        // there are also PUB32 for: (non-static-procedures, __imp_Imports, and maybe float values???)
        begin_symbol(0x110e);{                              // PUB32
            out_int(2, u32);                                // flags ?????
            out_int(entry_point->debug_symbol_offset, u32); // offset
            out_int(SECTION_ID_text, u16);                  // segment
            out_string(*entry_point->identifier);           // name
        }end_symbol();
        
        
        // emit a 'LPROCREF' or 'PROCREF' for each function
        // @cleanup: does this break for local functions of the same name
        for_ast_list(*functions){
            struct ast_function *function = cast(struct ast_function *)it->value;
        
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            function->ref_offset = ref_offset;
            
            u16 PROCREF  = 0x1125;
            u16 LPROCREF = 0x1127;
            // @hack:
            u16 symbol = (function == entry_point) ? PROCREF : LPROCREF;
            begin_symbol(symbol);{
                out_int(0, u32);                             // sumName < this appears to always be 0
                out_int(function->debug_symbol_offset, u32); // offset in the module symbol stream
                out_int(1, u16);                             // module index of the stream containing the symbol
                out_string(*function->identifier);           // name (not sure if this is magled or not)
            }end_symbol();
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, ref_offset, scratch, *function->identifier);
            gsi_amount_of_global_symbols += 1;
        }
        
        // emit a 'S_CONSTANT' for every enum member @cleanup: dumb loop, but maybe okay
        for(u64 i = 0; i < globals.compound_types.capacity; i++){
            struct ast_node *node = globals.compound_types.nodes + i;
            if(!node->hash) continue;
            struct ast_compound_type *ast_enum = cast(struct ast_compound_type *)node->ast;
            if(ast_enum->base.kind != AST_enum) continue;
            
            for_ast_list(ast_enum->declarations){
                struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
                u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
                s32 value = integer_literal_as_s32(decl->assign_expr);
                begin_symbol(0x1107);{ // S_CONSTANT
                    out_int(ast_enum->base.pdb_type_index, u32);
                    stream_emit_size_and_name(context, value, true, decl->identifier);
                }end_symbol();
                
                gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, ref_offset, scratch, *decl->identifier);
                gsi_amount_of_global_symbols += 1;
            }
        }
        
        // emit 'S_GDATA32' or 'S_LDATA32' for every global
        for_ast_list(*uninitialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            // @cleanup: should we emit a GDATA if it is not static?
            
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            begin_symbol(0x110c);{
                out_int(decl->type->pdb_type_index, u32); // type_index
                out_int(to_u32(decl->relative_virtual_address - bss->VirtualAddress), u32); // offset in section
                out_int(SECTION_ID_bss, u16);             // section id
                out_string(*decl->identifier);
            } end_symbol();
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, ref_offset, scratch, *decl->identifier);
            gsi_amount_of_global_symbols += 1;
        }
        
        for_ast_list(*initialized_declarations){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            // @cleanup: should we emit a GDATA if it is not static?
            // @cleanup: const?
            
            u32 ref_offset = pdb_current_offset_from_location(context, symbol_record_start);
            begin_symbol(0x110c);{
                out_int(decl->type->pdb_type_index, u32); // type_index
                out_int(to_u32(decl->relative_virtual_address - data->VirtualAddress), u32); // offset in section
                out_int(SECTION_ID_data, u16);             // section id
                out_string(*decl->identifier);
            } end_symbol();
            
            gsi_amount_of_bucket_offsets += global_symbol_stream_hash_table_add(gsi_hash_table, ref_offset, scratch, *decl->identifier);
            gsi_amount_of_global_symbols += 1;
        }
        
        
        // emit 'UDT' for every typedef
        
        // @cleanup: then there are 'PUB32' for __IMPORT_DESCRIPTOR_KERNEL32 and so on 
        
    }
    end_counter(dbi_stream);
    
    // header dump stream
    { // :header_dump
        set_current_stream(context, STREAM_section_header_dump);
        for(s16 i = 0; i < SECTION_ID_count; i++){
            IMAGE_SECTION_HEADER *header = exe->section_headers[i];
            if(!header) continue;
            out_struct(*header);
        }
    }
    
    
    begin_counter(gsi_stream);
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
        // pdb_string_hash(symbol_name) -> {offset, referance_count}
        // this hash_table has (IPHR_HASH + 1) buckets and chains collisions.
        // We serialize all non empty buckets in hash table order with the chains being in sorted order.
        // Finally to see the chain order there is a 'hash_bucket_offsets' array, that specifies
        // the offset in bytes of the n'th present bucket.
        
        // :gsi_hash_table
        // We have constructed this hash table iterativly while adding the symbols to the symbol_stream
        
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
                stream_write_bytes(context, &records, &it->symbol_offset, sizeof(u32));
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
    end_counter(gsi_stream);
    
    begin_counter(psi_stream);
    // public symbol hash stream
    { // :public_symbol_hash_stream
        set_current_stream(context, STREAM_public_symbol_hash);
        
        // public symbol stream header
        struct pdb_location size = stream_allocate_bytes(context, sizeof(u32));// symbol hash byte size
        out_int(4, u32);  // address map byte size
        out_int(0, u32);  // number of thunks
        out_int(0, u32);  // thunk byte size
        out_int(0, u16);  // thunk table section index
        out_int(0, u16);  // pad
        out_int(0, u32);  // thunk table index
        out_int(0, u32);  // number of sections
        
        
        struct pdb_location before_gsi = get_current_pdb_location(context);
        // now comes a copy of the gsi stream, but for public symbols
        out_int(-1, u32); // unclear
        out_int(0xeffe0000 + 19990810, u32); // signature
        
        // @incomplete: right now we only have _entry_point_ later look at the gsi case
        out_int(8, u32); // size of hash records in bytes
        
        u32 bitmap_size = (IPHR_HASH + 32)/32;
        out_int(bitmap_size * 4 +  4, u32); // size of buckets and bitmap in bytes
        
        out_int(entry_point->ref_offset + 1, u32); // offset of the symbol
        out_int(1, u32); // referance count (always 1)
        
        // then the bitmap
        for(u32 i = 0; i < bitmap_size; i++){
            if(i == 0) {
                out_int(1, u32);
            }else{
                out_int(0, u32);
            }
        }
        
        out_int(0, u32); // bucket offsets (one bucket one offset)
        
        u32 diff = pdb_current_offset_from_location(context, before_gsi);
        stream_write_bytes(context, &size, &diff, sizeof(u32));
        
        // "The address map is an array of symbol offsets sorted so that it can be binary searched by address."
        // after the gsi stream copy thing there is an address map mapping "addresses" to symbol offsets
        // @note @hardcoded @hack: right now we only have the entry point so we do not have to sort
        //                         and the entrypoint has offset 0 in the symbol record stream
        out_int(0, u32);
    }
    end_counter(psi_stream);
    
    begin_counter(directory_stream);
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
        
        u32 *sizes = push_data(scratch, u32, amount_of_streams);
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
    end_counter(directory_stream);
    
    
    {
        u8 *directory_page_list_page = push_data(arena, u8, 0x1000);
        
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
    begin_counter(free_page_map);
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
    end_counter(free_page_map);
    
    end_tempoary_memory(temp);
    
#if READ_PDB_AFTER_EMITING_IT
    struct os_file pdb_file = {
        .memory = context->pdb_base,
        .size = context->pdb_size,
    };
    
    read_pdb(scratch, pdb_file);
#endif
    
    end_counter(print_pdb);
    
    return ret;
}

#undef out_struct
#undef out_int
#undef out_align
#undef begin_symbol
#undef end_symbol
