
//////////////////////////////////////////////////////////////////////////
//                               Tokenizer                              //
//////////////////////////////////////////////////////////////////////////

// The main entry point is 'file_tokenize_and_preprocess'.
// The tokenizer loads a file and spits out a 'token_bucket_array', which
// then is traversed using the 'next_token', 'prev_token', 'get_current_token'
// etc. api.
// Whenever the tokenizer loads a file it first calls 'tokenize_file_raw'.
// This gives back a 'token_bucket_array' which has not been preprocessed
// and no keywords have been resolved.
// Then 'file_tokenize_and_preprocess' loops over all tokens using
// 'maybe_expand_current_token_and_eat' which handles defines.
// This is some of the most scary code in the entire code base, because the
// C-preprocessor is really hacky.                              11.09.2020


struct define_argument{
    struct define_argument *next;
    struct atom argument;
    smm argument_index;
    
    // We only expand arguments that we have to:
    // "A parameter in the replacement list, unless preceded by a # or ## 
    // preprocessing token or followed by a ## preprocessing token, is replaced
    // by the corresponding argument after all macros is contained therein
    // have been expanded."                                 31.01.2021
    smm needs_to_be_expanded_count;
    
    // We store whether we have to stringify this argument, so that we know whether we can skip
    // whitespace and comment tokens while collecting arguments for the define.
    // @cleanup: We could think about using this to only ever expand the argument once.
    smm stringify_index;
};

struct define_argument_list{
    struct define_argument *first;
    struct define_argument *last;
    smm count;
};

func struct define_argument *lookup_define_argument(struct define_argument_list list, struct token *ident){
    struct define_argument *ret = null;
    for(struct define_argument *arg_it = list.first; arg_it; arg_it = arg_it->next){
        if(atoms_match(arg_it->argument, ident->atom)){
            ret = arg_it;
            break;
        }
    }
    return ret;
}

struct define_list{
    struct define_node *first;
    struct define_node *last;
};

struct define_replacement_node{
    struct define_replacement_node *next;
    enum define_replacement_kind{
        DEFINE_REPLACEMENT_tokens,   // just append the 'tokens' in the replacement node
        DEFINE_REPLACEMENT_argument, // append the expanded argument in 'argument_index'
        DEFINE_REPLACEMENT_hash,     // stringify argument 'stringify_argument' and append it
        DEFINE_REPLACEMENT_hashhash, // concatenate prefix_argument and postfix_argument
    } kind;
    
    union{
        struct token_array tokens; // DEFINE_REPLACEMENT_tokens
        smm stringify_index;    // DEFINE_REPLACEMENT_hash
        
        struct{
            smm argument_index;        
            struct define_argument *argument;
            smm needs_to_be_expanded_counter;
        }; // DEFINE_REPLACEMENT_argument,
        
        struct{
            // :cannot_store_prefix
            smm prefix_argument;         // can be '-1' if invalid
            smm postfix_argument;        // can be '-1' if invalid
            struct token *postfix_token; // used if 'postfix_argument == -1'
        }; // DEFINE_REPLACEMENT_hashhash
    };
};

struct define_node{
    struct define_node *next;
    struct define_node *prev;
    struct token *defined_token;
    struct atom name;
    
    // 
    // Flags:
    u32 is_function_like : 1;
    u32 is_varargs       : 1;
    u32 is_builtin       : 1;
    u32 is_disabled      : 1; // Disabled if we are within its expansion.
    
    // 
    // Builtin defines:
    u32 is_defined  : 1;
    u32 is___pragma : 1;
    u32 is___FILE__ : 1;
    u32 is___LINE__ : 1;
    
    struct define_argument_list arguments;
    smm stringify_count; // The amount of arguments that have to be stringified when evaluating the define.
    
    struct replacement_list{
        struct define_replacement_node *first;
        struct define_replacement_node *last;
    } replacement_list;
};

func struct token *get_current_token_raw(struct context *context){
    
    while(true){
        if(!context->token_stack.first) return &globals.invalid_token;
        struct token_stack_node *node = context->token_stack.first;
        
        if(node->at >= node->tokens.amount){
            if(node->define_to_reenable_on_exit){
                node->define_to_reenable_on_exit->is_disabled = false;
                context->define_depth--;
                if(context->define_depth == 0){
                    context->macro_expansion_token = null;
                }
                assert(context->define_depth >= 0);
            }
            
            sll_pop_front(context->token_stack);
            continue;
        }
        
        return node->tokens.data + node->at;
    }
}


func b32 tokenizer_is_at_the_end_of_the_file(struct context *context){
    return get_current_token_raw(context)->type == TOKEN_invalid;
}


func struct token *next_token_raw(struct context *context){
    struct token *ret = get_current_token_raw(context);
    
    struct token_stack_node *node = context->token_stack.first;
    if(node){
        // get_current_token just made sure this is true!
        assert(node->at < node->tokens.amount);
        node->at++;    
    }
    
    return ret;
}

func struct token *expect_token_raw(struct context *context, struct token *site, enum token_type type, char *error){
    struct token *token = get_current_token_raw(context);
    if(token->type != type){
        report_syntax_error(context, site, error);
        return token;
    }
    return next_token_raw(context);
}

func struct token *peek_token_raw(struct context *context, enum token_type type){
    struct token *token = get_current_token_raw(context);
    if(token->type == type){
        return token;
    }
    return null;
}

func struct token *peek_token_eat_raw(struct context *context, enum token_type type){
    struct token *token = get_current_token_raw(context);
    if(token->type == type){
        return next_token_raw(context);
    }
    return null;
}

func b32 skip_until_tokens_are_balanced_raw(struct context *context, struct token *initial_token, enum token_type open, enum token_type closed){
    if(!initial_token){
        initial_token = next_token_raw(context);
    }
    
    u64 count = 1;
    while(true){
        if(tokenizer_is_at_the_end_of_the_file(context)){
            // :Error this should report the beginning and the end
            report_error(context, initial_token, "Scope not ended.");
            
            return false;
        }else if(peek_token_raw(context, closed)){
            if (--count == 0) break;
        }else if(peek_token_raw(context, open)){
            count++;
        }
        next_token_raw(context);
    }
    
    next_token_raw(context);
    return true;
}

//_____________________________________________________________________________________________________________________

func void print_token(struct context *context, struct token *token, b32 print_whitespace_and_comments);
func void print_define(struct context *context, struct define_node *define, char *prefix){
    (void)(context);
    
    char *file = globals.file_table.data[define->defined_token->file_index]->absolute_file_path;
    print("%s(%u,%u): [%lld] %s %.*s", file, define->defined_token->line, define->defined_token->column, 
            context->current_compilation_unit->index, prefix, define->name.size, define->name.data);
    
    if(define->is_function_like){
        print("(");
        
        for(struct define_argument *arg = define->arguments.first; arg; arg = arg->next){
            print("%.*s", arg->argument.size, arg->argument.data);
            if(arg->next){
                print(", ");
            }
        }
        
        print(")");
    }
    print(" ");
    
    for(struct define_replacement_node *node = define->replacement_list.first; node; node = node->next){
        switch(node->kind){
            case DEFINE_REPLACEMENT_tokens:{
                for(smm i = 0; i < node->tokens.size; i++){
                    print("%.*s", node->tokens.data[i].size, node->tokens.data[i].data);
                }
            }break;
            case DEFINE_REPLACEMENT_argument:{
                struct define_argument *arg = define->arguments.first;
                for(int i = 0; i < node->argument_index; i++){
                    arg = arg->next;
                }
                
                print("%.*s", arg->argument.size, arg->argument.data);
            }break;
            case DEFINE_REPLACEMENT_hash:{
                struct define_argument *arg = define->arguments.first;
                for(; arg; arg = arg->next){
                    if(arg->stringify_index == node->stringify_index) break;
                }
                assert(arg);
                
                print("#%.*s", arg->argument.size, arg->argument.data);
            }break;
            case DEFINE_REPLACEMENT_hashhash:{
                if(node->prefix_argument != -1){
                    struct define_argument *arg = define->arguments.first;
                    for(int i = 0; i < node->prefix_argument; i++){
                        arg = arg->next;
                    }
                    print("%.*s", arg->argument.size, arg->argument.data);
                }
                
                print("##");
                
                if(node->postfix_argument != -1){
                    struct define_argument *arg = define->arguments.first;
                    for(int i = 0; i < node->prefix_argument; i++){
                        arg = arg->next;
                    }
                    print("%.*s", arg->argument.size, arg->argument.data);
                }
                
                if(node->postfix_token){
                    print("%.*s", node->postfix_token->size, node->postfix_token->data);
                }
            }break;
            invalid_default_case();
        }
    }
    
    print("\n");
}

func void register_define(struct context *context, struct define_node *new_node){
    
    assert(new_node->name.size != 0);
    
#if 0
    if(context->current_compilation_unit->index == 41){
        print_define(context, new_node, "#define");
    }
#endif
    
#ifdef DEFINE_TO_PRINT
    if(string_match(new_node->name.string, string(DEFINE_TO_PRINT))){
        print_define(context, new_node, "#define");
    }
#endif
    
    if(2 * context->define_table.amount + 1 > context->define_table.capacity){
        begin_counter(context, define_table_grow);
        u64 new_capacity = 2 * max_of(context->define_table.capacity, 256);
        u64 new_mask = new_capacity - 1;
        struct define_list *new_lists = push_data(&context->scratch, struct define_list, new_capacity);
        
        for(u64 i = 0; i < context->define_table.capacity; i++){
            struct define_list list = context->define_table.lists[i];
            for(struct define_node *node = list.first; node;){
                // save 'next' because we are gonna clober it in just a second
                struct define_node *next = node->next;
                
                // insert 'node' into the new table
                u64 hash  = node->name.string_hash;
                u64 index = hash & new_mask;
                dll_push_back(new_lists[index], node);
                node = next;
            }
        }
        
        context->define_table.capacity = new_capacity;
        context->define_table.lists    = new_lists;
        context->define_table.mask     = new_mask;
        end_counter(context, define_table_grow);
    }
    
    // insert the node into the table
    u64 hash = new_node->name.string_hash;
    struct define_list *at = context->define_table.lists + (hash & context->define_table.mask);
    
    dll_push_front(*at, new_node);
    context->define_table.amount += 1;
    
}

func struct define_node *lookup_define(struct context *context, struct atom name){
    
    smm slot_index = (name.string_hash & context->define_table.mask);
    struct define_list *slot = context->define_table.lists + slot_index;
    
    struct define_node *define = null;
    
    for(struct define_node *node = slot->first; node; node = node->next){
        if(atoms_match(node->name, name)){
            define = node;
            break;
        }
    }
    
    return define;
}

//_____________________________________________________________________________________________________________________

func void tokenizer_report_error(struct context *context, struct token *token, struct token *macro_expansion_token, char *error){
    struct token *ret = push_struct(&context->scratch, struct token);
    *ret = *token;
    // :copy_expanded_location
    if(macro_expansion_token){
        ret->line   = macro_expansion_token->line;
        ret->column = macro_expansion_token->column;
        ret->file_index   = macro_expansion_token->file_index;
    }
    report_error(context, ret, error);
}

//_____________________________________________________________________________________________________________________

func b32 u8_is_valid_in_c_ident(u8 a){
    // either ascii alpha numeric, $ or utf8 are allowed
    return u8_is_alpha_numeric(a) || (a == '$') || (a & 0x80);
}

struct escaped_string{
    struct string string;
    
    enum string_kind{
        // @warning: we use that the string_kind is just the element size.
        // :string_kind_is_element_size
        STRING_KIND_invalid,
        STRING_KIND_utf8  = 1,  //  u8"", ""
        STRING_KIND_utf16 = 2, //  u16"", L"", u""
        STRING_KIND_utf32 = 4, //  u32"", U""
    } string_kind;
};


static u64 parse_hex_string_to_u64(struct string *characters, int *overflow){
    
    smm length = characters->size;
    
    u64 value = 0;
    for(smm index = 0; index < characters->size; index++){
        u32 hex = characters->data[index];
        
        u32 number = 0;
        if('9' >= hex && hex >= '0'){
            number = hex - '0';
        }else if('f' >= hex && hex >= 'a'){
            number = hex - 'a' + 10;
        }else if('F' >= hex && hex >= 'A'){
            number = hex - 'A' + 10;
        }else{
            length = index;
            break;
        }
        
        if(value & (15ui64 << 60)) *overflow = true;
        value <<= 4; // multiply by 16
        u64 new_val = value + number;
        if(new_val < value) *overflow = true;
        value = new_val;
    }
    
    string_eat_front(characters, length);
    
    return value;
}

// this expects that the '\' was already removed from 'to_escape'.
static u32 eat_escape_from_string_and_return_codepoint(struct context *context, struct token *token, struct string *to_escape, u32 maximal_value){
    
    assert(to_escape->size); // The string should not end in a '\', how did we get here?
    
    u8 *escape_start = to_escape->data;
    u8 escaped_character = to_escape->data[0];
    string_eat_front(to_escape, 1);
    
    switch(escaped_character){
        
        // simple-escapes-sequence:
        //   \' \" \? \\ \a \b \f \n \r \t \v
        case '\'': return '\''; 
        case '"':  return '"'; 
        case '?':  return '?'; 
        case '\\': return '\\'; 
        case 'a':  return '\a'; 
        case 'b':  return '\b'; 
        case 'f':  return '\f'; 
        case 'n':  return '\n'; 
        case 'r':  return '\r'; 
        case 't':  return '\t'; 
        case 'v':  return '\v'; 
        case 'e':  return 27; // non-standard "Escape" character.
        
        // octal-escape-sequence:
        //    \octal-digit
        //    \octal-digit octal-digit
        //    \octal-digit octal-digit octal-digit
        case '0': case '1': case '2': case '3': case '4':case '5':case '6':case '7':{
            
            u32 value = escaped_character - '0';
            for(u32 index = 1; index < 3; index++){
                if(to_escape->size == 0 || !('0' <= to_escape->data[0] && to_escape->data[0] <= '7')){
                    break;
                }
                
                value = 8 * value + (to_escape->data[0] - '0');
                string_eat_front(to_escape, 1);
            }
            
            // 0 <= value <= 0x1ff = 0o777
            if(value > maximal_value){
                report_error(context, token, "Octal escape '\\%.*s' (0x%x) constant to big for character. Maximal value for character type is 0x%x.", to_escape->data - escape_start, escape_start, value, maximal_value);
            }
            return value;
        }break;
        
        // hexadecimal-escape-sequence
        //    \x hexadecimal-digit
        //    hexadecimal-escape-sequence hexadecimal-digit
        case 'x':{
            
            if(!to_escape->size || !u8_is_hex_number(to_escape->data[0])){    
                report_error(context, token, "Expected at least one digit after '\\x'."); //:Error
                return 0;
            }
            
            int overflow = 0;
            u64 value = parse_hex_string_to_u64(to_escape, &overflow);
            
            if(overflow){
                report_error(context, token, "Encountered integer overflow while parsing hex escape sequence '\\%.*s'.", to_escape->data - escape_start, escape_start);
            }else if(value > maximal_value){
                report_error(context, token, "Hex escape sequence '\\%.*s' (0x%llx) constant to big for character. Maximal value for character type is 0x%x.", to_escape->data - escape_start, escape_start, value, maximal_value);
            }
            return (u32)value;
        }break;
        
        case 'U': case 'u':{
            int size = escaped_character == 'u' ? 4 : 8;
            
            if(to_escape->size < size){
                report_error(context, token, "Unicode code point escape sequence '\\%c' must be followed by %s hex characters.", escaped_character, size == 4 ? "four" : "eight");
                string_eat_front(to_escape, to_escape->size);
                return 0;
            }
            
            struct string hex_chars = string_eat_front(to_escape, size);
            int overflow = 0;
            u64 value = parse_hex_string_to_u64(&hex_chars, &overflow);
            assert(!overflow);
            
            if(hex_chars.size){
                // 
                // If there are still some characters in 'hex_chars' that means 
                // 'parse_hex_string_to_u64' was not able to parse all the characters as hex.
                // Report this as an error.
                // 
                report_error(context, token, "Unicode code point escape sequence '\\%c%.*s' must be followed by %s hex characters, but %c is not a hex character.", escaped_character, hex_chars.size, hex_chars.data, size == 4 ? "four" : "eight", hex_chars.data[0]);
            }
            
            return (u32)value;
        }break;
        
        default:{
            report_error(context, token, "Unknown escape sequence '\\%c'.", escaped_character);
            return 0;
        }break;
    }
}

u32 utf8_read_codepoint(struct context *context, struct token *token, struct string *string){
    
    u8 leading_character = string->data[0];
    string_eat_front(string, 1);
    
    // Return if the character is in the ascii range.
    if((leading_character & 0b10000000) != 0b10000000) return leading_character;
    
    // Figure out the size of the rune based on the leading ones.
    u32 size = 0; 
    for(u32 bit = 0b01000000; bit; bit >>= 1){
        if(!(bit & leading_character)) break;
        size += 1;
    }
    
    if(size > string->size){
        // :Error
        report_syntax_error(context, token, "Literal contains invalid utf-8.");
        return 0;
    }
    
    // The 'leading character' has 'size + 1' leading ones.
    u32 leading_character_bits = 8 - (size + 1);
    u32 leading_character_mask = (1u << leading_character_bits) - 1;
    
    //
    // The leading character contains the "top" bits of the codepoint.
    // The last byte contains the lowest bits of the codepoint.
    // Each byte after the leading character contains 6 bits of the codepoint.
    //
    u32 shift = 1u << (6 * size);
    u32 codepoint = shift * (leading_character & leading_character_mask);
    shift >>= 6;
    
    for(u32 index = 0; index < size; index++, shift >>= 6){
        u8 character = string->data[index];
        
        if((character & 0b11000000) != 0b10000000){
            report_syntax_error(context, token, "Literal contains invalid utf-8.");
            return 0;
        }
        
        codepoint |= shift * (character & 0b00111111);
    }
    
    string_eat_front(string, size);
    return codepoint;
}

enum string_kind string_prefix_to_kind(struct string prefix){
    
    enum string_kind string_kind = STRING_KIND_invalid;
    
    switch(prefix.size){
        case 0:{
            string_kind = STRING_KIND_utf8;
        }break;
        case 1:{
            if(prefix.data[0] == 'L') string_kind = STRING_KIND_utf16;
            if(prefix.data[0] == 'u') string_kind = STRING_KIND_utf16;
            if(prefix.data[0] == 'U') string_kind = STRING_KIND_utf32;
        }break;
        case 2:{
            if(string_match(prefix, string("u8"))) string_kind = STRING_KIND_utf8;
        }break;
        case 3:{
            if(string_match(prefix, string("u16"))) string_kind = STRING_KIND_utf16;
            if(string_match(prefix, string("u32"))) string_kind = STRING_KIND_utf32;
        }break;
        invalid_default_case();
    }
    
    return string_kind;
}

struct token_and_string{
    struct token *token;
    struct string string;
};

struct string escape_and_convert_string_array(struct context *context, struct token_and_string *strings, smm amount_of_strings, smm total_characters, enum string_kind string_kind){
    
    //
    // The source is assumed to be in utf8:
    // 
    // utf8 works as follows:
    //     | Byte 1 | Byte 2 | Byte 3 | Byte 4 |  Codepoints  |
    //     |________|________|________|________|______________|
    //     |0xxxxxxx|        |        |        |    0 - 7F    |
    //     |110xxxxx|10xxxxxx|        |        |   80 - 7FF   |
    //     |1110xxxx|10xxxxxx|10xxxxxx|        |  800 - FFFF  |
    //     |11110xxx|10xxxxxx|10xxxxxx|10xxxxxx|10000 - 10FFFF|
    // 
    // For codepoint 0x12345 = 0b10010001101000101, the highest bits
    // are in 'Byte 1' the lowest bits are in 'byte 4', hence
    // '0b000_010010_001101_000101' will become:
    //     utf8(11110000 10010010 10001101 10000101)
    //
    // utf16 works as follows:
    //     (word & 0xF800) != 0xD800 -> word (ranges 0 - 0xD7FF and E000 - FFFF)
    // if '(word & 0xF800) == 0xD800' then:
    //     |     word 1     |     word 2     |           Codepoint            |
    //     |________________|________________|________________________________|
    //     |110110yyyyyyyyyy|110111xxxxxxxxxx| yyyyyyyyyyxxxxxxxxxx + 0x10000 |
    // this gives the range 10000 - 10FFFF.
    //
    // utf32 is just a linear encoding of code points.
    //
    
    // :string_kind_is_element_size
    u8 *buffer = push_struct_(context->arena, total_characters * string_kind, string_kind);
    smm buf_size = total_characters; // We have at most one character for every character in the source string.
    smm buf_at = 0;
    
    switch(string_kind){
#define handle_escaping_out(c) { assert(buf_at < buf_size);  buf[buf_at++] = (c); }
        
        case STRING_KIND_utf8:{
            u8 *buf = buffer;
            
            for(u32 string_index = 0; string_index < amount_of_strings; string_index++){
                
                struct token *token     = strings[string_index].token;
                struct string to_escape = strings[string_index].string;
                
                while(to_escape.size){
                    
                    if(to_escape.data[0] != '\\'){
                        //
                        // For utf8 we don't want to do any real processing.
                        // If the user wants his invalid utf8 in there, let them have it.
                        // Just process escapes.
                        //
                        handle_escaping_out(to_escape.data[0]);
                        string_eat_front(&to_escape, 1);
                        continue;
                    }
                    
                    string_eat_front(&to_escape, 1);
                    
                    // Handle escaped newlines.
                    if(string_eat_newline(&to_escape)) continue;
                    
                    // @note: We know the string cannot end in a backslash, 
                    //        so the next character is fine to read.
                    
                    // The escapes \u and \U need reencoding.
                    int needs_encoding = (to_escape.data[0]|32) == 'u';
                    
                    u32 codepoint = eat_escape_from_string_and_return_codepoint(context, token, &to_escape, 0xff);
                    
                    if(needs_encoding){
                        if(codepoint < 0x80){
                            handle_escaping_out((u8)codepoint);
                            continue;
                        }
                        
                        // 
                        // Figure out how many bits are set.
                        u32 bit_size = 32 - count_leading_zeros32(codepoint);
                        
                        // 
                        // We can fit 6-bits per byte of utf-8, except the first one.
                        // We calculate the minimum amount of trailing bytes, 
                        // and how many bits do not fit.
                        // 
                        u32 utf8_bytes = bit_size / 6;
                        u32 utf8_bits  = bit_size % 6;
                        
                        // 
                        // utf8 sets one bit per byte in the leading byte, plus we need one zero.
                        if((utf8_bytes + /*count the leading byte*/1) + /*need a separating zero*/1 + utf8_bits > 8){
                            // We could not fit the utf8_bits.
                            utf8_bytes += 1;
                        }
                        
                        // The 'size_mask' has one bit set per byte in the encoding, including the leading byte.
                        // The mask is shifted to be the top bits of the leading byte.
                        u8 size_mask = ((1 << (utf8_bytes + 1)) - 1) << (8 - (utf8_bytes + 1));
                        
                        
                        //
                        // The leading character contains the "top" bits of the codepoint.
                        // The last byte contains the lowest bits of the codepoint.
                        // Each byte after the leading character contains 6 bits of the codepoint.
                        //
                        u8 leading_byte = (u8)(size_mask | (codepoint >> (6 * utf8_bytes)));
                        handle_escaping_out(leading_byte);
                        
                        u32 shift = 6 * (utf8_bytes - 1);
                        for(u32 index = 0; index < utf8_bytes; index++, shift -= 6){
                            u8 bits = 0x80 | ((codepoint >> shift) & 0b00111111);
                            handle_escaping_out(bits);
                        }
                    }else{
                        // Allow for specifying invalid utf8 with \x<hex> and \0<octal>
                        handle_escaping_out((u8)codepoint);
                    }
                }
            }
        }break;
        
        case STRING_KIND_utf16:{
            u16 *buf = (u16 *)buffer;
            
            for(u32 string_index = 0; string_index < amount_of_strings; string_index++){
                
                struct token *token     = strings[string_index].token;
                struct string to_escape = strings[string_index].string;
                
                while(to_escape.size){
                    if(to_escape.data[0] != '\\'){
                        u32 codepoint = utf8_read_codepoint(context, token, &to_escape);
                        
                        if(0xD800 <= codepoint && codepoint < 0xE000){
                            // invalid unicode range for utf-16... Its fine, noone will ever see this message
                            report_error(context, token, "This contains codepoint 0x%x, which is not encodable in utf-16.", codepoint);
                        }
                        
                        if(codepoint < 0x10000){
                            handle_escaping_out((u16)codepoint);
                        }else{
                            codepoint = codepoint - 0x10000;
                            assert(codepoint < 0x100000);
                            
                            // first the high bits, then the low bits
                            handle_escaping_out(0xD800 | ((codepoint >> 10) & 0b1111111111));
                            handle_escaping_out(0xDD00 | ((codepoint >>  0) & 0b1111111111));
                        }
                        continue;
                    }
                    
                    string_eat_front(&to_escape, 1);
                    
                    // Handle escaped newlines.
                    if(string_eat_newline(&to_escape)) continue;
                    
                    // @note: We know the string cannot end in a backslash, 
                    //        so the next character is fine to read.
                    
                    // The escapes \u and \U need reencoding.
                    int needs_encoding = (to_escape.data[0]|32) == 'u';
                    
                    u32 codepoint = eat_escape_from_string_and_return_codepoint(context, token, &to_escape, 0xffff);
                    
                    if(needs_encoding){
                        // 
                        // @cleanup: copy and paste from above.
                        // 
                        
                        if(0xD800 <= codepoint && codepoint < 0xE000){
                            // invalid unicode range for utf-16... Its fine, noone will ever see this message
                            report_error(context, token, "This contains codepoint 0x%x, which is not encodable in utf-16.", codepoint);
                        }
                        
                        if(codepoint < 0x10000){
                            handle_escaping_out((u16)codepoint);
                        }else{
                            codepoint = codepoint - 0x10000;
                            assert(codepoint < 0x100000);
                            
                            // first the high bits, then the low bits
                            handle_escaping_out(0xD800 | ((codepoint >> 10) & 0b1111111111));
                            handle_escaping_out(0xDD00 | ((codepoint >>  0) & 0b1111111111));
                        }
                    }else{
                        // Allow the user to specify invalid utf-16 through escapes!
                        handle_escaping_out((u16)codepoint);
                    }
                }
            }
        }break;
        
        case STRING_KIND_utf32:{
            u32 *buf = (void *)(buffer + buf_at * string_kind);
            
            for(u32 string_index = 0; string_index < amount_of_strings; string_index++){
                
                struct token *token     = strings[string_index].token;
                struct string to_escape = strings[string_index].string;
                
                while(to_escape.size){
                    if(to_escape.data[0] != '\\'){
                        u32 codepoint = utf8_read_codepoint(context, token, &to_escape);
                        handle_escaping_out(codepoint);
                        continue;
                    }
                    
                    string_eat_front(&to_escape, 1);
                    
                    // Handle escaped newlines.
                    if(string_eat_newline(&to_escape)) continue;
                    
                    u32 codepoint = eat_escape_from_string_and_return_codepoint(context, token, &to_escape, 0xffffffff);
                    handle_escaping_out((u32)codepoint);
                }
            }
        }break;
        
        invalid_default_case();
#undef handle_escaping_out
    }
    
    // @cleanup: Decalloctate excess if possible?
    struct string escaped_string = {
        .data = buffer,
        .size = buf_at * string_kind,
    };
    
    return escaped_string;
}


// return buffer is in 'context->scratch'
func struct escaped_string handle_escaping(struct context *context, struct token *token){
    assert(token->type == TOKEN_string_literal || token->type == TOKEN_character_literal);
    
    struct string to_escape = token_get_string(token);
    
    u8 delimiter = token->type == TOKEN_string_literal ? '"' : '\'';
    struct string prefix = eat_until_char(&to_escape, delimiter, /*eat_delimiter*/false);
    
    enum string_kind string_kind = string_prefix_to_kind(prefix);
    
    to_escape = strip_quotes(to_escape);
    
    struct token_and_string token_and_string = {
        .string = to_escape,
        .token  = token,
    };
    
    struct escaped_string ret = {
        .string = escape_and_convert_string_array(context, &token_and_string, 1, to_escape.size, string_kind),
        .string_kind = string_kind,
    };
    
    return ret;
}

struct parsed_integer{
    u64 value;
    
    enum number_kind{
        NUMBER_KIND_invalid,
        
        NUMBER_KIND_int,
        NUMBER_KIND_long,
        NUMBER_KIND_long_long,
        
        NUMBER_KIND_unsigned,
        NUMBER_KIND_unsigned_long,
        NUMBER_KIND_unsigned_long_long,
        
        NUMBER_KIND_s8,  // i8
        NUMBER_KIND_s16, // i16
        NUMBER_KIND_s32, // i32
        NUMBER_KIND_s64, // i64
        NUMBER_KIND_s128, // i128 @cleanup: remove me later
        
        NUMBER_KIND_u8,  // ui8
        NUMBER_KIND_u16, // ui16
        NUMBER_KIND_u32, // ui32
        NUMBER_KIND_u64, // ui64
        NUMBER_KIND_u128, // i128 @cleanup: remove me later
        
        NUMBER_KIND_float32,
        NUMBER_KIND_float64,
    }number_kind;
};

func enum number_kind parse_integer_suffix(struct string suffix){
    switch(suffix.size){
        case 0: return NUMBER_KIND_int;
        case 1:{
            if((suffix.data[0]|32) == 'u') return NUMBER_KIND_unsigned;
            if((suffix.data[0]|32) == 'l') return NUMBER_KIND_long;
        }break;
        case 2:{
            if(string_match_case_insensitive(suffix, string("ul"))) return NUMBER_KIND_unsigned_long;
            if(string_match_case_insensitive(suffix, string("lu"))) return NUMBER_KIND_unsigned_long;
            if(string_match_case_insensitive(suffix, string("ll"))) return NUMBER_KIND_long_long;
            if(string_match_case_insensitive(suffix, string("i8"))) return NUMBER_KIND_s8;
        }break;
        case 3:{
            if(string_match_case_insensitive(suffix, string("i16"))) return NUMBER_KIND_s16;
            if(string_match_case_insensitive(suffix, string("i32"))) return NUMBER_KIND_s32;
            if(string_match_case_insensitive(suffix, string("i64"))) return NUMBER_KIND_s64;
            
            if(string_match_case_insensitive(suffix, string("ui8"))) return NUMBER_KIND_u8;
            
            if(string_match_case_insensitive(suffix, string("ull"))) return NUMBER_KIND_unsigned_long_long;
            if(string_match_case_insensitive(suffix, string("llu"))) return NUMBER_KIND_unsigned_long_long;
        }break;
        case 4:{
            if(string_match_case_insensitive(suffix, string("ui16"))) return NUMBER_KIND_u16;
            if(string_match_case_insensitive(suffix, string("ui32"))) return NUMBER_KIND_u32;
            if(string_match_case_insensitive(suffix, string("ui64"))) return NUMBER_KIND_u64;
        }break;
    }
    
    return NUMBER_KIND_invalid;
}


// @cleanup: for now its fine, but we disregard the suffix here. We could try to unite this with
//           the parsing code even more.

func struct parsed_integer parse_base10_literal(struct context *context, struct token *lit_token){
    struct parsed_integer ret = zero_struct;
    
    b32 report_overflow = false;
    smm suffix_start = lit_token->size;
    
    u64 val = 0;
    for(smm i = 0; i < lit_token->size; i++){
        if(lit_token->data[i] == '\'') continue;
        
        u32 number = (lit_token->data[i] - '0');
        if(number > 9){
            suffix_start = i;
            break;
        }
        
        if((val >> 32) * 10 > max_u32) report_overflow = true;
        val *= 10;
        
        u64 new_val = val + number;
        if(new_val < val) report_overflow = true;
        val = new_val;
    }
    
    if(report_overflow){
        report_warning(context, WARNING_compile_time_overflow, lit_token, "Compile time overflow.");
    }
    
    ret.value = val;
    struct string suffix = create_string(lit_token->data + suffix_start, lit_token->size - suffix_start);
    ret.number_kind = parse_integer_suffix(suffix);
    
    if(ret.number_kind == NUMBER_KIND_invalid){
        report_error(context, lit_token, "Invalid suffix '%.*s' on integer literal.", suffix.size, suffix.data);
    }
    
    return ret;
}

// @cleanup: get rid of this function!
//           and also 'parse_integer' maybe.
//           At least most of the uses.
func struct parsed_integer parse_character_literal(struct context *context, struct token *lit_token){
    struct parsed_integer ret = zero_struct;
    
    struct escaped_string escaped = handle_escaping(context, lit_token);
    
    if(escaped.string.size > 8){
        report_error(context, lit_token, "Character literal is too big.");
    }else if(escaped.string.size == 0){
        report_error(context, lit_token, "Empty character constant is not allowed.");
    }
    
    u64 value = 0;
    for(smm i = 0; i < escaped.string.size; i++){
        value <<= 8;
        value += escaped.string.data[i];
    }
    
    ret.value = value;
    switch(escaped.string_kind){
        case STRING_KIND_utf8:  ret.number_kind = NUMBER_KIND_s8;  break;
        case STRING_KIND_utf16: ret.number_kind = NUMBER_KIND_u16; break;
        case STRING_KIND_utf32: ret.number_kind = NUMBER_KIND_u32; break;
        default: ret.number_kind = NUMBER_KIND_invalid; break;
    }
    
    return ret;
}

func struct parsed_integer parse_binary_literal(struct context *context, struct token *lit_token){
    struct parsed_integer ret = zero_struct;
    
    b32 report_overflow = false;
    smm suffix_start = lit_token->size;
    assert(lit_token->data[0] == '0');
    assert(lit_token->data[1] == 'b' || lit_token->data[1] == 'B');
    
    if(lit_token->size == 2){
        report_error(context, lit_token, "Incomplete binary literal.");
    }
    
    u64 val = 0;
    for(smm i = 2; i < lit_token->size; i++){
        if(lit_token->data[i] == '\'') continue;
        
        u32 bin = lit_token->data[i] - '0';
        if(bin > 1) {
            suffix_start = i;
            break; // only allow '0' and '1'
        }
        
        if(val & 0x8000000000000000ull) report_overflow = true;
        val = (val << 1) | bin;
    }
    
    if(report_overflow){
        report_warning(context, WARNING_compile_time_overflow, lit_token, "Compile time overflow.");
    }
    
    struct string suffix = create_string(lit_token->data + suffix_start,
            lit_token->size - suffix_start);
    ret.number_kind = parse_integer_suffix(suffix);
    ret.value = val;
    
    if(ret.number_kind == NUMBER_KIND_invalid){
        report_error(context, lit_token, "Invalid suffix '%.*s' on hex literal.", suffix.size, suffix.data);
    }
    
    return ret;
}

func struct parsed_integer parse_hex_literal(struct context *context, struct token *lit_token){
    struct parsed_integer ret = zero_struct;
    
    struct string literal = lit_token->string;
    
    assert(literal.data[0] == '0' && (literal.data[1]|32) == 'x');
    
    if(literal.size == 2){
        report_error(context, lit_token, "Incomplete hex literal.");
    }
    
    string_eat_front(&literal, 2);
    
    b32 report_overflow = false;
    
    smm length = literal.size;
    
    u64 value = 0;
    for(smm index = 0; index < literal.size; index++){
        u32 hex = literal.data[index];
        if(hex == '\'') continue;
        
        u32 number = 0;
        if('9' >= hex && hex >= '0'){
            number = hex - '0';
        }else if('f' >= hex && hex >= 'a'){
            number = hex - 'a' + 10;
        }else if('F' >= hex && hex >= 'A'){
            number = hex - 'A' + 10;
        }else{
            length = index;
            break;
        }
        
        if(value & (15ui64 << 60)) report_overflow = true;
        value <<= 4; // multiply by 16
        u64 new_val = value + number;
        if(new_val < value) report_overflow = true;
        value = new_val;
    }
    
    string_eat_front(&literal, length);
    
    if(report_overflow){
        report_warning(context, WARNING_compile_time_overflow, lit_token, "Compile time overflow.");
    }
    
    ret.number_kind = parse_integer_suffix(literal);
    ret.value = value;
    
    if(ret.number_kind == NUMBER_KIND_invalid){
        report_error(context, lit_token, "Invalid suffix '%.*s' on hex literal.", literal.size, literal.data);
    }
    
    return ret;
}

//_____________________________________________________________________________________________________________________

func struct token_array tokenize_raw(struct context *context, struct string string, u32 file_index, b32 is_stupid_hash_hash_hack, smm *lines){
    
    if(!string.size) return (struct token_array)zero_struct;
    
    begin_counter(context, tokenize_raw);
    
    struct token *tokens;
    smm bytes_reserved;
    smm bytes_committed;
    if(is_stupid_hash_hash_hack){
        //
        // In the stupid '##' hack case we don't want to allocate the memory with 
        // 'os_reserve_memory' as that would be at least a page and would call to the os.
        // instead we are supposed to just push 8 tokens onto 'context->scratch'.
        //
        bytes_reserved = sizeof(struct token) * 8;
        bytes_committed = bytes_reserved;
        
        tokens = push_uninitialized_data(&context->scratch, struct token, 8);
    }else{
        //
        // There can at most be one raw token per byte. Thus reserve this much!
        // In the loop we then incrementally commit the memory.
        //
        bytes_reserved = align_up(string.amount * sizeof(struct token), 0x1000);
        bytes_committed = 0;
        
        struct os_virtual_buffer token_buffer = os_reserve_memory(0, bytes_reserved);
        if(!token_buffer.memory){
            assert(file_index < array_count(globals.file_table.data));
            char *file_name = globals.file_table.data[file_index]->absolute_file_path;
            
            print("%s: Allocation failure when allocating memory for file.\n", file_name);
            os_panic(1);
        }
        
        tokens = (struct token *)token_buffer.memory;
    }
    
    smm amount_of_tokens = 0;
    
    u8 *at = string.data;
    u8 *end_of_file = string.data + string.size;
    
    smm line   = 1;
    smm column = 1;
    
    while(at < end_of_file){
        
        smm size_used = amount_of_tokens * sizeof(struct token);
        if(size_used == bytes_committed){
            smm size_left = bytes_reserved - bytes_committed;
            
            smm max_commit = sizeof(struct token) * 0x1000;
            smm to_commit  = min_of(size_left, max_commit);
            
            u8 *current_end = (u8 *)tokens + size_used;
            void *success = os_commit_memory(current_end, to_commit).memory;
            assert(success == current_end);
            
            bytes_committed += to_commit;
        }
        
        struct token *cur = tokens + amount_of_tokens++;
        cur->file_index = file_index;
        cur->line   = (u32)line;
        cur->column = (u32)column;
        
#define next_token__internal(_type, size)\
cur->type = _type;                    \
column += size;
        
#define next_token__internal_char(_type, size)\
cur->type      = _type;                    \
column += size;                     \
at     += size;
        
        
        u8 *start = at;
        
        switch (*at){
            case '\x00':
            case '\x01': case '\x02': case '\x03': case '\x04':
            case '\x05': case '\x06': case '\x07': case '\x08':
            case '\x0e': case '\x0f': case '\x10': case '\x11':
            case '\x12': case '\x13': case '\x14': case '\x15':
            case '\x16': case '\x17': case '\x18': case '\x19':
            case '\x1a': case '\x1b': case '\x1c': case '\x1d':
            case '\x1e': case '\x1f': case '\x7f':{
                
                static char *invalid_byte_name[0x80] = {
                    [0x00] = "NUL (^@)", 
                    [0x01] = "SOH (^A)", [0x02] = "STX (^B)", [0x03] = "ETX (^C)", [0x04] = "EOT (^D)", 
                    [0x05] = "ENQ (^E)", [0x06] = "ACK (^F)", [0x07] = "BEL (^G)", [0x08] = "BS (^H)",
                    
                    [0x0e] = "SO (^N)",  [0x0f] = "SI (^O)",  [0x10] = "DLE (^P)", [0x11] = "DC1 (^Q)",
                    [0x12] = "DC2 (^R)", [0x13] = "DC3 (^S)", [0x14] = "DC4 (^T)", [0x15] = "NAK (^U)",
                    [0x16] = "SYN (^V)", [0x17] = "ETB (^W)", [0x18] = "CAN (^X)", [0x19] = "EM (^Y)",
                    [0x1a] = "SUB (^Z)", [0x1b] = "ESC (^[)", [0x1c] = "FS (^\\)", [0x1d] = "GS (^])",
                    [0x1e] = "RS (^^)",  [0x1f] = "US (^_)",  [0x7f] = "DEL",
                };
                
                report_error(context, cur, "Invalid %s byte 0x%.2x in file.", invalid_byte_name[*at], *at);
                next_token__internal_char(TOKEN_invalid, 1);
            }break;
            case '\\':{
                at++;
                
                while(u8_is_whitespace(*at)) at++;
                b32 got_newline = false;
                if(*at == '\n'){
                    at++;
                    if(at[0] == '\r') at++;
                    got_newline = true;
                }else if(*at == '\r'){
                    at++;
                    if(at[0] == '\n') at++;
                    got_newline = true;
                }
                u8 *end = at;
                
                next_token__internal(TOKEN_whitespace, (end - start));
                if(got_newline){
                    line++;
                    column = 1;
                }else{
                    report_warning(context, WARNING_missing_newline_after_backslash, cur, "No newline after '\\'.");
                }
            }break;
            case '\v': case '\t': case '\f': case ' ': {
                while(u8_is_whitespace(*++at));
                next_token__internal(TOKEN_whitespace, (at - start));
            }break;
            case '\n':{
                if(at[1] == '\r'){
                    next_token__internal_char(TOKEN_newline, 2);
                }else{
                    next_token__internal_char(TOKEN_newline, 1);
                }
                line++;
                column = 1;
            }break;
            case '\r':{
                if(at[1] == '\n'){
                    next_token__internal_char(TOKEN_newline, 2);
                }else{
                    next_token__internal_char(TOKEN_newline, 1);
                }
                line++;
                column = 1;
            }break;
            case '@':{
                next_token__internal_char(TOKEN_at_sign, 1);
            }break;
            case '`':{
                next_token__internal_char(TOKEN_backtick, 1);
            }break;
            case '#':{
                if(at[1] == '#'){
                    next_token__internal_char(TOKEN_hashhash, 2);
                }else{
                    next_token__internal_char(TOKEN_hash, 1);
                }
            }break;
            case '=':{
                if(at[1] == '='){
                    next_token__internal_char(TOKEN_logical_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_equals, 1);
                }
            }break;
            case '(':{
                next_token__internal_char(TOKEN_open_paren, 1);
            }break;
            case ')':{
                next_token__internal_char(TOKEN_closed_paren, 1);
            }break;
            case '{':{
                next_token__internal_char(TOKEN_open_curly, 1);
            }break;
            case '}':{
                next_token__internal_char(TOKEN_closed_curly, 1);
            }break;
            case '[':{
                next_token__internal_char(TOKEN_open_index, 1);
            }break;
            case ']':{
                next_token__internal_char(TOKEN_closed_index, 1);
            }break;
            case ';':{
                next_token__internal_char(TOKEN_semicolon, 1);
            }break;
            case ':':{
                next_token__internal_char(TOKEN_colon, 1);
            }break;
            case ',':{
                next_token__internal_char(TOKEN_comma, 1);
            }break;
            case '.':{
                if(at[1] == '.' && at[2] == '.'){
                    next_token__internal_char(TOKEN_dotdotdot, 3);
                }else if(u8_is_number(at[1])){
                    goto handle_numbers;
                }else{
                    next_token__internal_char(TOKEN_dot, 1);
                }
            }break;
            case '>':{
                if(at[1] == '>'){
                    if(at[2] == '='){
                        next_token__internal_char(TOKEN_right_shift_equals, 3);
                    }else{
                        next_token__internal_char(TOKEN_right_shift, 2);
                    }
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_bigger_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_bigger, 1);
                }
            }break;
            case '<':{
                if(at[1] == '<'){
                    if(at[2] == '='){
                        next_token__internal_char(TOKEN_left_shift_equals, 3);
                    }else{
                        next_token__internal_char(TOKEN_left_shift, 2);
                    }
                    
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_smaller_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_smaller, 1);
                }
            }break;
            case '~':{
                next_token__internal_char(TOKEN_bitwise_not, 1);
            }break;
            case '!':{
                if(at[1] == '='){
                    next_token__internal_char(TOKEN_logical_unequals, 2);
                }else{
                    next_token__internal_char(TOKEN_logical_not, 1);
                }
            }break;
            case '?':{
                next_token__internal_char(TOKEN_question_mark, 1);
            }break;
            case '+':{
                if(at[1] == '+'){
                    next_token__internal_char(TOKEN_increment, 2);
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_plus_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_plus, 1);
                }
            }break;
            case '-':{
                if(at[1] == '-'){
                    next_token__internal_char(TOKEN_decrement, 2);
                }else if(at[1] == '>'){
                    next_token__internal_char(TOKEN_arrow, 2);
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_minus_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_minus, 1);
                }
            }break;
            case '&':{
                if(at[1] == '&'){
                    next_token__internal_char(TOKEN_logical_and, 2);
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_and_equals, 2);
                }else {
                    next_token__internal_char(TOKEN_and, 1);
                }
            }break;
            case '|':{
                if(at[1] == '|'){
                    next_token__internal_char(TOKEN_logical_or, 2);
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_or_equals, 2);
                }else {
                    next_token__internal_char(TOKEN_or, 1);
                }
            }break;
            case '^':{
                if(at[1] == '='){
                    next_token__internal_char(TOKEN_xor_equals, 2);
                }else {
                    next_token__internal_char(TOKEN_xor, 1);
                }
            }break;
            case '*':{
                if(at[1] == '='){
                    next_token__internal_char(TOKEN_times_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_times, 1);
                }
            }break;
            case '/':{
                if(at[1] == '*'){
                    at += 2;
                    
                    u8 *last_newline = at - column; // actually the newline, as 1 counted
                    while(*at){
                        if(at[1] == '/' && at[0] == '*'){
                            break;
                        }else if(at[0] == '\n'){
                            line++;
                            last_newline = at;
                            at++;
                        }else{
                            at += 1;
                        }
                    }
                    
                    if(*at == 0){
                        cur->data = start;
                        cur->size = 8;
                        report_error(context, cur, "File ended during block comment.");
                    }
                    
                    at += 2;
                    column = at - last_newline;
                    
                    next_token__internal(TOKEN_comment, 0); // note we already adjusted the column, so we dont want to adjust it here again
                }else if(at[1] == '/'){
                    at += 2;
                    
                    while(*at && *at != '\n') at++;
                    
                    next_token__internal(TOKEN_comment, at - start);
                }else if(at[1] == '='){
                    next_token__internal_char(TOKEN_div_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_slash, 1);
                }
            }break;
            case '%':{
                if(at[1] == '='){
                    next_token__internal_char(TOKEN_mod_equals, 2);
                }else{
                    next_token__internal_char(TOKEN_mod, 1);
                }
            }break;
            case '\'':{
                character_literal_case:; // we jump here for L'', U'', u'' and such.
                cur->type = TOKEN_character_literal;
                
                b32 no_end = false;
                
                u8 _ = *at++;
                assert(_ == '\'');
                
                while(true){
                    if(at[0] == '\\'){
                        if(at[1] == '\r' && at[2] == '\n'){
                            at += 3;
                        }else{
                            at += 2; // Skip the escaped character, whatever it is.
                        }
                    }else if(at[0] == '\''){
                        break;
                    }else if(!*at || at[0] == '\n'){
                        no_end = true;
                        break;
                    }else{
                        at += 1;
                    }
                }
                
                if(!no_end){
                    at += 1; // eat the end quote
                }
                
                column += at - start;
                
                if(no_end){
                    cur->data = start;
                    cur->size = 8;
                    report_error(context, cur, "Unterminated character literal.");
                }
                
            }break;
            case '"':{
                string_literal_case:; // we jump here for L"", U"", u"" and such.
                cur->type = TOKEN_string_literal;
                
                b32 no_end = false;
                
                u8 _ = *at++;
                assert(_ == '"');
                
                while(true){
                    if(at[0] == '\\'){
                        if(at[1] == '\r' && at[2] == '\n'){
                            at += 3;
                            line += 1;
                        }else{
                            line += (at[1] == '\n');
                            at += 2; // Skip the escaped character, whatever it is.
                        }
                    }else if(at[0] == '\"'){
                        at += 1; // eat the end quote
                        break;
                    }else if(!*at || at[0] == '\n'){
                        no_end = true;
                        break;
                    }else{
                        at += 1;
                    }
                }
                
                column += at - start;
                
                if(no_end){
                    cur->data = start;
                    cur->size = 8;
                    report_error(context, cur, "Unterminated string literal.");
                }
            }break;
            
            case 'L':
            case 'U':{
                if(at[1] == '"'){
                    at++;
                    goto string_literal_case;
                }else if(at[1] == '\''){
                    at++;
                    goto character_literal_case;
                }else{
                    goto identifier_case;
                }
            }break;
            
            case 'u':{
                if(at[1] == '"'){
                    at++;
                    goto string_literal_case;
                }else if(at[1] == '\''){
                    at++;
                    goto character_literal_case;
                }else if(at[1] == '8' && at[2] == '"'){
                    at += 2;
                    goto string_literal_case;
                }else{
                    goto identifier_case;
                }
            }break;
            
            case '0':{
                if((at[1]|32) == 'x'){
                    at += 2;
                    
                    while(u8_is_hex_number(*at) || *at == '\'') at++;
                    
                    if(*at == '.'){
                        
                        at += 1;
                        
                        while(u8_is_hex_number(*at) || *at == '\'') at++;
                        
                        if((*at | 32) == 'p'){
                            at++;
                            
                            if(*at == '-' || *at == '+'){   
                                at++;
                            }
                        }
                        while(u8_is_valid_in_c_ident(*at)) at++; // suffix
                        
                        next_token__internal(TOKEN_float_hex_literal, at - start);
                    }else if((*at|32) == 'p'){
                        // @copy and paste from above
                        at++;
                        
                        if(*at == '-' || *at == '+') at++;
                        
                        while(u8_is_valid_in_c_ident(*at)) at++; // suffix
                        
                        next_token__internal(TOKEN_float_hex_literal, at - start);
                    }else{
                        while(u8_is_valid_in_c_ident(*at)) at++; // suffix
                        
                        next_token__internal(TOKEN_hex_literal, at - start);
                    }
                }else if((at[1]|32) == 'b'){
                    at += 2;
                    
                    while(*at == '0' || *at == '1' || *at == '\'') at++;
                    
                    while(u8_is_valid_in_c_ident(*at)) at++; // suffix
                    
                    next_token__internal(TOKEN_binary_literal, at - start);
                }else{
                    // @cleanup: octal?
                    goto handle_numbers;
                }
            }break;
            case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':{
                handle_numbers:;
                enum token_type token_type = TOKEN_base10_literal;
                
                // handle all numbers before the dot
                while(u8_is_number(*at) || *at == '\'') at++;
                
                if(*at == '.'){
                    token_type = TOKEN_float_literal;
                    at++;
                    
                    // eat all the numbers after the dot
                    while(u8_is_number(*at) || *at == '\'') at++;
                }
                
                if((*at | 32) == 'e'){
                    token_type = TOKEN_float_literal;
                    at++;
                    
                    if(*at == '-' || *at == '+'){   
                        at++;
                    }
                    
                    while(u8_is_number(*at) || *at == '\'') at++;
                }
                
                // suffix
                while(u8_is_valid_in_c_ident(*at)) at++;
                next_token__internal(token_type, at - start);
            }break;
            default:{
                //
                // @note: we know that the byte is a valid c identifier start, 
                //        as we have excluded all invalid bytes in the first case.
                //
                
                identifier_case: // We jump here for cases like 'L' if its not 'L""'.
                
                u64 string_hash = (5381 << 5) + 5381 + *at;
                
                while(u8_is_valid_in_c_ident(*++at)){
                    string_hash = (string_hash << 5) + string_hash + *at;
                }
                
                next_token__internal(TOKEN_identifier, at - start);
                
                cur->string_hash = string_hash;
                
                // assert(string_hash == string_djb2_hash(create_string(start, at - start)));
            }break;
        }
        
        cur->data = start;
        cur->size = (at - start);
        
        assert(cur->type < TOKEN_count);
    }
    
    struct token_array token_array = {
        .data = tokens,
        .size = amount_of_tokens,
    };
    
    // minus one as lines are one based
    if(lines) *lines = line - 1;
    
    end_counter(context, tokenize_raw);
    return token_array;
}

func void eat_whitespace_and_comments(struct context *context){
    smm did_something;
    do{
        did_something  = (smm)peek_token_eat_raw(context, TOKEN_whitespace);
        did_something |= (smm)peek_token_eat_raw(context, TOKEN_comment);
    }while(did_something);
}

func void eat_whitespace_comments_and_newlines(struct context *context){
    smm did_something;
    do{
        did_something  = (smm)peek_token_eat_raw(context, TOKEN_whitespace);
        did_something |= (smm)peek_token_eat_raw(context, TOKEN_comment);
        did_something |= (smm)peek_token_eat_raw(context, TOKEN_newline);
    }while(did_something);
}


func struct token *expand_define(struct context *context, struct token *token_to_expand, struct define_node *define, struct token *macro_expansion_site){
    
    if(define->is_builtin){
        if(define->is___pragma){
            
            // :microsoft_extension
            // for now unsupported, just skip all the tokens, such that they do not get emitted
            
            if(peek_token_raw(context, TOKEN_open_paren)){
                // @incomplete: handle pragmas
                skip_until_tokens_are_balanced_raw(context, null, TOKEN_open_paren, TOKEN_closed_paren);
            }
            
            return null;
        }
        
        if(define->is_defined){
            if(!context->in_static_if_condition) return token_to_expand;
            
            //
            // defined() macro. We first parse the invokation, it should be 'defined <macro>' or 'defined (<macro>)'.
            // if it is, disable '<macro>', push a token stack node with the '<macro>' or '(<macro>)' 
            // and return the 'defined' token.
            //                                                                                      14.06.2022
            eat_whitespace_and_comments(context);
            struct token *open_paren = peek_token_eat_raw(context, TOKEN_open_paren);
            
            eat_whitespace_and_comments(context);
            struct token *identifier = peek_token_eat_raw(context, TOKEN_identifier);
            if(!identifier){
                tokenizer_report_error(context, token_to_expand, macro_expansion_site, "Builtin macro 'defined' expected an identifier following it. Possible invokations are 'defined <identifier>' and 'defined(<identifer>)'.");
                return token_to_expand;
            }
            
            struct token *tokens = null;
            smm amount_of_tokens = 0;
            
            if(open_paren){
                eat_whitespace_and_comments(context);
                struct token *closed_paren = peek_token_eat_raw(context, TOKEN_closed_paren);
                if(!closed_paren){
                    tokenizer_report_error(context, token_to_expand, macro_expansion_site, "Builtin macro 'defined' expected exactly one identifier in its argument list. Possible invokations are 'defined <identifier>' and 'defined(<identifier>)'.");
                    return token_to_expand;
                }
                
                // @cleanup: is there a reason to have the '(' and the ')'?
                tokens = push_uninitialized_data(&context->scratch, struct token, 3);
                tokens[0] = *open_paren;
                tokens[1] = *identifier;
                tokens[2] = *closed_paren;
                amount_of_tokens = 3;
            }else{
                tokens = identifier;
                amount_of_tokens = 1;
            }
            
            //
            // We are good, push a token stack node
            //
            
            struct define_node *define_to_disable = lookup_define(context, identifier->atom);
            if(define_to_disable && define_to_disable->is_disabled) define_to_disable = null;
            
            if(define_to_disable){
                context->define_depth++;
                define_to_disable->is_disabled = true;
            }
            
            struct token_array token_array = {.data = tokens, .size = amount_of_tokens };
            
            struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
            node->define_to_reenable_on_exit = define_to_disable;
            node->tokens = token_array;
            
            sll_push_front(context->token_stack, node);
            
            return token_to_expand;
        }
        
        if(define->is___FILE__ || define->is___LINE__){
            struct token *token = push_uninitialized_struct(&context->scratch, struct token);
            *token = *macro_expansion_site;
            
            if(define->is___FILE__){
                // @cleanup: This seems very slow.
                struct string file_name = strip_file_path(string_from_cstring(globals.file_table.data[token->file_index]->absolute_file_path));
                
                token->type = TOKEN_string_literal;
                token->string = push_format_string(context->arena, "\"%.*s\"", file_name.size, file_name.data);
            }else{
                token->type = TOKEN_base10_literal;
                token->string = push_format_string(context->arena, "%u", token->line);
            }
            
            return token;
        }
    }
    
    struct token_array *argument_token_arrays    = null; // define->arguments.count long
    struct token_array *expanded_argument_tokens = null; // define->arguments.count long
    struct string      *stringified_arguments    = null; // define->stringify_count long
    
    if(define->is_function_like){
        
        struct token *open_paren = next_token_raw(context);
        assert(open_paren->type == TOKEN_open_paren); // This should have been ensured by the outside.
        
        if(sll_is_empty(define->arguments)){
            eat_whitespace_and_comments(context);
            
            if(!peek_token_eat_raw(context, TOKEN_closed_paren)){
                tokenizer_report_error(context, token_to_expand, macro_expansion_site, "Function-like define takes zero arguments. Expected ')'.");
            }
            
            goto skip_define_argument_parsing_because_the_define_has_no_arguments;
        }
        
        
        // @note: this is not on 'arg' below anymore, as we might expand the same define for some of the arguments,
        //        which would ruin the pointers.
        argument_token_arrays    = push_uninitialized_data(&context->scratch, struct token_array, define->arguments.count);
        expanded_argument_tokens = push_uninitialized_data(&context->scratch, struct token_array, define->arguments.count);
        stringified_arguments    = push_uninitialized_data(&context->scratch, struct string,      define->stringify_count);
        
        
        //
        // collect the arguments into the 'argument_token_arrays'
        //
        {
            // argument_token_arrays need to retain whitespace information
            //
            // if '#define print(arg) printf("%s", arg)' and '#define stringify(a) #a'
            // then when we evaluate 'print(stringify(a))' we have no information 
            // on whether 'arg' needs to be strigified. Now if we strip all whitespace from within arg,
            // stringify(a + b) -> stringify(a+b), which would then evaluate to '"a+b"' but we want
            // "a + b".
            // Thus we need to keep the whitespaces (in the minimized form) in the 'argument_token_arrays'.
            // Here we also have to keep in mind that leading and trailing whitespace should be deleted,
            // i.e stringify(   a + b    ) = "a + b".
            //                                                                        16.06.2022
            
            //
            // Delete leading whitespace of the first argument.
            //
            eat_whitespace_comments_and_newlines(context); 
            
            //
            // @WARNING: Daisy-Chain these tokens. This is fine as all arenas are fixed size now.
            //
            smm amount = 0;
            struct token *array = push_data(&context->scratch, struct token, 0);
            
            struct define_argument *arg = define->arguments.first;
            smm paren_depth = 1;
            
            u8 *argument_stringified_start = arena_current(context->arena);
            struct token *last_was_whitespace = null;
            
            if(define->stringify_count){
                *push_uninitialized_struct(context->arena, u8) = '"';
            }
            
            while(true){
                struct token *token = next_token_raw(context);
                
                if(token->type == TOKEN_comma){
                    
                    if(paren_depth == 1){
                        if(arg->next){
                            argument_token_arrays[arg->argument_index].data   = array;
                            argument_token_arrays[arg->argument_index].amount = amount;
                            
                            if(arg->stringify_index != -1){
                                *push_uninitialized_struct(context->arena, u8) = '"';
                                stringified_arguments[arg->stringify_index] = create_string(argument_stringified_start, arena_current(context->arena) - argument_stringified_start);
                                
                                argument_stringified_start = arena_current(context->arena);
                                *push_uninitialized_struct(context->arena, u8) = '"';
                            }
                            
                            //
                            // Reinitialize the whitespace tracker and delete the leading whitespace of the next argument
                            //
                            eat_whitespace_comments_and_newlines(context);
                            last_was_whitespace = null;
                            
                            array  = array + amount;
                            amount = 0;
                            
                            arg = arg->next;
                            continue;
                        }else if(define->is_varargs){
                            // just collect all the arguments into the __VA_ARGS__ argument.
                        }else{
                            begin_error_report(context);
                            tokenizer_report_error(context, token_to_expand, macro_expansion_site, "Too many arguments in function-like macro invocation.");
                            report_error(context, define->defined_token, "... Here is the definition of the macro.");
                            end_error_report(context);
                            return token_to_expand;
                        }
                    }
                    
                }else if(token->type == TOKEN_open_paren){
                    paren_depth += 1;
                }else if(token->type == TOKEN_closed_paren){
                    paren_depth -= 1;
                    
                    if(paren_depth == 0){
                        argument_token_arrays[arg->argument_index].data   = array;
                        argument_token_arrays[arg->argument_index].amount = amount;
                        
                        
                        if(arg->stringify_index != -1){
                            *push_uninitialized_struct(context->arena, u8) = '"';
                            stringified_arguments[arg->stringify_index] = create_string(argument_stringified_start, arena_current(context->arena) - argument_stringified_start);
                        }
                        
                        
                        if(arg->next){
                            if(define->is_varargs && !arg->next->next){
                                // varargs with no arguments in __VA_ARGS__ is fine
                                argument_token_arrays[arg->next->argument_index] = (struct token_array)zero_struct;
                                break;
                            }
                            
                            begin_error_report(context);
                            tokenizer_report_error(context, token_to_expand, macro_expansion_site, "Too few arguments in function-like macro invocation.");
                            report_error(context, define->defined_token, "... Here is the definition of the macro.");
                            end_error_report(context);
                            return token_to_expand;
                        }else{
                            break;
                        }
                    }
                }else if(token->type == TOKEN_invalid){
                    tokenizer_report_error(context, token_to_expand, macro_expansion_site, "Expected a ')' while collecting arguments for macro expansion.");
                    return token_to_expand;
                }
                
                if(arg->stringify_index != -1){
                    
                    if(!(token->type == TOKEN_whitespace || token->type == TOKEN_comment || token->type == TOKEN_newline)){
                        //
                        // If we encounter a non-whitespace token but we had a whitespace token, emit a whitespace
                        //
                        if(last_was_whitespace){
                            *push_uninitialized_struct(context->arena, u8) = ' ';
                        }
                    }
                    
                    switch(token->type){
                        
                        // these are handled above
                        case TOKEN_whitespace: case TOKEN_comment: case TOKEN_newline: break;
                        
                        case TOKEN_character_literal:
                        case TOKEN_string_literal:{
                            struct string string = token_get_string(token);
                            
                            smm size = 0;
                            for(smm i = 0; i < string.size; i++){
                                if(string.data[i] == '\''){
                                    size += 2;
                                }else if(string.data[i] == '"'){
                                    size += 2;
                                }else if(string.data[i] == '\\'){
                                    size += 2;
                                }else{
                                    size += 1;
                                }
                            }
                            
                            u8 *data = push_uninitialized_data(context->arena, u8, size);
                            
                            smm at = 0;
                            for(smm i = 0; i < string.size; i++){
                                if(string.data[i] == '\''){
                                    data[at++] = '\\';
                                    data[at++] = '\'';
                                }else if(string.data[i] == '"'){
                                    data[at++] = '\\';
                                    data[at++] = '"';
                                }else if(string.data[i] == '\\'){
                                    data[at++] = '\\';
                                    data[at++] = '\\';
                                }else{
                                    data[at++] = string.data[i];
                                }
                            }
                            
                            assert(at == size);
                        }break;
                        default:{
                            push_string_copy(context->arena, token_get_string(token));
                        }break;
                    }
                }
                
                
                if(token->type == TOKEN_whitespace || token->type == TOKEN_comment || token->type == TOKEN_newline){
                    last_was_whitespace = token;
                    continue;
                }else if(last_was_whitespace){
                    amount++;
                    *push_uninitialized_struct(&context->scratch, struct token) = *last_was_whitespace;
                    last_was_whitespace = null;
                }
                
                amount++;
                *push_uninitialized_struct(&context->scratch, struct token) = *token;
            }
        }
        
        
        // @note: fast_path for empty replacement lists
        if(!sll_is_empty(define->replacement_list)){
            //
            // Expand the 'argument_token_array' to 'expanded_argument_token_arrays'
            //
            
            // save the tokenizer state so we can expand the arguments in isolation
            // @cleanup: put the cspec quote here 
            struct token_stack_node *saved_first = context->token_stack.first;
            struct token_stack_node *saved_last  = context->token_stack.last;
            
            //
            // expand the arguments of the define.
            //
            for(struct define_argument *arg = define->arguments.first; arg; arg = arg->next){
                assert(arg->argument_index < define->arguments.count);
                
                // if we don't need the expanded version don't expand
                if(arg->needs_to_be_expanded_count == 0) continue; 
                
                if(argument_token_arrays[arg->argument_index].amount){
                    
                    struct token_stack_node *arg_node = push_struct(&context->scratch, struct token_stack_node);
                    arg_node->tokens = argument_token_arrays[arg->argument_index];
                    
                    context->token_stack.first = context->token_stack.last = arg_node;
                    
                    //
                    // @note: these cannot be daisy chained, as there is a recursive call
                    // 
                    
                    smm amount   = 0;
                    smm capacity = 8;
                    struct token *array = push_uninitialized_data(&context->scratch, struct token, capacity);
                    
                    while(!tokenizer_is_at_the_end_of_the_file(context)){
                        struct token *token = next_token_raw(context);
                        
                        if(token->type == TOKEN_identifier){
                            struct define_node *define_in_argument = lookup_define(context, token->atom);
                            if(define_in_argument){
                                
                                int skip = 0;
                                
                                if(define_in_argument->is_function_like){
                                    eat_whitespace_comments_and_newlines(context); 
                                    if(!peek_token_raw(context, TOKEN_open_paren)){
                                        skip = 1;
                                    }
                                }
                                
                                if(!skip){
                                    if(define_in_argument->is_disabled){
                                        dynarray_maybe_grow(struct token, &context->scratch, array, amount, capacity);
                                        
                                        // We have to prevent this identifier from being expanded again, after the define might be reenabled.
                                        // For example:
                                        // 
                                        //     #define member a.member
                                        //     #define def(m) m
                                        //     def(member);
                                        //     
                                        // def(member) -> expand(a.member) -> a.member
                                        // 
                                        // This is because of the following passage from the c-spec:
                                        // 
                                        // "Furthermore, if any nested replacements encounter the name of the macro being replaced,
                                        //  it is not replaced. These nonreplaced macro name preprocessing tokens are no longer
                                        //  available for further replacement even if they are later (re)examined in contexts in which
                                        //  that macro name preprocessing token would otherwise have been replaced.
                                        // 
                                        // We implement this by making these tokens not identifiers anymore.
                                        // 
                                        struct token *new_token = &array[amount++];
                                        *new_token = *token;
                                        new_token->type = TOKEN_identifier_dont_expand_because_it_comes_from_a_fully_expanded_macro;
                                        continue;
                                    }else{
                                        token = expand_define(context, token, define_in_argument, macro_expansion_site);
                                        if(!token) continue; 
                                    }
                                } 
                            }
                        }
                        
                        if(token->type == TOKEN_invalid) break;
                        
                        dynarray_maybe_grow(struct token, &context->scratch, array, amount, capacity);
                        array[amount++] = *token;
                    }
                    
                    expanded_argument_tokens[arg->argument_index].data   = array;
                    expanded_argument_tokens[arg->argument_index].amount = amount;
                }else{
                    // if there are no tokens in the argument, the expanded argument is empty
                    expanded_argument_tokens[arg->argument_index] = (struct token_array)zero_struct;
                }
            }
            
            // reset the tokenizer state
            context->token_stack.first = saved_first;
            context->token_stack.last  = saved_last;
        }
    }
    skip_define_argument_parsing_because_the_define_has_no_arguments:;
    
    // we queue into these new nodes by appending with sll_push_back, then we can just append these to the front of 
    // context->token_stack.
    struct{
        struct token_stack_node *first;
        struct token_stack_node *last;
    } new_nodes = zero_struct;
    
    for(struct define_replacement_node *replacement = define->replacement_list.first; replacement; replacement = replacement->next){
        
        // @note: we copy all (relevant) tokens outside the tokenizer, thus all allocations can be on 'scratch'
        switch(replacement->kind){
            case DEFINE_REPLACEMENT_tokens:{
                struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
                node->tokens = replacement->tokens;
                
                sll_push_back(new_nodes, node);
            }break;
            case DEFINE_REPLACEMENT_argument:{
                struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
                node->tokens = expanded_argument_tokens[replacement->argument_index];
                
                sll_push_back(new_nodes, node);
            }break;
            case DEFINE_REPLACEMENT_hash:{
                struct token *string_token = push_uninitialized_struct(&context->scratch, struct token);
                string_token->type   = TOKEN_string_literal;
                
                string_token->column     = macro_expansion_site->column;
                string_token->line       = macro_expansion_site->line;
                string_token->file_index = macro_expansion_site->file_index;
                
                assert(replacement->stringify_index != -1);
                string_token->data = stringified_arguments[replacement->stringify_index].data;
                string_token->size = stringified_arguments[replacement->stringify_index].size;                
                string_token->string_hash = 0;
                
                struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
                node->tokens.data   = string_token;
                node->tokens.amount = 1;
                
                sll_push_back(new_nodes, node);
            }break;
            case DEFINE_REPLACEMENT_hashhash:{
                
                b32 prefix_is_empty  = false;
                b32 postfix_is_empty = false;
                
                struct token *prefix_token  = null;
                struct token *postfix_token = null;
                
                if(replacement->prefix_argument != -1){
                    // @note: push the non expanded argument
                    struct token_array arg_tokens = argument_token_arrays[replacement->prefix_argument];
                    
                    if(arg_tokens.amount == 0){
                        prefix_is_empty = true;
                    }else if(arg_tokens.amount == 1){
                        prefix_token = arg_tokens.data;
                    }else{
                        struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
                        node->tokens.data   = arg_tokens.data;
                        node->tokens.amount = arg_tokens.amount - 1;
                        sll_push_back(new_nodes, node);
                        
                        prefix_token = arg_tokens.data + arg_tokens.amount - 1;
                    }
                }else{
                    // :cannot_store_prefix
                    // we cannot predict the prefix token while parsing defines, as there can be ## chains and
                    // thus we have to lookup the _prefix_token_ here, from the previous tokens.
                    
                    while(true){
                        struct token_stack_node *last = new_nodes.last;
                        assert(last);
                        
                        for(smm i = last->tokens.amount - 1; i >= 0; i--){
                            struct token *token = last->tokens.data + i;
                            if(token->type == TOKEN_whitespace || token->type == TOKEN_comment || token->type == TOKEN_newline){
                                continue;
                            }
                            
                            // delete all the tokens until (and including) a relevant one
                            prefix_token = token;
                            last->tokens.amount = i;
                            break;
                        }
                        
                        if(prefix_token) break;
                        
                        struct token_stack_node *prev_node = null;
                        
                        // yikes, we could additionally keep these in an array, to make this O(1).
                        for(struct token_stack_node *it = new_nodes.first; it; it = it->next){
                            if(it->next == last){
                                prev_node = it;
                                break;
                            }
                        }
                        assert(prev_node);
                        
                        new_nodes.last = prev_node;
                        prev_node->next = null;
                    }
                }
                
                struct token_stack_node *postfix_node = null;
                
                if(replacement->postfix_argument != -1){
                    struct token_array arg_tokens = argument_token_arrays[replacement->postfix_argument];
                    
                    if(arg_tokens.amount == 0){
                        postfix_is_empty = true;
                    }else if(arg_tokens.amount == 1){
                        postfix_token = arg_tokens.data;
                    }else{
                        postfix_token = arg_tokens.data;
                        
                        postfix_node = push_struct(&context->scratch, struct token_stack_node);
                        postfix_node->tokens.data   = arg_tokens.data   + 1;
                        postfix_node->tokens.amount = arg_tokens.amount - 1;
                    }
                }else{
                    postfix_token = replacement->postfix_token;
                }
                
                struct string prev_string = prefix_is_empty  ? string("") : token_get_string(prefix_token);
                struct string next_string = postfix_is_empty ? string("") : token_get_string(postfix_token);
                
                // @leak: this needs to be allocated in 'context->arena' as the resulting tokens
                //        need to point to something valid
                struct string concat = string_concatenate(context->arena, prev_string, next_string);
                
                struct token_array tokens = tokenize_raw(context, concat, macro_expansion_site->file_index, /* is_stupid_hash_hash_hack */ true, /* lines */null);
                
                if(tokens.amount){
                    // @note: amount can be 0 for 
                    //            #define C(a,b) a##b
                    //            C(,)
                    struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
                    node->tokens = tokens;
                    
                    sll_push_back(new_nodes, node);
                }
                
                if(postfix_node){
                    sll_push_back(new_nodes, postfix_node);
                }
            }break;
        }
    }
    
    // 
    // Rescanning
    // 
    
    // "After all parameters in the replacement list have been substituted [...]. 
    //  The resulting token sequence is rescanned along with all subsequent preprocessing
    //  tokens of the source file, for more macro names to replace."
    
    if(!sll_is_empty(new_nodes)){
        
        // Disable the define during the rescan.
        // 
        // "If the name of the macro being replaced is found during this scan of the replacement list
        //  (not including the rest of the source file's preprocessing tokens), it is not replaced.
        //  Furthermore, if any nested replacements encounter the name of the macro being replaced,
        //  it is not replaced."
        //  
        
        define->is_disabled = true;
        new_nodes.last->define_to_reenable_on_exit = define;
        
        if(context->define_depth == 0){
            assert(!context->macro_expansion_token);
            context->macro_expansion_token = macro_expansion_site;
        }
        context->define_depth++;
        
        // 
        // Push the new_nodes to the front of the token_stack so we will re-scan them.
        // 
        sll_push_front_list(context->token_stack, new_nodes);
    }
    
    return null;
}

//_____________________________________________________________________________________________________________________
// *_token routines

func b32 in_current_token_array(struct context *context){
    return (context->token_at < context->tokens.size);
}

func void begin_token_array(struct context *context, struct token_array array){
    assert(array.amount != 0);
    
    context->tokens = array;
    context->token_at = 0;
}

func struct token *get_current_token(struct context *context){
    if(context->token_at < 0 || context->token_at >= context->tokens.amount) return &globals.invalid_token;
    return context->tokens.data + context->token_at;
}

// variant, that cannot return '&context->invalid_token'
func struct token *get_current_token_for_error_report(struct context *context){
    if(context->token_at < 0){
        return context->tokens.data;
    }else if(context->token_at >= context->tokens.amount){
        return context->tokens.data + (context->tokens.amount - 1);
    }else{
        return context->tokens.data + context->token_at;
    }
}

func struct token *next_token(struct context *context){
    struct token *ret = get_current_token(context);
    // :token_at_overshoot
    context->token_at++;
    return ret;
}

func struct token *prev_token(struct context *context){
    // :token_at_overshoot
    context->token_at--;
    return get_current_token(context);
}

func struct token *expect_token(struct context *context, enum token_type type, char *error){
    struct token *token = get_current_token(context);
    if(token->type != type){
        struct token *site = get_current_token_for_error_report(context);
        report_syntax_error(context, site, error);
        return token;
    }
    
    // :token_at_overshoot
    context->token_at++;
    return token;
}

func struct token *peek_token(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type == type){
        return token;
    }
    return null;
}

func struct token *peek_token_eat(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type == type){
        // :token_at_overshoot
        context->token_at++;
        return token;
    }
    return null;
}


//_____________________________________________________________________________________________________________________
// static if evaluation

enum static_if_evaluate_operation{
    STATIC_IF_EVALUATE_none,
    
    STATIC_IF_EVALUATE_parenthesized_expression,
    STATIC_IF_EVALUATE_ternary_condition,
    STATIC_IF_EVALUATE_ternary_if_true,
    
    STATIC_IF_EVALUATE_logical_not,
    STATIC_IF_EVALUATE_bitwise_not,
    STATIC_IF_EVALUATE_unary_minus,
    
    STATIC_IF_EVALUATE_left_shift,
    STATIC_IF_EVALUATE_right_shift,
    
    STATIC_IF_EVALUATE_and,
    STATIC_IF_EVALUATE_or,
    STATIC_IF_EVALUATE_xor,
    
    STATIC_IF_EVALUATE_times,
    STATIC_IF_EVALUATE_div,
    STATIC_IF_EVALUATE_mod,
    
    STATIC_IF_EVALUATE_plus,
    STATIC_IF_EVALUATE_minus,
    
    STATIC_IF_EVALUATE_logical_equals,
    STATIC_IF_EVALUATE_logical_unequals,
    STATIC_IF_EVALUATE_logical_bigger_equals,
    STATIC_IF_EVALUATE_logical_smaller_equals,
    STATIC_IF_EVALUATE_logical_bigger,
    STATIC_IF_EVALUATE_logical_smaller,
    
    STATIC_IF_EVALUATE_logical_and,
    STATIC_IF_EVALUATE_logical_or,
    
    STATIC_IF_EVALUATE_operation_count,
};

func struct static_if_evaluate_stack_node *static_if_stack_current(struct context *context){
    assert(0 <= context->static_if_stack_at && context->static_if_stack_at < array_count(context->static_if_evaluate_stack));
    return context->static_if_evaluate_stack + context->static_if_stack_at;
}

// @note: Technically returns an element out of bounds so be careful!
func struct static_if_evaluate_stack_node *static_if_stack_pop(struct context *context){
    struct static_if_evaluate_stack_node *ret = context->static_if_evaluate_stack + context->static_if_stack_at;
    if(context->static_if_stack_at > 0) context->static_if_stack_at -= 1;
    return ret;
}

func void static_if_stack_push(struct context *context, struct token *token, enum static_if_evaluate_operation operation, s64 value, int is_unsigned){
    if(context->static_if_stack_at + 1 >= array_count(context->static_if_evaluate_stack)){
        report_error(context, token, "Expression in '#if' nests too deep.");
        return;
    }
    
    // @note pre incremented
    struct static_if_evaluate_stack_node *node = context->static_if_evaluate_stack + ++context->static_if_stack_at;
    node->token     = token;
    node->operation = operation;
    node->is_unsigned = is_unsigned;
    node->value     = value;
    node->macro_expansion_token = context->macro_expansion_token;
    node->should_skip_undefined_identifier = context->static_if_evaluate_should_skip_undefined_identifier;
}

func void static_if_report_error(struct context *context, struct token *token, struct token *macro_expansion_token, struct token *directive, char *error){
    
    if(token->type == TOKEN_invalid){
        report_error(context, directive, "File ends within '#if'.");
    }else{
        tokenizer_report_error(context, token, macro_expansion_token, error);
    }
}

// directive is only here to report errors
func s64 static_if_evaluate(struct context *context, struct token *directive){
    
    
    // prevent the usage of the *token_raw routines
    prevent_usage(peek_token_raw);
    prevent_usage(next_token_raw);
    prevent_usage(get_current_token_raw);    
    prevent_usage(peek_token_eat_raw);
    
    begin_counter(context, static_if_evaluate);
    context->static_if_evaluate_should_skip_undefined_identifier = false;
    context->static_if_stack_at = 0;
    
    retry:;
    if(context->error) return 0;
    
    // "[...], all signed integer types and all unsigned integer types act as if they have the same 
    //  representation as, respectively, the types intmax_t and uintmax_t defined in the header <stdint.h>."
    
    // 
    // I think this means that there are only two types 'intmax_t' and 'uintmax_t', cl, clang and gcc both print "Hello" here:
    // 
    //     #if (123u - 1245ll) > 0
    //          #error "Hello"
    //     #endif
    //     
    // Meaning (123u - 1245ll) = (123ull - 1234ll) > 0
    // 
    
    u64 value = 0;
    int is_unsigned = 0;
    
    struct token *test = next_token(context);
    
    switch(test->type){
        case TOKEN_logical_not: static_if_stack_push(context, test, STATIC_IF_EVALUATE_logical_not, 0, 0); goto retry;
        case TOKEN_bitwise_not: static_if_stack_push(context, test, STATIC_IF_EVALUATE_bitwise_not, 0, 0); goto retry;
        case TOKEN_minus:       static_if_stack_push(context, test, STATIC_IF_EVALUATE_unary_minus, 0, 0); goto retry;
        case TOKEN_plus:        /* just ignore this I guess */                                             goto retry;
        
        case TOKEN_character_literal:{
            struct parsed_integer parsed_integer = parse_character_literal(context, test);
            switch(parsed_integer.number_kind){
                case NUMBER_KIND_s8:  value = (s8)parsed_integer.value; break;
                case NUMBER_KIND_u16: value = (u16)parsed_integer.value; is_unsigned = 1; break;
                case NUMBER_KIND_u32: value = (u32)parsed_integer.value; is_unsigned = 1; break;
                case NUMBER_KIND_invalid:{
                    static_if_report_error(context, test, context->macro_expansion_token, directive, "Invalid prefix on string literal.");
                }break;
                invalid_default_case();
            }
        }break;
        
        case TOKEN_binary_literal:
        case TOKEN_hex_literal:
        case TOKEN_base10_literal:{
            struct parsed_integer parsed_integer;
            if(test->type == TOKEN_base10_literal){
                parsed_integer = parse_base10_literal(context, test);
            }else if(test->type == TOKEN_hex_literal) {
                parsed_integer = parse_hex_literal(context, test);
            }else{
                parsed_integer = parse_binary_literal(context, test);
            }
            
            value = parsed_integer.value;
            
            switch(parsed_integer.number_kind){
                
                // For 0xffffffff (hex u32_max):
                // 
                // Okay, As far as I undestand, all of these types (are!) just int64 so the first type that matches is s64. 
                // Hence, we don't have the case where we need to check for values between s32_max and u32_max.
                
                case NUMBER_KIND_int:
                case NUMBER_KIND_long:
                case NUMBER_KIND_long_long:{
                    if(value < max_s64) break;
                    report_warning(context, WARNING_integer_literal_too_large_to_be_signed, test, "Integer literal exceeds the maximum value representable as a signed integer and is interpreted as unsigned.");
                    is_unsigned = 1;
                }break;
                
                case NUMBER_KIND_unsigned:
                case NUMBER_KIND_unsigned_long:
                case NUMBER_KIND_unsigned_long_long:{
                    is_unsigned = 1;
                }break;
                
                case NUMBER_KIND_s8:  value = (s8)value;  break;
                case NUMBER_KIND_s16: value = (s16)value; break;
                case NUMBER_KIND_s32: value = (s32)value; break;
                case NUMBER_KIND_s64: value = (s64)value; break;
                
                case NUMBER_KIND_u8:  value = (u8)value;  is_unsigned = 1; break;
                case NUMBER_KIND_u16: value = (u16)value; is_unsigned = 1; break;
                case NUMBER_KIND_u32: value = (u32)value; is_unsigned = 1; break;
                case NUMBER_KIND_u64: value = (u64)value; is_unsigned = 1; break;
                
                default: break; // We have reported an error in `parse_*_literal`.
            }
        }break;
        
        case TOKEN_identifier:{
            if(string_match(token_get_string(test), string("defined"))){ // @cleanup: atoms match ?
                eat_whitespace_and_comments(context);
                struct token *got_paren = peek_token_eat(context, TOKEN_open_paren);
                
                eat_whitespace_and_comments(context);
                
                struct token *token = next_token(context);
                
                // We are gonna report an error, set the token.
                if(token->type != TOKEN_identifier){ 
                    static_if_report_error(context, test, context->macro_expansion_token, directive, "Expected an identifier after 'defined'.");
                    return 0;
                }
                
                if(got_paren){
                    test = next_token(context);
                    
                    if(test->type != TOKEN_closed_paren){
                        static_if_report_error(context, test, context->macro_expansion_token, directive, "Expected a ')' after 'defined' identifier.");
                        return 0;
                    }
                }
                
                if(context->should_exit_statement) return 0;
                
                value = lookup_define(context, token->atom) != null;
                is_unsigned = 0;
            }else{
                if(!context->static_if_evaluate_should_skip_undefined_identifier){
                    struct token *ret = push_uninitialized_struct(&context->scratch, struct token);
                    *ret = *test;
                    // :copy_expanded_location
                    if(context->macro_expansion_token){
                        ret->line   = context->macro_expansion_token->line;
                        ret->column = context->macro_expansion_token->column;
                        ret->file_index   = context->macro_expansion_token->file_index;
                    }
                    report_warning(context, WARNING_undefined_static_if_operand, ret, "Undefined identifier in '#if' operand gets evaluated to zero.");
                }
                
                value = 0;
                is_unsigned = 0;
            }
        }break;
        case TOKEN_open_paren:{
            static_if_stack_push(context, test, STATIC_IF_EVALUATE_parenthesized_expression, 0, 0);
            goto retry;
            
            resume_for_parenthesized_expression:;
            if(test->type != TOKEN_closed_paren){
                static_if_report_error(context, test, context->macro_expansion_token, directive, "Expected ')' in '#if' argument."); // :Error
                return 0;
            }
        }break;
        default:{
            static_if_report_error(context, test, context->macro_expansion_token, directive, "Unexpected token in '#if' operand.");
            return 0;
        }break;   
    }
    
    static enum precedence{
        PRECEDENCE_prefix         = 2,
        PRECEDENCE_multiplicative = 3,
        PRECEDENCE_additive       = 4,
        PRECEDENCE_shift          = 5,
        PRECEDENCE_relational     = 6,
        PRECEDENCE_equality       = 7,
        PRECEDENCE_bitwise_and    = 8,
        PRECEDENCE_bitwise_xor    = 9,
        PRECEDENCE_bitwise_or     = 10,
        PRECEDENCE_logical_and    = 11,
        PRECEDENCE_logical_or     = 12,
        PRECEDENCE_ternary        = 13,
        PRECEDENCE_assignment     = 14,
        PRECEDENCE_comma          = 15,
        PRECEDENCE_parenthesized_expression = 16,
    } token_to_precedence[TOKEN_count] = {
        [TOKEN_times] = PRECEDENCE_multiplicative, 
        [TOKEN_slash] = PRECEDENCE_multiplicative, 
        [TOKEN_mod]   = PRECEDENCE_multiplicative,
        
        [TOKEN_plus]  = PRECEDENCE_additive, 
        [TOKEN_minus] = PRECEDENCE_additive,
        
        [TOKEN_left_shift]  = PRECEDENCE_shift, 
        [TOKEN_right_shift] = PRECEDENCE_shift,
        
        [TOKEN_bigger_equals]  = PRECEDENCE_relational,
        [TOKEN_smaller_equals] = PRECEDENCE_relational,
        [TOKEN_bigger]         = PRECEDENCE_relational,
        [TOKEN_smaller]        = PRECEDENCE_relational,
        
        [TOKEN_logical_equals]   = PRECEDENCE_equality,
        [TOKEN_logical_unequals] = PRECEDENCE_equality,
        
        [TOKEN_and] = PRECEDENCE_bitwise_and,
        [TOKEN_xor] = PRECEDENCE_bitwise_xor,
        [TOKEN_or]  = PRECEDENCE_bitwise_or,
        [TOKEN_logical_and] = PRECEDENCE_logical_and,
        [TOKEN_logical_or]  = PRECEDENCE_logical_or,
        
        [TOKEN_question_mark] = PRECEDENCE_ternary,
        
        // [TOKEN_comma] = PRECEDENCE_comma, // ?
    };
    
    struct token *binary_expression = next_token(context);
    
    while(context->static_if_stack_at > 0){
        static enum precedence operation_to_precedence[STATIC_IF_EVALUATE_operation_count] = {
            [STATIC_IF_EVALUATE_bitwise_not] = PRECEDENCE_prefix,
            [STATIC_IF_EVALUATE_logical_not] = PRECEDENCE_prefix,
            [STATIC_IF_EVALUATE_unary_minus] = PRECEDENCE_prefix,
            
            [STATIC_IF_EVALUATE_times]  = PRECEDENCE_multiplicative,
            [STATIC_IF_EVALUATE_div] = PRECEDENCE_multiplicative,
            [STATIC_IF_EVALUATE_mod]    = PRECEDENCE_multiplicative,
            
            [STATIC_IF_EVALUATE_plus]  = PRECEDENCE_additive,
            [STATIC_IF_EVALUATE_minus] = PRECEDENCE_additive,
            
            [STATIC_IF_EVALUATE_left_shift]  = PRECEDENCE_shift,
            [STATIC_IF_EVALUATE_right_shift] = PRECEDENCE_shift,
            
            [STATIC_IF_EVALUATE_logical_bigger_equals]  = PRECEDENCE_relational,
            [STATIC_IF_EVALUATE_logical_smaller_equals] = PRECEDENCE_relational,
            [STATIC_IF_EVALUATE_logical_bigger]         = PRECEDENCE_relational,
            [STATIC_IF_EVALUATE_logical_smaller]        = PRECEDENCE_relational,
            
            [STATIC_IF_EVALUATE_logical_equals]   = PRECEDENCE_equality,
            [STATIC_IF_EVALUATE_logical_unequals] = PRECEDENCE_equality,
            
            [STATIC_IF_EVALUATE_and] = PRECEDENCE_bitwise_and,
            [STATIC_IF_EVALUATE_xor] = PRECEDENCE_bitwise_xor,
            [STATIC_IF_EVALUATE_or]  = PRECEDENCE_bitwise_or,
            
            [STATIC_IF_EVALUATE_logical_and] = PRECEDENCE_logical_and,
            [STATIC_IF_EVALUATE_logical_or]  = PRECEDENCE_logical_or,
            
            [STATIC_IF_EVALUATE_parenthesized_expression] = PRECEDENCE_parenthesized_expression,
            
            
            // ternary:
            //     <expr> ? <parenthesized> : <ternary>
            [STATIC_IF_EVALUATE_ternary_condition] = PRECEDENCE_parenthesized_expression,
            [STATIC_IF_EVALUATE_ternary_if_true] = PRECEDENCE_ternary, // right-to-left associative
            // [STATIC_IF_EVALUATE_comma_expression] = PRECEDENCE_comma,
        };
        
        struct static_if_evaluate_stack_node *current = static_if_stack_current(context);
        
        enum precedence operation_precedence = operation_to_precedence[current->operation];
        enum precedence token_precedence     = token_to_precedence[binary_expression->type];
        
        if(token_precedence && operation_precedence > token_precedence) break;
        
        // Ternaries are right associative.
        if(operation_precedence == token_precedence && operation_precedence == PRECEDENCE_ternary) break;
        
        static_if_stack_pop(context);
        
        switch(current->operation){
            case STATIC_IF_EVALUATE_logical_not: value = !value; break;
            case STATIC_IF_EVALUATE_bitwise_not: value = ~value; break;
            case STATIC_IF_EVALUATE_unary_minus: value = -(s64)value; break; // @cleanup: warning?
            
            case STATIC_IF_EVALUATE_left_shift:{
                is_unsigned |= current->is_unsigned;
                value = is_unsigned ? ((u64)current->value << (u64)value) : ((s64)current->value << (s64)value);
            }break;
            case STATIC_IF_EVALUATE_right_shift:{
                is_unsigned |= current->is_unsigned;
                value = is_unsigned ? ((u64)current->value >> (u64)value) : ((s64)current->value >> (s64)value);
            }break;
            
            case STATIC_IF_EVALUATE_and: is_unsigned |= current->is_unsigned; value = current->value & value; break;
            case STATIC_IF_EVALUATE_or:  is_unsigned |= current->is_unsigned; value = current->value | value; break;
            case STATIC_IF_EVALUATE_xor: is_unsigned |= current->is_unsigned; value = current->value ^ value; break;
            
            case STATIC_IF_EVALUATE_times:{
                is_unsigned |= current->is_unsigned;
                value = is_unsigned ? ((u64)current->value * (u64)value) : ((s64)current->value * (s64)value);
            }break;
            
            case STATIC_IF_EVALUATE_div:{
                is_unsigned |= current->is_unsigned;
                if(value == 0){
                    static_if_report_error(context, get_current_token_for_error_report(context), static_if_stack_current(context)->macro_expansion_token, directive, "Divide by zero."); // @cleanup: More context if identifier was undefined.
                    return 0;
                }
                value = is_unsigned ? ((u64)current->value / (u64)value) : ((s64)current->value / (s64)value);
            }break;
            
            case STATIC_IF_EVALUATE_mod:{
                is_unsigned |= current->is_unsigned;
                if(value == 0){
                    static_if_report_error(context, get_current_token_for_error_report(context), static_if_stack_current(context)->macro_expansion_token, directive, "Mod with zero."); // @cleanup: More context if identifier was undefined.
                    return 0;
                }
                value = is_unsigned ? ((u64)current->value % (u64)value) : ((s64)current->value % (s64)value);
            }break;
            
            case STATIC_IF_EVALUATE_plus:  is_unsigned |= current->is_unsigned; value = current->value + value; break;
            case STATIC_IF_EVALUATE_minus: is_unsigned |= current->is_unsigned; value = current->value - value; break;
            
            case STATIC_IF_EVALUATE_logical_equals:   is_unsigned = 0; value = (current->value == value); break;
            case STATIC_IF_EVALUATE_logical_unequals: is_unsigned = 0; value = (current->value != value); break;
            
            case STATIC_IF_EVALUATE_logical_bigger_equals:{
                value = (is_unsigned | current->is_unsigned) ? ((u64)current->value >= (u64)value) : ((s64)current->value >= (s64)value);
                is_unsigned = 0;
            }break;
            
            case STATIC_IF_EVALUATE_logical_smaller_equals:{
                value = (is_unsigned | current->is_unsigned) ? ((u64)current->value <= (u64)value) : ((s64)current->value <= (s64)value);
                is_unsigned = 0;
            }break;
            
            case STATIC_IF_EVALUATE_logical_bigger:{
                value = (is_unsigned | current->is_unsigned) ? ((u64)current->value > (u64)value) : ((s64)current->value > (s64)value);
                is_unsigned = 0;
            }break;
            case STATIC_IF_EVALUATE_logical_smaller:{
                value = (is_unsigned | current->is_unsigned) ? ((u64)current->value < (u64)value) : ((s64)current->value < (s64)value);
                is_unsigned = 0;
            }break;
            
            case STATIC_IF_EVALUATE_logical_and:{
                is_unsigned = 0; 
                value = (current->value && value); 
                
                // Restore the old value of 'should_skip_undefined_identifier'.
                context->static_if_evaluate_should_skip_undefined_identifier = current->should_skip_undefined_identifier;
            }break;
                
            case STATIC_IF_EVALUATE_logical_or:{
                is_unsigned = 0; 
                value = (current->value || value); 
                
                // Restore the old value of 'should_skip_undefined_identifier'.
                context->static_if_evaluate_should_skip_undefined_identifier = current->should_skip_undefined_identifier;
            }break;
            
            case STATIC_IF_EVALUATE_parenthesized_expression:{
                // @note: leave 'value' where it is!
                test = binary_expression;
                goto resume_for_parenthesized_expression;
            }break;
            
            case STATIC_IF_EVALUATE_ternary_condition:{
                
                if(binary_expression->type != TOKEN_colon){
                    static_if_report_error(context, binary_expression, context->macro_expansion_token, directive, "Expected a ':'.");
                    return 0;
                }
                
                static_if_stack_push(context, current->token, STATIC_IF_EVALUATE_ternary_condition, current->value, current->is_unsigned);
                static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_ternary_if_true, value, is_unsigned);
                goto retry;
            }break;
            
            case STATIC_IF_EVALUATE_ternary_if_true:{
                struct static_if_evaluate_stack_node *if_true   = current;
                struct static_if_evaluate_stack_node *condition = static_if_stack_pop(context);
                
                assert(if_true->operation == STATIC_IF_EVALUATE_ternary_if_true && condition->operation == STATIC_IF_EVALUATE_ternary_condition);
                
                value = condition->value ? if_true->value : value;
            }break;
            
            invalid_default_case();
        }
    }
    
    switch(binary_expression->type){
        case TOKEN_times:            static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_times,                  value, is_unsigned); goto retry;
        case TOKEN_slash:            static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_div,                    value, is_unsigned); goto retry;
        case TOKEN_mod:              static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_mod,                    value, is_unsigned); goto retry;
        case TOKEN_plus:             static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_plus,                   value, is_unsigned); goto retry;
        case TOKEN_minus:            static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_minus,                  value, is_unsigned); goto retry;
        case TOKEN_left_shift:       static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_left_shift,             value, is_unsigned); goto retry;
        case TOKEN_right_shift:      static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_right_shift,            value, is_unsigned); goto retry;
        case TOKEN_bigger_equals:    static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_bigger_equals,  value, is_unsigned); goto retry;
        case TOKEN_smaller_equals:   static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_smaller_equals, value, is_unsigned); goto retry;
        case TOKEN_logical_equals:   static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_equals,         value, is_unsigned); goto retry;
        case TOKEN_logical_unequals: static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_unequals,       value, is_unsigned); goto retry;
        case TOKEN_bigger:           static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_bigger,         value, is_unsigned); goto retry;
        case TOKEN_smaller:          static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_smaller,        value, is_unsigned); goto retry;
        case TOKEN_and:              static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_and,                    value, is_unsigned); goto retry;
        case TOKEN_or:               static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_or,                     value, is_unsigned); goto retry;
        case TOKEN_xor:              static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_xor,                    value, is_unsigned); goto retry;
        case TOKEN_logical_and:{
            static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_and, value, is_unsigned); 
            
            // 
            // Don't report 'undefined identifier'-warnings within the rhs if the lhs was 0.
            // This prevents us from reporting an error for '#if defined(_MSC_VER) && _MSC_VER >= 1337'.
            // 
            context->static_if_evaluate_should_skip_undefined_identifier |= (value == 0);
            
            goto retry;
        }
        
        case TOKEN_logical_or:{
            static_if_stack_push(context, binary_expression, STATIC_IF_EVALUATE_logical_or, value, is_unsigned); 
            
            // 
            // Don't report 'undefined identifier'-warnings within the rhs if the lhs was not 0.
            // This prevents us from reporting an error for '#if !defined(_MSC_VER) || _MSC_VER < 1337'.
            // 
            context->static_if_evaluate_should_skip_undefined_identifier |= (value != 0);
            
            goto retry;
        }
        
        case TOKEN_question_mark: static_if_stack_push(context, test, STATIC_IF_EVALUATE_ternary_condition, value, is_unsigned); goto retry;
        
        default: break;
    }
    
    
    assert(context->static_if_stack_at == 0);
    end_counter(context, static_if_evaluate);
    
    return value;
}


//_____________________________________________________________________________________________________________________

struct file *load_or_get_source_file_by_absolute_path(struct context *context, char *absolute_file_path, smm file_size, int is_system_include){
    
    smm file_name_hash = string_djb2_hash(string_from_cstring(absolute_file_path));
    
    struct temporary_memory temp = begin_temporary_memory(context->arena);
    
    struct file *file = push_struct(context->arena, struct file);
    file->absolute_file_path = absolute_file_path;
    file->is_system_include  = is_system_include;
    atomic_store(int, file->in_progress, true);
    
    for(s32 table_index = 0; table_index < array_count(globals.file_table.data); table_index++){
        s32 index = (table_index + file_name_hash) & (array_count(globals.file_table.data) - 1);
        
        struct file *other = globals.file_table.data[index];
        
        if(!other){
            if(atomic_compare_and_swap(&globals.file_table.data[index], file, null) == null){
                // 
                // We are the ones who inserted this file!
                // So load it from the disk.
                // 
                file->file_index = index;
                smm size = atomic_postincrement(&globals.file_table.size);
                (void)size;
                // @cleanup report error on too many includes?
                
                solidify_temporary_memory(temp);
                break;
            }
            other = globals.file_table.data[index];
            assert(other);
        }
        
        if(cstring_match(absolute_file_path, other->absolute_file_path)){
            
            // Wait for the file to be done.
            while(atomic_load(smm, other->in_progress) == true) _mm_pause();
            
            end_temporary_memory(temp);
            return other;
        }
    }
    
    begin_counter(context, load_file);
    
    // :padded_file_size
    // 
    // @WARNING: We have to add 128 padding bytes as we are using 'hash_md5_inplace' in coff_writer.c
    // 
    smm pad_size = 128;
    smm padded_file_size = file_size + pad_size;
    
    u8 *file_buffer = push_uninitialized_data(context->arena, u8, padded_file_size);
    
    struct os_file os_file = os_load_file(absolute_file_path, file_buffer, padded_file_size);
    
    // 
    // Clear the added padding, this will *really* zero-terminate the file.
    // 
    // memset(buf + file_size, 0, pad_size); 
    // 
    // @note: We do not have to clear the padding, as we never deallocate from 'arena'.
    // 
    
    end_counter(context, load_file);
    
    if(os_file.amount != file_size){
        report_error(context, 0, "Error: File '%s' changed during compilation.", absolute_file_path);
        goto end;
    }
    
    file->file = os_file;
    file->amount_of_times_included = 0;
    
    // :newline_at_the_end_of_the_file
    // 
    // Put a newline at the very end of the file, this makes sure directives do not have to 
    // worry about end of file.
    // Put another newline there, so we don't have to wory about backslashes ruining our fun.
    file_buffer[file_size] = '\n';
    file_buffer[file_size + 1] = '\n';
    
    struct string file_contents = {
        .data = os_file.data,
        .size = os_file.size + 2,
    };
    
    // 
    // BOM (Byte Order Mark) handling.
    // 
    if(file_buffer[0] == 0xEF && file_buffer[1] == 0xBB && file_buffer[2] == 0xBF){
        // UTF8-BOM
        file_contents.data += 3;
        file_contents.size -= 3;
    }else if((file_buffer[0] == 0xEF && file_buffer[1] == 0xFF) || (file_buffer[0] == 0xFF && file_buffer[1] == 0xFE)){
        // Big Endian UTF16
        report_error(context, 0, "Error: File '%s' starts with a UTF-16 Byte Order Mark. UTF-16 source files are not supported at this point.", absolute_file_path);
        goto end;
    }
    
    file->tokens = tokenize_raw(context, file_contents, file->file_index, /* is_stupid_hack */ false, &file->lines);
    
    end:
    atomic_store(int, file->in_progress, false);
    
    return file;
}


func int handle_include_directive(struct context *context, struct token *directive, int is_system_include, struct string include_string){
    begin_counter(context, handle_include);
    
    include_string = push_zero_terminated_string_copy(context->arena, include_string);
    hacky_canonicalize_file_for_case_insensitivity(&include_string);
    
    struct file *file = null;
    
    if(!is_system_include){
        
        //
        // First check relative to this file.
        //
        struct file *parent_file = globals.file_table.data[directive->file_index];
        
        // For the purposes of reporting warnings we want to treat ""-includes in system include files the same as system includes.
        if(parent_file->is_system_include) is_system_include = true;
        
        struct string path = strip_file_name(string_from_cstring(parent_file->absolute_file_path));
        struct string absolute_file_path = canonicalize_slashes(concatenate_file_paths(&context->scratch, path, include_string));
        struct os_file dummy = os_load_file((char *)absolute_file_path.data, 0, 0);
        if(!dummy.file_does_not_exist){
            // 
            // The include was a relative ""-include.
            // 
            
            file = load_or_get_source_file_by_absolute_path(context, push_cstring_from_string(context->arena, absolute_file_path), dummy.size, is_system_include);
        }
    }
    
    if(!file){
        // 
        // This is either a <>-include, or it failed the relative lookup.
        // 
        
        smm capacity = globals.system_include_table.capacity;
        struct system_include_file_entry *entries = globals.system_include_table.entries;
        
        u64 hash = string_djb2_hash(include_string);
        
        struct system_include_file_entry *entry = null;
        
        for(smm table_index = 0; table_index < capacity; table_index++){
            smm index = (hash + table_index) & (capacity - 1);
            
            if(!entries[index].absolute_file_path) break;
            
            if(string_match(entries[index].include_string, include_string)){
                entry = &entries[index];
                break;
            }
        }
        
        if(!entry){
            report_error(context, directive, "'%.*s' include file not found.", include_string.size, include_string.data);
            return 1;
        }
        
        if(entry->in_progress == 0){
            // 
            // The file is not yet loaded, try to load it.
            // 
            
            if(atomic_compare_and_swap_smm(&entry->in_progress, 1, 0) == 0){
                // 
                // We are the ones who are supposed to load the file.
                // 
                entry->file = load_or_get_source_file_by_absolute_path(context, entry->absolute_file_path, entry->file_size, is_system_include);
            }
        }
        
        // Wait for the file to be done.
        while(atomic_load(smm, entry->in_progress) == 1) _mm_pause();
        
        file = entry->file;
    }
    
    assert(file);
    
    //
    // Check if we are one of the files that contained a '#pragma once' in this compilation unit,
    // and if we do return no tokens.
    //
    b32 skip_include = false;
    for(struct pragma_once_list_node *once = context->pragma_once_file_list.first; once; once = once->next){
        if(once->file == file){
            // We don't have a file to report but we also don't have to load a file.
            skip_include = true;
            break;
        }
    }
    
    if(!skip_include){
        if(globals.cli_options.show_includes){
            print("%s\n", file->absolute_file_path);
        }
        
        //
        // Wait for the file to finish and return the stashed tokens.
        //
        struct token_array tokens = file->tokens;
        
        atomic_preincrement(&file->amount_of_times_included);
        
        if(tokens.amount){
            struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
            node->tokens = tokens;
            sll_push_front(context->token_stack, node);
        }
    }
    
    // @cleanup:
    // don't report on junk, as we just pushed a new file... maybe report above!
    end_counter(context, handle_include);
    end_counter(context, handle_directive);
    
    return 0;
}

struct static_if_stack_node{
    struct static_if_stack_node *next;
    struct token *token;  // used to report an error if there was no matchin #endif (and for debugging)
    b32 is_disabled_elif; // used to handle '#if 1' -> '#elif 0' -> '#else' -> '#error asd' ->  '#endif'
    
    b32 is_true;
    b32 is_else;
};

// @note: if we left the tokens in a linked list of buckets, we would not have to copy them
func struct token_array file_tokenize_and_preprocess(struct context *context, struct string initial_absolute_file_path, smm file_size){
    
    {
        if(globals.cli_options.show_includes){
            print("\n%.*s\n", initial_absolute_file_path.size, initial_absolute_file_path.data);
        }
        
        struct file *main_file = load_or_get_source_file_by_absolute_path(context, (char *)initial_absolute_file_path.data, file_size, false);
        atomic_preincrement(&main_file->amount_of_times_included);
        context->current_compilation_unit->main_file = main_file;
        
        struct token_array array = main_file->tokens;
        if(context->error || array.size == 0) return (struct token_array)zero_struct;
        
        sll_clear(context->token_stack);
        struct token_stack_node *node = push_struct(&context->scratch, struct token_stack_node);
        node->tokens = array;
        sll_push_front(context->token_stack, node);
        
        // 
        // Push the predefined tokens as the first thing to be parsed!.
        // 
        struct token_stack_node *predefines_node = push_struct(&context->scratch, struct token_stack_node);
        predefines_node->tokens = globals.predefined_tokens;
        sll_push_front(context->token_stack, predefines_node);
    }
    
    struct memory_arena *scratch = &context->scratch;
    
#define TOKEN_EMIT_RESERVE_SIZE (mega_bytes(100) * sizeof(struct token))
#define TOKEN_EMIT_COMMIT_SIZE  (0x10000 * sizeof(struct token))
    
    smm reserved_tokens  = TOKEN_EMIT_RESERVE_SIZE / sizeof(struct token);
    smm committed_tokens = 0;
    smm emitted_tokens   = 0;
    struct os_virtual_buffer token_buffer = os_reserve_memory(0, TOKEN_EMIT_RESERVE_SIZE);
    struct token *emitted_token_buffer = (struct token *)token_buffer.memory;
    
    struct{
        struct static_if_stack_node *first;  // top    of the stack
        struct static_if_stack_node *last;   // bottom of the stack
    } static_if_stack = zero_struct;
    
    b32 have_postponed_directive = false;
    struct postponed_directive{
        struct token *directive;
        smm saved_emitted_token_count;
        enum preprocessor_directive directive_kind;
    } postponed_directive = zero_struct;
    
    if(!token_buffer.memory){
        report_error(context, null, "Error: Could not allocate token array for file '%.*s'.", initial_absolute_file_path.size, initial_absolute_file_path.data);
        goto end;
    }
    
    b32 got_newline = true;
    while(!tokenizer_is_at_the_end_of_the_file(context)){
        
        if(static_if_stack.first && !static_if_stack.first->is_true){
            // if we are in a diabled static if, we search for a newline into hash
            begin_counter(context, if0_block);
            while(true){
                // no need to expand the token, if we are in a '#if 0' block.
                struct token *token = get_current_token_raw(context);
                
                if(got_newline && token->type == TOKEN_hash) break;
                if(token->type == TOKEN_invalid){
                    end_counter(context, static_if_skip);
                    goto end;
                }
                
                if(token->type != TOKEN_whitespace && token->type != TOKEN_comment){
                    if(token->type == TOKEN_newline){
                        got_newline = true;
                    }else{
                        got_newline = false;
                    }
                }
                
                next_token_raw(context);
            }
            end_counter(context, if0_block);
        }
        
        if(got_newline && peek_token_eat_raw(context, TOKEN_hash)){
            got_newline = false; // Directives do not eat their newlines, this also needs to be here for 'postponed_directives'.
            
            begin_counter(context, handle_directive);
            
            eat_whitespace_and_comments(context);
            
            if(peek_token_raw(context, TOKEN_newline)){
                // do nothing
                continue;
            }
            
            struct token *directive = next_token_raw(context);
            
            struct string directive_string = token_get_string(directive);
            
            enum preprocessor_directive directive_kind = DIRECTIVE_invalid;
            
            {
                u64 index = directive->string_hash & (globals.directive_table_size - 1);
                if(atoms_match(globals.directive_table[index].directive, directive->atom)){
                    directive_kind = globals.directive_table[index].type;
                }
            }
            
            if(directive_kind < FIRST_DIRECTIVE_WHICH_SHOULD_BE_STILL_BE_RUN_IN_DISABLED_STATIC_IF){
                
                if(static_if_stack.first && !static_if_stack.first->is_true){
                    // Skip the directive.
                    // @note: This also skips invalid directives in disabled blocks.
                    
                    while(!peek_token_raw(context, TOKEN_newline)){
                        next_token_raw(context);
                    }
                    continue;
                }
            }
            
            switch(directive_kind){
                case DIRECTIVE_invalid:{
                    if(!static_if_stack.first || static_if_stack.first->is_true){
                        // 
                        // Only report on invalid preprocessor directives, if we are in active code.
                        // This allows writing arbitrary stuff inside of an `#if 0`-block.
                        // 
                        report_error(context, directive, "Invalid preprocessor directive.");
                        goto end;
                    }
                }break;
                
                case DIRECTIVE_if:{
                    eat_whitespace_and_comments(context);
                    
                    if(peek_token_raw(context, TOKEN_newline)){
                        report_error(context, directive, "Expected an argument after '#if'.");
                        goto end;
                    }
                    
                    // disable the static_if_evaluate if it doesn't matter anyway
                    if(!static_if_stack.first || static_if_stack.first->is_true){
                        //
                        // 'node->is_true' will be set accordingly in the 'postponed_directive' code.
                        //
                        struct static_if_stack_node *node = push_struct(scratch, struct static_if_stack_node);
                        node->is_true = 1;
                        node->token = directive;
                        sll_push_front(static_if_stack, node);
                        
                        assert(!have_postponed_directive);
                        have_postponed_directive = true;
                        postponed_directive.directive = directive;
                        postponed_directive.saved_emitted_token_count = emitted_tokens;
                        postponed_directive.directive_kind = directive_kind;
                        context->in_static_if_condition = 1;
                        
                        end_counter(context, handle_directive);
                        continue;
                    }else{
                        while(!peek_token_raw(context, TOKEN_newline)){
                            next_token_raw(context);
                        }
                    }
                    
                    // @note: value will always be '0' if this '#if' is contained in a '#if 0' block
                    struct static_if_stack_node *node = push_struct(scratch, struct static_if_stack_node);
                    node->is_true = 0;
                    node->token = directive;
                    
                    sll_push_front(static_if_stack, node);
                }break;
                
                case DIRECTIVE_elif:{
                    
                    struct static_if_stack_node *node = static_if_stack.first;
                    
                    eat_whitespace_and_comments(context);
                    if(peek_token_raw(context, TOKEN_newline)){
                        report_error(context, directive, "Expected an argument after '#elif'.");
                        goto end;
                    }
                    
                    if(!node){
                        report_error(context, directive, "'#elif' without matching '#if'.");
                        goto end;
                    }
                    
                    if(node->token->file_index != directive->file_index){
                        begin_error_report(context);
                        report_error(context, directive, "'#elif' matches '#%.*s' in different file.", node->token->size, node->token->data);
                        report_error(context, node->token, "... Here is '#%.*s' it matches.", node->token->size, node->token->data);
                        end_error_report(context);
                        goto end;
                    }
                    
                    if(node->is_else){
                        begin_error_report(context);
                        report_error(context, directive, "'#elif' after '#else'.");
                        report_error(context, node->token, "... Here is the previous '#else'.");
                        end_error_report(context);
                        goto end;
                    }
                    
                    //
                    // @WARNING: we edit 'node' directly, as the '#if' that it was previously is not needed anymore.
                    //
                    node->token = directive;
                    
                    // 
                    // First check whether the current '#if' (node) was not disabled by a outer #if, e.g.:
                    // 
                    //    #if 0
                    //       #if 1   // <- this is 'node' (disabled by outer #if).
                    //       #elif 1 // <- this is us.
                    //       #endif
                    //    #endif
                    //    
                    // We do this by checking whether it is a top level #if (node->next == null)
                    // or whether the outer #if is enabled (node->next->is_true).
                    // 
                    if(!node->next || node->next->is_true){
                        if(node->is_true || node->is_disabled_elif){
                            // 
                            // If the 'node' is enabled (is_true) this means (as we are an #elif),
                            // we are disabled. 
                            // 
                            // If the previous node is an disabled #elif, e.g:
                            //   
                            //   #if 1
                            //   #elif 1 <- this is 'node'.
                            //   #elif X <- this is us.
                            //   #endif
                            // 
                            // Then we are also disabled.
                            // 
                            
                            node->is_true = false;
                            node->is_disabled_elif = true;
                            
                            // Skip the condition as we are ignoring it.
                            while(!peek_token_raw(context, TOKEN_newline)) next_token_raw(context);
                        }else{
                            //
                            // We should be enabled depending on our condition, 
                            // 'node->is_true' will be set accordingly in the 'postponed_directive' code.
                            //
                            node->is_true = true;
                            
                            assert(!have_postponed_directive);
                            have_postponed_directive = true;
                            postponed_directive.directive = directive;
                            postponed_directive.saved_emitted_token_count = emitted_tokens;
                            postponed_directive.directive_kind = directive_kind;
                            context->in_static_if_condition = 1;
                            
                            end_counter(context, handle_directive);
                            continue;
                        }
                    }else{
                        // 
                        // If there was an outer #if and it disabled this #if (node), 
                        // the #if should also be disabled.
                        // 
                        assert(!node->is_true);
                        
                        // Skip the condition as we are ignoring it.
                        while(!peek_token_raw(context, TOKEN_newline)) next_token_raw(context);
                    }
                }break; 
                case DIRECTIVE_ifdef:
                case DIRECTIVE_ifndef:{
                    eat_whitespace_and_comments(context);
                    if(!peek_token_raw(context, TOKEN_identifier)){
                        report_error(context, directive, "Expected an identifier.");
                        goto end;
                    }
                    struct token *defined_identifier = next_token_raw(context);
                    
                    b32 evaluates_to_true = false;
                    if(!static_if_stack.first || static_if_stack.first->is_true){
                        b32 is_defined = lookup_define(context, defined_identifier->atom) != null;
                        evaluates_to_true = (directive_kind == DIRECTIVE_ifdef) ? is_defined : !is_defined;
                    }
                    
                    struct static_if_stack_node *node = push_struct(scratch, struct static_if_stack_node);
                    node->is_true = evaluates_to_true;
                    node->token = directive;
                    
                    sll_push_front(static_if_stack, node);
                }break;
                case DIRECTIVE_else:{
                    struct static_if_stack_node *node = static_if_stack.first;
                    
                    if(!node){
                        report_error(context, directive, "'#else' without matching '#if'.");
                        goto end;
                    }
                    
                    if(node->token->file_index != directive->file_index){
                        begin_error_report(context);
                        report_error(context, directive, "'#else' matches '#%.*s' in different file.", node->token->size, node->token->data);
                        report_error(context, node->token, "... Here is '#%.*s' it matches.", node->token->size, node->token->data);
                        end_error_report(context);
                        goto end;
                    }
                    
                    if(node->is_else){
                        begin_error_report(context);
                        report_error(context, directive, "Double '#else'.");
                        report_error(context, node->token, "... Here is the previous '#else'.");
                        end_error_report(context);
                        goto end;
                    }
                    
                    //
                    // @WARNING: we edit 'node' directly, as the '#if' that it was previously is not needed anymore.
                    //
                    node->is_else = true; // save that it was an else
                    node->token = directive;
                    
                    // if 'node' is a top level '#if' or if the '#if' is active (i.e '#if 1')
                    if(!node->next || node->next->is_true){
                        if(node->is_true || node->is_disabled_elif){
                            // we are an '#else' or '#elif' after a '#if 1', thus we are false
                            node->is_true = false;
                        }else{
                            node->is_true = true;
                        }
                    }else{
                        assert(!node->is_true);
                    }
                }break;
                case DIRECTIVE_endif:{
                    struct static_if_stack_node *node = static_if_stack.first;
                    
                    if(!node){
                        report_error(context, directive, "'#endif' without matching '#if'");
                        goto end;
                    }
                    
                    if(node->token->file_index != directive->file_index){
                        begin_error_report(context);
                        report_error(context, directive, "'#endif' matches '#%.*s' in different file.", node->token->size, node->token->data);
                        report_error(context, node->token, "... Here is '#%.*s' it matches.", node->token->size, node->token->data);
                        end_error_report(context);
                        goto end;
                    }
                    
                    sll_pop_front(static_if_stack);
                }break;
                
                case DIRECTIVE_define:{
                    begin_counter(context, define_parsing);
                    
                    eat_whitespace_and_comments(context);
                    if(!peek_token_raw(context, TOKEN_identifier)){
                        report_error(context, directive, "Expected an identifier after '#define'.");
                        goto end;
                    }
                    struct token *defined_identifier = next_token_raw(context);
                    struct atom name = defined_identifier->atom;
                    
                    struct define_argument_list arguments = zero_struct;
                    
                    b32 is_function_like = false;
                    b32 is_varargs = false;
                    if(peek_token_eat_raw(context, TOKEN_open_paren)){
                        is_function_like = true;
                        eat_whitespace_and_comments(context);
                        
                        if(!peek_token_eat_raw(context, TOKEN_closed_paren)){
                            do{
                                eat_whitespace_and_comments(context);
                                
                                if(peek_token_eat_raw(context, TOKEN_dotdotdot)){
                                    is_varargs = true;
                                    
                                    struct define_argument *node = push_struct(scratch, struct define_argument);
                                    node->argument = globals.keyword__VA_ARGS__;
                                    node->stringify_index = -1;
                                    
                                    sll_push_back(arguments, node);
                                    node->argument_index = arguments.count;
                                    arguments.count += 1;
                                    
                                    break;
                                }else if(!peek_token_raw(context, TOKEN_identifier)){
                                    report_error(context, defined_identifier, "Expected a parameter in function-like macro definition.");
                                    goto end;
                                }
                                
                                struct token *define_argument_token = next_token_raw(context);
                                
                                struct define_argument *node = push_struct(scratch, struct define_argument);
                                node->argument = define_argument_token->atom;
                                node->stringify_index = -1;
                                for(struct define_argument *old_arg = arguments.first; old_arg; old_arg = old_arg->next){
                                    if(atoms_match(old_arg->argument, node->argument)){
                                        report_error(context, define_argument_token, "Two arguments have the same identifier.");
                                        goto end;
                                    }
                                }
                                
                                sll_push_back(arguments, node);
                                node->argument_index = arguments.count;
                                arguments.count += 1;
                                
                                eat_whitespace_and_comments(context);
                            }while(peek_token_eat_raw(context, TOKEN_comma));
                            
                            expect_token_raw(context, defined_identifier, TOKEN_closed_paren, "Expected ')' at the end of function-like macro definition.");
                        }
                    }
                    
                    eat_whitespace_and_comments(context); // eat unnecessary leading whitespace.
                    
                    if(peek_token_raw(context, TOKEN_hashhash)){
                        report_error(context, get_current_token_raw(context), "'##' at the beginning of a macro replacement list is illegal.");
                        goto end;
                    }
                    
                    struct replacement_list replacement_list = zero_struct;
                    u32 define_amount   = 0;
                    smm stringify_count = 0;
                    struct token *define_tokens = null;
                    
                    // :newline_at_the_end_of_the_file
                    // 
                    // We don't have to worry about the file ending and not having a newline at the end,
                    // as we put a newline at the end of the file.
                    // 
                    while(!peek_token_raw(context, TOKEN_newline)){
                        b32 handled = false;
                        
                        if(peek_token_raw(context, TOKEN_identifier)){
                            struct token *ident = get_current_token_raw(context);
                            struct define_argument *arg = lookup_define_argument(arguments, ident);
                            if(arg){
                                next_token_raw(context);
                                handled = true;
                                eat_whitespace_and_comments(context);
                                
                                struct define_replacement_node *replacement = push_struct(&context->scratch, struct define_replacement_node);
                                
                                if(peek_token_raw(context, TOKEN_hashhash)){
                                    struct token *hashhash = next_token_raw(context);
                                    
                                    eat_whitespace_and_comments(context);
                                    
                                    replacement->kind = DEFINE_REPLACEMENT_hashhash;
                                    replacement->prefix_argument  = arg->argument_index;
                                    replacement->postfix_argument = -1;
                                    
                                    if(peek_token_raw(context, TOKEN_newline)){
                                        report_error(context, hashhash, "'##' at the end of a macro replacement list is illegal.");
                                        goto end;
                                    }
                                    
                                    // @note: skip the 'postfix_token' in any case
                                    struct token *postfix_token = next_token_raw(context);
                                    replacement->postfix_token = postfix_token;
                                    
                                    if(postfix_token->type == TOKEN_identifier){
                                        struct define_argument *other_arg = lookup_define_argument(arguments, postfix_token);
                                        if(other_arg){
                                            replacement->postfix_argument = other_arg->argument_index;
                                        }
                                    }
                                }else{
                                    arg->needs_to_be_expanded_count += 1;
                                    
                                    replacement->kind = DEFINE_REPLACEMENT_argument;
                                    replacement->argument_index = arg->argument_index;
                                    replacement->argument       = arg;
                                    replacement->needs_to_be_expanded_counter = arg->needs_to_be_expanded_count;
                                }
                                sll_push_back(replacement_list, replacement);
                            }
                        }else if(peek_token_raw(context, TOKEN_hash)){
                            handled = true;
                            struct token *hash = next_token_raw(context);
                            eat_whitespace_and_comments(context);
                            struct define_argument *arg = null;
                            if(peek_token_raw(context, TOKEN_identifier)){
                                struct token *ident = next_token_raw(context);
                                arg = lookup_define_argument(arguments, ident);
                            }
                            
                            if(!arg){
                                report_error(context, hash, "Expected a formal argument after '#' in macro replacement list for '%.*s'.", defined_identifier->size, defined_identifier->data);
                                goto end;
                            }
                            
                            if(arg->stringify_index == -1) arg->stringify_index = stringify_count++;
                            
                            struct define_replacement_node *replacement = push_struct(&context->scratch, struct define_replacement_node);
                            replacement->kind = DEFINE_REPLACEMENT_hash;
                            replacement->stringify_index = arg->stringify_index;
                            
                            sll_push_back(replacement_list, replacement);
                        }else if(peek_token_raw(context, TOKEN_hashhash)){
                            handled = true;
                            struct token *hashhash = next_token_raw(context);
                            eat_whitespace_and_comments(context);
                            if(peek_token_raw(context, TOKEN_newline)){
                                report_error(context, hashhash, "'##' at the end of a macro replacement list is illegal.");
                                goto end;
                            }
                            
                            struct define_replacement_node *replacement = push_struct(&context->scratch, struct define_replacement_node);
                            replacement->kind = DEFINE_REPLACEMENT_hashhash;
                            replacement->prefix_argument  = -1;
                            replacement->postfix_argument = -1;
                            
                            replacement->postfix_token = next_token_raw(context);
                            
                            if(replacement->postfix_token->type == TOKEN_identifier){
                                struct token *ident = replacement->postfix_token;
                                struct define_argument *arg = lookup_define_argument(arguments, ident);
                                if(arg){
                                    replacement->postfix_argument = arg->argument_index;
                                }
                            }
                            sll_push_back(replacement_list, replacement);
                        }
                        
                        if(!handled){
                            //
                            // if the token was not a special case above, we want to add it to a 'DEFINE_REPLACEMENT_tokens'
                            // if the last one is already a 'DEFINE_REPLACEMENT_tokens' just add it to that one.
                            // Otherwise we have to create a new one!
                            //
                            // @WARNING: We Daisy-Chain the tokens within the 'DEFINE_REPLACEMENT_tokens'.
                            //           This is possible, as if we hit another case, we end the current array.
                            //           Thus we never push other memory in the middle of an array.
                            //           This is sort of spoopy tho.   
                            //                                                        20.11.2021
                            if(replacement_list.last && replacement_list.last->kind == DEFINE_REPLACEMENT_tokens){
                                //
                                // We alread have a 'DEFINE_REPLACEMENT_tokens' add the token to this entry
                                //
                                assert(replacement_list.last->tokens.data == define_tokens);
                                assert(context->scratch.current == (u8 *)(define_tokens + define_amount));
                            }else{
                                //
                                // First push the new node.
                                //
                                struct define_replacement_node *replacement = push_struct(&context->scratch, struct define_replacement_node);
                                replacement->kind = DEFINE_REPLACEMENT_tokens;
                                sll_push_back(replacement_list, replacement);
                                
                                //
                                // Then fill out the token array!
                                //
                                assert(!define_tokens);
                                define_tokens   = push_data(scratch, struct token, 0);
                                define_amount   = 0;
                                
                                replacement->tokens.data = define_tokens;
                            }
                            
                            define_amount++;
                            *push_uninitialized_struct(&context->scratch, struct token) = *next_token_raw(context);
                            replacement_list.last->tokens.amount = define_amount;
                        }else{
                            
                            // reset the define_token array
                            define_amount   = 0;
                            define_tokens = null;
                        }
                        
                        eat_whitespace_and_comments(context);
                    }
                    
                    struct define_node *redecl = lookup_define(context, name);
                    if(!redecl){
                        struct define_node *new_node = push_struct(scratch, struct define_node);
                        new_node->replacement_list   = replacement_list;
                        new_node->name               = name;
                        new_node->defined_token      = defined_identifier;
                        new_node->arguments          = arguments;
                        new_node->stringify_count    = stringify_count;
                        new_node->is_function_like   = is_function_like;
                        new_node->is_varargs         = is_varargs;
                        
                        register_define(context, new_node);
                    }else{
                        
                        if(redecl->defined_token == &globals.invalid_token){
                            report_error(context, defined_identifier, "Cannot use '%.*s' as a macro name.", redecl->name.size, redecl->name.data);
                            goto end;
                        }
                        
                        
                        // "An identifier currently defined as an object-like macro [...] shall not be
                        //  redefined unless the second definition is a object like macro and the two
                        //  replacement lists are identical."
                        
                        // @sigh: this was a bit more well defined when we still used to have two
                        //        linear arrays for 'replacement_lists', but this should be fine.
                        //                                                        -09.11.2020
                        
                        struct define_replacement_node *decl_list = replacement_list.first;
                        struct define_replacement_node *redecl_list = redecl->replacement_list.first;
                        
                        b32 defines_are_equivalent = true;
                        
                        while(decl_list != null && redecl_list != null){
                            if(decl_list->kind != redecl_list->kind){
                                defines_are_equivalent = false;
                                break;
                            }
                            
                            switch(decl_list->kind){
                                case DEFINE_REPLACEMENT_tokens:{
                                    u32 i = 0;
                                    u32 j = 0;
                                    
                                    struct token_array tokens = decl_list->tokens;
                                    struct token_array redecl_tokens = decl_list->tokens;
                                    
                                    while(true){
                                        while(i < tokens.amount &&
                                                (tokens.data[i].type == TOKEN_whitespace ||
                                                tokens.data[i].type == TOKEN_comment)) i++;
                                        
                                        while(j < redecl_tokens.amount &&
                                                (redecl_tokens.data[j].type == TOKEN_whitespace ||
                                                redecl_tokens.data[j].type == TOKEN_comment)) j++;
                                        
                                        if(i == tokens.amount || j == redecl_tokens.amount){
                                            if(i != tokens.amount || j != redecl_tokens.amount){
                                                // @cleanup: this was commented out for some reason? 
                                                defines_are_equivalent = false;
                                            }
                                            break;
                                        }
                                        
                                        if(!atoms_match(tokens.data[i].atom, redecl_tokens.data[j].atom)){
                                            defines_are_equivalent = false;
                                            break;
                                        }else{
                                            i += 1;
                                            j += 1;
                                        }
                                    }
                                }break;
                                case DEFINE_REPLACEMENT_argument:{
                                    if(decl_list->argument_index != redecl_list->argument_index){
                                        defines_are_equivalent = false;
                                    }
                                }break;
                                case DEFINE_REPLACEMENT_hash:{
                                    // @cleanup: is this reliable?
                                    if(decl_list->stringify_index != redecl_list->stringify_index){
                                        defines_are_equivalent = false;
                                    }
                                }break;
                                case DEFINE_REPLACEMENT_hashhash:{
                                    if(decl_list->prefix_argument != redecl_list->prefix_argument ||
                                            decl_list->postfix_argument != redecl_list->postfix_argument){
                                        defines_are_equivalent = false;
                                    }
                                }break;
                                invalid_default_case();
                            }
                            
                            if(!defines_are_equivalent)break;
                            
                            decl_list   = decl_list->next;
                            redecl_list = redecl_list->next;
                        }
                        
                        // both have to be null at the end.
                        defines_are_equivalent = defines_are_equivalent && decl_list == redecl_list;
                        
                        // "Likewise, an identifier currently defined as a function-like macro shall not
                        //  be redefined by another '#define ' preprocessing directive unless the second
                        //  definition is a function-like macro definition that has the same number and
                        //  spelling of parameters, and the two replacement lists are identical."
                        
                        if(!sll_is_empty(redecl->arguments) || !sll_is_empty(arguments)){
                            // function like macros
                            
                            struct define_argument *redecl_it = redecl->arguments.first;
                            struct define_argument *decl_it = arguments.first;
                            for(; redecl_it && decl_it; redecl_it = redecl_it->next, decl_it = decl_it->next){
                                if(!atoms_match(redecl_it->argument, decl_it->argument)){
                                    defines_are_equivalent = false;
                                    break;
                                }
                            }
                            
                            if(redecl_it || decl_it){
                                defines_are_equivalent = false;
                            }else{
                                // we expect this to be true, so no fast path for '!='
                                assert(redecl->arguments.count == arguments.count);
                            }
                        }
                        
                        if(!defines_are_equivalent){
                            begin_error_report(context);
                            report_warning(context, WARNING_incompatible_redefinition_of_macro, defined_identifier, "Redefinition of macro '%.*s'.", name.amount, name.data);
                            if(redecl->defined_token->file_index == -1){
                                // @error: Print the actual definition.
                                report_warning(context, WARNING_incompatible_redefinition_of_macro, redecl->defined_token, "The previous definition was the predefined or a commandline argument.");
                            }else{
                                report_warning(context, WARNING_incompatible_redefinition_of_macro, redecl->defined_token, "... Here is the previous definition.");
                            }
                            end_error_report(context);
                            
                            redecl->replacement_list   = replacement_list;
                            redecl->name               = name;
                            redecl->defined_token      = defined_identifier;
                            redecl->arguments          = arguments;
                            redecl->stringify_count    = stringify_count;
                            redecl->is_function_like   = is_function_like;
                            redecl->is_varargs         = is_varargs;
                        }
                    }
                    
                    end_counter(context, define_parsing);
                }break;
                case DIRECTIVE_undef:{
                    eat_whitespace_and_comments(context);
                    if(!peek_token_raw(context, TOKEN_identifier)){
                        report_error(context, directive, "Expected an identifier after '#undef'.");
                        goto end;
                    }
                    struct token *undef_token = next_token_raw(context);
                    
                    struct define_node *define = lookup_define(context, undef_token->atom);
                    
                    if(define){
                        u64 index = define->name.string_hash & context->define_table.mask;
                        dll_remove(context->define_table.lists[index], define);
                    }else{
                        report_warning(context, WARNING_undef_on_undefined, undef_token, "'#undef' on undefined identifier.");
                    }
                    
                }break;
                
                case DIRECTIVE_embed:
                case DIRECTIVE_include:{
                    
                    eat_whitespace_and_comments(context);
                    
                    // 
                    // If the directive matches '#include "q-char-sequence" newline',
                    // or '#include <h-char-sequence> newline', handle the include immediately,
                    // else postpone the #include and handle it after preprocessing.
                    // 
                    
                    struct string file_name = zero_struct;
                    int is_system_include = false;
                    
                    if(peek_token_raw(context, TOKEN_string_literal)){
                        is_system_include = false;
                        
                        file_name = strip_prefix_and_quotes(token_get_string(next_token_raw(context)));
                    }else if(peek_token_raw(context, TOKEN_smaller)){
                        is_system_include = true;
                        
                        struct token *smaller = next_token_raw(context);
                        
                        while(!peek_token_raw(context, TOKEN_bigger) && !peek_token_raw(context, TOKEN_newline)){
                            next_token_raw(context);
                        }
                        
                        if(peek_token_raw(context, TOKEN_newline)){
                            report_error(context, get_current_token_raw(context), "Newline in system include path.");
                            goto end;
                        }
                        
                        struct token *bigger = next_token_raw(context);
                        
                        u8 *string_start = smaller->data + 1;
                        u8 *string_end   = bigger->data;
                        file_name = create_string(string_start, string_end - string_start);
                        
                        assert(smaller->type == TOKEN_smaller && smaller->data);
                        assert(bigger->type  == TOKEN_bigger  && bigger->data);
                    }
                    
                    if(directive_kind == DIRECTIVE_include){
                        if(file_name.data){
                            
                            // We have do handle the junk ourselves, as we will push new tokens.
                            eat_whitespace_and_comments(context);
                            if(!peek_token_raw(context, TOKEN_newline)){
                                report_warning(context, WARNING_junk_after_directive, get_current_token_raw(context), "Junk after '#%.*s'.", directive_string.size, directive_string.data);
                            }
                            
                            while(!peek_token_raw(context, TOKEN_newline)){
                                next_token_raw(context);
                            }
                            got_newline = true;
                            
                            int error = handle_include_directive(context, directive, is_system_include, file_name);
                            if(error) goto end;
                            
                            continue;
                        }else{
                            assert(!have_postponed_directive);
                            
                            //
                            // Postpone the '#include' as it has to be preprocessed before evaluating it.
                            //
                            postponed_directive.saved_emitted_token_count = emitted_tokens;
                            postponed_directive.directive = directive;
                            postponed_directive.directive_kind = directive_kind;
                            
                            have_postponed_directive = true;
                            continue;
                        }
                    }else if(directive_kind == DIRECTIVE_embed){
                        assert(!have_postponed_directive); // @paranoid
                        
                        struct token_array prefix   = zero_struct;
                        struct token_array suffix   = zero_struct;
                        struct token_array if_empty = zero_struct;
                        
                        // 
                        // #embed "file"
                        // #embed "file" limit(10) prefix(0x01) suffix(,) if_empty(error)
                        // 
                        eat_whitespace_and_comments(context);
                        while(peek_token_raw(context, TOKEN_identifier)){
                            struct token *parameter = next_token_raw(context);
                            
                            eat_whitespace_and_comments(context);
                            if(!peek_token_eat_raw(context, TOKEN_open_paren)){
                                report_syntax_error(context, parameter, "Expected a '(' after #embed-parameter.");
                                goto end;
                            }
                            
                            struct token_array tokens = zero_struct;
                            tokens.data = get_current_token_raw(context);
                            
                            // @cleanup: This should allow for "pp-balanced-token".
                            while(!peek_token_raw(context, TOKEN_closed_paren) && !peek_token_raw(context, TOKEN_newline)){
                                next_token_raw(context);
                            }
                            
                            tokens.size = get_current_token_raw(context) - tokens.data;
                            
                            if(!peek_token_eat_raw(context, TOKEN_closed_paren)){
                                // :Error
                                report_syntax_error(context, parameter, "Expected a ')' for #embed-parameter.");
                                goto end;
                            }
                            
                            if(string_match(parameter->string, string("prefix")) || string_match(parameter->string, string("__prefix__"))){
                                prefix = tokens;
                            }else if(string_match(parameter->string, string("suffix")) || string_match(parameter->string, string("__suffix__"))){
                                suffix = tokens;
                            }else if(string_match(parameter->string, string("if_empty")) || string_match(parameter->string, string("__if_empty__"))){
                                if_empty = tokens;
                            }else if(string_match(parameter->string, string("limit")) || string_match(parameter->string, string("__limit__"))){
                                // @incomplete: limit.
                                report_error(context, parameter, "#embed-parameter 'limit' is currently unsupported.");
                                goto end;
                            }else{
                                report_error(context, parameter, "Unknown #embed-parameter.");
                                goto end;
                            }
                        }
                        
                        struct file *parent_file = globals.file_table.data[directive->file_index];
                        
                        if(is_system_include){
                            // @incomplete
                            report_error(context, directive, "<>-#embed currently not supported.");
                            goto end;
                        }
                        
                        // 
                        // Currently, we are only supporting ""-embed, and we are not caching the files.
                        // 
                        struct string path = strip_file_name(string_from_cstring(parent_file->absolute_file_path));
                        struct string absolute_file_path = canonicalize_slashes(concatenate_file_paths(&context->scratch, path, file_name));
                        struct os_file file = load_file_into_arena((char *)absolute_file_path.data, context->arena);
                        
                        if(file.file_does_not_exist){
                            report_error(context, directive, "File '%.*s' does not exist.", absolute_file_path.size, absolute_file_path.data);
                            goto end;
                        }
                        
                        smm embed_tokens = prefix.size + suffix.size + (file.size ? 1 : if_empty.size);
                        
                        while(emitted_tokens + embed_tokens >= committed_tokens){
                            if(committed_tokens == reserved_tokens){
                                report_error(context, directive, "Compilation exceeds current maximum amount of tokens per compilation unit (%lld).", reserved_tokens);
                                goto end;
                            }
                            
                            struct os_virtual_buffer committed = os_commit_memory(emitted_token_buffer + committed_tokens, TOKEN_EMIT_COMMIT_SIZE);
                            assert(committed.committed == TOKEN_EMIT_COMMIT_SIZE);
                            assert((struct token *)committed.base == emitted_token_buffer + committed_tokens);
                            
                            committed_tokens += TOKEN_EMIT_COMMIT_SIZE / sizeof(struct token);
                        }
                        
                        
                        if(file.size){
                            
                            for(smm index = 0; index < prefix.size; index++){
                                if(prefix.data[index].type == TOKEN_whitespace || prefix.data[index].type == TOKEN_comment) continue;
                                emitted_token_buffer[emitted_tokens++] = prefix.data[index];
                            }
                            
                            // @hmm: We could consider expanding this into the sequence specified
                            //       If the file size is small.
                            //       This would allow for most use cases for #embed outside of
                            //       character arrays, while also getting the speed-up for large files.
                            struct token *embed_token = &emitted_token_buffer[emitted_tokens++];
                            *embed_token = *directive;
                            embed_token->type = TOKEN_embed;
                            embed_token->string.data = file.data;
                            embed_token->string.size = file.size;
                            
                            for(smm index = 0; index < suffix.size; index++){
                                if(suffix.data[index].type == TOKEN_whitespace || suffix.data[index].type == TOKEN_comment) continue;
                                emitted_token_buffer[emitted_tokens++] = suffix.data[index];
                            }
                        }else{
                            for(smm index = 0; index < if_empty.size; index++){
                                if(if_empty.data[index].type == TOKEN_whitespace || if_empty.data[index].type == TOKEN_comment) continue;
                                emitted_token_buffer[emitted_tokens++] = if_empty.data[index];
                            }
                        }
                        
                    }else invalid_code_path;
                }break;
                case DIRECTIVE_error:{
                    // @note: this whole block is contained inside of an if that checks, that we are not
                    //        '#if 0'ed out.
                    struct string_list error_list = zero_struct;
                    while(!peek_token_raw(context, TOKEN_newline)){
                        struct string token_string = push_token_string(context, next_token_raw(context), true);
                        string_list_postfix(&error_list, &context->scratch, token_string);
                    }
                    
                    struct string flattened = string_list_flatten(error_list, &context->scratch);
                    
                    report_error(context, directive, "'#error': '%.*s'", flattened.size, flattened.data);
                    goto end;
                }break; 
                case DIRECTIVE_pragma:{
                    eat_whitespace_and_comments(context);
                    
                    struct token *pragma_directive = expect_token_raw(context, directive, TOKEN_identifier, "Expected a directive after '#pragma'.");
                    
                    if(atoms_match(pragma_directive->atom, globals.pragma_once)){
                        struct pragma_once_list_node *node = push_struct(&context->scratch, struct pragma_once_list_node);
                        node->file = globals.file_table.data[pragma_directive->file_index];
                        
                        sll_push_back(context->pragma_once_file_list, node);
                    }else if(atoms_match(pragma_directive->atom, globals.pragma_comment)){
                        
                        // 
                        // #pragma comment(<directive>, ...)
                        // 
                        
                        eat_whitespace_and_comments(context);
                        expect_token_raw(context, pragma_directive, TOKEN_open_paren, "Expected a '(' after '#pragma comment'.");
                        
                        eat_whitespace_and_comments(context);
                        struct token *pragma_comment_directive = expect_token_raw(context, pragma_directive, TOKEN_identifier, "Expected a directive after '#pragma comment('.");
                        
                        struct token *string_literal = null;
                        
                        eat_whitespace_and_comments(context);
                        if(peek_token_eat_raw(context, TOKEN_comma)){
                            
                            eat_whitespace_and_comments(context);
                            string_literal = expect_token_raw(context, pragma_comment_directive, TOKEN_string_literal, "Expected a string literal after '#pragma comment(lib, '.");
                            if(string_literal->type != TOKEN_string_literal) string_literal = null;
                            
                            eat_whitespace_and_comments(context);
                        }
                        
                        expect_token_raw(context, pragma_comment_directive, TOKEN_closed_paren, "Expected a ')' after '#pragma comment(<comment-type>, \"<comment>\"'.");
                        
                        
                        if(string_match(pragma_comment_directive->string, string("lib"))){
                            
                            // e.g.: #pragma comment(lib, "Shell32.lib")
                            
                            // peek_token_eat_raw(context, pragma_comment_directive, TOKEN_comma, "Expected a ',' after '#pragma comment(lib'.")
                            if(!string_literal){
                                report_error(context, pragma_comment_directive, "Expected a comment argument for #pragma comment(lib, \"<comment-argument>\").");
                            }else{
                                struct string library = strip_prefix_and_quotes(string_literal->string);
                                
                                if(!string_match(get_file_extension(library), string(".lib"))){
                                    library = string_concatenate(context->arena, library, string(".lib"));
                                }
                                
                                static struct ticket_spinlock pragma_comment_lib_spinlock = {0};
                                ticket_spinlock_lock(&pragma_comment_lib_spinlock);
                                
                                string_list_add_uniquely(&globals.specified_libraries, context->arena, library);
                                
                                ticket_spinlock_unlock(&pragma_comment_lib_spinlock);
                            }
                            
                        }else if(string_match(pragma_comment_directive->string, string("linker"))){
                            
                            if(!string_literal){
                                report_error(context, pragma_comment_directive, "Expected a comment argument for #pragma comment(linker, \"<comment-argument>\").");
                            }else{
                                struct string linker_line = strip_prefix_and_quotes(string_literal->string);
                                eat_whitespaces(&linker_line);
                                
                                struct string linker_switch = eat_until_char(&linker_line, ':', /*eat_delimiter*/1);
                                if(!string_match_case_insensitive(linker_switch, string("/ALTERNATENAME:"))){
                                    report_warning(context, WARNING_unsupported_pragma, string_literal, "Unsupported linker switch %.*s ignored.", linker_switch.size, linker_switch.data);
                                }else{
                                    eat_whitespaces(&linker_line);
                                    
                                    struct atom identifier = atom_for_string(eat_identifier(&linker_line));
                                    if(!linker_line.size || linker_line.data[0] != '='){
                                        report_error(context, string_literal, "Could not find '=' after '%.*s' in /ALTERNATENAME linker switch.", identifier.size, identifier.data);
                                    }else{
                                        string_eat_front(&linker_line, 1);
                                        
                                        struct atom alias = atom_for_string(eat_identifier(&linker_line));
                                        if(linker_line.size){
                                            report_warning(context, WARNING_unsupported_pragma, string_literal, "Junk '%.*s' after linker switch %.*s%.*s=%.*s ignored.", linker_line.size, linker_line.data, linker_switch.size, linker_switch.data, identifier.size, identifier.data, alias.size, alias.data);
                                        }
                                        
                                        static struct ticket_spinlock pragma_comment_linker_spinlock = {0};
                                        ticket_spinlock_lock(&pragma_comment_linker_spinlock);
                                        
                                        struct alternate_name *name = globals.alternate_names.first;
                                        for(; name; name = name->next){
                                            if(atoms_match(name->source, identifier) && atoms_match(name->destination, alias)){
                                                break;
                                            }
                                        }
                                        
                                        if(!name){
                                            name = push_struct(context->arena, struct alternate_name);
                                            name->token = string_literal;
                                            name->source = identifier;
                                            name->destination = alias;
                                            
                                            sll_push_back(globals.alternate_names, name);
                                        }
                                        
                                        ticket_spinlock_unlock(&pragma_comment_linker_spinlock);
                                    }
                                }
                            }
                        }else{
                            report_warning(context, WARNING_unsupported_pragma, pragma_directive, "Unsupported '#pragma comment(%.*s, ...)' ignored.", pragma_comment_directive->string.size, pragma_comment_directive->string.data);
                        }
                    }else if(atoms_match(pragma_directive->atom, globals.pragma_compilation_unit)){
                        // 
                        // #pragma compilation_unit("cfile.c")
                        // 
                        
                        eat_whitespace_and_comments(context);
                        expect_token_raw(context, pragma_directive, TOKEN_open_paren, "Expected a '(' after '#pragma compilation_unit'.");
                        
                        eat_whitespace_and_comments(context);
                        struct token *string_literal = expect_token_raw(context, pragma_directive, TOKEN_string_literal, "Expected a string literal after '#pragma compilation_unit('.");
                        
                        eat_whitespace_and_comments(context);
                        expect_token_raw(context, pragma_directive, TOKEN_closed_paren, "Expected a ')' after '#pragma compilation_unit(\"<.c>\"'.");
                        
                        if(string_literal->type == TOKEN_string_literal){
                            struct string c_file = strip_prefix_and_quotes(string_literal->string);
                            
                            struct file *parent_file = globals.file_table.data[string_literal->file_index];
                            struct string path = strip_file_name(string_from_cstring(parent_file->absolute_file_path));
                            struct string absolute_file_path = canonicalize_slashes(concatenate_file_paths(&context->scratch, path, c_file));
                            
                            static struct ticket_spinlock pragma_compilation_unit_spinlock = {0};
                            ticket_spinlock_lock(&pragma_compilation_unit_spinlock);
                            
                            int found = 0;
                            for(struct string_list_node *list_node = globals.pragma_compilation_units.list.first; list_node; list_node = list_node->next){
                                if(string_match_case_insensitive(list_node->string, absolute_file_path)){
                                    found = 1;
                                    break;
                                }
                            }
                            
                            if(!found){
                                
                                struct os_file dummy = os_load_file((char *)absolute_file_path.data, 0, 0);
                                if(dummy.file_does_not_exist){
                                    report_error(context, pragma_directive, "File '%.*s' does not exist.", absolute_file_path.size, absolute_file_path.data);
                                }else{
                                    string_list_postfix(&globals.pragma_compilation_units, context->arena, absolute_file_path); // @note: This copies.
                                    
                                    // 
                                    // Allocate a new compilation unit.
                                    // @cleanup: Maybe I should make this a routine at this point...
                                    // 
                                    struct compilation_unit *compilation_unit = push_struct(context->arena, struct compilation_unit);
                                    compilation_unit->index = globals.compilation_units.last->index + 1; // @cleanup: are we sure 'compilation_unit->last' exists?
                                    compilation_unit->static_declaration_table = ast_table_create(128);
                                    compilation_unit->static_sleeper_table = sleeper_table_create(1 << 8);
                                    compilation_unit->is_token_static_table.capacity = 0x100;
                                    compilation_unit->is_token_static_table.size = 0;
                                    compilation_unit->is_token_static_table.data = push_data(context->arena, struct is_token_static, 0x100);
                                    
                                    globals.compilation_units.last->next = compilation_unit;
                                    globals.compilation_units.last = compilation_unit;
                                    
                                    
                                    // 
                                    // This feels somewhat dumb.
                                    // 
                                    struct work_tokenize_file *work = push_struct(context->arena, struct work_tokenize_file);
                                    work->absolute_file_path = push_zero_terminated_string_copy(context->arena, absolute_file_path);
                                    work->file_size = dummy.size;
                                    work->compilation_unit = compilation_unit;
                                    
                                    struct work_queue_entry *work_entry = push_struct(context->arena, struct work_queue_entry);
                                    work_entry->data  = work;
                                    work_queue_add(&globals.work_queue_tokenize_files, work_entry);
                                }
                            }
                            
                            ticket_spinlock_unlock(&pragma_compilation_unit_spinlock);
                        }
                        
                    }else{
                        while(!peek_token_raw(context, TOKEN_newline)){
                            next_token_raw(context);
                        }
                        
                        report_warning(context, WARNING_unsupported_pragma, pragma_directive, "Unsupported pragma ignored.");
                    }
                    
                }break;
                case DIRECTIVE_line:{
                    report_warning(context, WARNING_unsupported_pragma, directive, "#line currently not supported.");
                }break;
            }
            
            // We get here for any active directive. Report on junk!
            eat_whitespace_and_comments(context);
            if(!peek_token_raw(context, TOKEN_newline)){
                report_warning(context, WARNING_junk_after_directive, get_current_token_raw(context), "Junk after '#%.*s'.", directive_string.size, directive_string.data);
            }
            
            while(!peek_token_raw(context, TOKEN_newline)){
                next_token_raw(context);
            }
            
            end_counter(context, handle_directive);
            continue;
        }
        
        assert(!static_if_stack.first || static_if_stack.first->is_true);
        
        // Expand and emit tokens until we are not in a macro expansion anymore
        struct token *macro_expansion_site = null;
        do{
            struct token *token = next_token_raw(context);
            
            if(token->type == TOKEN_identifier){
                struct define_node *define = lookup_define(context, token->atom);
                if(define && !define->is_disabled){
                    
                    int skip = 0;
                    if(define->is_function_like){
                        eat_whitespace_comments_and_newlines(context); 
                        if(!peek_token_raw(context, TOKEN_open_paren)){
                            skip = 1;
                        }
                    }
                    
                    if(!skip){
                        if(!macro_expansion_site) macro_expansion_site = token;
                        token = expand_define(context, token, define, macro_expansion_site);
                        
                        // We will only ever return a token for predefined identifiers.
                        // Otherwise, we will expand the macro push the resulting tokens to the stack for rescanning 
                        // and then return null.
                        if(!token) continue;
                    }
                }
            }
            
            if(token->type == TOKEN_invalid) break; // @note: Only happens, if we are at the end of the token array, and the define expanded to nothing.
            
            assert(TOKEN_invalid < token->type && token->type < TOKEN_count);
            
            if(token->type == TOKEN_whitespace) continue;
            if(token->type == TOKEN_comment)    continue;
            if(token->type == TOKEN_newline){
                got_newline = true;
                
                if(have_postponed_directive && !context->define_depth){
                    //
                    // Handle the postponed directive!
                    // This can be either and '#include' or '#if' or '#elif'.
                    // The tokens for the directives are 
                    //     [current_directive->saved_emitted_token_count, emitted_tokens).
                    // Load these and then evaluate the directive.
                    //
                    struct token_array token_array = {
                        .data = emitted_token_buffer + postponed_directive.saved_emitted_token_count,
                        .size = emitted_tokens       - postponed_directive.saved_emitted_token_count,
                    };
                    assert(token_array.amount >= 0);
                    
                    if(!token_array.amount){
                        report_error(context, postponed_directive.directive, "Expected argument for directive.");
                        goto end;
                    }
                    
                    begin_token_array(context, token_array);
                    
                    //
                    // We now use the usual (non-raw) version of *_token procedures to evaluate the directive.
                    //
                    
                    // :Error Print what the define expanded to.
                    
                    if(postponed_directive.directive_kind == DIRECTIVE_include){
                        // 
                        // @copy_and_paste from the non-expanded case.
                        // 
                        struct string file_name = zero_struct;
                        int is_system_include = false;
                        
                        if(peek_token(context, TOKEN_string_literal)){
                            is_system_include = false;
                            
                            file_name = strip_prefix_and_quotes(token_get_string(next_token(context)));
                        }else if(peek_token(context, TOKEN_smaller)){
                            is_system_include = true;
                            
                            struct token *smaller = next_token(context);
                            
                            struct string_list include_string = zero_struct;
                            
                            while(!peek_token(context, TOKEN_bigger) && !peek_token(context, TOKEN_invalid)){
                                struct string token_string = push_token_string(context, next_token(context), true);
                                string_list_postfix(&include_string, &context->scratch, token_string);
                            }
                            
                            if(peek_token(context, TOKEN_invalid)){
                                report_error(context, smaller, "Newline in <>-include.");
                                goto end;
                            }
                            
                            struct token *bigger = next_token(context);
                            
                            file_name = string_list_flatten(include_string, &context->scratch);
                            
                            assert(smaller->type == TOKEN_smaller && smaller->data);
                            assert(bigger->type  == TOKEN_bigger  && bigger->data);
                            
                        }else{
                            report_error(context, postponed_directive.directive, "Expected either \"\"-include or <>-include.");
                            goto end;
                        }
                        
                        int should_error = handle_include_directive(context, postponed_directive.directive, is_system_include, file_name);
                        if(should_error) goto end;
                    }else{
                        static_if_stack.first->is_true = static_if_evaluate(context, postponed_directive.directive) != 0;
                        if(context->should_exit_statement) goto end;
                    }
                    
                    
                    //
                    // We have completed the 'postponed_directive', _delete_ the tokens from the array, 
                    // by reseting the 'emitted_tokens' back to where the 'postponed_directive' started.
                    //
                    have_postponed_directive = false;
                    emitted_tokens = postponed_directive.saved_emitted_token_count;
                    context->in_static_if_condition = 0;
                }
                
                continue;
            }
            
            got_newline = false;
            
            if(emitted_tokens == committed_tokens){
                if(committed_tokens == reserved_tokens){
                    report_error(context, token, "Compilation exceeds current maximum amount of tokens per compilation unit (%lld).", reserved_tokens);
                    goto end;
                }
                
                struct os_virtual_buffer committed = os_commit_memory(emitted_token_buffer + committed_tokens, TOKEN_EMIT_COMMIT_SIZE);
                assert(committed.committed == TOKEN_EMIT_COMMIT_SIZE);
                assert((struct token *)committed.base == emitted_token_buffer + committed_tokens);
                
                committed_tokens += TOKEN_EMIT_COMMIT_SIZE / sizeof(struct token);
            }
            
            assert(emitted_tokens < committed_tokens);
            struct token *out_token = emitted_token_buffer + emitted_tokens++;
            if(token->type == TOKEN_identifier_dont_expand_because_it_comes_from_a_fully_expanded_macro){
                out_token->type = TOKEN_identifier;
            }else{
                out_token->type = token->type;
            }
            
            // If we expanded this token from a macro, copy the location :copy_expanded_location
            if(context->macro_expansion_token){
                out_token->line   = context->macro_expansion_token->line;
                out_token->column = context->macro_expansion_token->column;
                out_token->file_index   = context->macro_expansion_token->file_index;
            }else{
                out_token->column = token->column;
                out_token->line   = token->line;
                out_token->file_index   = token->file_index;
            }
            
            out_token->data = token->data;
            out_token->size = token->size;
            
            if(out_token->type == TOKEN_identifier){
                out_token->string_hash = token->string_hash;
                
                // @note: Do not apply keywords if we 'have_postponed_directive', because otherwise 
                //        things like defined(__int64) will error.
                if(!have_postponed_directive){
                    u64 index = token->string_hash & (globals.keyword_table_size - 1);
                    if(atoms_match(globals.keyword_table[index].keyword, out_token->atom)){
                        out_token->type = globals.keyword_table[index].type;
                    }
                } 
            }
        } while(context->define_depth > 0);
    }
    
    // :newline_at_the_end_of_the_file
    assert(context->error || !have_postponed_directive);
    
    if(static_if_stack.first){
        struct token *token = static_if_stack.first->token;
        report_error(context, token, "'#%.*s' without matching '#endif'.", token->size, token->data);
    }
    
    end:;
    
    return (struct token_array){ .data = emitted_token_buffer, .size = emitted_tokens };
}

