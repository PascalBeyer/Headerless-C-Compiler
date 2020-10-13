

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
// C-preprocessor is really hacky.                              -11.09.2020

////////////////////////////////////////////////////////////////////////////

func void begin_token_array(struct context *context, struct token_bucket_array array, smm start_at){
    context->tokens = array;
    context->current_token_bucket = array.first;
    
    //assert(start_at < context->current_token_bucket->amount);
    while(context->current_token_bucket && start_at >= context->current_token_bucket->amount){
        start_at = 0;
        context->current_token_bucket = context->current_token_bucket->next;
    }
    
    context->token_at = start_at;
}

func struct token *get_current_token(struct context *context){
    if(context->current_token_bucket == null) return &context->invalid_token;
    assert(context->token_at <= context->current_token_bucket->amount);
    return context->current_token_bucket->tokens + context->token_at;
}

func struct token *next_token_raw(struct context *context){
    struct token *ret = get_current_token(context);
    if(context->current_token_bucket){
        if(context->token_at + 1 >= context->current_token_bucket->amount){
            context->token_at = 0;
            context->current_token_bucket = context->current_token_bucket->next;
            assert(!context->current_token_bucket || context->current_token_bucket->amount > 0);
        }else{
            context->token_at++;
        }
    }
    return ret;
}

func struct token *next_token(struct context *context){
    struct token *ret = get_current_token(context);
    
    while(context->current_token_bucket){
        next_token_raw(context);
        struct token *token = get_current_token(context);
        if(token->type == TOKEN_whitespace) continue;
        if(token->type == TOKEN_comment)    continue;
        if(token->type == TOKEN_newline)    continue;
        break;
    }
    
    return ret;
}


func struct token *prev_token_raw(struct context *context){
    if(!context->current_token_bucket){
        context->current_token_bucket = context->tokens.last;
        context->token_at = context->tokens.last->amount - 1;
    }else{
        if(context->token_at - 1 < 0){
            if(!context->current_token_bucket->prev) return &context->invalid_token; // @cleanup: needed for the gross, define expasion hack, where we go back to the last significant token.
            context->current_token_bucket = context->current_token_bucket->prev;
            assert(context->current_token_bucket->amount > 0);
            context->token_at = context->current_token_bucket->amount - 1;
        }else{
            context->token_at--;
        }
    }
    
    return get_current_token(context);
}

func struct token *prev_token(struct context *context){
    while(true){
        prev_token_raw(context);
        struct token *token = get_current_token(context);
        if(token->type == TOKEN_whitespace) continue;
        if(token->type == TOKEN_comment)    continue;
        if(token->type == TOKEN_newline)    continue;
        break;
    }
    
    struct token *ret = get_current_token(context);
    return ret;
}

func struct token *expect_token(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type != type){
        struct string expected = token_type_to_string(type);
        struct string gotten   = token_type_to_string(token->type);
        // :Error this could be alot better
        report_error(context, token, "Expected '%.*s', got '%.*s'.", expected.size, expected.data, gotten.size, gotten.data);
        return token;
    }
    return next_token(context);
}

func struct token *expect_token_raw(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type != type){
        struct string expected = token_type_to_string(type);
        struct string gotten   = token_type_to_string(token->type);
        // :Error this could be alot better
        report_error(context, token, "Expected '%.*s', got '%.*s'.", expected.size, expected.data, gotten.size, gotten.data);
        return token;
    }
    return next_token_raw(context);
}

func struct token *peek_token(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type == type){
        return get_current_token(context);
    }
    return null;
}

func struct token *peek_token_eat(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type == type){
        return next_token(context);
    }
    return null;
}

func struct token *peek_token_eat_raw(struct context *context, enum token_type type){
    struct token *token = get_current_token(context);
    if(token->type == type){
        return next_token_raw(context);
    }
    return null;
}

func b32 in_current_token_array(struct context *context){
    return (context->current_token_bucket != null);
}



func b32 skip_until_tokens_are_ballanced(struct context *context, enum token_type open, enum token_type closed){
    u64 count = 1;
    while(true){
        if(!in_current_token_array(context)){
            struct string end_token = token_type_to_string(closed);
            // :Error this should report the beginning and the end
            report_error(context, prev_token(context), "Invalid token in scope. Maybe you missed a '%.*s'.", end_token.amount, end_token.data);
            return false;
        }else if(peek_token(context, closed)){
            if (--count == 0) break;
        }else if(peek_token(context, open)){
            count++;
        }
        next_token(context);
    }
    next_token(context);
    return true;
}

struct token_array{
    struct token *data;
    union{
        smm amount;
        smm size;
        smm count;
    };
};

////////////////////////////////////////////////////////////////////////////////////

struct define_argument{
    struct define_argument *next;
    unique_string argument;
    smm argument_index;
};

struct define_argument_list{
    struct define_argument *first;
    struct define_argument *last;
    smm count;
};

struct define_list{
    struct define_node *first;
    struct define_node *last;
};

struct define_node{
    struct define_node *next;
    struct define_node *prev;
    struct token *defined_token;
    struct token_array tokens; // list of tokens that replace the define
    unique_string name;
    b32 is_function_like;
    b32 is_varargs;
    b32 is_disabled; // disabled if we are within its expansion
    struct define_argument_list arguments;
};


func void print_token(struct context *context, struct token *token, b32 print_whitespace_and_comments);
func void print_define(struct context *context, struct define_node *define, char *prefix){
    print("%s %.*s", prefix, define->name->size, define->name->data);
    if(define->is_function_like){
        print("(");
        
        for(struct define_argument *arg = define->arguments.first; arg; arg = arg->next){
            print("%.*s", arg->argument->size, arg->argument->data);
            if(arg->next){
                print(", ", arg->argument->size, arg->argument->data);
            }
        }
        
        print(")");
    }
    print(" ");
    for(smm i = 0; i < define->tokens.amount; i++){
        print_token(context, define->tokens.data + i, true);
    }
    print("\n");
}

func void register_define(struct context *context, struct define_node *new_node){
    
#ifdef DEFINE_TO_PRINT
    if(string_match(*new_node->name, string(DEFINE_TO_PRINT))){
        print_define(context, new_node, "#define ");
    }
#endif
    
    if(2 * context->define_table.amount + 1 > context->define_table.capacity){
        u64 new_capacity = 2 * max_of(context->define_table.capacity, 256);
        u64 new_mask = new_capacity - 1;
        struct define_list *new_lists = push_zero_data(&context->scratch, struct define_list, new_capacity);
        
        for(u64 i = 0; i < context->define_table.capacity; i++){
            struct define_list list = context->define_table.lists[i];
            for(struct define_node *node = list.first; node;){
                // save 'next' because we are gonna clober it in just a second
                struct define_node *next = node->next;
                dll_remove(list, node); // not sure if this is neccessary
                
                // insert 'node' into the new table
                u64 hash  = xor_shift64((u64)node->name);
                u64 index = hash & new_mask;
                dll_push_back(new_lists[index], node);
                node = next;
            }
        }
        
        context->define_table.capacity = new_capacity;
        context->define_table.lists    = new_lists;
        context->define_table.mask     = new_mask;
        context->define_table.amount   = context->define_table.amount;
    }
    
    // inser the node into the table
    u64 hash = xor_shift64((u64)new_node->name);
    u64 index = hash & context->define_table.mask;
    struct define_list *at = context->define_table.lists + (index & context->define_table.mask);
    
    dll_push_front(*at, new_node);
    context->define_table.amount += 1;
    
}

func struct define_node *lookup_define(struct context *context, unique_string name){
    struct define_node *define = null;
    
    u64 hash = xor_shift64((u64)name);
    u64 index = hash & context->define_table.mask;
    struct define_list *at = context->define_table.lists + (index & context->define_table.mask);
    
    if(!dll_is_empty(*at)){
        for(struct define_node *node = at->first; node; node = node->next){
            if(node->name == name){
                define = node;
                break;
            }
        }
    }
    
    if(define && define->is_disabled) return null;
    
    return define;
    
    /*
    
    // @speed @yuck @clenup: we check every define for every identifier    
    struct define_node *define = null;
    for(struct define_node *node = context->define_list.first; node; node = node->next){
        if(node->hash == name){
            define = node;
            break;
        }
    }
    end_counter(lookup_define);
    return define;*/
}


func void validify_bucket_array(struct token_bucket_array array){
#if DEBUGGING_TOKEN_BUCKET_ARRAYS
    for(struct token_bucket *it = array.first; it; it = it->next){
        assert(!it->next || it->next->prev == it);
        assert(!it->prev || it->prev->next == it);
        assert(it->next  || it == array.last);
        assert(it->prev  || it == array.first);
    }
#else
    (void)array;
#endif
}

///////////////////////////////////////////////////////////////////////////////////
struct token_marker{
    struct token_bucket *bucket;
    smm token_at;
};

struct token_marker make_token_marker(struct token_bucket *bucket, smm token_at){
    return (struct token_marker){bucket, token_at};
}

func struct token_marker get_current_token_marker(struct context *context){
    struct token_marker ret;
    ret.bucket   = context->current_token_bucket;
    ret.token_at = context->token_at;
    return ret;
}

func void apply_token_marker(struct context *context, struct token_marker marker){
    // @cleanup: is there a good assert here.. I mean we could linearly search for it.
    context->current_token_bucket = marker.bucket;
    context->token_at = marker.token_at;
}

func struct token_bucket *replace_token_range__internal(struct memory_arena *arena, struct token_bucket_array *tokens, struct token_marker begin, struct token_marker end, struct token_bucket_array replace_with){    
    // Things to do:
    // 1) truncate end or 'if(end.bucket == begin.bucket)' push a new one that is the end of 'end.bucket'
    // 2) truncate 'begin.bucket'
    // 3) link: 'begin.bucket'-> 'replace_with.first' ... 'replace_with.last' -> 'end.bucket'
    // 4) maybe adjust 'tokens->first', 'tokens->last'
    
#if DEBUGGING_TOKEN_BUCKET_ARRAYS
    {
        smm found_begin = 0;
        smm found_end   = 0;
        if(end.bucket == null)   found_end++;
        if(begin.bucket == null) found_end++;
        
        for(struct token_bucket *bucket = tokens->first; bucket; bucket = bucket->next){
            if(bucket == begin.bucket) found_begin++;
            if(bucket == end.bucket)   found_end++;
        }
        assert(found_begin == 1);
        assert(found_end   == 1);
    }
    
    {
        for(struct token_bucket *bucket = replace_with.first; bucket; bucket = bucket->next){
            assert(bucket->amount);
        }
    }
#endif
    
    if(!begin.bucket){
        // we want to append 'replace_with' to 'tokens'
        assert(!end.bucket);
        dll_push_back_list(*tokens, replace_with);
        return null;
    }
    
    if(begin.token_at == 0){
        begin.bucket = begin.bucket->prev;
        if(begin.bucket) begin.token_at = begin.bucket->amount;
    }
    
    if(!end.bucket){
        if(!begin.bucket){
            // we want to replace the entire range of tokens with 'replace_with'
            *tokens = replace_with;
            return null;
        }
        
        // we want to delete the end and append 'replace_with'
        begin.bucket->amount = begin.token_at;
        tokens->last = begin.bucket;
        tokens->last->next = null;
        
        dll_push_back_list(*tokens, replace_with);
        return null;
    }
    
    assert(end.token_at < end.bucket->amount);
    assert(!begin.bucket || begin.token_at <= begin.bucket->amount);
    
    // 1) truncate 'end.bucket' or push a new one.
    if(end.bucket == begin.bucket){
        struct token_bucket *new_end = push_struct(arena, struct token_bucket);
        new_end->tokens = end.bucket->tokens + end.token_at;
        new_end->amount = end.bucket->amount - end.token_at;
        
        new_end->next = end.bucket->next;
        end.bucket = new_end;
        end.token_at = 0;
    }else{
        end.bucket->tokens += end.token_at;
        end.bucket->amount -= end.token_at;
        end.token_at = 0;
    }
    
    // 2) truncate 'begin.bucket'
    if(begin.bucket){
        begin.bucket->amount = begin.token_at;
    }
    
    if(sll_is_empty(replace_with)){
        if(begin.bucket){
            begin.bucket->next = end.bucket;
        }else{
            tokens->first = end.bucket;
        }
        
        end.bucket->prev = begin.bucket;
    }else{
        // 3) link up
        if(begin.bucket){
            begin.bucket->next = replace_with.first;
        }else{
            tokens->first = replace_with.first;
        }
        replace_with.first->prev = begin.bucket;
        end.bucket->prev = replace_with.last;
        replace_with.last->next = end.bucket;
    }
    
    // 4) link up 'end.bucket' in case we have replaced it
    if(end.bucket->next){
        end.bucket->next->prev = end.bucket;
    }else{
        tokens->last = end.bucket;
    }
    
    return end.bucket;
}


// returns the bucket _past_ 'replace_with'
func struct token_bucket *replace_token_range(struct context *context, struct token_marker begin, struct token_marker end, struct token_bucket_array replace_with){
    // 'arena' as things have to be alive during the whole duration of the program
    return replace_token_range__internal(context->arena, &context->tokens, begin, end, replace_with);
}

///////////////////////////////////////////////////////////////////////////////////
func void next_token__internal(struct token *ret, struct file_stack_node *t, enum token_type type, smm size){
    ret->file = t;
    ret->column = cast(u32)t->column;
    ret->type = type;
    ret->line = cast(u32)t->line;
    
    t->column += size;
}

func void next_token__internal_char(struct token *ret, struct file_stack_node *t, enum token_type type, smm size){
    next_token__internal(ret, t, type, size);
    t->at += size;
}


func void next_token__internal_string_no_intern(struct token *ret,  struct file_stack_node *t, enum token_type type, smm size, u8 *start){
    next_token__internal(ret, t, type, size);
    ret->string = create_string(start, size);
}
///////////////////////////////////////////////////////////////////////////////////////

// return buffer is in 'context->scratch'
func struct string handle_escaping(struct context *context, struct file_stack_node *file, u8 terminator){
    assert(*file->at == terminator);
    *file->at++;
    smm buffer_size = 1024;
    u8 *buf = push_data(&context->scratch, u8, 1024);
    
    smm buf_at = 0;
    
#define handle_escaping_out(c)\
    buf[buf_at++] = c;\
    if(buf_at == buffer_size){\
        u8 *old_buf = buf;\
        buffer_size *= 2;\
        buf = push_data(&context->scratch, u8, buffer_size);\
        memcpy(buf, old_buf, buf_at);\
    }
    
    u8 *column_start = file->at;
    while((file->at < file->end) && *file->at != terminator){
        if(*file->at != '\\'){
            handle_escaping_out(*file->at);
            if(*file->at == '\n'){
                column_start = file->at;
                file->column = 1;
                file->line++;
            }
            
            file->at++;
            continue;
        }
        if(++file->at >= file->end) break;
        switch(*file->at++){
            case 'n':  handle_escaping_out('\n'); break;
            case 'b':  handle_escaping_out('\b'); break;
            case 'v':  handle_escaping_out('\v'); break;
            case 't':  handle_escaping_out('\t'); break;
            case 'f':  handle_escaping_out('\f'); break;
            case 'r':  handle_escaping_out('\r'); break;
            // @hmm: msvc just consumes all digits as 'int' and then has like a value check, i.e '\x1000000001' is not an error and evaluates to '1'
            case '0': case '1': case '2': case '3': case '4':case '5':case '6':case '7':{
                // octal constant
                file->at -= 1; // go back to the first integer
                u32 value = 0; 
                u32 i = 0; 
                for(;i < 3; i++){
                    int v = (int)(*file->at);
                    if('0' <= v && v <= '7'){
                        v -= '0';
                    }else{
                        break;
                    }
                    value = 8 * value + v;
                    file->at++;
                }
                
                if(i == 0){
                    struct token cur = zero_struct;
                    next_token__internal_string_no_intern(&cur, file, TOKEN_invalid, 2, file->at - 1);
                    report_error(context, &cur, "Expected at least one diget in octal constant."); //:Error
                }else{
                    if(value > 255){
                        struct token cur = zero_struct;
                        next_token__internal_string_no_intern(&cur, file, TOKEN_invalid, 2, file->at - 1);
                        report_error(context, &cur, "Constant to big for character."); //:Error
                    }
                    handle_escaping_out((u8)value);
                }
            }break;
            case 'x':{
                // hex constant
                u32 value = 0;
                u32 i = 0;
                for(;i < 2; i++){
                    int v = (int)(*file->at);
                    if('a' <= v && v <= 'f'){
                        v += (10 - 'a');
                    }else if('A' <= v && v <= 'F'){
                        v += 10 - 'A';
                    }else if('0' <= v && v <= '9'){
                        v -= '0';
                    }else{
                        break;
                    }
                    value = 16 * value + v;
                    file->at++;
                }
                
                if(i == 0){
                    struct token cur = zero_struct;
                    next_token__internal_string_no_intern(&cur, file, TOKEN_invalid, 2, file->at - 1);
                    report_error(context, &cur, "Expected at least one diget after '\\x'."); //:Error
                }else{
                    handle_escaping_out((u8)value);
                }
            }break;
            case '\\': handle_escaping_out('\\'); break;
            case '\'': handle_escaping_out('\''); break;
            case '\"': handle_escaping_out('\"'); break;
            case '\n': {
                handle_escaping_out('\n');
                file->column = 1;
                file->line++;
            }break;
            default:{
                struct token cur = zero_struct;
                next_token__internal_string_no_intern(&cur, file, TOKEN_invalid, 2, file->at - 1);
                report_error(context, &cur, "Unknown escape sequence.");
            }break;
        }
    }
    
#undef handle_escaping_out
    if(file->at >= file->end){
        struct token cur = zero_struct;
        next_token__internal_string_no_intern(&cur, file, TOKEN_invalid, buf_at, buf);
        report_error(context, &cur, "File ends in string literal.");
    }else{
        file->at++;
    }
    
    file->column += file->at - column_start;
    return create_string(buf, buf_at);
}

// @cleanup: maybe use the |32 trick here?
func int tokenizer_parse_number_postfix(struct file_stack_node *file){
    int kind;
    if(file->at[0] == 'l' || file->at[0] == 'L'){
        if(file->at[1] == file->at[0]){
            if(file->at[2] == 'u' || file->at[2] == 'U'){
                // llu, llU, LLu, LLU
                kind = NUMBER_KIND_unsigned_long_long;
                file->at += 3;
            }else{
                // ll, LL
                kind = NUMBER_KIND_long_long;
                file->at += 2;
            }
        }else if(file->at[1] == 'u' || file->at[1] == 'U'){
            // lu, lU, LU
            kind = NUMBER_KIND_unsigned_long;
            file->at += 2;
        }else{
            // l, L
            kind = NUMBER_KIND_long;
            file->at += 1;
        }
    }else if(file->at[0] == 'u' || file->at[0] == 'U'){
        if(file->at[1] == 'l' || file->at[1] == 'L'){
            if(file->at[2] == file->at[1]){
                // ull, Ull, uLL, ULL
                kind = NUMBER_KIND_unsigned_long_long;
                file->at += 3;
            }else{
                // ul, uL, Ul, UL
                kind = NUMBER_KIND_unsigned_long;
                file->at += 2;
            }
        }else{
            // u, U
            kind = NUMBER_KIND_unsigned;
            file->at += 1;
        }
    }else{
        kind = NUMBER_KIND_int;
    }
    return kind;
}

// used by 'tokenize_file_raw' and as a hack when tokenizing after concatination with '##' in a define
func struct token tokenize_one_token_raw(struct context *context, struct file_stack_node *file){
    struct token cur = zero_struct;
    
    switch (*file->at){
        case '\0':{
            next_token__internal_char(&cur, file, TOKEN_invalid, 1);
            report_error(context, &cur, "Null byte in file.");
        }break;
        case '\\':{
            // @hmm: this should possible happen in the preprocessor code...
            u8 *start = file->at++;
            while(u8_is_whitespace(*file->at)) file->at++;
            b32 got_newline = false;
            if(*file->at == '\n'){
                file->at++;
                if(file->at[0] == '\r') file->at++;
                got_newline = true;
            }else if(*file->at == '\r'){
                file->at++;
                if(file->at[0] == '\n') file->at++;
                got_newline = true;
            }
            u8 *end = file->at;
            
            next_token__internal_string_no_intern(&cur, file, TOKEN_whitespace, (end - start), start);
            if(got_newline){
                file->line++;
            }else{
                report_warning(context, &cur, "No newline after '\\'.");
            }
        }break;
        case '\v': case '\t': case '\f': case ' ': {
            u8 *start = file->at;
            while(u8_is_whitespace(*++file->at));
            u8 *end = file->at;
            next_token__internal_string_no_intern(&cur, file, TOKEN_whitespace, (end - start), start);
        }break;
        case '\n':{
            if(file->at[1] == '\r'){
                next_token__internal_char(&cur, file, TOKEN_newline, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_newline, 1);
            }
            file->line++;
            file->column = 1;
        }break;
        case '\r':{
            if(file->at[1] == '\n'){
                next_token__internal_char(&cur, file, TOKEN_newline, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_newline, 1);
            }
            file->line++;
            file->column = 1;
        }break;
        case '@':{
            next_token__internal_char(&cur, file, TOKEN_at_sign, 1);
        }break;
        case '#':{
            if(file->at[1] == '#'){
                next_token__internal_char(&cur, file, TOKEN_hashhash, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_hash, 1);
            }
        }break;
        case '=':{
            if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_logical_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_equals, 1);
            }
        }break;
        case '(':{
            next_token__internal_char(&cur, file, TOKEN_open_paren, 1);
        }break;
        case ')':{
            next_token__internal_char(&cur, file, TOKEN_closed_paren, 1);
        }break;
        case '{':{
            next_token__internal_char(&cur, file, TOKEN_open_curly, 1);
        }break;
        case '}':{
            next_token__internal_char(&cur, file, TOKEN_closed_curly, 1);
        }break;
        case '[':{
            next_token__internal_char(&cur, file, TOKEN_open_index, 1);
        }break;
        case ']':{
            next_token__internal_char(&cur, file, TOKEN_closed_index, 1);
        }break;
        case ';':{
            next_token__internal_char(&cur, file, TOKEN_semicolon, 1);
        }break;
        case ':':{
            next_token__internal_char(&cur, file, TOKEN_colon, 1);
        }break;
        case ',':{
            next_token__internal_char(&cur, file, TOKEN_comma, 1);
        }break;
        case '.':{
            if(file->at[1] == '.' && file->at[2] == '.'){
                next_token__internal_char(&cur, file, TOKEN_dotdotdot, 3);
            }else if(u8_is_number(file->at[1])){
                goto handle_numbers;
            }else{
                next_token__internal_char(&cur, file, TOKEN_dot, 1);
            }
        }break;
        case '>':{
            if(file->at[1] == '>'){
                if(file->at[2] == '='){
                    next_token__internal_char(&cur, file, TOKEN_right_shift_equals, 3);
                }else{
                    next_token__internal_char(&cur, file, TOKEN_right_shift, 2);
                }
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_bigger_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_bigger, 1);
                cur.string.data = file->at - 1; // :smaller_bigger_and_system_includes
            }
        }break;
        case '<':{
            if(file->at[1] == '<'){
                if(file->at[2] == '='){
                    next_token__internal_char(&cur, file, TOKEN_left_shift_equals, 3);
                }else{
                    next_token__internal_char(&cur, file, TOKEN_left_shift, 2);
                }
                
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_smaller_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_smaller, 1);
                cur.string.data = file->at; // :smaller_bigger_and_system_includes
            }
        }break;
        case '~':{
            next_token__internal_char(&cur, file, TOKEN_bitwise_not, 1);
        }break;
        case '!':{
            if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_logical_unequals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_logical_not, 1);
            }
        }break;
        case '?':{
            next_token__internal_char(&cur, file, TOKEN_question_mark, 1);
        }break;
        case '+':{
            if(file->at[1] == '+'){
                next_token__internal_char(&cur, file, TOKEN_increment, 2);
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_plus_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_plus, 1);
            }
        }break;
        case '-':{
            if(file->at[1] == '-'){
                next_token__internal_char(&cur, file, TOKEN_decrement, 2);
            }else if(file->at[1] == '>'){
                next_token__internal_char(&cur, file, TOKEN_arrow, 2);
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_minus_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_minus, 1);
            }
        }break;
        case '&':{
            if(file->at[1] == '&'){
                next_token__internal_char(&cur, file, TOKEN_logical_and, 2);
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_and_equals, 2);
            }else {
                next_token__internal_char(&cur, file, TOKEN_and, 1);
            }
        }break;
        case '|':{
            if(file->at[1] == '|'){
                next_token__internal_char(&cur, file, TOKEN_logical_or, 2);
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_or_equals, 2);
            }else {
                next_token__internal_char(&cur, file, TOKEN_or, 1);
            }
        }break;
        case '^':{
            if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_xor_equals, 2);
            }else {
                next_token__internal_char(&cur, file, TOKEN_xor, 1);
            }
        }break;
        case '*':{
            if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_times_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_times, 1);
            }
        }break;
        case '/':{
            if(file->at[1] == '*'){
                u8 *start = file->at;
                file->at += 2;
                u32 comment_depth = 1;
                while(*file->at){
                    if(file->at[0] == '/' && file->at[1] == '*'){
                        comment_depth++;
                        file->at += 2;
                    }else if(file->at[0] == '*' && file->at[1] == '/'){
                        comment_depth--;
                        file->at += 2;
                        if(comment_depth == 0){
                            break;
                        }
                    }else if(*file->at == '\n'){
                        file->column = 1;
                        file->line++;
                        start = file->at++;
                    }else{
                        file->at++;
                    }
                }
                u8 *end = file->at;
                next_token__internal_string_no_intern(&cur,  file, TOKEN_comment, end - start, start);
            }else if(file->at[1] == '/'){
                u8 *start = file->at;
                file->at += 2;
                while(*file->at && *file->at != '\n'){
                    file->at++;
                }
                // @note: we want to emit a TOKEN_newline after this, @speed, maybe do that here
                //if(*file->at) file->at++;
                file->column = 1;
                //file->line++;
                u8 *end = file->at;
                next_token__internal_string_no_intern(&cur, file, TOKEN_comment, end - start, start);
            }else if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_div_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_slash, 1);
            }
        }break;
        case '%':{
            if(file->at[1] == '='){
                next_token__internal_char(&cur, file, TOKEN_mod_equals, 2);
            }else{
                next_token__internal_char(&cur, file, TOKEN_mod, 1);
            }
        }break;
        case '\'':{
            // we jump here for all the 'L""', 'u8""', 'u""', 'U""' situations, aftersetting number_kind.
            character_literal_case:;
            cur.type   = TOKEN_character_literal;
            cur.column = cast(u32) file->column;
            cur.line   = cast(u32) file->line;
            cur.file   = file;
            
            // @note: we don't restrict these to lines.
            struct string escaped = handle_escaping(context, file, '\'');
            if(escaped.size > 8){
                cur.value = &escaped;
                cur.type = TOKEN_invalid;
                report_error(context, &cur, "Character literal to long to be represented by a number.");
                
                break;
            }
            
            u64 value = 0;
            for(s32 i = 0; i < escaped.size; i++){
                value = (value << 8) + escaped.data[i];
            }
            
            cur.number = value;
        }break;
        case '"':{
            // we jump here for all the 'L""', 'u8""', 'u""', 'U""' situations, aftersetting number_kind.
            string_literal_case:; 
            cur.type   = TOKEN_string_literal;
            cur.column = cast(u32)file->column;
            cur.line   = cast(u32)file->line;
            cur.file   = file;
            
            // @note: we don't restrict these to lines.
            struct string escaped = handle_escaping(context, file, '"');
            cur.value = string_table_insert(&globals.string_table, escaped, context->arena);
        }break;
        case 'L':{
            if(file->at[1] == '"'){
                file->at++;
                cur.number_kind = STRING_KIND_utf16;
                goto string_literal_case;
            }else if(file->at[1] == '\''){
                file->at++;
                cur.number_kind = STRING_KIND_utf16;
                goto character_literal_case;
            }else{
                goto identifier_case;
            }
            
        }break;
        case '0':{
            if(file->at[1] == 'x' || file->at[1] == 'X'){
                
                u8 *start = file->at;
                file->at += 2;
                
                u64 value = 0;
                while(true){
                    int v = (u32)(*file->at);
                    if('a' <= v && v <= 'f'){
                        v += (10 - 'a');
                    }else if('A' <= v && v <= 'F'){
                        v += 10 - 'A';
                    }else if('0' <= v && v <= '9'){
                        v -= '0';
                    }else{
                        break;
                    }
                    // @cleanup: overflow
                    value = 16 * value + v;
                    file->at++;
                }
                
                int kind = tokenizer_parse_number_postfix(file);
                next_token__internal(&cur, file, TOKEN_hex_literal, (file->at - start));
                cur.number = value;
                cur.number_kind = kind;
                // @cleanup: eat u, d and such?
            }else{
                // @cleanup: octal?
                goto handle_numbers;
            }
        }break;
        case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':{
            // @note: we do not have to handle unary - here, as the unary - is it's own token.
            handle_numbers:;
            u8 *start       = file->at;
            b32 is_floating = false;
            u64 before_dot = 0;
            
            // @cleanup: we need a bunch of checks here that the number is sane.
            
            // @cleanup: overflow
            while(u8_is_number(*file->at)){
                before_dot = (before_dot * 10) + (*file->at++ - '0');
            }
            
            
            if(*file->at == '.'){
                is_floating = true;
                file->at++;
            }
            
            u64 after_dot = 0;
            // @note this works because if we didn't get a dot, this is not a number
            s64 after_dot_exponent = 0;
            while(u8_is_number(*file->at)){
                after_dot = (10 * after_dot)  + (*file->at++ - '0');
                after_dot_exponent++;
            }
            
            s64 exponent = 0;
            if(*file->at == 'e'){
                is_floating = true;
                file->at++;
                b32 is_negative = false;
                if(*file->at == '-'){
                    is_negative = true;
                    file->at++;
                }else if(*file->at == '+'){
                    file->at++;
                }
                
                while(u8_is_number(*file->at)){
                    exponent = (exponent * 10) + (*file->at++ - '0');
                }
                exponent = is_negative ? -exponent : exponent;
            }
            
            if(is_floating){
                u8 *end = file->at;
                
                b32 is_32_bit = false;
                if(*file->at == 'f'){
                    is_32_bit = true;
                    file->at++;
                }
                
                cur._f64 = strtod((char *)start, (char **)&end);
                
                if(is_32_bit){
                    cur.number_kind = NUMBER_KIND_float32;
                }else{
                    cur.number_kind = NUMBER_KIND_float64;
                }
                // @cleanup: should we just have TOKEN_float32_literal and TOKEN_float64_literal
                //           instead of number_kind?
                
                next_token__internal(&cur, file, TOKEN_float_literal, file->at - start);
            }else{
                int kind = tokenizer_parse_number_postfix(file);
                next_token__internal(&cur, file, TOKEN_base10_literal, file->at - start);
                cur.number = before_dot;
                cur.number_kind = kind;
            }
        }break;
        default:{
            identifier_case:
            
            // @speed: we should calculate the hash in the skip code.
            if(!u8_is_alpha_numeric(*file->at)){
                next_token__internal(&cur, file, TOKEN_invalid, 1);
                // this is purly here to print the character
                cur.string = create_string(file->at, 1); // @cleanup: this does not actually work if its utf8...
                file->at++;
                report_error(context, &cur, "expected a character from the alphabet (utf8-not suppored).");
                break;
            }
            
            u8 *start = file->at;
            // @note: apperantly its a microsoft extension to allow '$' @sigh
            while(u8_is_alpha_numeric(*file->at) || *file->at == '$'){
                file->at++;
            }
            
            smm size = file->at - start;
            next_token__internal(&cur, file, TOKEN_identifier, size);
            cur.value = string_table_insert(&globals.identifier_table, create_string(start, size), context->arena);
        }break;
    }
    
    assert(cur.type < TOKEN_count);
    return cur;
}

func void eat_whitespace_and_comments(struct context *context){
    smm did_something;
    do{
        did_something  = (smm)peek_token_eat_raw(context, TOKEN_whitespace);
        did_something |= (smm)peek_token_eat_raw(context, TOKEN_comment);
    }while(did_something);
}

func struct token_bucket_array tokenize_file_raw(struct context *context, struct string absolute_file_path, b32 is_system_include){
    
    begin_counter(tokenize_file_raw);
    struct token_bucket_array ret = zero_struct;
    
    begin_counter(load_file);
    char *c_file_name = push_cstring_from_string(&context->scratch, absolute_file_path);
    struct os_file dummy = os_load_file(c_file_name, 0, 0);
    if(dummy.file_does_not_exist){
        report_error(context, 0, "Error: File '%s' changed during compilation.\n", c_file_name);
        return ret;
    }
    
    u8 *buf = push_data(context->arena, u8, dummy.amount + 32);
    struct os_file os_file = os_load_file(c_file_name, buf, dummy.amount + 32);
    end_counter(load_file);
    
    if(os_file.amount != dummy.amount){ // @cleanup: we could avoid this by doing it manually I think
        report_error(context, 0, "Error: File '%s' changed during compilation.\n", c_file_name);
        return ret;
    }
    memset(buf + os_file.amount, 0, 32); // really zero-terminate the file
    
    struct file_stack_node *file = push_struct(context->arena, struct file_stack_node);
    file->file = os_file;
    file->absolute_file_path = absolute_file_path;
    file->at   = os_file.memory;
    file->end  = os_file.memory + os_file.size;
    file->line = 1;
    file->column = 0;
    file->is_system_include = is_system_include;
    // @cleanup: figure out why this _changes_ throughout the program?
    file->md5 = hash_md5(&context->scratch, os_file.memory, os_file.size);
    
    { // add the file to the list atomically, also these should probably be unique. -> we also maybe do not have to 'tokenize_raw' the same file
        struct file_stack_node *list;
        do{
            list = atomic_load(struct file_stack_node *, globals.file_list.first);
            file->next = list;
        }while(atomic_compare_and_swap(&globals.file_list.first, file, list) != list);
    }
    atomic_add(&globals.file_list.amount, 1);
    
    u32 tokens_per_bucket = 128;
    
    // @note: right now we CANNOT make this 'TOKEN_invalid' terminated, as we chunk the thing, to pass it to
    //        'WORK_parse_global_scope_entry' and the chucks would not be invalid terminated.
    
#if print_tokenizer_stats
    smm begin_time = __rdtsc();
#endif
    
    struct token_bucket *current_bucket = null;
    while(file->at < file->end){
        struct token cur = tokenize_one_token_raw(context, file);
        
        if(cur.type == TOKEN_invalid) break;
        
        if(!current_bucket || current_bucket->amount == tokens_per_bucket){
            current_bucket = push_struct(context->arena, struct token_bucket);
            current_bucket->tokens = push_data(context->arena, struct token, tokens_per_bucket);
            dll_push_back(ret, current_bucket);
        }
        assert(current_bucket->amount < tokens_per_bucket);
        current_bucket->tokens[current_bucket->amount++] = cur;
        
        assert(cur.type < TOKEN_count);
    }
    
    assert(context->error || file->at == file->end);
    end_counter(tokenize_file_raw);
    
#if print_tokenizer_stats
    s64 cycles = __rdtsc() - begin_time;
    s64 bytes  = os_file.size;
    
    print("tokenized file '%.*s':\n", file->absolute_file_path.amount, file->absolute_file_path.data);
    print("   cycles: %lld\n", cycles);
    print("   bytes: %lld\n", bytes);
    print("   cycles/byte: %f\n", (f64)cycles/(f64)bytes);
    print("   lines: %lld\n", file->line);
    
    s64 total_tokens = 0;
    s64 total_identifiers = 0;
    s64 total_whitespaces = 0;
    for(struct token_bucket *bucket = ret.first; bucket; bucket = bucket->next){
        total_tokens += bucket->amount;
        for(u32 i = 0; i < bucket->amount; i++){
            switch(bucket->tokens[i].type){
                case TOKEN_identifier: total_identifiers++; break;
                case TOKEN_whitespace: total_whitespaces++; break;
            }
        }
    }
    print("   tokens: %lld\n", total_tokens);
    print("      idents: %lld\n", total_identifiers);
    print("      whitespace: %lld\n", total_whitespaces);
#endif
    
    return ret;
}

func void maybe_expand_current_token_and_eat(struct context *context, struct string absolute_file_path){
    if(!peek_token(context, TOKEN_identifier)){
        assert(get_current_token(context)->type < TOKEN_count);
        next_token_raw(context);
        return;
    }
    
    struct token *token = get_current_token(context);
    struct define_node *define = lookup_define(context, token->value);
    
    if(!define){
        if(token->value == globals.keyword___FILE__){
            token->type = TOKEN_string_literal;
            *token->value = absolute_file_path;
        }else if(token->value == globals.keyword___LINE__){
            token->type = TOKEN_base10_literal;
            token->number = token->line;
        }
        
        if(token->value == globals.keyword___pragma){
            // :microsoft_extension
            struct token_marker begin = get_current_token_marker(context);
            next_token_raw(context);
            expect_token(context, TOKEN_open_paren); // @note: here we ignore whitespace
            // @incomplete: handle pragmas 
            skip_until_tokens_are_ballanced(context, TOKEN_open_paren, TOKEN_closed_paren);
            struct token_marker end = get_current_token_marker(context);
            
            struct token_bucket_array empty_bucket = zero_struct;
            replace_token_range(context, begin, end, empty_bucket); // delete the pragma
        }
        
        // @note: keyword table is perfect.
        u64 index = xor_shift64((u64)token->value) & (globals.keyword_table_size - 1);
        if(globals.keyword_table[index].string == token->value){
            token->type = globals.keyword_table[index].type;
        }
        
        next_token_raw(context);
        return;
    }
    
    struct token_marker begin_marker = get_current_token_marker(context);
    next_token_raw(context);
    
#if DEBUG_DEFINE_ARGUMENT_SUBSTITUTION
    {
        print("%s-like macro invocation '%.*s' = '", define->is_function_like ? "function" : "object", token->value->size, token->value->data);
        for(smm i = 0; i < define->tokens.amount; i++){
            if((define->tokens.data + i)->type != TOKEN_newline){
                print_token(context, define->tokens.data + i, false);
            }
        }
        struct string file_breaf = strip_file_path(define->defined_token->file->absolute_file_path);
        print("'   @(define at %.*s %d)\n", file_breaf.size, file_breaf.data, define->defined_token->line);
    }
#endif
    
    
    // @note: this is not on 'arg' below anymore, as we might expand the same define for some of the arguments,
    //        which would ruin the pointers.
    struct token_array *argument_token_arrays = push_data(&context->scratch, struct token_array, define->arguments.count);
    struct token_bucket_array *expanded_argument_tokens = push_data(&context->scratch, struct token_bucket_array, define->arguments.count);
    
    
    // if it is a function like macro, parse the arguments
    if(define->is_function_like){
        eat_whitespace_and_comments(context);
        if(!peek_token_eat_raw(context, TOKEN_open_paren)){
            return; // not the define.
        }
        
        if(!define->arguments.first){
            expect_token_raw(context, TOKEN_closed_paren);
        }
        
#if DEBUG_DEFINE_ARGUMENT_SUBSTITUTION
        struct string file_breaf = strip_file_path(token->file->absolute_file_path);
        struct string define_loc_breaf = strip_file_path(define->defined_token->file->absolute_file_path);
        print("arguments to '%.*s' at %.*s %d: (define was defined %.*s %d)\n", define->hash->size, define->hash->data, file_breaf.size, file_breaf.data, token->line, define_loc_breaf.size, define_loc_breaf.data, define->defined_token->line);
#endif
        
        for(struct define_argument *arg = define->arguments.first; arg; arg = arg->next){
            eat_whitespace_and_comments(context);
            
            u32 argument_capacity = 8;
            u32 argument_amount = 0;
            struct token *argument_tokens = push_data(&context->scratch, struct token, argument_capacity);
            
            smm paren_depth = 1;
            while(true){
                if(peek_token(context, TOKEN_invalid)){
                    report_error(context, token, "File ending early.");
                    return;
                }
                
                if(peek_token(context, TOKEN_comma)){
                    // don't break on '__VA_ARGS__' but collect the rest of the arguments into a single one.
                    // this is how we handle '__VA_ARGS__'
                    if(paren_depth == 1 && arg->argument != globals.keyword__VA_ARGS__) break;
                }
                
                if(peek_token(context, TOKEN_open_paren)){
                    paren_depth += 1;
                }
                
                if(peek_token(context, TOKEN_closed_paren)){
                    paren_depth -= 1;
                    if(paren_depth == 0) break;
                }
                
                struct token *arg_token = next_token_raw(context);
                
                dynarray_maybe_grow(struct token, &context->scratch, argument_tokens, argument_amount, argument_capacity);
                
                argument_tokens[argument_amount++] = *arg_token;
            }
            
            argument_token_arrays[arg->argument_index].data   = argument_tokens;
            argument_token_arrays[arg->argument_index].amount = argument_amount;
            
#if DEBUG_DEFINE_ARGUMENT_SUBSTITUTION
            print("    %.*s:", arg->argument->size, arg->argument->data);
            for(smm i = 0; i < argument_amount; i++){
                if((argument_tokens + i)->type != TOKEN_newline){
                    print_token(context, argument_tokens + i, false);
                }
            }
            print("\n");
#endif
            // @note: define arguments can be empty apperantly i.e. 'macro(a,,b)' is legal
            
            if(arg->next){
                if(peek_token_eat_raw(context, TOKEN_closed_paren)){
                    if(arg->next->argument == globals.keyword__VA_ARGS__){
                        // varargs with no arguments in __VA_ARGS__ is fine
                        argument_token_arrays[arg->next->argument_index].amount = 0;
                        break;
                    }
                    begin_error_report(context);
                    report_error(context, token, "Too few arguments for function-like macro.");
                    report_error(context, define->defined_token, "...here is the definition.");
                    end_error_report(context);
                    return;
                }
                
                expect_token_raw(context, TOKEN_comma);
            }else{
                
                // if what follows is a closed paren, then we are done parsing the arguments
                if(peek_token_eat_raw(context, TOKEN_closed_paren)){
                    break;
                }
                
                report_error(context, get_current_token(context), "Too many arguments to in function-like macro invocation.");
                return;
            }
        }
    }    
    
#if DEBUGGING_MACRO_EXPANSION
    static int bug_counter = 0;    
    ++bug_counter;
    print("bug counter: %d\n", bug_counter);
    if(token->line == 1340){
        int break_me = true;
    }
    
    print_define(parser, define, ">>>unlink");
    print("define %p, prev %p\n", define, prev);
    smm amount_of_defines = 0;
    for(struct define_node *node = parser->define_list.first; node; node = node->next){
        //print_define(parser, node, "    ");
        print("   %.*s", node->hash->size, node->hash->data);
        amount_of_defines++;
    }
    print("\n");
#endif
    
#if DEBUGGING_MACRO_EXPANSION
    smm amount_of_defines_after_remove = 0;
    for(struct define_node *node = parser->define_list.first; node; node = node->next){
        //print_define(parser, node, "    ");
        //print("   %.*s", node->hash->size, node->hash->data);
        amount_of_defines_after_remove++;
    }
    assert(amount_of_defines == amount_of_defines_after_remove + 1);
#endif
    
    
    
    // @note: fast_path for 'define->tokens.amount == 0'
    if(define->is_function_like && define->tokens.amount){
        struct token_marker pre_argument_expansion_marker = get_current_token_marker(context);
        struct token_bucket_array pre_argument_expansion_bucket_array = context->tokens;
        
        // we need to expand all the arguments... T.T
        // @hack: we use apply token marker here... not sure if that good
        for(struct define_argument *arg = define->arguments.first; arg; arg = arg->next){
            // @cleanup: this used to say '&context->scratch' but I was scared now, cause I have a weird but. @audit
            struct token_bucket *bucket = push_struct(context->arena, struct token_bucket);
            bucket->tokens = argument_token_arrays[arg->argument_index].data;
            bucket->amount = argument_token_arrays[arg->argument_index].amount;
            
            struct token_bucket_array bucket_array;
            bucket_array.first = bucket;
            bucket_array.last  = bucket;
            
            begin_token_array(context, bucket_array, 0);
            
            while(in_current_token_array(context)){
                maybe_expand_current_token_and_eat(context, absolute_file_path);
            }
            
            assert(arg->argument_index < define->arguments.count);
            validify_bucket_array(context->tokens);
            expanded_argument_tokens[arg->argument_index] = context->tokens;
        }
        context->tokens = pre_argument_expansion_bucket_array;
        apply_token_marker(context, pre_argument_expansion_marker);
    }
    
    
#if DEBUG_DEFINE_ARGUMENT_SUBSTITUTION
    if(define->is_function_like){
        if(bug_counter == 826){
            os_debug_break();   
        }
        print("%d: Expanded args of '%.*s' %d to:\n", bug_counter, token->value->amount, token->value->data, token->line);
        bug_counter++;
        smm i = 0;
        for(struct define_argument *arg = define->arguments.first; arg; arg = arg->next){
            assert(arg->argument_index == i++);
            print("   %.*s: ", arg->argument->size, arg->argument->data);
            for(struct token_bucket *bucket = expanded_argument_tokens[arg->argument_index].first; bucket; bucket = bucket->next){
                for(smm token_index = 0; token_index < bucket->amount; token_index++){
                    if((bucket->tokens + token_index)->type != TOKEN_newline){
                        print_token(context, bucket->tokens + token_index, false);
                    }
                }
            }
            print("\n");
        }
    }
#endif
    
    struct token_marker end_marker = get_current_token_marker(context);
    
    struct token_bucket_array new_buckets = zero_struct;
    
    if(define->tokens.amount){
        struct token *new_tokens = push_data(context->arena, struct token, define->tokens.amount);
        
        // copy all tokens in the define to the place we are at
        for(smm i = 0; i < define->tokens.amount; i++){
            assert(define->tokens.data[i].type < TOKEN_count);
            new_tokens[i].type    = define->tokens.data[i].type;
            new_tokens[i].to_copy = define->tokens.data[i].to_copy;
            new_tokens[i].file    = token->file;
            new_tokens[i].line    = token->line;
            new_tokens[i].column  = token->column;
        }
        
        struct token_bucket *first_new_bucket = push_struct(context->arena, struct token_bucket);
        first_new_bucket->tokens = new_tokens;
        first_new_bucket->amount = define->tokens.amount;
        
        new_buckets.first = first_new_bucket;
        new_buckets.last  = first_new_bucket;
        
        // @cleanup: all this code should probably live inside the 'if(define->is_function_like)' below
        struct hashhash_marker{
            struct hashhash_marker *next;
            struct token_marker marker;
        };
        
        struct{
            struct hashhash_marker *first;
            struct hashhash_marker *last;
        } hashhash_marker_list = zero_struct;
        
        // if its a function-like macro apply argument substitutions
        if(define->is_function_like){
            
            
            smm token_at_in_last_bucket = 0;
            for(smm new_token_index = 0; new_token_index < define->tokens.amount; token_at_in_last_bucket++, new_token_index++){
                assert(new_tokens + new_token_index == new_buckets.last->tokens + token_at_in_last_bucket);
                assert(token_at_in_last_bucket < new_buckets.last->amount);
                
                if(new_tokens[new_token_index].type == TOKEN_hash){
                    struct token *hash_token = new_tokens + new_token_index;
                    
                    // @note: no lenght check needed: we already verifyied that it is correct
                    struct token_marker begin = make_token_marker(new_buckets.last, token_at_in_last_bucket);
                    
                    do{
                        token_at_in_last_bucket++;
                        new_token_index++;
                    }while(new_tokens[new_token_index].type == TOKEN_whitespace ||
                           new_tokens[new_token_index].type == TOKEN_comment);
                    
                    assert(new_tokens[new_token_index].type == TOKEN_identifier);
                    assert(new_tokens[new_token_index].is_define_argument);
                    
                    struct string_list list = zero_struct;
                    
                    // argument is one of our arguemnts, so its fine, but I still feel bad about this...
                    // @cleanup: for now I will do a bad assert look here.
                    struct define_argument *argument = new_tokens[new_token_index].is_define_argument;
                    {
                        b32 found = false;
                        for(struct define_argument *it = define->arguments.first; it; it = it->next){
                            if(it == argument) found = true;
                        }
                        assert(found);
                    }
                    struct token_array args = argument_token_arrays[argument->argument_index];
                    
                    for(u32 arg_index = 0; arg_index < args.count; arg_index++){
                        struct token *arg = args.data + arg_index;
                        switch(arg->type){
                            case TOKEN_whitespace:{
                                string_list_postfix(&list, &context->scratch, string(" "));
                            }break;
                            case TOKEN_string_literal:{
                                string_list_postfix(&list, &context->scratch, string("\""));
                                string_list_postfix(&list, &context->scratch, token_to_string(context, *arg));
                                string_list_postfix(&list, &context->scratch, string("\""));
                            }break;
                            default:{
                                string_list_postfix(&list, &context->scratch, token_to_string(context, *arg));
                            }break;
                        }
                    }
                    
                    struct string string = string_list_flatten(list, context->arena);// @cleanup: can this be scratch?
                    struct token *string_token = push_struct(context->arena, struct token);
                    string_token->type = TOKEN_string_literal;
                    string_token->column = hash_token->column;
                    string_token->line = hash_token->line;
                    string_token->file = hash_token->file;
                    string_token->value = string_table_insert(&globals.string_table, string, context->arena);
                    
                    struct token_marker end = zero_struct;
                    if(token_at_in_last_bucket + 1 < new_buckets.last->amount){
                        end = make_token_marker(new_buckets.last, token_at_in_last_bucket + 1);
                    }
                    
                    struct token_bucket *bucket = push_struct(context->arena, struct token_bucket);
                    bucket->tokens = string_token;
                    bucket->amount = 1;
                    
                    struct token_bucket_array replace_with = {
                        .first = bucket,
                        .last = bucket,
                    };
                    
                    replace_token_range__internal(context->arena, &new_buckets, begin, end, replace_with);
                    token_at_in_last_bucket = -1; // @note: zero next iteration
                    continue;
                }else if(new_tokens[new_token_index].type == TOKEN_hashhash){
                    // we have already checked that we have a previous and a next token. @cleanup: link these
                    struct hashhash_marker *marker = push_struct(&context->scratch, struct hashhash_marker);
                    marker->marker = make_token_marker(new_buckets.last, token_at_in_last_bucket);
                    sll_push_front(hashhash_marker_list, marker);
                    continue;
                }
                
                if(new_tokens[new_token_index].type != TOKEN_identifier) continue;
                struct define_argument *arg = new_tokens[new_token_index].is_define_argument;
                if(!arg) continue;
                // @cleanup: I think we are supposed to not expand if we immediatly preced a '##' or if we are preceded by '##'
                
                smm amount = 0;
                for(struct token_bucket *it = expanded_argument_tokens[arg->argument_index].first; it; it = it->next){
                    amount += it->amount;
                }
                
                struct token_bucket *arg_bucket = null;
                if(amount){
                    struct token *arg_tokens = push_data(context->arena, struct token, amount);
                    {   // flatten 'arg_tokens' into a single token_array.
                        smm at = 0;
                        for(struct token_bucket *bucket = expanded_argument_tokens[arg->argument_index].first; bucket; bucket = bucket->next){
                            for(smm i = 0; i < bucket->amount; i++){
                                arg_tokens[at + i] = bucket->tokens[i];
                            }
                            at += bucket->amount;
                        }
                    }
                    
                    if(amount){
                        arg_bucket = push_struct(context->arena, struct token_bucket);
                        arg_bucket->tokens = arg_tokens;
                        arg_bucket->amount = amount;
                    }
                }
                
                // replace the identifier with the arg_bucket
                
                struct token_marker begin = make_token_marker(new_buckets.last, token_at_in_last_bucket);
                struct token_marker end = zero_struct;
                if(token_at_in_last_bucket + 1 < new_buckets.last->amount){
                    end = make_token_marker(new_buckets.last, token_at_in_last_bucket + 1);
                }
                
                struct token_bucket_array replace_with = {
                    .first = arg_bucket,
                    .last = arg_bucket,
                };
                
                replace_token_range__internal(context->arena, &new_buckets, begin, end, replace_with);
                token_at_in_last_bucket = -1; // @note: zero next iteration
            }
        }
        
        // We iterate _backwards_, so non of them turn invalid.
        for(struct hashhash_marker *marker = hashhash_marker_list.first; marker; marker = marker->next){
            struct token_bucket *bucket = marker->marker.bucket;
            smm token_at = marker->marker.token_at;
            assert(bucket->tokens[token_at].type == TOKEN_hashhash);
            
            struct token *prev = null;
            struct token_marker begin = zero_struct;
            for(struct token_bucket *bucket_it = bucket; bucket_it; bucket_it = bucket_it->prev){
                b32 should_break = false;
                for(smm it = (bucket == bucket_it) ? token_at - 1 : bucket_it->amount - 1; it >= 0; it--){
                    if(bucket_it->tokens[it].type != TOKEN_whitespace &&
                       bucket_it->tokens[it].type != TOKEN_comment){
                        prev = bucket_it->tokens + it;
                        begin = make_token_marker(bucket_it, it);
                        should_break = true;
                        break;
                    }
                }
                if(should_break) break;
            }
            
            
            struct token *next = null;
            struct token_marker end = zero_struct;
            for(struct token_bucket *bucket_it = bucket; bucket_it; bucket_it = bucket_it->next){
                b32 should_break = false;
                for(smm it = (bucket == bucket_it) ? token_at + 1 : 0; it < bucket_it->amount; it++){
                    if(bucket_it->tokens[it].type != TOKEN_whitespace &&
                       bucket_it->tokens[it].type != TOKEN_comment){
                        next = bucket_it->tokens + it;
                        if(it + 1 != bucket_it->amount){
                            end = make_token_marker(bucket_it, it + 1);
                        }else{
                            end = make_token_marker(bucket_it->next, 0);
                        }
                        should_break = true;
                        break;
                    }
                }
                if(should_break) break;
            }
            assert(prev);
            assert(next);
            
            struct string prev_string = push_token_string(context, prev, false);
            struct string next_string = push_token_string(context, next, false);
            
            struct string concat = string_concatinate(context->arena, prev_string, next_string);
            
            // @cleanup: @hack
            struct file_stack_node hack_file = zero_struct;
            hack_file.at     = concat.data;
            hack_file.end    = concat.data + concat.size;
            hack_file.line   = token->line;
            hack_file.column = token->column;
            hack_file.file   = token->file->file;
            hack_file.absolute_file_path = token->file->absolute_file_path;
            
            struct token concatinated_token = tokenize_one_token_raw(context, &hack_file);
            assert(concatinated_token.type != TOKEN_invalid); // @cleanup: can this happen?
            // maybe->bad suffix on number or something
            
            // remove 'hack_file' from token, ... this is really hacky @cleanup:
            concatinated_token.file = token->file;
            
            struct token *tokens = null;
            smm amount = 0;
            
            // result is at most two tokens. (i think)
            if(hack_file.at < hack_file.end){
                struct token concatinated_token2 = tokenize_one_token_raw(context, &hack_file);
                // remove 'hack_file' from token, ... this is really hacky @cleanup:
                concatinated_token2.file = token->file;
                if(concatinated_token2.type != TOKEN_invalid){
                    tokens = push_data(context->arena, struct token, 2);
                    tokens[0] = concatinated_token;
                    tokens[1] = concatinated_token2;
                    amount = 2;
                    
                }
            }
            
            if(!tokens){
                tokens = push_data(context->arena, struct token, 2);
                amount = 1;
                tokens[0] = concatinated_token;
            }
            // @cleanup: this whole thing should be a wrapper we do it in multiple places
            struct token_bucket *new_bucket = push_struct(context->arena, struct token_bucket);
            new_bucket->tokens = tokens;
            new_bucket->amount = amount;
            
            struct token_bucket_array replace_with;
            replace_with.first = new_bucket;
            replace_with.last  = new_bucket;
            
            replace_token_range__internal(context->arena, &new_buckets, begin, end, replace_with);
        }
    }
    
    // disable the define after expanding the arguments, to allow 'identity(identity(a))'
    // but before recursively expanding the body
    validify_bucket_array(new_buckets);
    define->is_disabled = true;
    {   // recursively expand 'new_buckets' while the 'define' is still unlinked.
        struct token_bucket_array old_bucket_array = context->tokens;
        begin_token_array(context, new_buckets, 0);
        
        while(in_current_token_array(context)){
            maybe_expand_current_token_and_eat(context, absolute_file_path);
        }
        
        // @warning: this corrupts the current token marker, we rely on the fact that we set it below
        
        new_buckets = context->tokens;
        context->tokens = old_bucket_array;
    }
    // reenable the define
    define->is_disabled = false;
    validify_bucket_array(new_buckets);
    
    
#if DEBUGGING_MACRO_EXPANSION
    print_define(parser, define, "<<<relink");
    print("\n");
    for(struct define_node *node = parser->define_list.first; node; node = node->next) {
        //print_define(parser, node, "    ");
        print("   %.*s", node->hash->size, node->hash->data);
        amount_of_defines--;
    }
    print("\n");
    assert(amount_of_defines == 0);
#endif
    
    
    struct token_bucket *one_past_new_buckets = replace_token_range(context, begin_marker, end_marker, new_buckets);
    assert(!new_buckets.last || new_buckets.last->next == one_past_new_buckets);
    validify_bucket_array(context->tokens);
    
    // if we added something put it to there, otherwise put it to _after_ what we removed
    // @note: this has to stay here see @warning above
    // @note: we set the 'bucket_to_apply' to the beginning of the just expanded set of tokens.
    //        this allows for recursive defines like a -> b and then next time trough we expand b() -> c
    // Update: but this is actually wrong behavior.
    //            #define asd2() xxx
    //            #define parens ()
    //            #define asd asd2
    //            #define id(a) a
    //                id(asd parens)
    //        complains about undefined identifier 'xxx', but 'asd parens' complains about 
    //        undefined ident 'asd2'
    // Update: on the other hand, the second node is still valid. a -> b -> b()-> c should expand to c.
    //         this maybe as a hack, (maybe this is fine) we search backwards for the last significant token
    //         and start from there again. (note that we are not allowed to exand it if if is 'a' again)
    
    if(!sll_is_empty(new_buckets)){
        // @cleanup: maybe we should search for this token_marker, before we 'replace_token_range', like this we run the risk of skipping to before the replacement list
        assert(new_buckets.last->amount);
        apply_token_marker(context, make_token_marker(new_buckets.last, new_buckets.last->amount - 1));
        while(peek_token(context, TOKEN_whitespace) || 
              peek_token(context, TOKEN_newline) || 
              peek_token(context, TOKEN_comment)){
            if(prev_token_raw(context)->type == TOKEN_invalid) break; // @sigh: gross hack
        }
        
        struct token *maybe_ident = peek_token(context, TOKEN_identifier);
        if(maybe_ident && maybe_ident->value == token->value){
            next_token_raw(context); // @note: do not expand this token again.
        }
        
    }else{
        apply_token_marker(context, make_token_marker(one_past_new_buckets, 0));
    }
}

//  ********************* static if evaluate ****************************************

// @note: logical or is the _entry point_ to static_if_evaluate-chain
func smm static_if_evaluate_logical_or(struct context *context);
func smm static_if_evaluate_primary(struct context *context){
    eat_whitespace_and_comments(context);
    struct token *test = next_token_raw(context);
    switch(test->type){
        case TOKEN_character_literal:
        case TOKEN_base10_literal:
        case TOKEN_hex_literal:{
            eat_whitespace_and_comments(context);
            return test->number;
        }break;
        case TOKEN_identifier:{
            if(test->value == globals.keyword_defined){
                eat_whitespace_and_comments(context);
                struct token *got_paren = peek_token_eat_raw(context, TOKEN_open_paren);
                
                eat_whitespace_and_comments(context);
                struct token *token = expect_token_raw(context, TOKEN_identifier);
                eat_whitespace_and_comments(context);
                
                if(got_paren) expect_token_raw(context, TOKEN_closed_paren);
                
                if(context->should_sleep) return 0;
                
                eat_whitespace_and_comments(context);
                return lookup_define(context, token->value) != null;
            }
            
            if(!context->static_if_evaluate_should_skip_undefined_label){
                report_warning(context, test, "Undefined identifier in '#if' operand gets evaluated to zero.");
            }
            eat_whitespace_and_comments(context);
            return 0;
        }break;
        case TOKEN_open_paren:{
            // @note: logical or is the _entry point_
            smm value = static_if_evaluate_logical_or(context);
            eat_whitespace_and_comments(context);
            expect_token_raw(context, TOKEN_closed_paren);
            
            eat_whitespace_and_comments(context);
            return value;
        }break;
        default:{
            report_error(context, test, "Unexpected token in '#if' operand.");
            return 0;
        }break;
    }
}

func smm static_if_evaluate_prefix(struct context *context){
    eat_whitespace_and_comments(context);
    struct token *test = next_token_raw(context);
    smm value;
    switch(test->type){
        case TOKEN_logical_not:{
            value = !static_if_evaluate_prefix(context);
        }break;
        case TOKEN_bitwise_not:{
            value = ~static_if_evaluate_prefix(context);
        }break;
        case TOKEN_plus:{
            value = static_if_evaluate_prefix(context);
        }break;
        case TOKEN_minus:{
            value  = -static_if_evaluate_prefix(context);
        }break;
        default:{
            prev_token_raw(context);
            value = static_if_evaluate_primary(context);
        }break;
    }
    return value;
}

func smm static_if_evaluate_shift(struct context *context){
    smm value = static_if_evaluate_prefix(context);
    if(peek_token_eat_raw(context, TOKEN_left_shift)){
        value = value << static_if_evaluate_shift(context);
    }else if(peek_token_eat_raw(context, TOKEN_right_shift)){
        value = value >> static_if_evaluate_shift(context);
    }
    return value;
}

func smm static_if_evaluate_bitwise(struct context *context){
    smm value = static_if_evaluate_shift(context);
    if(peek_token_eat_raw(context, TOKEN_and)){
        value = value & static_if_evaluate_bitwise(context);
    }else if(peek_token_eat_raw(context, TOKEN_or)){
        value = value | static_if_evaluate_bitwise(context);
    }else if(peek_token_eat_raw(context, TOKEN_xor)){
        value = value ^ static_if_evaluate_bitwise(context);
    }
    return value;
}

func smm static_if_evaluate_multiplicative(struct context *context){
    smm value = static_if_evaluate_bitwise(context);
    if(peek_token_eat_raw(context, TOKEN_times)){
        value = value * static_if_evaluate_multiplicative(context);
    }else if(peek_token_eat_raw(context, TOKEN_slash)){
        struct token *token = get_current_token(context);
        smm div_by = static_if_evaluate_multiplicative(context);
        if(!div_by){
            report_error(context, token, "div by zero.");
        }else{
            value = value / div_by;
        }
    }else if(peek_token_eat_raw(context, TOKEN_mod)){
        struct token *token = get_current_token(context);
        smm div_by = static_if_evaluate_multiplicative(context);
        if(!div_by){
            report_error(context, token, "mod by zero.");
        }else{
            value = value % div_by;
        }
    }
    return value;
}

func smm static_if_evaluate_additive(struct context *context){
    smm value = static_if_evaluate_multiplicative(context);
    if(peek_token_eat_raw(context, TOKEN_plus)){
        value = value + static_if_evaluate_additive(context);
    }else if(peek_token_eat_raw(context, TOKEN_minus)){
        value = value - static_if_evaluate_additive(context);
    }
    return value;
}

func smm static_if_evaluate_compare(struct context *context){
    smm value = static_if_evaluate_additive(context);
    
    struct token *token = next_token_raw(context);
    switch(token->type){
        case TOKEN_logical_equals:   value = value == static_if_evaluate_compare(context); break;
        case TOKEN_logical_unequals: value = value != static_if_evaluate_compare(context); break;
        case TOKEN_bigger_equals:    value = value >= static_if_evaluate_compare(context); break;
        case TOKEN_smaller_equals:   value = value <= static_if_evaluate_compare(context); break;
        case TOKEN_bigger:           value = value >  static_if_evaluate_compare(context); break;
        case TOKEN_smaller:          value = value <  static_if_evaluate_compare(context); break;
        default: prev_token_raw(context); break;
    }
    return value;
}

func smm static_if_evaluate_logical_and(struct context *context){
    smm value = static_if_evaluate_compare(context);
    if(peek_token_eat_raw(context, TOKEN_logical_and)){
        if(value){
            value = static_if_evaluate_logical_and(context);
        }else{
            b32 old = context->static_if_evaluate_should_skip_undefined_label;
            context->static_if_evaluate_should_skip_undefined_label = true;
            static_if_evaluate_logical_and(context);// ignore the return value
            context->static_if_evaluate_should_skip_undefined_label = old;
        }
    }
    return value;
}

func smm static_if_evaluate_logical_or(struct context *context){
    smm value = static_if_evaluate_logical_and(context);
    if(peek_token_eat_raw(context, TOKEN_logical_or)){
        if(value){
            b32 old = context->static_if_evaluate_should_skip_undefined_label;
            context->static_if_evaluate_should_skip_undefined_label = true;
            static_if_evaluate_logical_or(context); // ignore the return value
            context->static_if_evaluate_should_skip_undefined_label = old;
        }else{
            value = static_if_evaluate_logical_or(context);
        }
    }
    return value;
}

func smm static_if_evaluate(struct context *context, struct string absolute_file_path){
    context->static_if_evaluate_should_skip_undefined_label = false;
    
    struct token_marker begin_marker = get_current_token_marker(context);
    
    // @cleanup: absolute_file_path should probably be on parser
    // expand tokens
    while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
        // @sigh: dont expand 'defined'
        struct token *token = peek_token(context, TOKEN_identifier);

        if(token && token->value == globals.keyword_defined){
            next_token_raw(context);
            eat_whitespace_and_comments(context);
            
            struct token *got_paren = peek_token_eat_raw(context, TOKEN_open_paren);
            eat_whitespace_and_comments(context);
            
            expect_token_raw(context, TOKEN_identifier);
            
            eat_whitespace_and_comments(context);
            if(got_paren) expect_token_raw(context, TOKEN_closed_paren);
            continue;
        }
        maybe_expand_current_token_and_eat(context, absolute_file_path);
    }
    
    apply_token_marker(context, begin_marker);
    return static_if_evaluate_logical_or(context);
}

//  ********************* end of static if evaluate **********************************


// @note: if we left the tokens in a linked list of buckets, we would not have to copy them
func struct token_bucket_array file_tokenize_and_preprocess(struct context *context, struct string absolute_file_path){
    
    struct static_if_stack_node{
        struct static_if_stack_node *next;
        struct token_marker token_marker;
        struct token *token; // right now purely for debugging, but I'm not sure all error_reports link to the right tokens.... @cleanup:
        b32 is_disabled_elif; // used to handle '#if 1' -> '#elif 0' -> '#else' -> '#error asd' ->  '#endif'
        
        b32 is_true;
        b32 is_else;
    };
    
    // front is top
    struct{
        struct static_if_stack_node *first;
        struct static_if_stack_node *last;
    } static_if_stack = zero_struct;
    
    {
        struct token_bucket_array array = tokenize_file_raw(context, absolute_file_path, false);
        if(dll_is_empty(array)) return array;
        begin_token_array(context, array, 0);
    }
    
    struct memory_arena *scratch = &context->scratch;
    
    b32 got_newline = true;
    while(in_current_token_array(context)){
        while(true){
            if(peek_token_eat_raw(context, TOKEN_whitespace)) continue;
            if(peek_token_eat_raw(context, TOKEN_comment))    continue;
            if(peek_token_eat_raw(context, TOKEN_newline)){
                got_newline = true;
                continue;
            }
            break;
        }
        
        if(got_newline && peek_token(context, TOKEN_hash)){
            struct token_marker directive_begin = get_current_token_marker(context);
            next_token_raw(context);
            
            // @note: we need the _raw_ variants here everywhere, as we should stop at a '\n'
            while(peek_token_eat_raw(context, TOKEN_whitespace) || peek_token_eat_raw(context, TOKEN_comment));
            
            if(peek_token(context, TOKEN_identifier)){
                struct token *directive = next_token_raw(context);
                // @speed: perfect hash table to switch on the keyword?
                
                if(directive->value == globals.keyword_if){
                    smm value = 0;
                    
                    // disable the static_if_evaluate if it doesn't matter anyway
                    if(!static_if_stack.first || static_if_stack.first->is_true){
                        value = static_if_evaluate(context, absolute_file_path);
                        if(context->should_sleep) goto end;
                    }else{
                        while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                            next_token_raw(context);
                        }
                    }
                    
                    // @note: value will always be '0' if this '#if' is contained in a '#if 0' block
                    struct static_if_stack_node *node = push_struct(scratch, struct static_if_stack_node);
                    node->is_true = value != 0;
                    node->token_marker = directive_begin;
                    node->token = directive;
                    sll_push_front(static_if_stack, node);
                    
                    // if the node is not true, then we later delte the block, so no need to delete the directive here. (also, that might result in making node->token_marker invalid)
                    if(!node->is_true) continue;
                    
                    //parser_report_warning(parser, get_current_token(parser), "static if %d", node->is_true);
                    
                }else if(directive->value == globals.keyword_ifdef || directive->value == globals.keyword_ifndef ){
                    b32 is_ifdef = (directive->value == globals.keyword_ifdef);
                    char *error_string = is_ifdef ? "ifdef" : "ifndef";
                    
                    eat_whitespace_and_comments(context);
                    if(!peek_token(context, TOKEN_identifier)){
                        report_error(context, get_current_token(context), "Expected an identifier after '#%s'", error_string);
                        goto end;
                    }
                    struct token *defined_identifier = next_token_raw(context);
                    
                    b32 evaluates_to_true = false;
                    if(!static_if_stack.first || static_if_stack.first->is_true){
                        b32 defined = lookup_define(context, defined_identifier->value) != null;
                        evaluates_to_true = is_ifdef ? defined : !defined;
                    }
                    
                    struct static_if_stack_node *node = push_struct(scratch, struct static_if_stack_node);
                    node->is_true = evaluates_to_true;
                    node->token_marker = directive_begin;
                    node->token = directive;
                    sll_push_front(static_if_stack, node);
                    
                    //parser_report_warning(parser, get_current_token(parser), "static if %d", node->is_true);
                    
                    eat_whitespace_and_comments(context);
                    if(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                        report_warning(context, get_current_token(context), "Junk after '#%s'.", error_string);
                    }
                    
                }else if(directive->value == globals.keyword_else || directive->value == globals.keyword_elif){
                    struct static_if_stack_node *node = static_if_stack.first;
                                        
                    if(!node){
                        report_error(context, directive, "'#%.*s' without matching '#if'", directive->value->amount, directive->value->data);
                        goto end;
                    }
                    
                    if(node->is_else){
                        report_error(context, directive, "double '#%.*s'", directive->value->amount, directive->value->data); // @cleanup: show both
                        goto end;
                    }
                    // if it was an else save that information, @note: we edit 'node' directly
                    if(directive->value != globals.keyword_elif) node->is_else = true;
                    
                    
                    // if 'node' is a top level '#if' or if the '#if' is active (i.e '#if 1')
                    if(!node->next || node->next->is_true){
                        if(node->is_true || node->is_disabled_elif){
                            // we are an '#else' or '#elif' after a '#if 1', thus we are false
                            node->is_true = false;
                            
                            // if we have something like
                            //    #if 1
                            //    #elif 0 // (condition does not matter)
                            //    #else
                            //    #endif
                            // the '#else' should not be active.
                            
                            if(!node->is_disabled_elif){
                                // adjust the marker to be _right before_ the '#else'
                                node->token_marker = directive_begin;
                            }
                            
                            if(directive->value == globals.keyword_elif) node->is_disabled_elif = true;
                        }else{
                            // we are in an '#else' or '#elif' after a '#if 0'
                            if(directive->value == globals.keyword_elif){
                                node->is_true = static_if_evaluate(context, absolute_file_path) != 0;
                                if(context->should_sleep) goto end;
                            }else{
                                
                                node->is_true = true;
                            }
                            
                            // if the static if was false _delete_ the token range
                            // delete everything including the '#elif expr'
                            struct token_marker end_marker = get_current_token_marker(context);
                            assert(end_marker.bucket); // @cleanup: when is this not true?
                            // @document the below if() that guards around this and maybe make a
                            // replace_token_range_and_adjust_current.
                            
                            struct token_bucket_array empty_bucket = zero_struct;
                            struct token_bucket *new_current = replace_token_range(context, node->token_marker, end_marker, empty_bucket);
                            apply_token_marker(context, make_token_marker(new_current, 0));
                        }
                        continue; // continue in either case.
                    }else{
                        assert(!node->is_true);
                    }
                    
                    //parser_report_warning(parser, get_current_token(parser), "static else %d %p", node->is_true, node->next);
                }else if(directive->value == globals.keyword_endif){
                    struct static_if_stack_node *node = static_if_stack.first;
                    if(!node){
                        report_error(context, directive, "'#endif' without matching '#if'");
                        goto end;
                    }else{
                        sll_pop_front(static_if_stack);
                    }
                    
                    eat_whitespace_and_comments(context);
                    if(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                        report_warning(context, get_current_token(context), "Junk after '#endif'.");
                    }
                    
                    if(!node->is_true){
                        // if the static if was false _delete_ the token range
                        struct token_marker end_marker = get_current_token_marker(context);
                        
                        assert(end_marker.bucket); // @cleanup: when is this not true?
                        // @document the below if() that guards around this and maybe make a
                        // replace_token_range_and_adjust_current.
                        
                        struct token_bucket_array empty_bucket = zero_struct;
                        struct token_bucket *new_current = replace_token_range(context, node->token_marker, end_marker, empty_bucket);
                        context->current_token_bucket = new_current;
                        context->token_at = 0;
                        continue; // continue so we do not clear the '#directive' range below
                    }
                }else if(!static_if_stack.first || static_if_stack.first->is_true){
                    // these below should only be applied if we are not in a disabled block.
                    
                    if(directive->value == globals.keyword_define){
                        
                        eat_whitespace_and_comments(context);
                        if(!peek_token(context, TOKEN_identifier)){
                            report_error(context, get_current_token(context), "Expected an identifier after '#define'");
                            goto end;
                        }
                        struct token *defined_identifier = next_token_raw(context);
                        unique_string name = defined_identifier->value;
                        
                        struct define_argument_list arguments = zero_struct;
                        
                        b32 is_function_like = false;
                        b32 is_varargs = false;
                        if(peek_token_eat_raw(context, TOKEN_open_paren)){
                            is_function_like = true;
                            if(!peek_token_eat_raw(context, TOKEN_closed_paren)){
                                do{
                                    eat_whitespace_and_comments(context);
                                    
                                    if(peek_token_eat(context, TOKEN_dotdotdot)){
                                        is_varargs = true;
                                        
                                        struct define_argument *node = push_struct(scratch, struct define_argument);
                                        node->argument = globals.keyword__VA_ARGS__;
                                        sll_push_back(arguments, node);
                                        node->argument_index = arguments.count;
                                        arguments.count += 1;
                                        
                                        break;
                                    }else if(!peek_token(context, TOKEN_identifier)){
                                        report_error(context, get_current_token(context), "Expected an identifier in function-like macro definition.");
                                        goto end;
                                    }
                                    
                                    struct define_argument *node = push_struct(scratch, struct define_argument);
                                    node->argument = next_token_raw(context)->value;
                                    for(struct define_argument *old_arg = arguments.first; old_arg; old_arg = old_arg->next){
                                        if(old_arg->argument == node->argument){
                                            report_error(context, prev_token(context), "Two arguments have the same identifier.");
                                            goto end;
                                        }
                                    }
                                    
                                    sll_push_back(arguments, node);
                                    node->argument_index = arguments.count;
                                    arguments.count += 1;
                                    
                                    eat_whitespace_and_comments(context);
                                }while(peek_token_eat_raw(context, TOKEN_comma));
                                
                                expect_token_raw(context, TOKEN_closed_paren);
                            }
                        }
                        
                        eat_whitespace_and_comments(context); // eat unneccessary leading whitespace.
                        
                        // @speed: I guess this is kinda stupid... first search for a new line then copy?
                        u32 define_capacity = 0x10;
                        u32 define_amount = 0;
                        struct token *define_tokens = push_data(scratch, struct token, define_capacity);
                        while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                            struct token *token = next_token_raw(context);
                            
                            if(token->type == TOKEN_identifier){
                                struct define_argument *arg = null;
                                for(struct define_argument *arg_it = arguments.first; arg_it; arg_it = arg_it->next){
                                    if(arg_it->argument == token->value){
                                        arg = arg_it;
                                        break;
                                    }
                                }
                                
                                if(arg){
                                    token->is_define_argument = arg; // @cleanup: setting this in the file is kinda weird...
                                }
                            }
                            
                            dynarray_maybe_grow(struct token, scratch, define_tokens, define_amount, define_capacity);
                            define_tokens[define_amount++] = *token; // dont copy the newline
                        }
                        
                        struct token_array tokens = zero_struct;
                        tokens.data = define_tokens;
                        tokens.amount = define_amount;
                        
                        // @yuck: check that we do not have # into a non-argument.
                        for(u32 i = 0; i < tokens.amount; i++){
                            if(tokens.data[i].type == TOKEN_hash){
                                i++; // skip the hash
                                // skip comments and whitespace
                                while(i < tokens.amount){
                                    if(tokens.data[i].type == TOKEN_whitespace || tokens.data[i].type == TOKEN_comment){
                                        i = i + 1;
                                    }else{
                                        break;
                                    }
                                }
                                b32 error = false;
                                if(i == tokens.amount){
                                    error = true;
                                }else if(tokens.data[i].type == AST_identifier){
                                    error = true;
                                }else{
                                    error = (tokens.data[i].is_define_argument == 0);
                                }
                                
                                if(error){
                                    report_error(context, tokens.data + i, "expected a formal argument after '#' in macro replacement list for '%.*s'.", defined_identifier->value->size, defined_identifier->value->data);
                                    goto end;
                                }
                            }else if(tokens.data[i].type == TOKEN_hashhash){
                                struct token *prev = null;
                                for(smm it = i - 1; it >= 0; it--){
                                    if(tokens.data[it].type != TOKEN_whitespace && tokens.data[it].type != TOKEN_comment){
                                        prev = tokens.data + it;
                                        break;
                                    }
                                }
                                
                                if(!prev){
                                    report_error(context, tokens.data + i, "'##' at the begining of a macro replacement list is illegal.");
                                    goto end;
                                }
                                
                                struct token *next = null;
                                for(u32 it = i + 1; it < tokens.amount; it++){
                                    if(tokens.data[it].type != TOKEN_whitespace && tokens.data[it].type != TOKEN_comment){
                                        next = tokens.data + it;
                                        break;
                                    }
                                }
                                
                                if(!next){
                                    report_error(context, tokens.data + i, "'##' at the end of a macro replacement list is illegal.");
                                    goto end;
                                }
                            }
                        }
                        
                        struct define_node *redecl = lookup_define(context, name);
                        if(redecl){
                            // "An identifier currently defined as an object-like macro [...] shall not be
                            //  redefiend unless the second definition is a object like macro and the two
                            //  replacement lists are identical."
                            b32 defines_are_equivalant = true;
                            
                            u32 i = 0;
                            u32 j = 0;
                            while(true){
                                while(i < define_amount &&
                                      (tokens.data[i].type == TOKEN_whitespace ||
                                       tokens.data[i].type == TOKEN_comment)) i++;
                                
                                while(j < redecl->tokens.amount &&
                                      (redecl->tokens.data[j].type == TOKEN_whitespace ||
                                       redecl->tokens.data[j].type == TOKEN_comment)) j++;
                                
                                if(i == define_amount || j == redecl->tokens.amount){
                                    if(i != define_amount || j != redecl->tokens.amount){
                                        defines_are_equivalant = false;
                                    }
                                    break;
                                }
                                
                                if(!token_match(&tokens.data[i], &redecl->tokens.data[j])){
                                    defines_are_equivalant = false;
                                    break;
                                }else{
                                    i += 1;
                                    j += 1;
                                }
                            }
                            
                            // "Likewise, an identifier currently defined as a function-like macro shall not
                            //  be redefined by another '#define ' preprocessing directive unless the second
                            //  definition is a function-like macro definition that has the same number and
                            //  spelling of paramenters, and the two replacement lists are identical."
                            
                            if(!sll_is_empty(redecl->arguments) || !sll_is_empty(arguments)){
                                // function like macros
                                
                                struct define_argument *redecl_it = redecl->arguments.first;
                                struct define_argument *decl_it = arguments.first;
                                for(; redecl_it && decl_it; redecl_it = redecl_it->next, decl_it = decl_it->next){
                                    if(redecl_it->argument != decl_it->argument){
                                        defines_are_equivalant = false;
                                        break;
                                    }
                                }
                                
                                if(redecl_it || decl_it){
                                    defines_are_equivalant = false;
                                }else{
                                    // we expect this to be true, so no fast path for '!='
                                    assert(redecl->arguments.count == arguments.count);
                                }
                            }
                            
                            if(!defines_are_equivalant){
                                begin_error_report(context);
                                report_error(context, defined_identifier, "Redeclaration of macro '%.*s'", name->amount, name->data);
                                report_error(context, redecl->defined_token, "here is the previous definition.");
                                end_error_report(context);
                                goto end;
                            }
                        }
                        
                        // dont push a new define if we found an equivalent redeclaration
                        if(!redecl){
                            struct define_node *new_node = push_struct(scratch, struct define_node);
                            new_node->tokens             = tokens;
                            new_node->name               = name;
                            new_node->defined_token      = defined_identifier;
                            new_node->arguments          = arguments;
                            new_node->is_function_like   = is_function_like;
                            new_node->is_varargs         = is_varargs;
                            
                            register_define(context, new_node);
                        }
#if 0
                        print_define(context, new_node, "#define");
#endif
                        
                    }else if(directive->value == globals.keyword_undef){
                        eat_whitespace_and_comments(context);
                        if(!peek_token(context, TOKEN_identifier)){
                            report_error(context, get_current_token(context), "expected an identifier after '#undef'");
                        }
                        struct token *undef_token = next_token_raw(context);
                        unique_string hash = undef_token->value;
                        
                        struct define_node *define = lookup_define(context, hash);
                        
                        if(define){
#if 0
                            print_define(context, define, "#undef");
#endif
                            u64 index = xor_shift64((u64)define->name) & context->define_table.mask;
                            dll_remove(context->define_table.lists[index], define);
                        }else{
                            report_warning(context, undef_token, "'#undef' on undefined identifier.");
                            
#if 0
                            //os_debug_break();
                            print("***************************************************\n");
                            for(struct define_node *node = context->define_list.first; node; node = node->next){
                                print_define(parser, node, "#define");
                            }
#endif
                        }
                        
                        eat_whitespace_and_comments(context);
                        if(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                            report_warning(context, get_current_token(context), "Junk after '#undef'.");
                        }
                        while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                            next_token_raw(context);
                        }
                        
                    }else if(directive->value == globals.keyword_include){
                        eat_whitespace_and_comments(context);
                        
                        struct string new_absolute_file_path;
                        b32 is_system_include; 
                        // @sigh: it seems ""-includes in system includes are system includes?
                        if(peek_token(context, TOKEN_string_literal)){
                            is_system_include = false;
                            
                            struct token *lit = next_token_raw(context);
                            struct string file_name = *lit->value;

                            struct string path = strip_file_name(lit->file->absolute_file_path);
                            new_absolute_file_path = concatinate_file_paths(&context->scratch, path, file_name);
                            
                            b32 found = false;
                            struct string found_location = zero_struct;
                            
                            struct os_file dummy = os_load_file((char *)new_absolute_file_path.data, 0, 0);
                            if(!dummy.file_does_not_exist){
                                found = true;
                                found_location = new_absolute_file_path;
                            }
                            
                            for(struct string_list_node *node = globals.additional_include_directories.list.first; node; node = node->next){
                                // if a file in a 'additional_include_directory' includes something
                                // the file paths will be the same, so skip that here.
                                

                                struct string other_path = concatinate_file_paths(&context->scratch, node->string, file_name);
                                
                                // for this 'easy' check to work all paths have to be canonical.
                                // right now we try to accomplish that by canonicalizing the 
                                // 'additional include paths'
                                if(string_match_case_insensitive(new_absolute_file_path, other_path)) continue;
                                
                                dummy = os_load_file((char *)other_path.data, 0, 0);
                                if(!dummy.file_does_not_exist){
                                    if(found){
                                        report_error(context, lit, "Cannot distinguish between the files '%.*s' and '%.*s'.", found_location.size, found_location.data, other_path.size, other_path.data);
                                        goto end;
                                    }
                                    
                                    found = true;
                                    new_absolute_file_path = other_path;
                                    found_location = other_path;
                                }
                            }
                            
                            if(!found){
                                report_error(context, lit, "Error: '%.*s' file not found.\n", lit->value->size, lit->value->data);
                                goto end;
                            }
                        }else if(peek_token(context, TOKEN_smaller)){
                            is_system_include = true;
                            
                            struct token *smaller = next_token_raw(context);
                            
                            while(!peek_token(context, TOKEN_bigger) &&
                                  !peek_token(context, TOKEN_invalid) &&
                                  !peek_token(context, TOKEN_newline)){
                                next_token_raw(context);
                            }
                            if(peek_token(context, TOKEN_newline)){
                                report_error(context, get_current_token(context), "Newline in system include path.");
                                goto end;
                            }
                            if(peek_token(context, TOKEN_newline)){
                                report_error(context, get_current_token(context), "File ended during system include path.");
                                goto end;
                            }
                            
                            struct token *bigger = next_token_raw(context);
                            
                            // :smaller_bigger_and_system_includes
                            struct string file_name = create_string(smaller->string.data, bigger->string.data - smaller->string.data);
                            
                            assert(smaller->type == TOKEN_smaller && smaller->string.data);
                            assert(bigger->type  == TOKEN_bigger  && bigger->string.data);
                            
                            b32 found = false;
                            struct string found_location = zero_struct;
                            
                            new_absolute_file_path = create_string(0, 0);
                            
                            for(struct string_list_node *node = globals.system_include_directories.list.first; node; node = node->next){
                                struct string path = concatinate_file_paths(&context->scratch, node->string, file_name);
                                struct os_file dummy = os_load_file((char *)path.data, 0, 0);
                                if(!dummy.file_does_not_exist){
                                    if(found){
                                        report_warning(context, smaller, "Cannot distinguish between the files '%.*s' and '%.*s'.", found_location.size, found_location.data, path.size, path.data);
                                    }
                                    
                                    found = true;
                                    new_absolute_file_path = path;
                                    found_location = path;
                                }
                            }
                            
                            if(!found){
                                report_error(context, smaller, "'%.*s' system include file not found.\n", file_name.size, file_name.data);
                                goto end;
                            }
                        }else{
                            report_error(context, get_current_token(context), "Expected either \"\"-include or <>-include after '#include'");
                            goto end;
                        }
                        
                        b32 found_pragma_once = false;
                        for(struct pragma_once_list_node *once = context->pragma_once_file_list.first; once; once = once->next){
                            if(string_match(once->file->absolute_file_path, new_absolute_file_path)){
                                found_pragma_once = true;
                                break;
                            }
                        }
                        
                        if(!found_pragma_once){
                            // copy the absolute file path into stable memory
                            new_absolute_file_path = push_string_copy(context->arena, new_absolute_file_path);
                            
                            //print("including file '%.*s'", new_absolute_file_path.amount, new_absolute_file_path.data);
                            
                            eat_whitespace_and_comments(context);
                            if(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                                report_warning(context, get_current_token(context), "Junk after '#include'.");
                            }
                            
                            while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                                next_token_raw(context);
                            }
                            
                            // @cleanup: think about self inclusion, maybe make a max inclusion depth...
                            struct token_marker directive_end = get_current_token_marker(context);
                            
                            struct token_bucket_array file_bucket_array = tokenize_file_raw(context, new_absolute_file_path, is_system_include);
                            replace_token_range(context, directive_begin, directive_end, file_bucket_array);
                            
                            apply_token_marker(context, make_token_marker(file_bucket_array.first, 0));
                            continue; // continue so we do not clear the '#directive' range below
                        }
                    }else if(directive->value == globals.keyword_error){
                        // @note: this whole block is contained inside of an if that checks, that we are not
                        //        '#if 0'ed out.
                        struct string_list error_list = zero_struct;
                        while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){                            
                            struct string token_string = push_token_string(context, next_token_raw(context), true);
                            string_list_postfix(&error_list, &context->scratch, token_string);
                        }
                        
                        struct string flattend = string_list_flatten(error_list, &context->scratch);
                        
                        report_error(context, directive, "'#error' detected: '%.*s'", flattend.size, flattend.data);
                        goto end;
                    }else if(directive->value == globals.keyword_pragma){
                        eat_whitespace_and_comments(context);
                        
                        struct token *pragma_directive = expect_token_raw(context, TOKEN_identifier);
                        if(pragma_directive){
                            if(pragma_directive->value == globals.pragma_once){
                                struct pragma_once_list_node *node = push_struct(&context->scratch, struct pragma_once_list_node);
                                node->file = pragma_directive->file;
                                
                                sll_push_back(context->pragma_once_file_list, node);
                            }else{
#if !SKIP_pragmas
                                report_warning(context, pragma_directive, "Unsupported pragma ignored.");
#endif
                            }
                        }
                        
                        while(!peek_token(context, TOKEN_newline) && !peek_token(context, TOKEN_invalid)){
                            next_token_raw(context);
                        }
                    }else{
                        report_error(context, directive, "Invalid preprocessor directive.");
                        goto end;
                    }
                }
                
            }else if(peek_token_eat_raw(context, TOKEN_newline)){
                // do nothing
            }else{
                report_error(context, get_current_token(context), "Expected a newline or a preprocessor directive after '#'.");
                goto end;
            }
            
            // delete the directive.
            struct token_marker directive_end = get_current_token_marker(context);
            struct token_bucket_array replace_with = zero_struct;
            struct token_bucket *new_current = replace_token_range(context, directive_begin, directive_end, replace_with);
            apply_token_marker(context, make_token_marker(new_current, 0));
            
            continue;
        }
        got_newline = false;
        
        if(!static_if_stack.first || static_if_stack.first->is_true){
            maybe_expand_current_token_and_eat(context, absolute_file_path);
        }else{
            // no need to expand the token, if we are in a '#if 0' block.
            next_token_raw(context);
        }
    }
    
    if(static_if_stack.first){
        report_error(context, static_if_stack.first->token, "'#if' without matching '#endif'\n");
    }
    
    end:;
    return context->tokens;
}

