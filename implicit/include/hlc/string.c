
#pragma compilation_unit("core.c")
#pragma compilation_unit("../runtime.c")

int u8_is_whitespace(u8 c){
    return (c ==  '\v') || (c ==  '\t') || (c ==  '\f') || (c ==  ' ');
}

struct string{
    union{
        char *data;
        char *memory;
    };
    union{
        u64 size;
        u64 length;
        u64 amount;
    };
};

int string_match(struct string a, struct string b){
    if(a.size != b.size) return 0;
    
    return memcmp(a.data, b.data, a.size) == 0;
}

struct string string_eat_front(struct string *string, u64 amount_to_eat){
    struct string front = { .data = string->data, .size = amount_to_eat };
    string->data += amount_to_eat;
    string->size -= amount_to_eat;
    return front;
}

struct string string_eat_line(struct string *string){
    
    u64 index = 0;
    for(; index < (string->size - 1); index++){
        if(string->data[index] == '\n') break;
    }
    
    return string_eat_front(string, index + 1);
}

struct string string_eat_whitespace(struct string *string){
    
    u64 index = 0;
    for(; index < string->length; index++){
        if(!u8_is_whitespace(string->data[index])) break;
    }
    
    return string_eat_front(string, index);
}

struct string string_eat_until_whitespace(struct string *string){
    
    u64 index = 0;
    for(; index < string->length; index++){
        if(u8_is_whitespace(string->data[index])) break;
    }
    
    return string_eat_front(string, index);
}

