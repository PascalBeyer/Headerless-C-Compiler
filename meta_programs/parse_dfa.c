
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hlc/assert.h>
#include <hlc/memory_arena.h>
#include <hlc/string.h>

struct string load_file(char *file_name){
    struct string ret = {0};
    
    FILE *handle = fopen(file_name, "rb");
    
    if(!handle) return ret;
    
    fseek(handle, 0, SEEK_END);
    
    size_t size = _ftelli64(handle);
    
    if(size == -1) return ret; // @note: '-1' might be the worst error value....
    
    fseek(handle, 0, SEEK_SET);
    
    char *memory = malloc(size + 64);
    memset(memory + size, 0, 64);
    
    fread(memory, 1, size, handle);
    
    fclose(handle);
    
    ret.data = memory;
    ret.size = size;
    
    return ret;
}

struct character_range{
    u8 characters[256];
};

struct character_class{
    struct character_class *next;
    struct string string;
    u64 line;
    struct character_range range;
};

char escaped_character(char c){
    char character = 0;
    switch(c){ // @incomplete:
        case 't': character = '\t'; break;
        case 'v': character = '\v'; break;
        case 'f': character = '\f'; break;
        case 'r': character = '\r'; break;
        case 'n': character = '\n'; break;
        case '\'': character = '\''; break;
        case '"': character = '"'; break;
        case '\\': character = '\\'; break;
        default: assert(0);
    }
    return character;
}

struct string eat_identifier(struct string *string, u64 current_line){
    struct string identifier = {.data = string->data };
    
    while(string->size && u8_is_valid_in_ident(string->data[0])){
        string->size -= 1;
        string->data += 1;
    }
    
    if(string->data == identifier.data){
        print("Error in line %llu no state identifier!\n", current_line);
        exit(1);
    }
    
    identifier.size = string->data - identifier.data;
    return identifier;
}

u64 parse_one_number(struct string *string, u64 current_line){
    struct string it = *string;
    
    struct string number_string = {.data = it.data };
    
    while(it.size && it.data[0] != ' ' && it.data[0] != ',' && it.data[0] != '.' && it.data[0] != ']'){
        it.size -= 1;
        it.data += 1;
    }
    
    if(it.data == number_string.data){
        print("Error in line %llu no number!\n", current_line);
        exit(1);
    }
    number_string.size = it.data - number_string.data;
    
    u64 character;
    if(number_string.size >= 3 && number_string.data[0] == '\'' && number_string.data[number_string.size-1] == '\''){
        number_string.size -= 2;
        number_string.data += 1;
        
        if(number_string.size == 1){
            character = (u8)number_string.data[0];
        }else if(number_string.data[0] == '\\' && number_string.size == 2){
            character = (u8)escaped_character(number_string.data[1]);
        }else{
            print("Error in line %llu invalid character '%.*s' expected either single character ('c') or escaped character ('\t')!\n", current_line, number_string.size, number_string.data);
            exit(1);
        }
    }else{
        char *end = it.data; 
        character = strtoull(number_string.data, &end, 0);
        if(character >= 265 || end != it.data){
            print("Error in line %llu invalid number '%.*s' = %llu!\n", current_line, number_string.size, number_string.data, character);
            exit(1);
        }
    }
    
    string_eat_whitespace(&it);
    
    *string = it;
    return character;
}

struct character_range parse_character_range(struct string *string, u64 current_line){
    
    struct string it = *string;
    
    it.size -= 1;
    it.data += 1;
    
    struct character_range range = {0};
    
    while(it.size){
        
        string_eat_whitespace(&it);
        
        u64 character = parse_one_number(&it, current_line);
        
        if(it.size) switch(it.data[0]){
            case ',':{
                it.size -= 1;
                it.data += 1;
                
                range.characters[character] = 1;
                
                string_eat_whitespace(&it);
                if(it.size && it.data[0] == ']'){
                    it.size -= 1;
                    it.data += 1;
                    *string = it;
                    return range;
                }
            }break;
            
            case '.':{
                if(it.size < 3 || it.data[1] != '.' || it.data[2] != '.'){
                    print("Error in line %llu expected '...'\n", current_line);
                    exit(1);
                }
                it.size -= 3;
                it.data += 3;
                
                string_eat_whitespace(&it);
                
                u64 end_character = parse_one_number(&it, current_line);
                
                if(end_character < character){
                    print("Error in line %llu invalid range %llu! > %llu\n", current_line, character, end_character);
                    exit(1);
                }
                
                for(u64 index = character; index <= end_character; index++){
                    range.characters[index] = 1;
                }
                
                string_eat_whitespace(&it);
                if(it.size && it.data[0] == ']'){
                    it.size -= 1;
                    it.data += 1;
                    *string = it;
                    return range;
                }else if(it.size && it.data[0] == ','){
                    it.size -= 1;
                    it.data += 1;
                }
            }break;
            case ']':{
                it.size -= 1;
                it.data += 1;
                
                range.characters[character] = 1;
                
                *string = it;
                return range;
            }break;
        }
    }
    
    // Not sure, should be an error probably.
    
    return range;
}

struct character_class *parse_character_class(struct memory_arena *arena, u64 current_line, struct character_class *previous_classes, struct string *string){
    
    // 
    // [int ... int]
    // [int, int]
    // identifier
    // 
    // class ^ class
    // class v class
    // class \ class
    // 
    
    struct character_class *character_class = push_struct(arena, struct character_class);
    
    struct string it = *string;
    
    int operation = 'v';
    
    while(1){
        struct character_range range;
        
        if(it.size && it.data[0] == '['){
            range = parse_character_range(&it, current_line);
        }else if(it.size && it.data[0] == '"'){
            memset(&range, 0, sizeof(range));
            
            it.size -= 1;
            it.data += 1;
            while(it.size && it.data[0] != '"'){
                char character = 0;
                
                if(it.data[0] == '\\'){
                    it.size -= 1;
                    it.data += 1;
                    
                    if(it.size){
                        character = escaped_character(it.data[0]);
                    }else{
                        break;
                    }
                }else{
                    character = it.data[0];
                }
                
                it.size -= 1;
                it.data += 1;
                
                range.characters[character] = 1;
            }
            
            if(it.size){ // eat "
                it.size -= 1;
                it.data += 1;
            }
        }else{
            
            struct string identifier = {.data = it.data };
            
            while(it.size && u8_is_valid_in_ident(it.data[0])){
                it.size -= 1;
                it.data += 1;
            }
            
            if(it.data == identifier.data){
                print("Error in line %llu no identifier!\n", current_line);
                _exit(1);
            }
            
            identifier.size = it.data - identifier.data;
            
            int found = 0;
            
            for(struct character_class *test = previous_classes; test; test = test->next){
                if(string_match(test->string, identifier)){
                    range = test->range;
                    found = 1;
                    break;
                }
            }
            if(!found){
                print("Error in line %llu identifier '%.*s' not found!\n", current_line, identifier.size, identifier.data);
                _exit(1);
            }
        }
        
        if(operation == 'v'){
            for(int index = 0; index < 256; index++){
                character_class->range.characters[index] |= range.characters[index];
            }
        }else if(operation == '^'){
            for(int index = 0; index < 256; index++){
                character_class->range.characters[index] &= range.characters[index];
            }
        }else if(operation == '\\'){
            for(int index = 0; index < 256; index++){
                character_class->range.characters[index] = character_class->range.characters[index] && !range.characters[index];
            }
        }
        
        string_eat_whitespace(&it);
        
        if(it.size && it.data[0] == 'v'){
            operation = 'v';
        }else if(it.size && it.data[0] == '^'){
            operation = '^';
        }else if(it.size && it.data[0] == '\\'){
            operation = '\\';
        }else{
            break;
        }
        
        it.size -= 1;
        it.data += 1;
        
        string_eat_whitespace(&it);
    }
    
    
    *string = it;
    
    return character_class;
}

struct state{
    struct state *next_state;
    struct string name;
    
    struct string default_token_to_emit;
    struct string default_next_state;
    
    u64 index;
    struct {
        struct string next_state;
        struct string token_to_emit;
        int out;
        int pre_out;
    } state_table[256];
    u64 line;
};

int u8_is_valid_in_ident(u8 c){
    return (c == '_') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || (c == '$') || (c >= 128) || (c == '-');
}

u32 u32_round_up_to_next_power_of_two(u32 v){
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;
    return v;
}

int main(int argc, char *argv[]){
    
    if(argc != 2){
        print("usage: %s <.dfa-file>\n", argv[0]);
        return 1;
    }
    
    struct string file = load_file(argv[1]);
    if(!file.data){
        print("Error: Could not open %s\n", argv[1]);
        return 1;
    }
    
    struct memory_arena arena = create_memory_arena(/*size_to_reserve*/64 * 1024ull * 1024ull);
    
    struct state *current_state = 0;
    struct character_class *current_character_classes = 0;
    
    u64 current_line = 1;
    
    struct token_values{
        struct string kind;
        u64 value;
    } *tokens = 0;
    u64 token_count = 0;
    // struct string token_prefix = {0};
    
    while(file.size){
        
        // Handle comments.
        if(file.data[0] == '#'){
            while(file.size && file.data[0] != '\n'){
                file.size -= 1;
                file.data += 1;
            }
            
            if(file.data[0] == '\n'){
                file.size -= 1;
                file.data += 1;
                
                current_line += 1;
            }
            continue;
        }
        
        u64 whitespace_count = 0;
        
        while(file.size && file.data[0] == ' '){
            whitespace_count += 1;
            file.size -= 1;
            file.data += 1;
        }
        
        if(file.size && file.data[0] == '#') continue;
        
        if(file.size && file.data[0] == '\n'){
            file.size -= 1;
            file.data += 1;
            
            current_line += 1;
            continue;
        }
        
        if(whitespace_count == 0){
            // 
            // We are parsing a character or state definition.
            // 
            
            struct string identifier = {.data = file.data };
            
            while(file.size && u8_is_valid_in_ident(file.data[0])){
                file.size -= 1;
                file.data += 1;
            }
            
            if(file.data == identifier.data){
                print("Error in line %llu no identifier!\n", current_line);
                return 1;
            }
            
            identifier.size = file.data - identifier.data;
            
            string_eat_whitespace(&file);
            
            if(file.size && file.data[0] == ':'){
                file.size -= 1;
                file.data += 1;
                // 
                // State definition.
                // 
                
                struct state *state = push_struct(&arena, struct state);
                state->next_state = current_state;
                state->name = identifier;
                state->line = current_line;
                
                for(struct state *other = current_state; other; other = other->next_state){
                    if(string_match(other->name, identifier)){
                        print("Error in line %llu, redefinition of %.*s, previous definition was in line %llu!\n", current_line, identifier.size, identifier.data, other->line);
                        return 1;
                    }
                }
                
                current_state = state;
            }else if(file.size && file.data[0] == '='){
                // 
                // Character class definition.
                // 
                
                file.size -= 1;
                file.data += 1;
                string_eat_whitespace(&file);
                
                struct character_class *character_class = parse_character_class(&arena, current_line, current_character_classes, &file);
                character_class->next = current_character_classes;
                character_class->string = identifier;
                character_class->line = current_line;
                
                for(struct character_class *other = current_character_classes; other; other = other->next){
                    if(string_match(other->string, identifier)){
                        print("Error in line %llu, redefinition of %.*s, previous definition was in line %llu!\n", current_line, identifier.size, identifier.data, other->line);
                        return 1;
                    }
                }
                
                current_character_classes = character_class;
            }else if(file.size && file.data[0] == '{'){
                // 
                // Parse out values.
                // 
                file.size -= 1;
                file.data += 1;
                string_eat_whitespace(&file);
                
                u64 current_value = 0;
                
                // token_prefix = identifier;
                
                tokens = push_array(&arena, struct token_values, 0);
                
                while(file.size){
                    
                    string_eat_whitespace(&file);
                    if(file.size && file.data[0] == '\n'){
                        file.size -= 1;
                        file.data += 1;
                        current_line += 1;
                        continue;
                    }
                    
                    if(file.size && file.data[0] == '}'){
                        file.size -= 1;
                        file.data += 1;
                        break;
                    }
                    
                    struct string token_string = {
                        .data = file.data,
                    };
                    
                    while(file.size && u8_is_valid_in_ident(file.data[0])){
                        file.size -= 1;
                        file.data += 1;
                    }
                    
                    token_string.size = file.data - token_string.data;
                    
                    string_eat_whitespace(&file);
                    
                    u64 value = current_value;
                    
                    if(file.size && file.data[0] == '='){
                        file.size -= 1;
                        file.data += 1;
                        
                        string_eat_whitespace(&file);
                        value = parse_one_number(&file, current_line);
                        string_eat_whitespace(&file);
                    }
                    
                    struct token_values *token_value = push_struct(&arena, struct token_values);
                    token_value->value = value;
                    token_value->kind = token_string;
                    
                    current_value = value + 1;
                    
                    if(file.size && file.data[0] == ','){
                        file.size -= 1;
                        file.data += 1;
                    }else{
                        print("Syntax error in line %llu\n", current_line);
                        return 1;
                    }
                }
                
                token_count = push_array(&arena, struct token_values, 0) - tokens;
            }
        }else{
            // 
            // We are parsing a transition definition.
            // 
            
            if(!current_state){
                print("Error in line %llu, whitespace found, but have not defined state yet\n", current_line);
                return 1;
            }
            
            if(file.size > 2 && file.data[0] == '_' && (file.data[1] == ' ' || file.data[1] == '-')){
                // 
                // Default transition definition.
                // 
                file.size -= 1;
                file.data += 1;
                
                string_eat_whitespace(&file);
                
                if(file.size < 2 || file.data[0] != '-' || file.data[1] != '>'){
                    print("Error in line %llu, expected '->'\n", current_line);
                    return 1;
                }
                
                file.size -= 2;
                file.data += 2;
                
                string_eat_whitespace(&file);
                
                int is_include = 0;
                if(file.size && file.data[0] == '@'){
                    file.size -= 1;
                    file.data += 1;
                    is_include = 1;
                }
                
                struct string identifier = eat_identifier(&file, current_line);
                
                if(file.size && file.data[0] == ','){
                    file.size -= 1;
                    file.data += 1;
                    string_eat_whitespace(&file);
                    
                    current_state->default_token_to_emit = eat_identifier(&file, current_line);
                }
                
                if(is_include){
                    current_state->default_next_state = identifier;
                }else{
                    for(u32 index = 0; index < 256; index++){
                        if(!current_state->state_table[index].next_state.data){
                            current_state->state_table[index].next_state = identifier;
                        }
                    }
                }
                
            }else{
                // 
                // State transition.
                // 
                
                struct character_class *character_class = parse_character_class(&arena, current_line, current_character_classes, &file);
                
                string_eat_whitespace(&file);
                if(file.size < 2 || file.data[0] != '-' || file.data[1] != '>'){
                    print("Error in line %llu, expected '->'\n", current_line);
                    return 1;
                }
                
                file.size -= 2;
                file.data += 2;
                
                string_eat_whitespace(&file);
                
                struct string identifier = eat_identifier(&file, current_line);
                struct string token_to_emit = {};
                int out = 0;
                if(file.size && file.data[0] == ','){
                    file.size -= 1;
                    file.data += 1;
                    
                    string_eat_whitespace(&file);
                    token_to_emit = eat_identifier(&file, current_line);
                    
                    if(file.size && file.data[0] == ','){
                        file.size -= 1;
                        file.data += 1;
                        
                        string_eat_whitespace(&file);
                        struct string out_string = eat_identifier(&file, current_line);
                        
                        if(!string_match(out_string, string("out"))){
                            print("Error in line %llu, unknown '%.*s'\n", current_line, out_string.size, out_string.data);
                            return 1;
                        }
                        out = 1;
                    }
                }
                
                for(u32 index = 0; index < 256; index++){
                    if(character_class->range.characters[index]){
                        if(current_state->state_table[index].next_state.data){
                            print("Error in line %llu, state transition for character 0x%.2x (%c) redefined.\n", current_line, index, (char)index);
                            return 1;
                        }
                        
                        current_state->state_table[index].next_state = identifier;
                        current_state->state_table[index].token_to_emit = token_to_emit;
                        current_state->state_table[index].out = out;
                    }
                }
            }
        }
    }
    
    if(!tokens){
        print("Error: Could not find token definitions\n");
        return 1;
    }
    
    // 
    // Resolve default tokens.
    // 
    for(struct state *state = current_state; state; state = state->next_state){
        
        for(u32 index = 0; index < 256; index++){
            struct string transition_to = state->state_table[index].next_state;
            
            // This state is plugged in, in the next loop by an include.
            if(!transition_to.size) continue;
            
            if(!state->state_table[index].token_to_emit.size){
                state->state_table[index].token_to_emit = state->default_token_to_emit;
                if(!state->state_table[index].token_to_emit.size){
                    state->state_table[index].token_to_emit = string("invalid");
                }
            }
        }
    }
    
    // 
    // Resolve include transitions.
    // 
    for(struct state *state = current_state; state; state = state->next_state){
        struct string identifier = state->default_next_state;
        
        if(!identifier.size) continue; // State does not have an include transition.
        
        struct state *other = current_state;
        for(; other; other = other->next_state){
            if(string_match(other->name, identifier)){
                break;
            }
        }
        
        if(!other){
            print("Error in line %llu, unknown state %.*s, currently cannot @ transition to future states.\n", state->line, identifier.size, identifier.data);
            return 1;
        }
        
        for(u32 index = 0; index < 256; index++){
            if(!state->state_table[index].next_state.data){
                state->state_table[index].pre_out = 1;
                state->state_table[index].next_state = other->state_table[index].next_state;
                state->state_table[index].token_to_emit = other->state_table[index].token_to_emit;
                state->state_table[index].out = other->state_table[index].out;
                if(other->state_table[index].pre_out){
                    print("Error in line %llu, cannot @ transition to a state (%.*s) that also has an @ transition.\n", state->line, identifier.size, identifier.data);
                    return 1;
                }
            }
        }
    }
    
    // 
    // Validate the transitions.
    // 
    
    int error = 0;
    
    for(struct state *state = current_state; state; state = state->next_state){
        for(u32 index = 0; index < 256; index++){
            struct string to = state->state_table[index].next_state;
            
            if(to.size == 0){
                print("Error in line %llu, state '%.*s' does have transition for index %u (%c).\n", state->line, state->name.size, state->name.data, index, (char)index);
                error = 1;
                break;
            }
            
            int found = 0;
            
            for(struct state *other = current_state; other; other = other->next_state){
                if(string_match(to, other->name)){
                    found = 1;
                    break;
                }
            }
            
            if(!found){
                print("Error in line %llu, state '%.*s' does have transition to state '%.*s', but this state was not defined.\n", state->line, state->name.size, state->name.data, to.size, to.data);
                error = 1;
                break;
            }
            
            found = 0;
            
            struct string token_to_emit = state->state_table[index].token_to_emit;
            for(u64 token_index = 0; token_index < token_count; token_index++){
                if(string_match(token_to_emit, tokens[token_index].kind)){
                    found = 1;
                }
            }
            
            if(!found){
                print("Error in line %llu, state '%.*s' references undefined token '%.*s'.\n", state->line, state->name.size, state->name.data, token_to_emit.size, token_to_emit.data);
                error = 1;
                break;
            }
        }
    }
    
    if(error) return 1;
    
    // Invert the order of the state nodes.
    {
        struct state *new_state = 0;
        while(current_state){
            struct state *next_current = current_state->next_state;
            current_state->next_state = new_state;
            new_state = current_state;
            current_state = next_current;
        }
        current_state = new_state;
    }
    
    for(struct state *state = current_state; state; state = state->next_state){
        
        // Canonicalize all of the state names.
        for(u64 index = 0; index < state->name.size; index++){
            if(state->name.data[index] == '-') state->name.data[index] = '_';
        }
        
        for(u64 index = 0; index < 256; index++){
            struct string transition_to = state->state_table[index].next_state;
            for(u64 i = 0; i < transition_to.size; i++){
                if(transition_to.data[i] == '-') transition_to.data[i] = '_';
            }
        }
    }
    
    print("enum token_kind{\n");
    for(u64 index = 0; index < token_count; index++){
        print("    TOKEN_%.*s = %llu,\n", tokens[index].kind.size, tokens[index].kind.data, tokens[index].value);
    }
    print("};\n\n");
    
    u64 amount_of_states = 0;
    u64 max_size = 0;
    print("enum dfa_states{\n");
    for(struct state *state = current_state; state; state = state->next_state){
        state->index = amount_of_states;
        print("    DFA_STATE_%.*s = %llu,\n", state->name.size, state->name.data, amount_of_states);
        amount_of_states += 1;
        max_size = max(state->name.size, max_size);
    }
    print("    \n");
    print("    DFA_STATE_count = %u,\n", u32_round_up_to_next_power_of_two((u32)amount_of_states)); // make it so that it can use a shift instead of an imul.
    print("};\n\n");
    
    
    print("static u8 dfa[256][DFA_STATE_count] = {\n");
    
    for(u64 index = 0; index < 256; index++){
        print("    /*0x%.2x*/{", index);
        for(struct state *state = current_state; state; state = state->next_state){
            struct string transition_to = state->state_table[index].next_state;
            
            u32 next_state_index = 0;
            for(struct state *other = current_state; other; other = other->next_state){
                if(string_match(transition_to, other->name)){
                    next_state_index = (u32)other->index;
                    break;
                }
            }
            
            print("0x%.2x, ", next_state_index);
        }
        print("},\n");
    }
    print("};\n\n");
    
    
    print("static u8 dfa_info[256][DFA_STATE_count] = {\n");
    
    for(u64 index = 0; index < 256; index++){
        print("    /*0x%.2x*/{", index);
        for(struct state *state = current_state; state; state = state->next_state){
            struct string token_to_emit = state->state_table[index].token_to_emit;
            int out = state->state_table[index].out;
            int pre_out = state->state_table[index].pre_out;
            
            u32 token_value = 0;
            for(u32 token_index = 0; token_index < token_count; token_index++){
                if(string_match(tokens[token_index].kind, token_to_emit)){
                    token_value = (u32)tokens[token_index].value;
                    break;
                }
            }
            
            u32 packed = token_value | (pre_out << 7) | (out << 6);
            print("0x%.2x, ", packed);
        }
        print("},\n");
    }
    print("};\n\n");
}

