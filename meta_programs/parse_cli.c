
// 
// Command line interface parsing code generator.
// This works based of `hlc.cli` and produces a structure
// `struct cli_options` and a function 
//    struct cli_options cli_parse_options(struct memory_arena *arena, int argc, char *argv[]);
// 
// The .cli file has the following syntax:
// 
// ```
// /option <arg> | /alias <arg> - Short description of the option.
// 
// Multi line long description of the argument.
// 
// /next_option [arg] - This is the next option that has an optional argument.
// ```
// From this, the following should be allowed:
// 
// hlc /option asdf
// hlc -alias:asdf
// hlc --option=asdf
// hlc /next_option=asdf
// hlc --nextoption
// hlc /NeXtOpTioN:asdf
// 
// Short options (<=3 characters) are case-sensative. (o, I, D, Fe, c, LD, W, MT)
// And allow specifying arguments without a separator:
//     hlc -I. -D_DEBUG /otest
// 
// 
// Furthermore, `hlc --help` should display:
// ```
//    <...>
//    -option <arg>         | Short description of the option.
//    -next_option [arg]    | This is the next option that has an optional argument.
//    <...>
// 
// And `hlc --help option` should display the long description.
// 
// Lastly, we allow Categories in the `hlc.cli` file.
// Categories are denotet by `# <Category-name>` and `hlc --help` displays the common options.
// Other options can be displayed using for example `hlc --help hidden`.
// 
// Currently, options can take at most one argument.
//                                                             - Pascal Beyer 13.10.2024

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;


typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef float f32;
typedef double f64;

#define array_count(a) (sizeof(a) / sizeof(*(a)))

#define true  1
#define false 0
#define null ((void *)0)

#if __HLC__
__declspec(printlike)
#endif
int print(char *format, ...){
    va_list va;
    va_start(va, format);
    int ret = vprintf(format, va);
    va_end(va);
    
    fflush(stdout); // I always want my prints to flush, but you can remove this if you don't :)
    
    return ret;
}

struct string{
    union{
        u64 size;
        u64 length;
        u64 amount;
    };
    union{
        char *data;
        char *memory;
    };
};

#define string(a) ((struct string){.data = a, .amount = (sizeof(a) - 1)})
#define const_string(a) {.data = a, .amount = (sizeof(a) - 1)}

struct string create_string(char *data, u64 size){
    struct string ret;
    ret.data = data;
    ret.size = size;
    return ret;
}

int string_match(struct string a, struct string b){
    if(a.size != b.size) return 0;
    
    return memcmp(a.data, b.data, a.size) == 0;
}

struct string string_eat_front(struct string *string, u64 amount_to_eat){
    struct string front = create_string(string->data, amount_to_eat);
    string->data += amount_to_eat;
    string->size -= amount_to_eat;
    return front;
}

struct string string_eat_line(struct string *string){
    if(!string->size) return *string;
    
    u64 index = 0;
    for(; index < string->size-1; index++){
        if(string->data[index] == '\n') break;
    }
    
    return string_eat_front(string, index + 1);
}

struct string string_eat_whitespace(struct string *string){
    
    u64 index = 0;
    for(; index < string->length; index++){
        if(string->data[index] != ' ') break;
    }
    
    return string_eat_front(string, index);
}

struct string string_eat_until_whitespace(struct string *string){
    
    u64 index = 0;
    for(; index < string->length; index++){
        if(string->data[index] == ' ') break;
    }
    
    return string_eat_front(string, index);
}

struct string string_eat_until_character(struct string *a, u8 eat_until, int eat_delimiter){
    u64 i = 0;
    for(; i < a->amount; i++){
        if(a->data[i] == eat_until) break;
    }
    
    u64 eat_extension = eat_delimiter && (i + eat_delimiter <= a->amount);
    
    struct string ret = create_string(a->data, i + eat_extension);
    string_eat_front(a, i + eat_extension);
    return ret;
}

struct string string_eat_characters_front(struct string *string, char *characters){
    u64 index = 0;
    for(; index < string->size; index++){
        char *s = characters;
        while(*s && *s != string->data[index]){
            s++;
        }
        if(!*s) break;
    }
    return string_eat_front(string, index);
}

struct string string_eat_until_characters_front(struct string *string, char *characters){
    u64 index = 0;
    for(; index < string->size; index++){
        char *s = characters;
        while(*s && *s != string->data[index]){
            s++;
        }
        if(*s) break;
    }
    return string_eat_front(string, index);
}

void string_strip_whitespace(struct string *string){
    
    while(string->size && string->data[string->size-1] == ' '){
        string->size -= 1;
    }
    
    while(string->size && string->data[0] == ' '){
        string->size -= 1;
        string->data += 1;
    }
}

int string_front_match(struct string a, char *c_string){
    u8 *it = (u8 *)c_string;
    for(u64 i = 0; i < a.amount; i++, it++){
        if(*it == 0) return true;
        if(a.data[i] != *it) return false;
    }
    
    return (*it == 0);
}


u8 u8_parse_hex_to_binary(u8 hex){
    if('9' >= hex && hex >= '0') return hex - '0';
    if('f' >= hex && hex >= 'a') return hex - 'a' + 10;
    if('F' >= hex && hex >= 'A') return hex - 'A' + 10;
    return 0;
}

// @cleanup: this is about the most lazy version
u64 string_to_u64(struct string str, int *_out_success){
    
    u64 ret = 0;
    if(str.size > 2 && str.data[0] == '0' && (str.data[1]|32) == 'x'){
        for(u64 i = 0; i < str.length; i++){
            ret *= 16;
            ret += u8_parse_hex_to_binary(str.data[i]);
        }
    }else{
        for(u64 i = 0; i < str.length; i++){
            u8 val = str.data[i] - '0';
            if(val > 10){
                *_out_success = false;
                return 0;
            }
            ret = ret*10 + val;
        }
    }
    return ret;
}

u64 string_djb2_hash(struct string str){
    
    u64 hash = 5381;
    for(u64 i = 0; str.size > i; i++){
        hash = (hash << 5) + hash + str.data[i];
    }
    return hash;
}

u64 u64_round_up_to_next_power_of_two(u64 v){
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    v++;
    return v;
}

char *CAPITALIZE(struct string string){
    static char buffer[0x100];
    // Capialize!
    for(u64 index = 0; index < string.size && index < sizeof(buffer); index++){
        buffer[index] = (string.data[index] & ~32);
    }
    buffer[(string.size < sizeof(buffer)) ? string.size : sizeof(buffer)-1] = 0;
    return buffer;
}

struct string_list_node{
    struct string_list_node *next;
    struct string string;
};

struct string string_from_cstring(char *data){
    struct string ret;
    ret.data   = data;
    ret.length = strlen(data);
    return ret;
}


struct string load_file(char *file_name){
    struct string ret = {0};
    
    FILE *handle = fopen(file_name, "rb");
    
    if(!handle){
        print("could not fopen '%s'\n", file_name);
        return ret;
    }
    
    fseek(handle, 0, SEEK_END);
    int size_or_minus_one = ftell(handle);
    if(size_or_minus_one < 0) return ret;
    fseek(handle, 0, SEEK_SET);
    
    size_t size = (size_t)size_or_minus_one;
    
    char *memory = malloc(size + 1);
    memory[size] = 0;
    
    fread(memory, 1, size, handle);
    
    fclose(handle);
    
    ret.memory = memory;
    ret.size   = size;
    
    return ret;
}

int main(int argc, char *argv[]){
    
    if(argc != 2){
        print("Usage: %s <.cli>\n", argv[0]);
        return 0;
    }
    
    char *cli_filename = argv[1];
    struct string file = load_file(cli_filename);
    if(!file.data) return 1;
    
    struct string line = string_eat_line(&file);
    
    struct option{
        struct option *next;
        
        struct string option_name;
        struct string_list_node *option_aliases;
        struct string option_argument;
        int is_optional_argument;
        enum option_argument_type{
            CLI_ARGUMENT_TYPE_none,
            CLI_ARGUMENT_TYPE_string,
            CLI_ARGUMENT_TYPE_u64, 
            CLI_ARGUMENT_TYPE_option,
            CLI_ARGUMENT_TYPE_warning,
            CLI_ARGUMENT_TYPE_enum,
            CLI_ARGUMENT_TYPE_directory,
            CLI_ARGUMENT_TYPE_directory_list,
            CLI_ARGUMENT_TYPE_string_list,
        } option_argument_type;
        
        
        struct string short_description;
        struct string long_description;
        
        struct enum_member{
            struct enum_member *next;
            
            struct string name;
            u64 value;
            struct string short_description;
            struct string long_description;
        } *enum_members;
        
    } *options = 0;
    
    static char *option_argument_type_strings[] = {
        [CLI_ARGUMENT_TYPE_none] = "CLI_ARGUMENT_TYPE_none",
        [CLI_ARGUMENT_TYPE_string] = "CLI_ARGUMENT_TYPE_string",
        [CLI_ARGUMENT_TYPE_u64] = "CLI_ARGUMENT_TYPE_u64", 
        [CLI_ARGUMENT_TYPE_option] = "CLI_ARGUMENT_TYPE_option",
        [CLI_ARGUMENT_TYPE_warning] = "CLI_ARGUMENT_TYPE_warning",
        [CLI_ARGUMENT_TYPE_enum] = "CLI_ARGUMENT_TYPE_enum",
        [CLI_ARGUMENT_TYPE_directory] = "CLI_ARGUMENT_TYPE_directory",
        [CLI_ARGUMENT_TYPE_directory_list] = "CLI_ARGUMENT_TYPE_directory_list",
        [CLI_ARGUMENT_TYPE_string_list] = "CLI_ARGUMENT_TYPE_string_list",
    };
    
    while(file.size){
        
        // @note: For now we skip Categories (e.g.: # Common / # Hidden)
        if(line.data[0] != '/'){
            line = string_eat_line(&file);
            continue;
        }
        
        struct string option_string = string_eat_until_character(&line, /*delimiter*/'-', /*eat delimiter*/0);
        if(line.size) string_eat_front(&line, 1); // eat the '-' we don't want it in either.
        string_eat_whitespace(&line);
        struct string short_description = line;
        while(short_description.size && (short_description.data[short_description.size-1] == '\r' || short_description.data[short_description.size-1] == '\n')){
            short_description.size -= 1;
        }
        string_strip_whitespace(&short_description);
        
        // 
        // Find the first non-empty line.
        // 
        struct string long_description;
        do{
            long_description = string_eat_line(&file);
            string_eat_characters_front(&long_description, "\r\n");
        }while(!long_description.size && file.size);
        
        if(long_description.data[0] == '/'){
            // There is no "long_description".
            line = long_description;
            long_description = (struct string){0};
        }else{
            
            // 
            // Find the next option.
            // 
            while(file.size && file.data[0] != /*start of next option_string*/'/'){
                line = string_eat_line(&file);
            }
            
            long_description.size = line.data - long_description.data;
        }    
        
        // 
        // Parse the option string:
        //     1) Necessary arguments: /base <u64>
        //     2) Optional arguments: /help [option]
        //     3) Argument types:
        //          * Integers: <u64>, etc
        //          * Strings: (default) <path>, ...
        //          * Enumerations: <enum>
        //          * Option (this is only for -help)
        // 
        // Option syntax:
        //      /option <argument> | /alias <argument>
        //      /option [argument]
        // 
        
        struct string main_option_name = {0};
        struct string_list_node *option_aliases = 0;
        struct string option_argument = {0};
        int is_optional_argument = -1;
        enum option_argument_type option_argument_type = CLI_ARGUMENT_TYPE_none;
        
        for(struct string it = option_string; it.size; ){
            struct string option = string_eat_until_character(&it, '|', /*eat_delimiter*/0);
            if(it.size) string_eat_front(&it, 1);
            string_eat_whitespace(&it);
            
            string_strip_whitespace(&option);
            
            if(it.size && it.data[0] != '/'){
                print("Error after option '%.*s' expected another option starting with '/' got '%c'\n", option.size, option.data, it.data[0]);
                return 1;
            }
            
            struct string option_name = string_eat_until_characters_front(&option, "[<");
            string_strip_whitespace(&option_name);
            string_eat_front(&option_name, 1); // eat the '/'.
            
            int is_initial_option = !option_aliases;
            
            if(is_initial_option){
                main_option_name = option_name;
            }
            
            struct string_list_node *new_alias = malloc(sizeof(struct string_list_node));
            new_alias->string = option_name;
            new_alias->next = option_aliases;
            option_aliases = new_alias;
            
            if(option.size){
                char open_bracket = option.data[0];
                int is_optional = (open_bracket == '[');
                char close_bracket = is_optional ? ']' : '>';
                
                string_eat_front(&option, 1); // eat the bracket.
                
                
                // Allow something like -D <<name>[=<text>]:string-list>, to give -D <name>[=<text>] of type string list.
                struct string argument_name = {0};
                struct string argument = {0};
                
                int level = 1;
                for(u32 index = 0; index < option.size; index++){
                    if(option.data[index] == open_bracket) level += 1;
                    if(option.data[index] == close_bracket) {
                        level -= 1;
                        if(level == 0){
                            argument = string_eat_front(&option, index);
                            break;
                        }
                    }
                    
                    if((level == 1) && option.data[index] == ':'){
                        argument_name = string_eat_front(&option, index);
                        string_eat_front(&option, 1); // eat the ':'
                        index = (u32)-1;
                    }
                }
                
                if(!argument.size){
                    print("Error after option '/%.*s%.*s', expected closing '%c'.\n", option_name.size, option_name.data, argument.size, argument.data, close_bracket);
                    return 1;
                }
                
                if(!argument_name.size) argument_name = argument;
                
                enum option_argument_type type;
                
                if(string_match(argument, string("u64"))){
                    type = CLI_ARGUMENT_TYPE_u64;
                }else if(string_match(argument, string("option"))){
                    type = CLI_ARGUMENT_TYPE_option;
                }else if(string_match(argument, string("warning"))){
                    type = CLI_ARGUMENT_TYPE_warning;
                }else if(string_match(argument, string("enum"))){
                    type = CLI_ARGUMENT_TYPE_enum;
                }else if(string_match(argument, string("path")) || string_match(argument, string("name"))){
                    type = CLI_ARGUMENT_TYPE_string;
                }else if(string_match(argument, string("dir"))){
                    type = CLI_ARGUMENT_TYPE_directory;
                }else if(string_match(argument, string("dir_list"))){
                    type = CLI_ARGUMENT_TYPE_directory_list;
                }else if(string_match(argument, string("string_list"))){
                    type = CLI_ARGUMENT_TYPE_string_list;
                }else{
                    print("Error: Unhandled argument type '%.*s' in option '%.*s'\n", argument.size, argument.data, option_name.size, option_name.data);
                    return 1;
                }
                
                if(!is_initial_option){
                    // We already had an option, check that the arguments are correct.
                    
                    if(is_optional_argument != is_optional || option_argument_type != type){
                        print("Error: Argument of alias '%.*s' mismatches initial argument of option '%.*s'.\n", option_name.size, option_string.data, main_option_name.size, main_option_name.data); 
                        return 1;
                    }
                }
                
                is_optional_argument = is_optional; 
                option_argument_type = type;
                option_argument = argument_name;
            }
        }
        
        struct enum_member *enum_members = 0;
        if(option_argument_type == CLI_ARGUMENT_TYPE_enum){
            // 
            // Scan the long description line by line for a line starting with '->'.
            // 
            
            struct string it = long_description;
            while(it.size && !string_front_match(it, "->")) string_eat_line(&it);
            
            // Patch up the long_description.size
            long_description.size = it.data - long_description.data;
            
            if(!it.size){
                print("Error: Expected at least one member in option '%.*s' for argument '%.*s' of type 'enum'.\n", main_option_name.size, main_option_name.data, option_argument.size, option_argument.data);
                return 1;
            }
            
            u64 last_enum_number = 0;
            
            do{
                // -> enum_entry (number) - Short description.
                // 
                // Long description.
                string_eat_front(&it, 2); // ->
                
                struct string enum_name = string_eat_until_characters_front(&it, "(-\n");
                string_strip_whitespace(&enum_name);
                
                if(it.size && it.data[0] == '('){
                    struct string number_string = string_eat_until_character(&it, ')', /*eat_delimiter*/1);
                    number_string.size -= 2;
                    number_string.data += 1; // strip the parens.
                    
                    int success = 1;
                    last_enum_number = string_to_u64(number_string, &success);
                    if(!success){
                        print("Failed to parse number argument '%.*s' of enum member '%.*s' in option '%.*s'.\n", number_string.size, number_string.data, enum_name.size, enum_name.data, main_option_name.size, main_option_name.data);
                        return 1;
                    }
                    
                    string_eat_whitespace(&it);
                }else{
                    last_enum_number += 1;
                }
                
                if(!it.size || it.data[0] != '-'){
                    print("Error: Expected a short description after enum entry '%.*s'.", enum_name.size, enum_name.data);
                    return 1;
                }
                string_eat_front(&it, 1);
                string_eat_whitespace(&it);
                
                struct string short_enum_description = it;
                string_eat_line(&it);
                short_enum_description.size = it.data - short_enum_description.data;
                while(short_enum_description.size && (short_enum_description.data[short_enum_description.size-1] == '\r' || short_enum_description.data[short_enum_description.size-1] == '\n')){
                    short_enum_description.size -= 1;
                }
                
                struct string long_enum_description = it;
                while(it.size && !string_front_match(it, "->")) string_eat_line(&it);
                
                long_enum_description.size = it.data - long_enum_description.data;
                
                struct enum_member *new_enum_member = malloc(sizeof(*new_enum_member));
                new_enum_member->name = enum_name;
                new_enum_member->value = last_enum_number;
                new_enum_member->short_description = short_enum_description;
                new_enum_member->long_description = long_enum_description;
                
                new_enum_member->next = enum_members;
                enum_members = new_enum_member;
            }while(it.size);
        }
        
        struct option *new_option = malloc(sizeof(struct option));
        new_option->option_name = main_option_name;
        new_option->option_aliases = option_aliases;
        new_option->option_argument = option_argument;
        new_option->is_optional_argument = is_optional_argument;
        new_option->option_argument_type = option_argument_type;
        new_option->short_description = short_description;
        new_option->long_description = long_description;
        new_option->enum_members = enum_members;
        
        new_option->next = options;
        options = new_option;
    }
    
    // 
    // Turn around all of the options and all of their aliases.
    // Such that they are in the order specified in the file.
    // 
    u64 amount_of_options_counting_aliases = 0;
    {
        struct option *new_list = 0;
        for(struct option *option = options, *next_option = 0; option; option = next_option){
            next_option = option->next;
            
            struct string_list_node *new_aliases = 0;
            for(struct string_list_node *alias = option->option_aliases, *next_alias = 0; alias; alias = next_alias){
                next_alias = alias->next;
                
                alias->next = new_aliases;
                new_aliases = alias;
                
                amount_of_options_counting_aliases++;
            }
            option->option_aliases = new_aliases;
            
            struct enum_member *new_enum_members = 0;
            for(struct enum_member *enum_member = option->enum_members, *next_enum_member = 0; enum_member; enum_member = next_enum_member){
                next_enum_member = enum_member->next;
                
                enum_member->next = new_enum_members;
                new_enum_members = enum_member;
            }
            option->enum_members = new_enum_members;
            
            option->next = new_list;
            new_list = option;
        }
        options = new_list;
    }
    
    
    // 
    // Write out a header comment.
    // 
    print("//\n");
    print("// DO NOT EDIT THIS FILE BY HAND!\n");
    print("//\n");
    print("// This file is auto-generated from `hlc.cli` using `parse_cli.c`.\n");
    print("// It handles parsing the command line arguments into a `cli_options` structure.\n");
    print("// The `main` function initiate this by calling `cli_parse_options`.\n");
    print("// For an explanation of what the command line parsing allows see `parse_cli.c`.\n");
    print("//\n\n");
    
    // 
    // Write out the option_kind enumeration.
    // 
    print("enum cli_option_kind{\n");
    print("    CLI_OPTION_none,\n");
    print("\n");
    for(struct option *option = options; option; option = option->next){
        print("    CLI_OPTION_%.*s,\n", option->option_name.size, option->option_name.data);
    }
    print("\n");
    print("    CLI_OPTION_count,\n");
    print("};\n\n");
    
    // 
    // Write out the cli_argument_type enumeration.
    // 
    print("enum cli_argument_type{\n");
    for(u64 index = 0; index < array_count(option_argument_type_strings); index++){
        print("    %s,\n", option_argument_type_strings[index]);
    }
    print("};\n\n");
    
    
    // 
    // Build the option table.
    // And write it out at the same time.
    // 
    u64 option_table_capacity = u64_round_up_to_next_power_of_two(amount_of_options_counting_aliases + (amount_of_options_counting_aliases/2));
    
    struct string *option_table = calloc(sizeof(*option_table), option_table_capacity);
    
    print("struct cli_option_hash_table_entry{\n");
    print("    struct string canonicalized_name;\n");
    print("    enum cli_argument_type argument_type;\n");
    print("    enum cli_option_kind option_kind;\n");
    print("    int argument_is_optional;\n");
    print("} cli_option_hash_table[0x%llx] = {\n", option_table_capacity);
    
    for(struct option *option = options; option; option = option->next){
        for(struct string_list_node *alias = option->option_aliases; alias; alias = alias->next){
            char *canonicalized_name_data = malloc(alias->string.size + 1);
            u64 canonicalized_name_size = 0;
            
            struct string option_name = alias->string;
            for(u64 index = 0; index < option_name.size; index++){
                char c = option_name.data[index];
                if(c == '-' || c == '_') continue;
                canonicalized_name_data[canonicalized_name_size++] = (c|32);
            }
            
            struct string canonicalized_name = {.data = canonicalized_name_data, .size = canonicalized_name_size};
            u64 hash = string_djb2_hash(canonicalized_name);
            for(u64 index = 0; index < option_table_capacity; index++){
                u64 hash_index = (hash + index) & (option_table_capacity - 1);
                
                if(!option_table[hash_index].data){
                    option_table[hash_index] = canonicalized_name;
                    
                    char *argument_type_string = option_argument_type_strings[option->option_argument_type];
                    print("    [%llu] = {{%llu, (u8 *)\"%.*s\"}, %s, CLI_OPTION_%.*s, %d},\n", hash_index, canonicalized_name.size, canonicalized_name.size, canonicalized_name.data, argument_type_string, option->option_name.size, option->option_name.data, option->is_optional_argument);
                    
                    break;
                }
                
                if(string_match(option_table[hash_index], canonicalized_name)){
                    print("Error: Canonicalization collision between to options. One of which is '%.*s' and the canoncialized version is '%.*s'.\n", option_name.size, option_name.data, canonicalized_name.size, canonicalized_name.data);
                    return 1;
                }
            }
        }
    }
    print("};\n\n");
    
    // 
    // Generate the structure for the cli_options.
    // 
    print("struct cli_options{\n");
    print("    \n");
    print("    struct string_list files; // Non-options. These are not checked, as they might contain wild-cards.\n");
    for(struct option *option = options; option; option = option->next){
        struct string argument_type = {0};
        
        switch(option->option_argument_type){
            case CLI_ARGUMENT_TYPE_none: argument_type = string("int"); break;
            case CLI_ARGUMENT_TYPE_u64:{ 
                argument_type = string("u64"); 
                print("    int %.*s_specified;\n", option->option_name.size, option->option_name.data);
            }break;
            
            case CLI_ARGUMENT_TYPE_option:  continue;
            case CLI_ARGUMENT_TYPE_warning: continue;
            
            case CLI_ARGUMENT_TYPE_enum:{
                
                struct string option_name = option->option_name;
                
                print("    enum %.*s{\n", option_name.size, option_name.data);
                for(struct enum_member *member = option->enum_members; member; member = member->next){
                    print("        %.*s_%.*s = %llu, // %.*s\n", option_name.size, CAPITALIZE(option_name), member->name.size, member->name.data, member->value, member->short_description.size, member->short_description.data);
                }
                print("    } %.*s; // %.*s\n", option_name.size, option_name.data, option->short_description.size, option->short_description.data);
                continue;
            }break;
            
            case CLI_ARGUMENT_TYPE_directory:
            case CLI_ARGUMENT_TYPE_string: argument_type = string("struct string"); break;
            
            case CLI_ARGUMENT_TYPE_directory_list: 
            case CLI_ARGUMENT_TYPE_string_list: argument_type = string("struct string_list"); break;
            
            
            default: assert(0);
        }
        
        print("    %.*s %.*s; // %.*s\n", argument_type.size, argument_type.data, option->option_name.size, option->option_name.data, option->short_description.size, option->short_description.data);
    }
    print("};\n\n");
    
    
    puts(
            "int cli_parse_options(struct cli_options *cli_options, struct memory_arena *arena, int argc, char *argv[]){\n"
            "    \n"
            "    for(int option_index = 1; option_index < argc; option_index++){\n"
            "        char *option_cstring = argv[option_index];\n"
            "        \n"
            "        //\n"
            "        // Allow /, -, and -- as option indicators.\n"
            "        //\n"
            "        int is_option = 0;\n"
            "        if(*option_cstring == '/'){\n"
            "            is_option = 1;\n"
            "            option_cstring++;\n"
            "        }else if(*option_cstring == '-'){\n"
            "            is_option = 1;\n"
            "            option_cstring++;\n"
            "            if(*option_cstring == '-') option_cstring++;\n"
            "        }\n"
            "        \n"
            "        if(!is_option){\n"
            "            string_list_postfix(&cli_options->files, arena, string_from_cstring(option_cstring));\n"
            "            continue;\n"
            "        }\n"
            "        \n"
            "        //\n"
            "        // Canonicalize the option.\n"
            "        //\n"
            "        u8  canonicalized_option_data[0x100];\n"
            "        u64 canonicalized_option_size = 0;\n"
            "        char *option_argument = null;\n"
            "        for(char *it = option_cstring; *it; it++){\n"
            "            if(*it == '-' || *it == '_') continue;\n"
            "            if(*it == '=' || *it == ':' || *it == ' '){\n"
            "                if(!option_argument) option_argument = it + 1;\n"
            "                break;\n"
            "            }\n"
            "            if(canonicalized_option_size == sizeof(canonicalized_option_data)){\n"
            "                print(\"Error: Option '%s' is too long. Command line options can be at most %lld bytes.\\n\", option_cstring, array_count(canonicalized_option_data));\n"
            "                return 0;\n"
            "            }\n"
            "            canonicalized_option_data[canonicalized_option_size++] = (*it|32);\n"
            "        }\n"
            "        \n"
            "        struct string canonicalized_option = {.data = canonicalized_option_data, .size = canonicalized_option_size};\n"
            "        \n"
            "        //\n"
            "        // Look up the canonicalized option in the hash table.\n"
            "        //\n"
            "        struct cli_option_hash_table_entry *option_hash_table_entry = 0;\n"
            "        u64 hash = string_djb2_hash(canonicalized_option);\n"
            "        for(u64 index = 0; index < array_count(cli_option_hash_table); index++){\n"
            "            u64 hash_index = (hash + index) & (array_count(cli_option_hash_table) - 1);\n"
            "            if(!cli_option_hash_table[hash_index].canonicalized_name.data) break;\n"
            "            if(string_match(cli_option_hash_table[hash_index].canonicalized_name, canonicalized_option)){\n"
            "                option_hash_table_entry = &cli_option_hash_table[hash_index];\n"
            "                break;\n"
            "            }\n"
            "        }\n"
            "        \n"
            "        //\n"
            "        // If we found the hash table entry, extract the argument_type and option_kind from it.\n"
            "        // Otherwise, we have to check the short options because they allow arguments without separator, like\n"
            "        //     -Iinclude\n"
            "        //\n"
            "        enum cli_argument_type option_argument_type = CLI_ARGUMENT_TYPE_none;\n"
            "        enum cli_option_kind   option_kind = CLI_OPTION_none;\n"
            "        int argument_is_optional = 0;\n"
            "        if(option_hash_table_entry){\n"
            "            option_argument_type = option_hash_table_entry->argument_type;\n"
            "            option_kind          = option_hash_table_entry->option_kind;\n"
            "            argument_is_optional = option_hash_table_entry->argument_is_optional;\n"
            "        }else{\n"
            "            // @note: Short options need to match case-sensatively.\n"
            "            struct string option = string_from_cstring(option_cstring);\n"
            "            "
            );
    
    {
        //
        // Generate all short-argument matches.
        //
        int first_short_argument = 1;
        for(struct option *option = options; option; option = option->next){
            if(option->option_argument_type == CLI_ARGUMENT_TYPE_none) continue;
            
            for(struct string_list_node *alias = option->option_aliases; alias; alias = alias->next){
                if(alias->string.size > 3) continue;
                char *else_string = "}else ";
                if(first_short_argument){
                    first_short_argument = 0;
                    else_string = "";
                }
                
                print("            %sif(string_front_match_eat(&option, \"%.*s\")){\n", else_string, alias->string.size, alias->string.data);
                print("                option_argument_type = %s;\n", option_argument_type_strings[option->option_argument_type]);
                print("                option_kind          = CLI_OPTION_%.*s;\n", option->option_name.size, option->option_name.data);
            }
        }
        print("            }\n");
        print("            \n");
    }
    puts(
            "            if(option_kind == CLI_OPTION_none){\n"
            "                print(\"Warning: Unknown command-line option '%s'.\\n\", option_cstring);\n" // @cleanup: Implement did you mean?
            "                continue;\n"
            "            }\n"
            "            \n"
            "            // We have eaten the option from 'option' so now its just the argument.\n"
            "            option_argument = (char *)option.data;\n"
            "            argument_is_optional = 0; // If we get here, there was an argument (otherwise the hash-table lookup would have worked), so we can safely set this to 0.\n"
            "        }\n"
            "        \n"
            "        //\n"
            "        // If the option needs an argument, make sure we have one.\n"
            "        //\n"
            "        if(!option_argument && (option_argument_type != CLI_ARGUMENT_TYPE_none) && !argument_is_optional){\n"
            "            option_argument = argv[++option_index];\n"
            "            if(!option_argument){ // @note: argv is null-pointer terminated.\n"
            "                print(\"Error: Expected argument after command line option '%s'.\\n\", option_cstring);\n"
            "                return 0;\n"
            "            }\n"
            "        }\n"
            "        \n"
            "        //\n"
            "        // Parse the argument.\n"
            "        //\n"
            "        struct string argument_string = option_argument ? string_from_cstring(option_argument) : (struct string){0};\n"
            "        u64 argument_as_u64 = 0;\n"
            "        \n"
            "        switch(option_argument_type){\n"
            "            case CLI_ARGUMENT_TYPE_none: break;\n"
            "            \n"
            "            case CLI_ARGUMENT_TYPE_string: break;\n"
            "            case CLI_ARGUMENT_TYPE_string_list: break;\n"
            "            case CLI_ARGUMENT_TYPE_enum: break;\n"
            "            case CLI_ARGUMENT_TYPE_option: break;\n"
            "            case CLI_ARGUMENT_TYPE_warning: break;\n"
            "            case CLI_ARGUMENT_TYPE_directory:\n"
            "            case CLI_ARGUMENT_TYPE_directory_list:{\n"
            "                if(!path_is_directory(option_argument)){\n"
            "                    print(\"Error: Argument '%s' of command line option '%s' must be a directory, but it is not.\\n\", option_argument, option_cstring);\n"
            "                    return 0;\n"
            "                }\n"
            "            }break;\n"
            "            case CLI_ARGUMENT_TYPE_u64:{\n"
            "                int success = true;\n"
            "                argument_as_u64 = string_to_u64(argument_string, &success);\n"
            "                if(!success){\n"
            "                    print(\"Error: Could not parse argument '%.*s' of option '%s' as a u64.\\n\", argument_string.size, argument_string.data, option_cstring);\n"
            "                    return 0;\n"
            "                }\n"
            "            }break;\n"
            "        }\n"
            "        \n"
            "        //\n"
            "        // We are ready to parse the command line option!\n"
            "        //\n"
            "        switch(option_kind){"
            );
    for(struct option *option = options; option; option = option->next){
        
        if(option->option_argument_type == CLI_ARGUMENT_TYPE_none){
            // This was a flag style argument.
            print("            case CLI_OPTION_%.*s: cli_options->%.*s = 1; break;\n", option->option_name.size, option->option_name.data, option->option_name.size, option->option_name.data);
            continue;
        }
        
        print("            \n");
        print("            case CLI_OPTION_%.*s:{\n", option->option_name.size, option->option_name.data);
        
        if(string_match(option->option_name, string("help"))){
            print("                not_implemented;\n");
            print("            }break;\n");
            continue;
        }
        
        switch(option->option_argument_type){
            case CLI_ARGUMENT_TYPE_option:{
                print("Error: Only the 'help' option should have argument type 'option' for now.\n");
                return 1;
            }break;
            
            case CLI_ARGUMENT_TYPE_warning:{
                // print("                not_implemented;\n"); @incomplete
            }break;
            case CLI_ARGUMENT_TYPE_enum:{
                for(struct enum_member *member = option->enum_members; member; member = member->next){
                    char *else_prefix = (member == option->enum_members) ? "" : "}else ";
                    print("                %sif(string_match(argument_string, string(\"%.*s\"))){\n", else_prefix, member->name.size, member->name.data);
                    print("                    cli_options->%.*s = %.*s_%.*s;\n", option->option_name.size, option->option_name.data, option->option_name.size, CAPITALIZE(option->option_name), member->name.size, member->name.data);
                }
                print("                }else{\n");
                puts("                    print(\"Error: Unhandled value '%.*s' for command line option '%s'.\\n\", argument_string.size, argument_string.data, option_cstring);");
                puts("                    return 0;");
                print("                }\n");
            }break;
            
            case CLI_ARGUMENT_TYPE_directory:
            case CLI_ARGUMENT_TYPE_string:{
                print("                cli_options->%.*s = argument_string;\n", option->option_name.size, option->option_name.data);
            }break;
            case CLI_ARGUMENT_TYPE_u64:{
                print("                cli_options->%.*s_specified = 1;\n", option->option_name.size, option->option_name.data);
                print("                cli_options->%.*s = argument_as_u64;\n", option->option_name.size, option->option_name.data);
            }break;
            
            case CLI_ARGUMENT_TYPE_string_list:
            case CLI_ARGUMENT_TYPE_directory_list:{
                print("                string_list_postfix(&cli_options->%.*s, arena, argument_string);\n", option->option_name.size, option->option_name.data);
            }break;
        }
        print("            }break;\n");
    }
    print("            case CLI_OPTION_count:\n");
    print("            case CLI_OPTION_none:\n");
    print("                invalid_code_path;\n");
    print("        }\n"); // switch terminator.
    
    print("    }\n\n"); // for loop terminator
    print("    return 1;\n");
    print("}\n");
}
