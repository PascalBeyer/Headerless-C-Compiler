//
// DO NOT EDIT THIS FILE BY HAND!
//
// This file is auto-generated from `hlc.cli` using `parse_cli.c`.
// It handles parsing the command line arguments into a `cli_options` structure.
// The `main` function initiate this by calling `cli_parse_options`.
// For an explanation of what the command line parsing allows see `parse_cli.c`.
//

enum cli_option_kind{
    CLI_OPTION_none,

    CLI_OPTION_help,
    CLI_OPTION_no_stdlib,
    CLI_OPTION_no_intrinsics,
    CLI_OPTION_no_premain,
    CLI_OPTION_no_debug,
    CLI_OPTION_no_dynamic_base,
    CLI_OPTION_image_base,
    CLI_OPTION_show_includes,
    CLI_OPTION_subsystem,
    CLI_OPTION_out,
    CLI_OPTION_entry,
    CLI_OPTION_no_entry,
    CLI_OPTION_dll,
    CLI_OPTION_obj,
    CLI_OPTION_thread_count,
    CLI_OPTION_I,
    CLI_OPTION_Wall,
    CLI_OPTION_Wnone,
    CLI_OPTION_W,
    CLI_OPTION_Wno,
    CLI_OPTION_no_discard,
    CLI_OPTION_dont_print_the_files,
    CLI_OPTION_seed,
    CLI_OPTION_report_warnings_in_system_includes,
    CLI_OPTION_test,

    CLI_OPTION_count,
};

enum cli_argument_type{
    CLI_ARGUMENT_TYPE_none,
    CLI_ARGUMENT_TYPE_string,
    CLI_ARGUMENT_TYPE_u64,
    CLI_ARGUMENT_TYPE_option,
    CLI_ARGUMENT_TYPE_warning,
    CLI_ARGUMENT_TYPE_enum,
    CLI_ARGUMENT_TYPE_directory,
    CLI_ARGUMENT_TYPE_directory_list,
};

struct cli_option_hash_table_entry{
    struct string canonicalized_name;
    enum cli_argument_type argument_type;
    enum cli_option_kind option_kind;
    int argument_is_optional;
} cli_option_hash_table[0x40] = {
    [46] = {{4, (u8 *)"help"}, CLI_ARGUMENT_TYPE_option, CLI_OPTION_help, 1},
    [13] = {{1, (u8 *)"h"}, CLI_ARGUMENT_TYPE_option, CLI_OPTION_help, 1},
    [36] = {{1, (u8 *)"?"}, CLI_ARGUMENT_TYPE_option, CLI_OPTION_help, 1},
    [37] = {{8, (u8 *)"nostdlib"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_stdlib, -1},
    [40] = {{12, (u8 *)"nointrinsics"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_intrinsics, -1},
    [14] = {{9, (u8 *)"nopremain"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_premain, -1},
    [41] = {{7, (u8 *)"nodebug"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_debug, -1},
    [34] = {{13, (u8 *)"nodynamicbase"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_dynamic_base, -1},
    [35] = {{9, (u8 *)"imagebase"}, CLI_ARGUMENT_TYPE_u64, CLI_OPTION_image_base, 0},
    [29] = {{12, (u8 *)"showincludes"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_show_includes, -1},
    [20] = {{9, (u8 *)"subsystem"}, CLI_ARGUMENT_TYPE_enum, CLI_OPTION_subsystem, 0},
    [30] = {{3, (u8 *)"out"}, CLI_ARGUMENT_TYPE_string, CLI_OPTION_out, 0},
    [21] = {{1, (u8 *)"o"}, CLI_ARGUMENT_TYPE_string, CLI_OPTION_out, 0},
    [16] = {{2, (u8 *)"fe"}, CLI_ARGUMENT_TYPE_string, CLI_OPTION_out, 0},
    [23] = {{5, (u8 *)"entry"}, CLI_ARGUMENT_TYPE_string, CLI_OPTION_entry, 0},
    [22] = {{7, (u8 *)"noentry"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_entry, -1},
    [33] = {{3, (u8 *)"dll"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_dll, -1},
    [24] = {{2, (u8 *)"ld"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_dll, -1},
    [32] = {{3, (u8 *)"obj"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_obj, -1},
    [8] = {{1, (u8 *)"c"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_obj, -1},
    [6] = {{11, (u8 *)"threadcount"}, CLI_ARGUMENT_TYPE_u64, CLI_OPTION_thread_count, 0},
    [15] = {{1, (u8 *)"i"}, CLI_ARGUMENT_TYPE_directory_list, CLI_OPTION_I, 0},
    [25] = {{4, (u8 *)"wall"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_Wall, -1},
    [12] = {{5, (u8 *)"wnone"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_Wnone, -1},
    [28] = {{1, (u8 *)"w"}, CLI_ARGUMENT_TYPE_warning, CLI_OPTION_W, 0},
    [57] = {{3, (u8 *)"wno"}, CLI_ARGUMENT_TYPE_warning, CLI_OPTION_Wno, 0},
    [0] = {{2, (u8 *)"wd"}, CLI_ARGUMENT_TYPE_warning, CLI_OPTION_Wno, 0},
    [60] = {{9, (u8 *)"nodiscard"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_no_discard, -1},
    [27] = {{17, (u8 *)"dontprintthefiles"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_dont_print_the_files, -1},
    [38] = {{4, (u8 *)"seed"}, CLI_ARGUMENT_TYPE_u64, CLI_OPTION_seed, 0},
    [61] = {{30, (u8 *)"reportwarningsinsystemincludes"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_report_warnings_in_system_includes, -1},
    [39] = {{4, (u8 *)"test"}, CLI_ARGUMENT_TYPE_none, CLI_OPTION_test, -1},
};

struct cli_options{
    int success;

    struct string_list files; // Non-options. These are not checked, as they might contain wild-cards.
    int no_stdlib; // Don't link to 'ucrt.lib'.
    int no_intrinsics; // Disables the 'implicit/intrinsics.c' compilation-unit.
    int no_premain; // Disables the 'implicit/premain.c' compilation-unit.
    int no_debug; // Disables generation of debugging information.
    int no_dynamic_base; // Generate a non-relocatable image.
    u64 image_base; // Set the default image base address of the image.
    int show_includes; // Prints all file paths of included files to stdout.
    enum subsystem{
        SUBSYSTEM_console = 3, // Sets the Subsystem field to `IMAGE_SUBSYSTEM_WINDOWS_CUI`.
        SUBSYSTEM_windows = 2, // Sets the Subsystem field to `IMAGE_SUBSYSTEM_WINDOWS_GUI`.
        SUBSYSTEM_efi_application = 10, // Sets the Subsystem field to `IMAGE_SUBSYSTEM_EFI_APPLICATION`.
    } subsystem; // Set the 'Subsystem' field of the image optional header.
    struct string out; // Sets the name of the output files.
    struct string entry; // Set the entry point symbol. This option implies the `no_premain` option.
    int no_entry; // Indicates that there is no entry point. This option implies /DLL.
    int dll; // Produce a Dynamic Link Library.
    int obj; // Produce an object file instead of an executable.
    u64 thread_count; // The amount of threads to be used during compilation. Default: 1
    struct string_list I; // Specify an additional include directory.
    int Wall; // Enable all warnings.
    int Wnone; // Disable all warnings.
    int no_discard; // Emit all functions and declarations.
    int dont_print_the_files; // Don't print the files because we are in a test suite.
    u64 seed; // Specifies a seed used to shuffle around declarations.
    int report_warnings_in_system_includes; // Self explanatory.
    int test; // Equivalent to `-no_premain -no_intrinsic -no_stdlib -entry main`.
};

struct cli_options cli_parse_options(struct memory_arena *arena, int argc, char *argv[]){
    struct cli_options cli_options = {0};

    for(int option_index = 1; option_index < argc; option_index++){
        char *option_cstring = argv[option_index];
        
        //
        // Allow /, -, and -- as option indicators.
        //
        int is_option = 0;
        if(*option_cstring == '/'){
            is_option = 1;
            option_cstring++;
        }else if(*option_cstring == '-'){
            is_option = 1;
            option_cstring++;
            if(*option_cstring == '-') option_cstring++;
        }
        
        if(!is_option){
            string_list_postfix(&cli_options.files, arena, string_from_cstring(option_cstring));
            continue;
        }
        
        //
        // Canonicalize the option.
        //
        u8  canonicalized_option_data[0x100];
        u64 canonicalized_option_size = 0;
        char *option_argument = null;
        for(char *it = option_cstring; *it; it++){
            if(*it == '-' || *it == '_') continue;
            if(*it == '=' || *it == ':' || *it == ' '){
                if(!option_argument) option_argument = it;
                break;
            }
            if(canonicalized_option_size == sizeof(canonicalized_option_data)){
                print("Error: Option '%s' is too long. Command line options can be at most %lld bytes.\n", option_cstring, array_count(canonicalized_option_data));
                return cli_options;
            }
            canonicalized_option_data[canonicalized_option_size++] = (*it|32);
        }
        
        struct string canonicalized_option = {.data = canonicalized_option_data, .size = canonicalized_option_size};
        
        //
        // Look up the canonicalized option in the hash table.
        //
        struct cli_option_hash_table_entry *option_hash_table_entry = 0;
        u64 hash = string_djb2_hash(canonicalized_option);
        for(u64 index = 0; index < array_count(cli_option_hash_table); index++){
            u64 hash_index = (hash + index) & (array_count(cli_option_hash_table) - 1);
            if(!cli_option_hash_table[hash_index].canonicalized_name.data) break;
            if(string_match(cli_option_hash_table[hash_index].canonicalized_name, canonicalized_option)){
                option_hash_table_entry = &cli_option_hash_table[hash_index];
                break;
            }
        }
        
        //
        // If we found the hash table entry, extract the argument_type and option_kind from it.
        // Otherwise, we have to check the short options because they allow arguments without separator, like
        //     -Iinclude
        //
        enum cli_argument_type option_argument_type = CLI_ARGUMENT_TYPE_none;
        enum cli_option_kind   option_kind = CLI_OPTION_none;
        int argument_is_optional = 0;
        if(option_hash_table_entry){
            option_argument_type = option_hash_table_entry->argument_type;
            option_kind          = option_hash_table_entry->option_kind;
            argument_is_optional = option_hash_table_entry->argument_is_optional;
        }else{
            // @note: Short options need to match case-sensatively.
            struct string option = string_from_cstring(option_cstring);
            
            if(string_front_match_eat(&option, "h")){
                option_argument_type = CLI_ARGUMENT_TYPE_option;
                option_kind          = CLI_OPTION_help;
            }else if(string_front_match_eat(&option, "?")){
                option_argument_type = CLI_ARGUMENT_TYPE_option;
                option_kind          = CLI_OPTION_help;
            }else if(string_front_match_eat(&option, "out")){
                option_argument_type = CLI_ARGUMENT_TYPE_string;
                option_kind          = CLI_OPTION_out;
            }else if(string_front_match_eat(&option, "o")){
                option_argument_type = CLI_ARGUMENT_TYPE_string;
                option_kind          = CLI_OPTION_out;
            }else if(string_front_match_eat(&option, "Fe")){
                option_argument_type = CLI_ARGUMENT_TYPE_string;
                option_kind          = CLI_OPTION_out;
            }else if(string_front_match_eat(&option, "I")){
                option_argument_type = CLI_ARGUMENT_TYPE_directory_list;
                option_kind          = CLI_OPTION_I;
            }else if(string_front_match_eat(&option, "W")){
                option_argument_type = CLI_ARGUMENT_TYPE_warning;
                option_kind          = CLI_OPTION_W;
            }else if(string_front_match_eat(&option, "Wno")){
                option_argument_type = CLI_ARGUMENT_TYPE_warning;
                option_kind          = CLI_OPTION_Wno;
            }else if(string_front_match_eat(&option, "wd")){
                option_argument_type = CLI_ARGUMENT_TYPE_warning;
                option_kind          = CLI_OPTION_Wno;
            }
            
            if(option_kind == CLI_OPTION_none){
                print("Warning: Unknown command-line option '%s'.\n", option_cstring);
                continue;
            }
            
            // We have eaten the option from 'option' so now its just the argument.
            option_argument = (char *)option.data;
            argument_is_optional = 0; // If we get here, there was an argument (otherwise the hash-table lookup would have worked), so we can safely set this to 0.
        }
        
        //
        // If the option needs an argument, make sure we have one.
        //
        if(!option_argument && (option_argument_type != CLI_ARGUMENT_TYPE_none) && !argument_is_optional){
            option_argument = argv[++option_index];
            if(!option_argument){ // @note: argv is null-pointer terminated.
                print("Error: Expected argument after command line option '%s'.\n", option_cstring);
                return cli_options;
            }
        }
        
        //
        // Parse the argument.
        //
        struct string argument_string = option_argument ? string_from_cstring(option_argument) : (struct string){0};
        u64 argument_as_u64 = 0;
        
        switch(option_argument_type){
            case CLI_ARGUMENT_TYPE_none: break;
            case CLI_ARGUMENT_TYPE_string: break;
            case CLI_ARGUMENT_TYPE_enum: break;
            case CLI_ARGUMENT_TYPE_option: break;
            case CLI_ARGUMENT_TYPE_warning: break;
            case CLI_ARGUMENT_TYPE_directory:
            case CLI_ARGUMENT_TYPE_directory_list:{
                if(!path_is_directory(option_argument)){
                    print("Error: Argument '%s' of command line option '%s' must be a directory, but it is not.\n", option_argument, option_cstring);
                    return cli_options;
                }
            }break;
            case CLI_ARGUMENT_TYPE_u64:{
                int success = true;
                argument_as_u64 = string_to_u64(argument_string, &success);
                if(!success){
                    print("Error: Could not parse argument '%.*s' of option '%s' as a u64.\n", argument_string.size, argument_string.data, option_cstring);
                    return cli_options;
                }
            }break;
        }
        
        //
        // We are ready to parse the command line option!
        //
        switch(option_kind){
            
            case CLI_OPTION_help:{
                not_implemented;
            }break;
            case CLI_OPTION_no_stdlib: cli_options.no_stdlib = 1; break;
            case CLI_OPTION_no_intrinsics: cli_options.no_intrinsics = 1; break;
            case CLI_OPTION_no_premain: cli_options.no_premain = 1; break;
            case CLI_OPTION_no_debug: cli_options.no_debug = 1; break;
            case CLI_OPTION_no_dynamic_base: cli_options.no_dynamic_base = 1; break;
            
            case CLI_OPTION_image_base:{
                cli_options.image_base = argument_as_u64;
            }break;
            case CLI_OPTION_show_includes: cli_options.show_includes = 1; break;
            
            case CLI_OPTION_subsystem:{
                if(string_match(argument_string, string("console"))){
                    cli_options.subsystem = SUBSYSTEM_console;
                }else if(string_match(argument_string, string("windows"))){
                    cli_options.subsystem = SUBSYSTEM_windows;
                }else if(string_match(argument_string, string("efi_application"))){
                    cli_options.subsystem = SUBSYSTEM_efi_application;
                }else{
                    print("Error: Unhandled value '%.*s' for command line option '%s'.\n", argument_string.size, argument_string.data, option_cstring);
                }
            }break;
            
            case CLI_OPTION_out:{
                cli_options.out = argument_string;
            }break;
            
            case CLI_OPTION_entry:{
                cli_options.entry = argument_string;
            }break;
            case CLI_OPTION_no_entry: cli_options.no_entry = 1; break;
            case CLI_OPTION_dll: cli_options.dll = 1; break;
            case CLI_OPTION_obj: cli_options.obj = 1; break;
            
            case CLI_OPTION_thread_count:{
                cli_options.thread_count = argument_as_u64;
            }break;
            
            case CLI_OPTION_I:{
                string_list_postfix(&cli_options.I, arena, argument_string);
            }break;
            case CLI_OPTION_Wall: cli_options.Wall = 1; break;
            case CLI_OPTION_Wnone: cli_options.Wnone = 1; break;
            
            case CLI_OPTION_W:{
                not_implemented;
            }break;
            
            case CLI_OPTION_Wno:{
                not_implemented;
            }break;
            case CLI_OPTION_no_discard: cli_options.no_discard = 1; break;
            case CLI_OPTION_dont_print_the_files: cli_options.dont_print_the_files = 1; break;
            
            case CLI_OPTION_seed:{
                cli_options.seed = argument_as_u64;
            }break;
            case CLI_OPTION_report_warnings_in_system_includes: cli_options.report_warnings_in_system_includes = 1; break;
            case CLI_OPTION_test: cli_options.test = 1; break;
                case CLI_OPTION_count:
                case CLI_OPTION_none:
                    invalid_code_path;
        }
    }

    cli_options.success = 1;
    return cli_options;
}