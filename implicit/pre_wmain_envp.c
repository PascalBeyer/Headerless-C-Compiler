
#define true  1
#define false 0

static typedef unsigned short wchar_t;

#pragma comment(lib, "kernel32")

static int character_is_whitespace(wchar_t c){
    return (c ==  L'\v') || (c ==  L'\t') || (c ==  L'\f') || (c ==  L' ');
}


// arguments:
//    'command_line'      = the unprocessed command line received by calling 'GetCommandLineA'
//    'command_line_size' = the size of the command line, not including the zero_terminator
//    'out_buffer'        = a buffer that is at least as long as the command line,
//                          which will recieve the processed command line as serial zero_terminated strings
//     the return value is the amount of arguments we got.
static int windows_parse_command_line__internal(wchar_t *command_line, __int64 command_line_size, wchar_t *out_buffer){
    
    // According to msdn:
    //   1) Arguments are delimited by white space, which is either a space or tab.
    //   2) The first argument must be valid and is the program name. Can be in quotes.
    //      Everything else does not apply.
    //   3) Quotes give rise to arguements that conain spaces.
    //      Double quotes ("") in qotes give rise to a single quote.
    //      If the command line ends before ending the last argument then all character read so far are
    //      the last argument
    //   4) \" is just ".
    //   5) \ is just \ if it does not precedes a ".
    //   6) if there are an even number of \ preceeding ", then they get halved and the " is a delimiter
    //   7) if there are an add number of \ preceeding ", then they get halved and the last \"  is just ".
    
    
    // Examples:                      argv[1]       argv[2]          argv[3]
    //   1) "abc"      d       e   ->  abc            d                e
    //   2) a\\b     d"e f"g   h   ->  a\\b         de fg              h
    //   3) a\\\"b     c       d   ->  a\"b           c                d
    //   4) a\\\\"b c"  d          ->  a\\           b c               d
    
    wchar_t *at = out_buffer;
    
    // if 'in_quotes' we are also 'in_argument'
    int in_quotes     = false;
    int in_argument   = false;
    
    int amount_of_arguments = 0;
    
    for(__int64 i = 0; i < command_line_size; ){
        // @note: accsessing command_line[i + 1] is save because of zero termination
        // assert(!in_quotes || in_argument);
        
        if(command_line[i] == L'\\'){
            if(!in_argument){
                in_argument = true;
                amount_of_arguments++;
            }
            
            int amount_of_slashes = 0;
            for(; i < command_line_size; i++){
                if(command_line[i] != L'\\') break;
                amount_of_slashes += 1;
            }
            
            if(command_line[i] == '"'){
                // emit one slash for every pair of slashes
                for(int s = 0; s < amount_of_slashes/2; s++) *at++ = L'\\';
                
                if(amount_of_slashes & 1){
                    *at++ = '"'; // it was escaped
                    i++; // eat the '"'
                }else{
                    continue;
                }
            }else{
                // just emit all the slashes
                for(int s = 0; s < amount_of_slashes; s++) *at++ = L'\\';
            }
        }else if(command_line[i] == L'"'){
            if(!in_quotes){
                i++; // skip the '"'
                in_quotes = true;
                
                if(!in_argument){
                    in_argument = true; // if the argument started with quotes ("asd"bcd -> asdbcd)
                    amount_of_arguments++;
                }
            }else{
                if(command_line[i + 1] == L'"'){
                    i += 2;
                    *at++ = L'"';
                }else{
                    i++;
                    in_quotes = false;
                }
            }
        }else if(character_is_whitespace(command_line[i])){
            if(in_quotes){
                *at++ = command_line[i];
            }else if(in_argument){
                in_argument = false; // end the argument
                *at++ = 0;
            }else{
                // do nothing we are currently in whitespace
            }
            i++;
        }else{
            *at++ = command_line[i]; // always just output the character
            i++;
            if(!in_argument){
                in_argument = true;
                amount_of_arguments++;
            }
        }
    }
    *at++ = 0; // zero terminate
    
    return amount_of_arguments;
}

__declspec(dllimport) void* GlobalAlloc(unsigned int uFlags, unsigned __int64 dwBytes);
__declspec(dllimport) wchar_t* GetCommandLineW(void);
__declspec(dllimport) wchar_t* GetEnvironmentStringsW(void);
__declspec(dllimport) __declspec(noreturn) void ExitProcess(unsigned int uExitCode);

int wmain(int argc, wchar_t *argv[], wchar_t *envp[]);

int _start(void){
    wchar_t *command_line = GetCommandLineW();
    
    unsigned __int64 command_line_size = 0;
    for(wchar_t *it = command_line; *it; it++) command_line_size++;
    
    wchar_t *preped_command_line = (wchar_t *)GlobalAlloc(/*GMEM_FIXED*/0, sizeof(wchar_t) * (command_line_size + 1));
    int argc = windows_parse_command_line__internal(command_line, command_line_size, preped_command_line);
    
    wchar_t **argv = (wchar_t **)GlobalAlloc(/*GMEM_FIXED*/0, (argc + 1) * sizeof(wchar_t *));
    wchar_t *at = preped_command_line;
    for(int i = 0; i < argc; i++){
        argv[i] = at;
        while(*at++); // skip to past the next zero_terminator
    }
    argv[argc] = 0;
    
    
    // 
    // Layout of the environment strings is as follows:
    // 
    //     var1=value1\0
    //     var2=value2\0
    //        ... 
    //     varN=valueN\0
    //     \0
    //     
    // We first figure out the size by iterating the strings once, then we allocate the array
    // and re-iterate the strings and fill out the array.
    // 
    // It seems like there are a couple of strage environment variables at the start.
    // E.g:
    // 
    //     =::=::\
    //     =C:=C:\Projects\Headerless-C-Compiler
    //     =ExitCode=00000001
    //     <normal enviroment variables here>
    //     
    // And we are supposed to ignore those. They all start with '=' so I am going to assume that is the case.
    // 
    wchar_t *environment_strings = GetEnvironmentStringsW();
    unsigned __int64 amount_of_environment_strings = 0;
    for(wchar_t *string = environment_strings; *string; ){
        if(*string != L'='){
            amount_of_environment_strings++;
        }
        
        for(; *string; string++){
        }
        string++;
    }
    
    wchar_t **envp_array = GlobalAlloc(/*GMEM_FIXED*/0, (amount_of_environment_strings + 1)* sizeof(wchar_t *));
    unsigned __int64 envp_index = 0;
    for(wchar_t *string = environment_strings; *string;){
        if(*string != L'='){
            envp_array[envp_index++] = string;
        }
        for(; *string; string++){
        }
        string++;
    }
    envp_array[amount_of_environment_strings] = 0;
    
    
    int exit_code = wmain(argc, argv, envp_array);
    
    ExitProcess((unsigned int)exit_code);
}

