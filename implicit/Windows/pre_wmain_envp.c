
#include "pre_main_common.c"

__declspec(dllimport) wchar_t* GetEnvironmentStringsW(void);

int wmain(int argc, wchar_t *argv[], wchar_t *envp[]);

int _start(void){
    
    pre_main_common();
    
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

