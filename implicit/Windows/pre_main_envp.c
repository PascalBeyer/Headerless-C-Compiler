
#include "pre_main_common.c"

__declspec(dllimport) char* GetEnvironmentStringsA(void);

int main(int argc, char *argv[], char *envp[]);

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
    char *environment_strings = GetEnvironmentStringsA();
    unsigned __int64 amount_of_environment_strings = 0;
    for(char *string = environment_strings; *string; ){
        if(*string != '='){
            amount_of_environment_strings++;
        }
        
        for(; *string; string++){
        }
        string++;
    }
    
    char **envp_array = GlobalAlloc(/*GMEM_FIXED*/0, (amount_of_environment_strings + 1)* sizeof(char *));
    unsigned __int64 envp_index = 0;
    for(char *string = environment_strings; *string;){
        if(*string != '='){
            envp_array[envp_index++] = string;
        }
        for(; *string; string++){
        }
        string++;
    }
    envp_array[amount_of_environment_strings] = 0;
    
    int exit_code = main(argc, argv, envp_array);

    ExitProcess((unsigned int)exit_code);
}

