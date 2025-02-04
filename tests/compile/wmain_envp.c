
#include <stdint.h>
#include <stdio.h>

int wmain(int argc, wchar_t *argv[], wchar_t *envp[]){
    
    for(size_t index = 0; ; index++){
        if(envp[index] == 0) break;
        printf("%ws\n", envp[index]);
    }
}

