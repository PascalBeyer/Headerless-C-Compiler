
#include <stdint.h>
#include <stdio.h>

int main(int argc, char *argv[], char *envp[]){
    
    for(size_t index = 0; ; index++){
        if(envp[index] == 0) break;
        printf("%s\n", envp[index]);
    }
}

