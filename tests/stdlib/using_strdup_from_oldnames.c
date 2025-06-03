// run

#include <string.h>
#include <stdio.h>

int main(){
    
    char *hello = "Hello, World!\n";
    
    char *hello2 = strdup(hello);
    
    return strcmp(hello, hello2);
}

