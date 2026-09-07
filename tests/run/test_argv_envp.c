// compile
// run path
// check "path found!"

#include <stdio.h>

int main(int argc, char *argv[], char *envp[]){
    
    if(argc != 2){
        printf("Incorrect argc %d\n", argc);
        return 1;
    }
    
    char *search_for = argv[1];
    
    if(search_for[0] != 'p' || search_for[1] != 'a' || search_for[2] != 't' || search_for[3] != 'h' || search_for[4] != 0) return 1; 
    
    char **env = envp;
    while(*env){
        
        char *a = search_for;
        char *b = *env;
        
        while(*a){
            if(!*b) break;
            
            if((*a|32) != (*b|32)) break;
            
            a++;
            b++;
        }
        
        if(!*a){
            printf("path found!\n");
            return 0;
        }
        env++;
    }
    
    printf("not found :(");
    return 1;
}
