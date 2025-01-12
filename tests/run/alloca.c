// run with a pile of arguments please

#include <malloc.h>
#include <string.h>

struct big_return{
    int a1, a2, a3, a4;
    int b1, b2, b3, b4;
    int c1, c2, c3, c4;
    int d1, d2, d3, d4;
} function_with_a_pile_of_arguments(
        int a1, int a2, int a3, int a4,
        int b1, int b2, int b3, int b4,
        int c1, int c2, int c3, int c4,
        int d1, int d2, int d3, int d4
        ){
    struct big_return ret = {
        a1, a2, a3, a4,
        b1, b2, b3, b4,
        c1, c2, c3, c4,
        d1, d2, d3, d4,
    };
    return ret;
}

int main(int argc, char *argv[]){
    
    char **pointers = alloca(argc * sizeof(*pointers));
    
    for(int i = 0; i < argc; i++){
        size_t length = strlen(argv[i]);
        pointers[i] = alloca(length + 1);
        memcpy(pointers[i], argv[i], length + 1);
    }
    
    struct big_return ret = function_with_a_pile_of_arguments(
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            );
    
    for(int i = 0; i < argc; i++){
        size_t length = strlen(argv[i]);
        if(memcmp(pointers[i], argv[i], length + 1) != 0) return 1;
    }
    
    return 0;
}
