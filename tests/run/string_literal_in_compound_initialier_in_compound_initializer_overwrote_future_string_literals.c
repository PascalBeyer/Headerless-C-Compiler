// run

#include <stddef.h>

struct string{
    char *data;
    size_t size;
};

struct string array[] = {
    (struct string){.data = "a", .size = 1},
};

int _start(){
    array;
    char *arst = "hello";
    
    return arst[0] != 'h' || arst[1] != 'e' || arst[2] != 'l' || arst[3] != 'l' || arst[4] != 'o';
}
