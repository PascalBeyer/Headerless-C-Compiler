#include <stdint.h>

int function(uintptr_t arg, int brg){
    void *pointer = 0;
    return (arg != pointer) + (arg == pointer) + (arg <= pointer) + (arg >= pointer) + (arg < pointer) + (arg > pointer)+
           (brg != pointer) + (brg == pointer) + (brg <= pointer) + (brg >= pointer) + (arg < pointer) + (arg > pointer);
}

int main(){
    void *pointer = 0;
    uintptr_t variable = pointer;
    int b = pointer;
    function(pointer, pointer);
}
