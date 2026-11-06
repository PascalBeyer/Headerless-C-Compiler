#include <stdint.h>

int _start(){
    
    typeof(__int8[8]) array = {
        1, 2, 3, 4, 5, 6, 7, 8
    };
    
    
    _Static_assert(sizeof(array) == 8);
    return 0;
}
