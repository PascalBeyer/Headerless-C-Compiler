
// Shows that all standart library headers "work", or at least compile.

// @note: list taken from wikipedia
#include <assert.h>
#include <complex.h>
#include <ctype.h>
#include <errno.h>
#include <fenv.h>
#include <float.h>
#include <inttypes.h>
#include <iso646.h>
#include <limits.h>
#include <locale.h>
//#include <math.h> // does not work because some transcendental math functions are not declared '_CRTIMP'...
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <tgmath.h> // includes <math.h>
#include <time.h>
#include <wchar.h>
#include <wctype.h>


// Windows does not support for C11, so these are not found
//#include <stdatomic.h>
//#include <stdalign.h> 
//#include <stdnoreturn.h>

int _start(){
    printf("Hello, World!\n");
    return 1;
}

// @note: referanced in <string.h>
void *memset(void *mem, int val, size_t amount){
    uint8_t *it = mem;
    for(size_t i = 0; i < amount; i++){
        *it++ = (uint8_t)val;
    }
    
    return mem;
}

void *memcpy(void *dest, const void *source, size_t amount){
    uint8_t *it  = dest;
    const uint8_t *it2 = source;
    for(size_t i = 0; i < amount; i++){
        *it++ = *it2++;
    }
    
    return dest;
}

// @note: untested
int memcmp(const void *buf1, const void *buf2, size_t count){
    int8_t *it1 = buf1;
    int8_t *it2 = buf2;
    for(size_t i = 0; i < count; i++, it1++, it2++){
        if(*it1 != *it2){
            return (*((uint8_t *)it1) - *((uint8_t *)it2));
        }
    }
    return 0;
}
