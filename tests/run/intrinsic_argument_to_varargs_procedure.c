// run

#include <stdarg.h>
#include <stdlib.h>
#include <intrin.h>

#define assert(a) (!(a) ? _exit(1) : (void)0)

int varargs(char *format, ...){
    va_list va;
    va_start(va, format);
    
    __m256i *value = va_arg(va, __m256i *);
    
    for(int i = 0; i < sizeof(value->m256i_i8); i++) assert(value->m256i_i8[i] == 'A');
    
    va_end(va);
    return 0;
}


int main(){

    __m256i d = _mm256_set1_epi8(0x41);
    
    varargs("%#?\n", d);
    
    return 0;
}



