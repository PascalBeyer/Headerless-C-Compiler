// run
// check "539 Hello, World! (struct v2){.x = 0, .y = 0}"
// check "i = 00001337 (struct v2){.x = 000, .y = 000}"
// check "i = 1337"
// check "1338"
// check "539 Hel 1337"

#include <stdio.h>
#include <stdarg.h>

__declspec(printlike) int print(char *format, ...){
    va_list va;
    va_start(va, format);
    int ret = vprintf(format, va);
    va_end(va);
    
    fflush(stdout); // I always want my prints to flush, but you can remove this if you don't :)
    
    return ret;
}

int main(){
    int i = 1337;
    char *h = "Hello, World!"; 
    struct v2 {int x,y;} v = {0};
    
    print("{:x} %s {}\n", i, h, v); // 539 Hello, World! (struct v2){.x = 0, .y = 0}
    print("{i=:.8} %.3?\n", v);     // i = 00001337 (struct v2){.x = 000, .y = 000}
    print(i);                       // i = 1337
    print(i + 1);                   // 1338
    print("%x {h:.3s} {i}\n", i);   // 539 Hel 1337
    
    int leaf = 0xd, subleaf = 1, cpuid[4];
    __asm__ {
        mov eax, leaf
        mov ecx, subleaf
        
        cpuid
        
        mov cpuid[0], eax
        mov cpuid[1], ebx
        mov cpuid[2], ecx
        mov cpuid[3], edx
    }
    print("{:x}", cpuid);
}
