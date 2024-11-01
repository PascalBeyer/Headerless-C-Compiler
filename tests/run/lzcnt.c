// run

#include <intrin.h>

int main(){
    unsigned short s = 0x100;
    unsigned int i = 0x10000;
    unsigned long long l = 0x100000000;
    
    s = __lzcnt16(s);
    if(s != 7) return 1;
    i = __lzcnt(i);
    if(i !=  15) return 1;
    l = __lzcnt64(l);
    if(l != 31) return 1;
    
    return 0;
}

