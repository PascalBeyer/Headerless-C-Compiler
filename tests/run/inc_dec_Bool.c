// run

#include <string.h>

int main(){
    char one = 1;
    char zero = 0;
    
    _Bool arst;
    arst++;  // {0,1} + 1 is 1.
    if(memcmp(&arst, &one, 1) != 0) return 1;
    
    arst++; // {0,1} + 1 is 1.
    if(memcmp(&arst, &one, 1) != 0) return 1;
    
    arst--; // 1 - 1 is 0.
    if(memcmp(&arst, &zero, 1) != 0) return 1;
    
    arst--; // 0 - 1 is 0.
    if(memcmp(&arst, &one, 1) != 0) return 1;
    
    ++arst;
    if(memcmp(&arst, &one, 1) != 0) return 1;
    
    ++arst;
    if(memcmp(&arst, &one, 1) != 0) return 1;
    
    --arst;
    if(memcmp(&arst, &zero, 1) != 0) return 1;
    
    --arst;
    if(memcmp(&arst, &one, 1) != 0) return 1;
    
    return 0;
}
