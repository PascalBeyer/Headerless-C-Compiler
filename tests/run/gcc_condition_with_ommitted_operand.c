// run
// check "a 2 - a 3 - a 1"

#include <stdio.h>

int main(){
    
    int a = 1, b = 2, c = 3;
    
    a = b ?: c;
    printf("a %u - ", a);
    a = 0 ?: c;
    printf("a %u - ", a);
    a = 1 ?: c;
    printf("a %u\n", a);
}
