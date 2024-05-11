// compile -stdlib other.c
// run
// check "static 1337"
// check "other static 6969"
// check "asd a"
// check "other asd d"
// check "a 1337"
// check "a 6969"

// if both do not initialize 'a' then its an unresolved external
int a = 1337;

static int _static = 1337;


static int asd(){
    return 0xa;
}


#include <stdio.h>

int main(){
    
    printf("static %d\n", _static);
    printf("other static %d\n", return_my_static());
    
    printf("asd %x\n", asd());
    printf("other asd %x\n", return_my_asd());
    
    printf("a %d\n", a);
    set_a();
    printf("a %d\n", a);
    return 0;
}
