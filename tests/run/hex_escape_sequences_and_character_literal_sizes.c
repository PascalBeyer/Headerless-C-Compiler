// run
// check "sizeof('\xff') = 4 (-1)"
// check "sizeof('\xff\xff') = 4 (65535)"
// check "sizeof('\xff\xff\xff\xff') = 4 (-1)"
// check "sizeof(L'\xffff') = 2 (65535)"
// check "sizeof(u'\xffff') = 2 (65535)"
// check "4294967295"

#include <stdio.h>

#define test(a) printf("sizeof(" #a ") = %d (%d)\n", sizeof(a), a);

int main(){
    test('\xff');
    test('\xff\xff');
    test('\xff\xff\xff\xff');
    
    test(L'\xffff');
    test(u'\xffff');
    
    unsigned __int64 asd = U'\xffffffff';
    printf("%llu\n", asd);
    return 0;
}
