// compile -Wall
// check "(16,42): Warning 26 at 'sizeof': Type void has undefined size. Returned value is 0. This is a GNU-extension."
// check "(16,56): Warning 26 at '_Alignof': Type void has undefined alignment. Returned value is 1. This is a GNU-extension."
// check "(16,72): Warning 26 at 'sizeof': Expression of type void has undefined size. Returned value is 0. This is a GNU-extension."
// check "(16,79): Warning 26 at '*': Dereferencing a void-pointer is a GNU-extension."
// check "(16,84): Warning 26 at '_Alignof': Expression of type void has undefined alignment. Returned value is 1. This is a GNU-extension."
// check "(16,93): Warning 26 at '*': Dereferencing a void-pointer is a GNU-extension."
// run
// check "hello world! 0 1 0 1"

#include <stdio.h>

int main(){
    void *a;
    
    printf("hello world! %u %u %u %u\n", sizeof(void), _Alignof(void), sizeof(*a), _Alignof(*a));
}
