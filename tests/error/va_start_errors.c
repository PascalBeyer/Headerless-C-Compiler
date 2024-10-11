// compile -stdlib
// check "(15,5): Error at '__va_start': Second argument to intrinsic function '__va_start' must be the last named argument of the varargs function."
// check "(17,5): Error at '__va_start': Call to intrinsic function '__va_start' must have exactly two arguments."
// fail  "(22,5): Error at '__va_start': Intrinsic function '__va_start' can only used in a varargs function."
// broken

#include <stdarg.h>

// @cleanup: I cannot test this in this file, because we won't parse functions if there are 
//           errors in the global variables.
//           
// int xxx = __va_start(0, 1); // "Intrinsic function '__va_start' can only used in a varargs function."

int varargs(int asd, ...){
    va_list va;
    __va_start(va, 1); // "Second argument to intrinsic function '__va_start' must be the last named argument of the varargs function."
    
    __va_start(1, asd, 1, 2, 3); // "Call to intrinsic function '__va_start' must have exactly two arguments."
}

int non_varargs(int asd){
    va_list va;
    __va_start(va, asd); // "Intrinsic function '__va_start' can only used in a varargs function."
}

int main(){
    varargs(1);
    non_varargs(1);
}
