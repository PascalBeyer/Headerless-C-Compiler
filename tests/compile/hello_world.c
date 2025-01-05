// compile

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
#include <math.h> 
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <time.h>
#include <wchar.h>
#include <wctype.h>

// Windows does not support for C11, so these are not found
//#include <stdatomic.h>
//#include <stdalign.h> 
//#include <stdnoreturn.h>

int main(){
    printf("Hello, World!\n");
    return 1;
}

