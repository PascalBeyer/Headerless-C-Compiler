
#include <stdio.h>
#include <stdarg.h>

__declspec(printlike) int print(char *format, ...){
    va_list va;
    va_start(va, format);
    int ret = vprintf(format, va);
    va_end(va);
    
    fflush(stdout);
    
    return ret;
}

