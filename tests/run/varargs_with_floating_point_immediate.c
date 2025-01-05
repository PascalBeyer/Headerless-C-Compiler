// run

#include <stdarg.h>
#include "test.h"

void va_args_function(char *pad, ...){
    va_list arg_list;
    va_start(arg_list, pad);
    
    float f = (float)va_arg(arg_list, double);
    
    assert(f == 1.0f);
    
    va_end(arg_list);
}


int main(){
    va_args_function("1.0f: %f\n", 1.0f);
	return 0;
}
