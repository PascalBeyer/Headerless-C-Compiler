// os windows
// run

#include <excpt.h>

void exception_function(){
    int *a = 0;
    *a = 1337;
}

void other_function(){
}

int _start(){
    other_function();
    
    int a = 1;
    __try{
        exception_function();
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        a = 0;
        other_function();
    }
    
    other_function();
    
    return a;
}

