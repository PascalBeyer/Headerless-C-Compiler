// run

#include <setjmp.h>


jmp_buf env;

int function(void){
    longjmp(env, 1);
    return 1;
}


int main(){
    if(setjmp(env) == 0){
        return function();
    }
    return 0;
}
