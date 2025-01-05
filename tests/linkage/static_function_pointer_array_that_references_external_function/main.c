// compile other.c
// run
// check "hi!"
// check "bye!"

#include <stdarg.h>
#include <stdio.h>


void bye(){
    printf("bye!\n");
}


void exec_pointers();

int main(){
    exec_pointers();
    return 0;
}


