// 
// Unfortunally, Windows does not draw a distinction between the main 
// thread and any other thread. This means when the main thread exits its 
// start routine, the process will not exit, only `ExitThread` is called.
// Hence, we need to provide this `pre_main_no_args.c` file, to exit the
// process when it returns from `main`.
// 

#include "pre_main_common.c"

int main();

int _start(void){
    
    pre_main_common();
    
    int exit_code = main();
    
    ExitProcess((unsigned int)exit_code);
}

