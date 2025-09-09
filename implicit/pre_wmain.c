
#include "pre_main_common.c"

int wmain(int argc, wchar_t *argv[]);

int _start(void){
    
    pre_main_common();
    
    int exit_code = wmain(argc, wargv);
    
    ExitProcess((unsigned int)exit_code);
}

