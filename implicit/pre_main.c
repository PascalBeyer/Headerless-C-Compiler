
#include "pre_main_common.c"

int main(int argc, char *argv[]);

int _start(void){
    
    pre_main_common();
    
    int exit_code = main(argc, argv);

    ExitProcess((unsigned int)exit_code);
}

