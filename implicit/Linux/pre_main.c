
int main(int argc, char *argv[]);

int _start(){
    
    struct{
        int argc;
        char *argv0;
    } *stack = _AddressOfReturnAddress();
    
    int argc = stack->argc;
    char **argv = &stack->argv0;
    
    int exit_code = main(argc, argv);
    
    __asm__{
        mov rax, 60
        mov edi, exit_code
        syscall
    }
    
    while(1){}
}
