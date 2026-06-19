
int main(int argc, char *argv[], char *envp[]);

int _start(){
    
    struct{
        int argc;
        char *argv0;
    } *stack = _AddressOfReturnAddress();
    
    int argc = stack->argc;
    char **argv = &stack->argv0;
    char **envp = argv + argc + 1;
    
    int exit_code = main(argc, argv, envp);
    
    __asm__{
        mov rax, 60
        mov edi, exit_code
        syscall
    }
    
    while(1){}
}

