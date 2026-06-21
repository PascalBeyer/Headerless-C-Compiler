
int main(int argc, char *argv[]);

__attribute__(noreturn) 
void __libc_start_main(int (*main_function)(int argc, char *argv[]), int argc, char **argv, void (*init)(void), void (*fini)(void), void (*rtld_fini)(void), void *stack_end);

int _start(){
    
    // Apperantly, the stack is not correctly aligned on entry.
    __asm__ { 
        sub rsp, 8
    }
    
    struct{
        void *return_address;
        int argc;
        char *argv0;
    } *stack = _AddressOfReturnAddress();
    
    int argc = stack->argc;
    char **argv = &stack->argv0;
    
    static void do_nothing(void){}
    
    __libc_start_main(main, argc, argv, do_nothing, do_nothing, do_nothing, stack);
    
}
