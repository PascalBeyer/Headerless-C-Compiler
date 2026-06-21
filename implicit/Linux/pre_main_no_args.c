
int main(void);

int _start(){
    
    // Apperantly, the stack is not correctly aligned on entry.
    __asm__ { 
        sub rsp, 8
    }
    
    int exit_code = main();
    
    __asm__{
        mov rax, 60
        mov edi, exit_code
        syscall
    }
    while(1){}
}
