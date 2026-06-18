
int main(void);

int _start(){
    
    int exit_code = main();
    
    __asm__{
        mov rax, 60
        mov edi, exit_code
        syscall
    }
    while(1){}
}
