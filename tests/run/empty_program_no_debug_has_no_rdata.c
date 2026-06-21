// compile -nodebug
// run

int _start(){ 
#ifndef _WIN32
    __asm__{
        mov rax, 60
        mov edi, 0
        syscall
    }
#endif
    return 0; 
}
