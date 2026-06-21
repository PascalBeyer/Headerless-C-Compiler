// run

int _start(){
    static int arst[] = {};

#ifndef _WIN32
    __asm__{
        mov rax, 60
        mov edi, sizeof(arst)
        syscall
    }
#endif

    return sizeof(arst);
}
