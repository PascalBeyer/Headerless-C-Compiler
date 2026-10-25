// os linux
// run

// There are no imports here, but we still need the dynamic linker to handle the absolute patch.
// for `pointer = &target`.
 
static int target = 42;
static int *pointer = &target;

int _start(void){
    if(pointer != &target){
        __asm__ {
            mov rax, 60
            mov rdi, 1
            bytes{0f 05} // exit(1)
        }
    }
    
    __asm__ {
        mov rax, 60
        xor rdi, rdi
        bytes{0f 05} // exit(0)
    }
    
    return 0;
}
