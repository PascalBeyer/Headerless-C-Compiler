// run

int _start(){
    
    int ret;
    
    __asm__{
        mov rcx, 10
        mov rax, 0
        
      loop:
        inc rax
        dec rcx
        jnz .loop
        
        mov ret, eax
    }
    
    return ret - 10;
}
