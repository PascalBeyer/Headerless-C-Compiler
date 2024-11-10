
struct __declspec(align(16)) aligned_char{
    unsigned char a;
};

typedef struct aligned_char jmp_buf[256];

#ifdef __HLC_COMPILE_TO_OBJECT__
int setjmp(jmp_buf environment_buffer, char *frame);
_Noreturn void longjmp(jmp_buf environment_buffer, int return_value);
#else

int setjmp(jmp_buf environment_buffer){
    int return_value;
    
    // The return address in Windows x64 calling convension is 8 bytes after (in stack order) the first argument.
    // Hence we can calculate it here. This is really aweful.
    volatile __int64 *pointer_to_return_address = (__int64 *)&environment_buffer - 1; 
    __int64 return_address = *pointer_to_return_address;
    
    __asm__{
        
        // Store all of the non-volatile registers.
        mov rcx, environment_buffer
        mov rdx, return_address
        
        mov [rcx + 0x00], rbp
        mov [rcx + 0x08], rbx
        mov [rcx + 0x10], rsp
        mov [rcx + 0x18], rsi
        mov [rcx + 0x20], rdi
        mov [rcx + 0x28], r12
        mov [rcx + 0x30], r13
        mov [rcx + 0x38], r14
        mov [rcx + 0x40], r15
        
        bytes {0f ae 59 50} // stmxcsr dword ptr [rcx+0x50]
        bytes {9b d9 79 54} // fstcw   word  ptr [rcx+0x54]
        
        mov word ptr [rcx + 0x56], 0
        mov [rcx + 0x58], rdx
        
        movups [rcx + 0x60], xmm6
        movups [rcx + 0x70], xmm7
        movups [rcx + 0x80], xmm8
        movups [rcx + 0x90], xmm9
        movups [rcx + 0xa0], xmm10
        movups [rcx + 0xb0], xmm11
        movups [rcx + 0xc0], xmm12
        movups [rcx + 0xd0], xmm13
        movups [rcx + 0xe0], xmm14
        movups [rcx + 0xf0], xmm15
        
        // Return 0 from the initial call.
        xor eax, eax
        
        bytes{ 
            4c 8d 05 04 00 00 00 // lea r8, long_jmp_target
            4c 89 41 48          // mov [rcx + 0x48], r8
        }
        
        mov return_address, rdx
        mov return_value, eax
    }
    
    // @note: The original pointer_to_return_address is not valid anymore.
    //        We have to reload it.
    pointer_to_return_address = (__int64 *)&environment_buffer - 1; 
    *pointer_to_return_address = return_address;
    
    return return_value;
}

_Noreturn void longjmp(jmp_buf environment_buffer, int return_value){
    
    __asm__{
        mov rcx, environment_buffer
        
        // load the return value already, because it might be on the stack.
        mov eax, return_value
        
        mov rbp, [rcx + 0x00]
        mov rbx, [rcx + 0x08]
        mov rsp, [rcx + 0x10]
        mov rsi, [rcx + 0x18]
        mov rdi, [rcx + 0x20]
        mov r12, [rcx + 0x28]
        mov r13, [rcx + 0x30]
        mov r14, [rcx + 0x38]
        mov r15, [rcx + 0x40]
        
        bytes {0f ae 51 50} // ldmxcsr DWORD PTR [rcx+0x50]
        bytes {d9 69 54}    // fldcw   WORD  PTR [rcx+0x54]
        
        mov rdx, [rcx + 0x58]
        
        movups xmm6,  [rcx + 0x60]
        movups xmm7,  [rcx + 0x70]
        movups xmm8,  [rcx + 0x80]
        movups xmm9,  [rcx + 0x90]
        movups xmm10, [rcx + 0xa0]
        movups xmm11, [rcx + 0xb0]
        movups xmm12, [rcx + 0xc0]
        movups xmm13, [rcx + 0xd0]
        movups xmm14, [rcx + 0xe0]
        movups xmm15, [rcx + 0xf0]
        
        // jump to the saved rip.
        bytes {ff 61 48} // jmp QWORD PTR [rcx+0x48]
    }
    
    // squelch the noreturn function returning waring please!
    __declspec(inline_asm) _Noreturn void do_not_warn_for_noreturn_please(){ }
    do_not_warn_for_noreturn_please();
}

#endif
