
// 
// To link to the ucrt.dll with MSVC you are supposed to link to 3 libraries:
//     1) ucrt.lib
//     2) vcruntime.lib
//     3) msvcrt.lib
// 
// The ucrt.lib is included with the Windows Software Development Kit.
// Hence, I think for now we can rely on that existing.
// On the other hand, vcruntime.lib and msvcrt.lib ship with MSVC
// and we don't want to require other people to install MSVC to use the compiler.
// Hence, we cannot link to the other two libraries.
// 
// Fortunally, there is not a lot of code in those.
// 
// vcruntime.lib contains functions, that are somehow "compiler specific", 
// for example exception handling, C++ run-time type information etc.
// For us, we only care about the following functions:
// 
//     wcsstr
//     wcsrchr
//     wcschr
//     strstr
//     strrchr
//     strchr
//     memset
//     memmove
//     memcpy
//     memcmp
//     memchr
//     longjmp
//     _setjmpex (technically __intrinsic_setjmpex, but that is used by msvcrt.lib's _setjmpex)
//     _setjmp   (technically __intrinsic_setjmp,   but that is used by msvcrt.lib's _setjmp)
//     
// msvcrt.lib contains functions, that are related to security features, like 
// 
//     `__guard_dispatch_icall` (Control Flow Guards)
//     `__security_cookie`
//     `__chkstk`
//     ...
//     
// Since we currently do not support any security features (and would not use Microsofts code anyway),
// We only have to implement the list of functions specified above.
// 

//_____________________________________________________________________________________________________________________
// Memory functions.


// @cleanup: How to handle types in "implicit" files.
#define wchar_t unsigned short
#define size_t  unsigned __int64


void *memset(void *mem, int val, unsigned __int64 amount){
    unsigned char *it = mem;
    for(unsigned __int64 i = 0; i < amount; i++){
        *it++ = (unsigned char)val;
    }
    return mem;
}

void *memcpy(void *dest, const void *source, unsigned __int64 amount){
    unsigned char *it  = dest;
    unsigned char *it2 = source;
    for(unsigned __int64 i = 0; i < amount; i++){
        *it++ = *it2++;
    }
    
    return dest;
}

void *memmove(void *dest, const void *source, unsigned __int64 amount){
    
    if(source >= dest || (char *)source + amount < (char *)dest){
        // There is no overlapp, or source is bigger than dest:
        // 
        // <---- dest ---->
        //        <--- source --->
        // 
        // or 
        // <--- source ---> 
        //                      <---- dest ---->
        // 
        // We can copy forward:
        // 
        
        unsigned char *it  = dest;
        unsigned char *it2 = source;
        
        for(unsigned __int64 i = 0; i < amount; i++){
            *it++ = *it2++;
        }
    }else{
        // 
        // There is overlapp and source is before dest.
        // 
        // <--- source --->
        //     <---- dest ---->
        // 
        // We need to copy backwards.
        // 
        
        unsigned char *it  = (unsigned char *)dest   + amount;
        unsigned char *it2 = (unsigned char *)source + amount;
        
        for(unsigned __int64 i = 0; i < amount; i++){
            *--it = *--it2;
        }
    }
    
    return dest;
}

int memcmp(void *_string1, void *_string2, unsigned __int64 amount){
    
    if(amount == 0) return 0;
    
    char *string1 = _string1;
    char *string2 = _string2;
    
    while(--amount && *string1 == *string2){
        string1 += 1;
        string2 += 1;
    }
    
    return *(unsigned char *)string1 - *(unsigned char *)string2;
}

void *memchr(void *mem, int c, size_t count){
    char *it = mem;
    for(size_t index = 0; index < count; index++){
        if(it[index] == (char)c) return it + index;
    }
    return 0;
}

//_____________________________________________________________________________________________________________________
// String functions.

char *strstr(char *haystack, char *needle){
    if(*needle) return haystack;
    
    while(*haystack){
        char *string1 = haystack;
        char *string2 = needle;
        
        // @note: We cannot overrun string1, as the zero-terminator would cause us to break.
        while(*string2){
            if(*string1++ != *string2++) break;
        }
        
        if(*string2) return haystack;
        
        haystack++;
    }
    
    return 0;
}

char *strchr(char *haystack, int needle){
    while(*haystack != needle) haystack++;
    return *haystack ? haystack : 0;
}

char *strrchr(char *haystack, int needle){
    char *end = haystack;
    while(*end++){};
    while(end >= haystack && *end != needle) end--;
    return end >= haystack ? end : 0;
}

//_____________________________________________________________________________________________________________________
// Wide-Character String functions

wchar_t *wcsstr(wchar_t *haystack, wchar_t *needle){
    if(*needle) return haystack;
    
    while(*haystack){
        wchar_t *string1 = haystack;
        wchar_t *string2 = needle;
        
        // @note: We cannot overrun string1, as the zero-terminator would cause us to break.
        while(*string2){
            if(*string1++ != *string2++) break;
        }
        
        if(*string2) return haystack;
        
        haystack++;
    }
    
    return 0;
}

wchar_t *wcschr(wchar_t *haystack, wchar_t needle){
    while(*haystack != needle) haystack++;
    return *haystack ? haystack : 0;
}

wchar_t *wcsrchr(wchar_t *haystack, wchar_t needle){
    wchar_t *end = haystack;
    while(*end++){};
    while(end >= haystack && *end != needle) end--;
    return end >= haystack ? end : 0;
}

#if 0
//_____________________________________________________________________________________________________________________
// Setjmp or longjmp.


int setjmp(unsigned char environment_buffer[256]){
    int return_value;
    unsigned __int32 mxcsr;
    unsigned __int16 fpu_control_word;
    
    __asm__{
        // Store some special registers.
        fstcw fpu_control_word
        stmxcsr mxcsr
        
        // Store all of the non-volatile registers.
        lea rdx, long_jmp_target
        mov rcx, environment_buffer
        
        mov [rcx + 0x00], rbp
        mov [rcx + 0x08], rbx
        mov [rcx + 0x10], rsp
        mov [rcx + 0x18], rsi
        mov [rcx + 0x20], rdi
        mov [rcx + 0x28], r12
        mov [rcx + 0x30], r13
        mov [rcx + 0x38], r14
        mov [rcx + 0x40], r15
        mov [rcx + 0x48], rdx
        mov [rcx + 0x50], mxcsr
        mov [rcx + 0x54], fpu_control_word
        mov [rcx + 0x56], 0
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
        
    long_jmp_target:
        mov return_value, eax
    }
    
    return return_value;
}

_Noreturn void longjmp(unsigned char environment_buffer environment_buffer[16], int return_value){
    unsigned __int32 mxcsr;
    unsigned __int16 fpu_control_word;
    
    __asm__{
        mov rcx, environment_buffer
        
        // *first* set mxcsr and fpu_control_word, as later we screw with rsp.
        mov mxcsr, [rcx + 0x50]
        mov fpu_control_word, [rcx + 0x54]
        
        fldcw fpu_control_word
        ldmxcsr mxcsr
        
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
        
        mov xmm6,  [rcx + 0x60]
        mov xmm7,  [rcx + 0x70]
        mov xmm8,  [rcx + 0x80]
        mov xmm9,  [rcx + 0x90]
        mov xmm10, [rcx + 0xa0]
        mov xmm11, [rcx + 0xb0]
        mov xmm12, [rcx + 0xc0]
        mov xmm13, [rcx + 0xd0]
        mov xmm14, [rcx + 0xe0]
        mov xmm15, [rcx + 0xf0]
        
        jmp [rcx + 0x48] // jump to the saved rip.
    }
}
#endif
