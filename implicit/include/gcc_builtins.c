
__declspec(inline_asm) void __builtin_ia32_pause(void){
    bytes{f3 90} // pause
}

__declspec(inline_asm) int __builtin_ctz(unsigned int x){
    mov ecx, x
    bsf eax, ecx
    return eax
}

__declspec(inline_asm) int __builtin_clz(unsigned int x){
    mov ecx, x
    bsr eax, ecx
    xor eax, 31
    return eax
}

__declspec(inline_asm) int __builtin_ctzl(unsigned long x){
#ifdef _WIN32
    mov ecx, x
    bsf eax, ecx
    return eax
#else
    mov rcx, x
    bsf rax, rcx
    return eax
#endif
}

__declspec(inline_asm) int __builtin_clzl(unsigned long x){
#ifdef _WIN32
    mov ecx, x
    bsr eax, ecx
    xor eax, 31
    return eax
#else
    mov rcx, x
    bsr rax, rcx
    xor eax, 63
    return eax
#endif
}

__declspec(inline_asm) int __builtin_ctzll(unsigned long long x){
    mov rcx, x
    bsf rax, rcx
    return eax
}

__declspec(inline_asm) int __builtin_clzll(unsigned long long x){
    mov rcx, x
    bsr rax, rcx
    xor eax, 63
    return eax
}

__declspec(inline_asm) int __builtin_popcount(unsigned int x){
    popcnt eax, x
    return eax
}

__declspec(inline_asm) int __builtin_popcountl(unsigned long x){
#ifdef _WIN32
    popcnt eax, x
    return eax
#else
    popcnt rax, x
    return eax
#endif
}

__declspec(inline_asm) int __builtin_popcountll(unsigned long long x){
    popcnt rax, x
    return eax
}

__declspec(inline_asm) int __builtin_ffs(int x){
    mov edx, 0xffffffff
    bsf eax, x
    cmove eax, edx
    add eax, 1
    return eax
}

__declspec(inline_asm) int __builtin_ffsll(long long x){
    mov edx, 0xffffffff
    bsf rax, x
    cmove eax, edx
    add eax, 1
    return eax
}

__declspec(inline_asm) void __builtin_trap(void){
    bytes{0f 0b} // ud2
}

__declspec(inline_asm) void __builtin_unreachable(void){
    bytes{0f 0b} // ud2
}


__declspec(inline_asm) int __builtin_isnan_float(float f){
    ucomiss f, f
    setp al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int __builtin_isnan_double(double d){
    ucomisd d, d
    setp al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int __builtin_isinf_float(float f){
    
    // Remove the sign-bit and check that we are <= FLT32_MAX.
    mov eax, 0x7fffffff
    xorps xmm0, xmm0
    movq xmm0, rax
    andps xmm0, f
    
    ucomiss xmm0, __FLT32_MAX__ // abs <= MAX?
    setbe al
    movzx eax, al
    
    xor eax, 1 // Invert result
    return eax
}

__declspec(inline_asm) int __builtin_isinf_double(double d){
    
    // Remove the sign-bit and check that we are <= FLT64_MAX.
    mov rax, 0x7fffffffffffffff
    xorpd xmm0, xmm0
    movq xmm0, rax
    andpd xmm0, d
    
    ucomisd xmm0, __FLT64_MAX__ // abs <= MAX?
    setbe al
    movzx eax, al
    
    xor eax, 1 // Invert result
    return eax
}

__declspec(inline_asm) int __builtin_isinf_sign_float(float f){
    
    // Remove the sign-bit.
    mov eax, 0x7fffffff
    xorps xmm0, xmm0
    movq xmm0, rax
    andps xmm0, f
    
    ucomiss xmm0, __FLT32_MAX__ // abs <= MAX?
    setbe al
    movzx eax, al
    xor eax, 1
    je .out
    
    movmskps eax, f
    and eax, 1
    je .positive
    
    mov eax, -1
    jmp .out
    
    positive:
    mov eax, 1
    
    out:
    return eax
}

__declspec(inline_asm) int __builtin_isinf_sign_double(double d){
    
    // Remove the sign-bit.
    mov rax, 0x7fffffffffffffff
    xorpd xmm0, xmm0
    movq xmm0, rax
    andpd xmm0, d
    
    ucomisd xmm0, __FLT64_MAX__ // abs <= MAX?
    setbe al
    movzx eax, al
    xor eax, 1
    je .out
    
    movmskpd eax, d
    and eax, 1
    je .positive
    
    mov eax, -1
    jmp .out
    
    positive:
    mov eax, 1
    
    out:
    return eax
}

__declspec(inline_asm) int __builtin_isnormal_float(float f){
    
    // Remove the sign-bit.
    mov eax, 0x7fffffff
    xorps xmm0, xmm0
    movq xmm0, rax
    andps xmm0, f
    
    ucomiss __FLT32_MAX__, xmm0 // MAX < abs?
    setb al
    movzx edx, al
    
    ucomiss xmm0, __FLT32_MIN__ // abs < MIN?
    setb al
    movzx eax, al
    
    or eax, edx // Combine and invert result.
    xor eax, 1
    
    return eax
}

__declspec(inline_asm) int __builtin_isnormal_double(double d){
    
    // Remove the sign-bit.
    mov rax, 0x7fffffffffffffff
    xorpd xmm0, xmm0
    movq xmm0, rax
    andpd xmm0, d
    
    ucomisd __FLT64_MAX__, xmm0 // MAX < abs?
    setb al
    movzx edx, al
    
    
    ucomisd xmm0, __FLT64_MIN__ // abs < MIN?
    setb al
    movzx eax, al
    
    or eax, edx // Combine and invert result.
    xor eax, 1
    
    return eax
}


__declspec(inline_asm) int __builtin_isfinite_float(float f){
    
    // Remove the sign-bit.
    mov eax, 0x7fffffff
    xorps xmm0, xmm0
    movq xmm0, rax
    andps xmm0, f
    
    ucomiss __FLT32_MAX__, xmm0 // MAX < abs?
    setb al
    movzx eax, al
    xor eax, 1
    return eax
}

__declspec(inline_asm) int __builtin_isfinite_double(double d){
    
    // Remove the sign-bit.
    mov rax, 0x7fffffffffffffff
    xorpd xmm0, xmm0
    movq xmm0, rax
    andpd xmm0, d
    
    ucomisd __FLT64_MAX__, xmm0 // MAX < abs?
    setb al
    movzx eax, al
    xor eax, 1
    return eax
}

// This return 0x80000000 or 0.
__declspec(inline_asm) int __builtin_signbitf(float f){
    movd eax, f
    and eax, 0x80000000
    return eax
}

// This returns 1 or 0.
__declspec(inline_asm) int __builtin_signbit(double d){
    movmskpd eax, d
    and eax, 1
    return eax
}

__declspec(inline_asm) unsigned __int16 __builtin_bswap16(unsigned __int16 val){
    ror val, 8
    return val
}

__declspec(inline_asm) unsigned __int32 __builtin_bswap32(unsigned __int32 val){
    bswap val
    return val
}

__declspec(inline_asm) unsigned __int64 __builtin_bswap64(unsigned __int64 val){
    bswap val
    return val
}

__declspec(inline_asm) void __builtin_prefetch__internal(char *a){
    prefetch 1, [a]
}


// @cleanup: why is this not prefixed __builtin_

__declspec(inline_asm) int __signbitf128(__float128){
    bytes{0f 0b} // ud2
    return eax
}


