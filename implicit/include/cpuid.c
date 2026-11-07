
enum cpuid_signature{
    
    // AuthenticAMD
    signature_AMD_ebx = 0x68747541,
    signature_AMD_edx = 0x69746e65,
    signature_AMD_ecx = 0x444d4163,
    
    // GenuineIntel
    signature_INTEL_ebx = 0x756e6547,
    signature_INTEL_edx = 0x49656e69,
    signature_INTEL_ecx = 0x6c65746e,
    
};

enum cpuid_1_ecx{
    bit_SSE3        = 0x00000001,
    bit_PCLMULQDQ   = 0x00000002,
    bit_DTES64      = 0x00000004,
    bit_MONITOR     = 0x00000008,
    bit_DSCPL       = 0x00000010,
    bit_VMX         = 0x00000020,
    bit_SMX         = 0x00000040,
    bit_EIST        = 0x00000080,
    bit_TM2         = 0x00000100,
    bit_SSSE3       = 0x00000200,
    bit_CNXTID      = 0x00000400,
    
    bit_FMA         = 0x00001000,
    bit_CMPXCHG16B  = 0x00002000,
    bit_xTPR        = 0x00004000,
    bit_PDCM        = 0x00008000,
    
    bit_PCID        = 0x00020000,
    bit_DCA         = 0x00040000,
    bit_SSE41       = 0x00080000,
    bit_SSE42       = 0x00100000,
    bit_x2APIC      = 0x00200000,
    bit_MOVBE       = 0x00400000,
    bit_POPCNT      = 0x00800000,
    bit_TSCDeadline = 0x01000000,
    bit_AESNI       = 0x02000000,
    bit_XSAVE       = 0x04000000,
    bit_OSXSAVE     = 0x08000000,
    bit_AVX         = 0x10000000,
    bit_F16C        = 0x20000000,
    bit_RDRND       = 0x40000000,
    bit_HV          = 0x80000000,
    
    // Aliases used by gcc.
    bit_SSE4_1      = bit_SSE41,
    bit_SSE4_2      = bit_SSE42,
    bit_PCLMUL      = bit_PCLMULQDQ,
    bit_AES         = bit_AESNI,
};

enum cpuid_1_edx{
    bit_FPU   = 0x00000001,
    bit_VME   = 0x00000002,
    bit_DE    = 0x00000004,
    bit_PSE   = 0x00000008,
    bit_TSC   = 0x00000010,
    bit_MSR   = 0x00000020,
    bit_PAE   = 0x00000040,
    bit_MCE   = 0x00000080,
    bit_CX8   = 0x00000100,
    bit_APIC  = 0x00000200,
    
    bit_SEP   = 0x00000800,
    bit_MTRR  = 0x00001000,
    bit_PGE   = 0x00002000,
    bit_MCA   = 0x00004000,
    bit_CMOV  = 0x00008000,
    bit_PAT   = 0x00010000,
    bit_PSE36 = 0x00020000,
    bit_PSN   = 0x00040000,
    bit_CLFSH = 0x00080000,
    
    bit_DS    = 0x00200000,
    bit_ACPI  = 0x00400000,
    bit_MMX   = 0x00800000,
    bit_FXSR  = 0x01000000,
    bit_SSE   = 0x02000000,
    bit_SSE2  = 0x04000000,
    bit_SS    = 0x08000000,
    bit_HTT   = 0x10000000,
    bit_TM    = 0x20000000,
    
    bit_PBE   = 0x80000000,
    
    // Aliases used by gcc.
    bit_CMPXCHG8B = bit_CX8,
    bit_FXSAVE    = bit_FXSR,
};

enum cpuid_7_0_ebx{
    bit_FSGSBASE   = 0x00000001,
    bit_TSC_ADJUST = 0x00000002,
    bit_SGX        = 0x00000004,
    bit_BMI        = 0x00000008,
    bit_HLE        = 0x00000010,
    bit_AVX2       = 0x00000020,
    bit_FPDP       = 0x00000040,
    bit_SMEP       = 0x00000080,
    bit_BMI2       = 0x00000100,
    bit_ENH_MOVSB  = 0x00000200,
    bit_INVPCID    = 0x00000400,
    bit_RTM        = 0x00000800,
    bit_RTM_M      = 0x00001000,
    bit_FPCSDS     = 0x00002000,
    bit_MPX        = 0x00004000,
    bit_RDT_A      = 0x00008000,
    bit_AVX512F    = 0x00010000,
    bit_AVX512DQ   = 0x00020000,
    bit_RDSEED     = 0x00040000,
    bit_ADX        = 0x00080000,
    bit_SMAP       = 0x00100000,
    bit_AVX512IFMA = 0x00200000,
    bit_PCOMMIT    = 0x00400000,
    bit_CLFLUSHOPT = 0x00800000,
    bit_CLWB       = 0x01000000,
    bit_PT         = 0x02000000,
    bit_AVX512PF   = 0x04000000,
    bit_AVX512ER   = 0x08000000,
    bit_AVX512CD   = 0x10000000,
    bit_SHA        = 0x20000000,
    bit_AVX512BW   = 0x40000000,
    bit_AVX512VL   = 0x80000000,
};

enum cpuid_7_0_ecx{
    bit_PREFETCHWT1     = 0x00000001,
    bit_AVX512VBMI      = 0x00000002,
    // bit_PKU             = 0x00000004, This seems wrong. Should be 8, no?
    bit_OSPKE           = 0x00000010,
    bit_WAITPKG         = 0x00000020,
    bit_AVX512VBMI2     = 0x00000040,
    bit_SHSTK           = 0x00000080,
    bit_GFNI            = 0x00000100,
    bit_VAES            = 0x00000200,
    bit_VPCLMULQDQ      = 0x00000400,
    bit_AVX512VNNI      = 0x00000800,
    bit_AVX512BITALG    = 0x00001000,
    bit_AVX512VPOPCNTDQ = 0x00004000,
    bit_RDPID           = 0x00400000,
    bit_CLDEMOTE        = 0x02000000,
    bit_MOVDIRI         = 0x08000000,
    bit_MOVDIR64B       = 0x10000000,
    bit_ENQCMD          = 0x20000000,
};

enum cpuid_7_0_edx{
    bit_AVX5124VNNIW       = 0x00000004,
    bit_AVX5124FMAPS       = 0x00000008,
    bit_UINTR              = 0x00000020,
    bit_AVX512VP2INTERSECT = 0x00000100,
    bit_SERIALIZE          = 0x00004000,
    bit_TSXLDTRK           = 0x00010000,
    bit_PCONFIG            = 0x00040000,
    bit_IBT                = 0x00100000,
    bit_AMXBF16            = 0x00400000,
    bit_AVX512FP16         = 0x00800000,
    bit_AMXTILE            = 0x01000000,
    bit_AMXINT8            = 0x02000000,
    
    // Gcc aliases.
    bit_AMX_BF16 = bit_AMXBF16,
    bit_AMX_TILE = bit_AMXTILE,
    bit_AMX_INT8 = bit_AMXINT8,
};

enum cpuid_80000001_ecx{
    bit_LAHF_LM = 0x00000001,
    bit_ABM     = 0x00000020,
    bit_SSE4a   = 0x00000040,
    bit_PRFCHW  = 0x00000100,
    bit_XOP     = 0x00000800,
    bit_LWP     = 0x00008000,
    bit_FMA4    = 0x00010000,
    bit_TBM     = 0x00200000,
    bit_MWAITX  = 0x20000000,
    
    bit_LZCNT = bit_ABM,
};

enum cpuid_80000001_edx{
    bit_MMXEXT = 0x00400000,
    bit_LM     = 0x20000000,
    bit_3DNOWP = 0x40000000,
    bit_3DNOW  = 0x80000000,
};

enum cpuid_80000008_ebx{
    bit_CLZERO   = 0x00000001,
    bit_RDPRU    = 0x00000010,
    bit_WBNOINVD = 0x00000200,
};

__declspec(inline_asm) void __cpuid_gnu(unsigned __int32 leaf, unsigned __int32 *__eax, unsigned __int32 *__ebx, unsigned __int32 *__ecx, unsigned __int32 *__edx){
    mov eax, leaf
    cpuid
    
    mov [__eax], eax
    mov [__ebx], ebx
    mov [__ecx], ecx
    mov [__edx], edx
}

__declspec(inline_asm) void __cpuid_count(unsigned __int32 leaf, unsigned __int32 count, unsigned __int32 *__eax, unsigned __int32 *__ebx, unsigned __int32 *__ecx, unsigned __int32 *__edx){
    mov eax, leaf
    mov ecx, count
    cpuid
    
    mov [__eax], eax
    mov [__ebx], ebx
    mov [__ecx], ecx
    mov [__edx], edx
}

// Returns highest supported input value for cpuid instruction if the 'ext' is either 0 or 0x80000000.
// Also optionally returns the first 4 bytes of the signature.
__declspec(inline_asm) int __get_cpuid_max(unsigned __int32 ext, unsigned __int32 *signature){
    mov eax, ext
    cpuid
    
    cmp signature, 0
    je .no_sig
    mov [signature], ebx
    
    no_sig:
    return eax
}

// Checks if the requested 'leaf' is supported, and if it is, returns the values.
__declspec(inline_asm) int __get_cpuid(unsigned __int32 leaf, unsigned __int32 *__eax, unsigned __int32 *__ebx, unsigned __int32 *__ecx, unsigned __int32 *__edx){
    mov eax, leaf
    and eax, 0x80000000
    cpuid
    
    cmp eax, 0
    jz .fail
    
    cmp eax, leaf
    jb .fail
    
    mov eax, leaf
    cpuid 
    
    mov [__eax], eax
    mov [__ebx], ebx
    mov [__ecx], ecx
    mov [__edx], edx
    
    mov eax, 1
    jmp .end
    
    fail:
    mov eax, 0
    
    end:
    return eax
}

