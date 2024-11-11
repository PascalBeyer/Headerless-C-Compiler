
__declspec(inline_asm) void _ReadWriteBarrier(){}


__declspec(inline_asm) void __debugbreak(){
    int3
}

__declspec(inline_asm) __declspec(noreturn) void __fastfail(unsigned int __exit_code){
    mov ecx, __exit_code
    int 0x29
}

#if 0

//
// @cleanup: is this supposed to work?
//
// I think currently thats a no, as 'cpuInfo' gets converted to just being 'int *cpuInfo'
__declspec(inline_asm) void __cpuid(int cpuInfo[4], int function_id){
    mov eax, function_id
    cpuid
    mov cpuInfo[0], eax
    mov cpuInfo[1], ebx
    mov cpuInfo[2], ecx
    mov cpuInfo[3], edx
}


__declspec(inline_asm) void __cpuidex(int cpuInfo[4], int function_id, int subfunction_id){
    mov eax, function_id
    mov ecx, subfunction_id
    cpuid
    mov cpuInfo[0], eax
    mov cpuInfo[1], ebx
    mov cpuInfo[2], ecx
    mov cpuInfo[3], edx
}
#else

__declspec(inline_asm) void __cpuid(int cpuInfo[4], int function_id){
    mov eax, function_id
    cpuid
    
    mov r8, cpuInfo
    mov [r8 +  0], eax
    mov [r8 +  4], ebx
    mov [r8 +  8], ecx
    mov [r8 + 12], edx
}


__declspec(inline_asm) void __cpuidex(int cpuInfo[4], int function_id, int subfunction_id){
    mov eax, function_id
    mov ecx, subfunction_id
    cpuid
    
    mov r8, cpuInfo
    mov [r8 +  0], eax
    mov [r8 +  4], ebx
    mov [r8 +  8], ecx
    mov [r8 + 12], edx
}
#endif

__declspec(inline_asm) unsigned __int64 __rdtsc(){
    rdtsc
    shl rdx, 32
    or rax, rdx
    return rax
}

__declspec(inline_asm) unsigned __int64 __rdtscp(unsigned __int32 *AUX){
    rdtscp
    shl rdx, 32
    or rax, rdx
    mov rdx, AUX
    mov [rdx], ecx
    return rax
}

__declspec(inline_asm) unsigned __int64 _xgetbv(unsigned int xcr){
    mov ecx, xcr
    xgetbv
    shl rdx, 32
    or  rax, rdx
    return rax
}

__declspec(inline_asm) unsigned __int8 _addcarry_u8(unsigned __int8 carry_in, unsigned __int8 operand_a, unsigned __int8 operand_b, unsigned __int8 *out){
    
    // set CF if 'carry_in' is not-zero
    mov cl, carry_in
    add cl, -1
    
    // perform the operation
    mov al, operand_a
    adc al, operand_b
    
    // set carry out if there was a carry
    setc cl
    
    // store the result
    mov rdx, out
    mov [rdx], al
    
    return cl
}

__declspec(inline_asm) unsigned __int8 _addcarry_u16(unsigned __int8 carry_in, unsigned __int16 operand_a, unsigned __int16 operand_b, unsigned __int16 *out){
    
    // set CF if 'carry_in' is not-zero
    mov cl, carry_in
    add cl, -1
    
    // perform the operation
    mov ax, operand_a
    adc ax, operand_b
    
    // set carry out if there was a carry
    setc cl
    
    // store the result
    mov rdx, out
    mov [rdx], ax
    
    return cl
}


__declspec(inline_asm) unsigned __int8 _addcarry_u32(unsigned __int8 carry_in, unsigned __int32 operand_a, unsigned __int32 operand_b, unsigned __int32 *out){
    
    // set CF if 'carry_in' is not-zero
    mov cl, carry_in
    add cl, -1
    
    // perform the operation
    mov eax, operand_a
    adc eax, operand_b
    
    // set carry out if there was a carry
    setc cl
    
    // store the result
    mov rdx, out
    mov [rdx], eax
    
    return cl
}


__declspec(inline_asm) unsigned __int8 _addcarry_u64(unsigned __int8 carry_in, unsigned __int64 operand_a, unsigned __int64 operand_b, unsigned __int64 *out){
    
    // set CF if 'carry_in' is not-zero
    mov cl, carry_in
    add cl, -1
    
    // perform the operation
    mov rax, operand_a
    adc rax, operand_b
    
    // set carry out if there was a carry
    setc cl
    
    // store the result
    mov rdx, out
    mov [rdx], rax
    
    return cl
}

__declspec(inline_asm) void __addgsbyte(unsigned __int32 offset, unsigned __int8 byte_to_add){
    mov eax, offset
    add byte ptr gs:[rax], byte_to_add
}

__declspec(inline_asm) void __addgsword(unsigned __int32 offset, unsigned __int16 word_to_add){
    mov eax, offset
    add word ptr gs:[rax], word_to_add
}

__declspec(inline_asm) void __addgsdword(unsigned __int32 offset, unsigned __int32 dword_to_add){
    mov eax, offset
    add dword ptr gs:[rax], dword_to_add
}

__declspec(inline_asm) void __addgsqword(unsigned __int32 offset, unsigned __int64 qword_to_add){
    mov eax, offset
    add qword ptr gs:[rax], qword_to_add
}

__declspec(inline_asm) void __incgsbyte(unsigned __int32 offset){
    mov eax, offset
    inc byte ptr gs:[rax]
}

__declspec(inline_asm) void __incgsword(unsigned __int32 offset){
    mov eax, offset
    inc word ptr gs:[rax]
}

__declspec(inline_asm) void __incgsdword(unsigned __int32 offset){
    mov eax, offset
    inc dword ptr gs:[rax]
}

__declspec(inline_asm) void __incgsqword(unsigned __int32 offset){
    mov eax, offset
    inc qword ptr gs:[rax]
}

__declspec(inline_asm) unsigned __int8 _BitScanForward(unsigned __int32 *out_bit_index, unsigned __int32 to_scan){
    // searches 'to_scan', if 'to_scan' is not zero the index is stored in eax,
    // otherwise otherwise the ZF is set
    bsf eax, to_scan
    
    // set 'found' if the ZF was not set
    setnz cl
    
    // save the index even if not found(in which case eax is undefined)
    mov rdx, out_bit_index
    mov [rdx], eax
    
    return cl
}

__declspec(inline_asm) unsigned __int8 _BitScanForward64(unsigned __int32 *out_bit_index, unsigned __int64 to_scan){
    // searches 'to_scan', if 'to_scan' is not zero the index is stored in eax,
    // otherwise otherwise the ZF is set
    bsf rax, to_scan
    
    // set 'found' if the ZF was not set
    setnz cl
    
    // save the index even if not found(in which case eax is undefined)
    mov rdx, out_bit_index
    mov [rdx], eax
    
    return cl
}


__declspec(inline_asm) unsigned __int8 _BitScanReverse(unsigned __int32 *out_bit_index, unsigned __int32 to_scan){
    // searches 'to_scan', if 'to_scan' is not zero the index is stored in eax,
    // otherwise otherwise the ZF is set
    bsr eax, to_scan
    
    // set 'found' if the ZF was not set
    setnz cl
    
    // save the index even if not found(in which case eax is undefined)
    mov rdx, out_bit_index
    mov [rdx], eax
    
    return cl
}

__declspec(inline_asm) unsigned __int8 _BitScanReverse64(unsigned __int32 *out_bit_index, unsigned __int64 to_scan){
    // searches 'to_scan', if 'to_scan' is not zero the index is stored in eax,
    // otherwise otherwise the ZF is set
    bsr rax, to_scan
    
    // set 'found' if the ZF was not set
    setnz cl
    
    // save the index even if not found(in which case eax is undefined)
    mov rdx, out_bit_index
    mov [rdx], eax
    
    return cl
}

__declspec(inline_asm) unsigned __int8 _bittest(__int32 *to_test, __int32 bit_index){
    // stores the bit at 'bit_index' and stores it in CF
    mov rcx, to_test
    bt [rcx], bit_index
    
    setc al
    return al
}

__declspec(inline_asm) unsigned __int8 _bittest64(__int64 *to_test, __int64 bit_index){
    // stores the bit at 'bit_index' and stores it in CF
    mov rcx, to_test
    bt [rcx], bit_index
    
    setc al
    return al
}

__declspec(inline_asm) unsigned __int8 _bittestandcomplement(__int32 *to_test, __int32 bit_index){
    mov rcx, to_test
    btc [rcx], bit_index
    
    setc al
    return al
}

__declspec(inline_asm) unsigned __int8 _bittestandcomplement64(__int64 *to_test, __int64 bit_index){
    mov rcx, to_test
    btc [rcx], bit_index
    
    setc al
    return al
}


__declspec(inline_asm) unsigned __int8 _bittestandreset(__int32 *to_test, __int32 bit_index){
    // stores the bit at 'bit_index'(< 32) and stores it in CF
    mov rcx, to_test
    btr [rcx], bit_index
    setc al
    
    return al
}

__declspec(inline_asm) unsigned __int8 _bittestandreset64(__int64 *to_test, __int64 bit_index){
    // stores the bit at 'bit_index'(< 62) and stores it in CF
    mov rcx, to_test
    btr [rcx], bit_index
    
    setc al
    return al
}


__declspec(inline_asm) unsigned __int8 _bittestandset(__int32 *to_test, __int32 bit_index){
    // stores the bit at 'bit_index'(< 32) and stores it in CF
    mov rcx, to_test
    bts [rcx], bit_index
    
    setc al
    return al
}

__declspec(inline_asm) unsigned __int8 _bittestandset64(__int64 *to_test, __int64 bit_index){
    // stores the bit at 'bit_index'(< 62) and stores it in CF
    mov rcx, to_test
    bts [rcx], bit_index
    
    setc al
    return al
}



__declspec(inline_asm) __int8 _InterlockedCompareExchange8(__int8 *destination, __int8 exchange, __int8 comparand){
    // cmpxchg compares al with the memory operand
    movzx eax, comparand
    
    // check atomically if '*destintaion == al'(comparand), if true 'exchange' is stored in '*destination'
    // otherwise *destination is stored in al
    mov rcx, destination
    lock cmpxchg [rcx], exchange
    
    // return the result(*destination) in either case
    return al
}


__declspec(inline_asm) __int16 _InterlockedCompareExchange16(__int16 *destination, __int16 exchange, __int16 comparand){
    
    // cmpxchg compares ax with the memory operand
    movzx eax, comparand
    
    // check atomically if '*destintaion == ax'(comparand), if true 'exchange' is stored in '*destination'
    // otherwise *destination is stored in ax
    mov rcx, destination
    lock cmpxchg [rcx], exchange
    
    // return the result(*destination) in either case
    return ax
}

__declspec(inline_asm) __int32 _InterlockedCompareExchange(__int32 *destination, __int32 exchange, __int32 comparand){
    // cmpxchg compares eax with the memory operand
    mov eax, comparand
    
    // check atomically if '*destintaion == eax'(comparand), if true 'exchange' is stored in '*destination'
    // otherwise *destination is stored in eax
    mov rcx, destination
    lock cmpxchg [rcx], exchange
    
    // return the result(*destination) in either case
    return eax
}

__declspec(inline_asm) __int64 _InterlockedCompareExchange64(__int64 *destination, __int64 exchange, __int64 comparand){
    
    // cmpxchg compares rax with the memory operand
    mov rax, comparand
    
    // check atomically if '*destintaion == rax'(comparand), if true 'exchange' is stored in '*destination'
    // otherwise *destination is stored in rax
    mov rcx, destination
    lock cmpxchg [rcx], exchange
    
    // return the result(*destination) in either case
    return rax
}

__declspec(inline_asm) unsigned __int8 _InterlockedCompareExchange128(__int64 *destination, __int64 exchange_high, __int64 exchange_low, __int64 *inout_comparand_result){
    
    // cmpxchg16b compares the memory operand with rdx:rax
    mov r9, inout_comparand_result
    mov rdx, [r9 + 8]
    mov rax, [r9 + 0]
    
    // on success cmpxchg16b places the value in rcx:rbx into the memory operand
    mov rcx, exchange_high
    mov rbx, exchange_low
    
    // on success the ZF is set and the destination is set to rcx:rdx, else ZF is cleared and
    // rdx:rax receives the value fetched from '*destination'.
    mov r8, destination
    lock cmpxchg16b [r8]
    
    // ZF is set iff the operation succeeded
    setz cl
    
    // whether or not we succeed we can write back rdx:rax to the result
    mov [r9 + 8], rdx
    mov [r9 + 0], rax
    
    return cl
}

// as we only support x64 this is just the same version as '_InterlockedCompareExchange64'
__declspec(inline_asm) void *_InterlockedCompareExchangePointer(void **destination, void *exchange, void *comparand){
    
    // cmpxchg compares rax with the memory operand
    mov rax, comparand
    
    // check atomically if '*destintaion == rax'(comparand), if true 'exchange' is stored in '*destination'
    // otherwise *destination is stored in rax
    mov rcx, destination
    lock cmpxchg [rcx], exchange
    
    // return the result(*destination) in either case
    return rax
}

__declspec(inline_asm) __int8 _InterlockedExchangeAdd8(__int8 *destination, __int8 to_add){
    // load 'to_add' into dl explicitly even tho it is already probably in there.
    mov dl, to_add
    
    // perform the atomic xadd, the preincremented '*destination' will be saved in dl
    mov rcx, destination
    lock xadd [rcx], dl
    
    // save the preincremented value in the 'result'
    return dl
}

__declspec(inline_asm) __int16 _InterlockedExchangeAdd16(__int16 *destination, __int16 to_add){
    // load 'to_add' into dx explicitly even tho it is already probably in there.
    mov dx, to_add
    
    // perform the atomic xadd, the preincremented '*destination' will be saved in dx
    mov rcx, destination
    lock xadd [rcx], dx
    
    // save the preincremented value in the 'result'
    return dx
}


__declspec(inline_asm) __int32 _InterlockedExchangeAdd(__int32 *destination, __int32 to_add){
    // load 'to_add' into rdx explicitly even tho it is already probably in there.
    mov edx, to_add
    
    // perform the atomic xadd, the preincremented '*destination' will be saved in edx
    mov rcx, destination
    lock xadd [rcx], edx
    
    // save the preincremented value in the 'result'
    return edx
}


__declspec(inline_asm) __int64 _InterlockedExchangeAdd64(__int64 *destination, __int64 to_add){
    // load 'to_add' into rdx explicitly even tho it is already probably in there.
    mov rdx, to_add
    
    // perform the atomic xadd, the preincremented '*destination' will be saved in rdx
    mov rcx, destination
    lock xadd [rcx], rdx
    
    // save the preincremented value in the 'result'
    return rdx
}

__declspec(inline_asm) long _InterlockedOr(long volatile *value, long mask){
    lock or [value], mask
    return mask // @cleanup: This is supposed to return the old value.
}

__declspec(inline_asm) __int8 _InterlockedIncrement8(__int8 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, 1
    
    // add the one, the preincremented value will be in dl
    mov rcx, to_increment
    lock xadd [rcx], dl
    
    // increment dl such that we have the 'incremented_value'
    inc dl
    
    // save the value
    return dl
}

__declspec(inline_asm) __int16 _InterlockedIncrement16(__int16 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, 1
    
    // add the one, the preincremented value will be in dx
    mov rcx, to_increment
    lock xadd [rcx], dx
    
    // increment dl such that we have the 'incremented_value'
    inc dx
    
    // save the value
    return dx
}

__declspec(inline_asm) __int32 _InterlockedIncrement(__int32 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, 1
    
    // add the one, the preincremented value will be in edx
    mov rcx, to_increment
    lock xadd [rcx], edx
    
    // increment dl such that we have the 'incremented_value'
    inc edx
    
    // save the value
    return edx
}


__declspec(inline_asm) __int64 _InterlockedIncrement64(__int64 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, 1
    
    // add the one, the preincremented value will be in rdx
    mov rcx, to_increment
    lock xadd [rcx], rdx
    
    // increment dl such that we have the 'incremented_value'
    inc rdx
    
    // save the value
    return rdx
}

__declspec(inline_asm) __int8 _InterlockedDecrement8(__int8 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, -1
    
    // add the one, the preincremented value will be in dl
    mov rcx, to_increment
    lock xadd [rcx], dl
    
    // increment dl such that we have the 'incremented_value'
    inc dl
    
    // save the value
    return dl
}

__declspec(inline_asm) __int16 _InterlockedDecrement16(__int16 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, -1
    
    // add the one, the preincremented value will be in dx
    mov rcx, to_increment
    lock xadd [rcx], dx
    
    // increment dl such that we have the 'incremented_value'
    inc dx
    
    // save the value
    return dx
}


__declspec(inline_asm) __int32 _InterlockedDecrement(__int32 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov edx, -1
    
    // add the one, the preincremented value will be in edx
    mov rcx, to_increment
    lock xadd [rcx], edx
    
    // increment dl such that we have the 'incremented_value'
    inc edx
    
    // save the value
    return edx
}


__declspec(inline_asm) __int64 _InterlockedDecrement64(__int64 *to_increment){
    // We need to return the incremented value, thus we have to use an xadd
    // to fetch the memory and cannot use a 'lock inc'.
    
    // load the one we xadd.
    mov rdx, -1
    
    // add the one, the preincremented value will be in rdx
    mov rcx, to_increment
    lock xadd [rcx], rdx
    
    // increment dl such that we have the 'incremented_value'
    inc rdx
    
    // save the value
    return rdx
}

__declspec(inline_asm) void __stosb(unsigned __int8 *destination, unsigned __int8 _byte, unsigned __int64 number_of_bytes_to_set){
    // stosb loads the byte in al into [rdi], rdi is incremented, rcx is decremented
    mov   rcx, number_of_bytes_to_set
    mov   rdi, destination
    movzx eax, _byte
    
    rep stosb
}

__declspec(inline_asm) void __stosw(unsigned __int16 *destination, unsigned __int16 word, unsigned __int64 number_of_words_to_set){
    // stosb loads the byte in al into [rdi], rdi is incremented, rcx is decremented
    mov   rcx, number_of_words_to_set
    mov   rdi, destination
    movzx eax, word
    
    rep stosw
}

__declspec(inline_asm) void __stosd(unsigned __int32 *destination, unsigned __int32 dword, unsigned __int64 number_of_dwords_to_set){
    // stosb loads the byte in al into [rdi], rdi is incremented, rcx is decremented
    mov rcx, number_of_dwords_to_set
    mov rdi, destination
    mov eax, dword
    
    rep stosd
}

__declspec(inline_asm) void __stosq(unsigned __int64 *destination, unsigned __int64 qword, unsigned __int64 number_of_qwords_to_set){
    mov rcx, number_of_qwords_to_set
    mov rdi, destination
    mov rax, qword
    
    rep stosq
}

__declspec(inline_asm) void __movsb(unsigned __int8 *destination, unsigned __int8 *source, unsigned __int64 number_of_bytes_to_copy){
    mov rcx, number_of_bytes_to_copy
    mov rdi, destination
    mov rsi, source
    
    rep movsb
}

__declspec(inline_asm) void __movsw(unsigned __int16 *destination, unsigned __int16 *source, unsigned __int64 number_of_words_to_copy){
    mov rcx, number_of_words_to_copy
    mov rdi, destination
    mov rsi, source
    
    rep movsw
}


__declspec(inline_asm) void __movsd(unsigned __int32 *destination, unsigned __int32 *source, unsigned __int64 number_of_dwords_to_copy){
    mov rcx, number_of_dwords_to_copy
    mov rdi, destination
    mov rsi, source
    
    rep movsd
}

__declspec(inline_asm) void __movsq(unsigned __int64 *destination, unsigned __int64 *source, unsigned __int64 number_of_qwords_to_copy){
    mov rcx, number_of_qwords_to_copy
    mov rdi, destination
    mov rsi, source
    
    rep movsq
}

__declspec(inline_asm) unsigned __int16 __popcnt16(unsigned __int16 value){
    popcnt ax, value
    return ax
}

__declspec(inline_asm) unsigned __int32 __popcnt(unsigned __int32 value){
    popcnt eax, value
    return eax
}

__declspec(inline_asm) unsigned __int64 __popcnt64(unsigned __int64 value){
    popcnt rax, value
    return rax
}

#if 0

__declspec(inline_asm) unsigned short _byteswap_ushort(unsigned short val){
    bswap val
    return val
}

__declspec(inline_asm) unsigned long _byteswap_ulong(unsigned long val){
    bswap val
    return val
}

__declspec(inline_asm) unsigned __int64 _byteswap_uint64(unsigned __int64 val){
    bswap val
    return val
}

#endif

__declspec(inline_asm) char _InterlockedExchange8(char *target, char value){
    movzx eax, value
    mov rcx, target
    xchg al, [rcx]
    return al
}

__declspec(inline_asm) short _InterlockedExchange16(short *target, short value){
    movzx eax, value
    mov rcx, target
    xchg ax, [rcx]
    return ax
}

__declspec(inline_asm) long _InterlockedExchange(long *target, long value){
    mov eax, value
    mov rcx, target
    xchg eax, [rcx]
    return eax
}

__declspec(inline_asm) __int64 _InterlockedExchange64(__int64 *target, __int64 value){
    mov rax, value
    mov rcx, target
    xchg rax, [rcx]
    return rax
}


__declspec(inline_asm) void *_InterlockedExchangePointer(void **target, void *value){
    mov rax, value
    mov rcx, target
    xchg rax, [rcx]
    return rax
}


__declspec(inline_asm) unsigned __int64 _umul128(unsigned __int64 Multiplier, unsigned __int64 Multiplicand, unsigned __int64 *HighProduct){
    mov rax, Multiplicand
    mul Multiplier
    mov [HighProduct], rdx
    return rax
}

__declspec(inline_asm) unsigned __int64 _udiv128(unsigned __int64 highDividend, unsigned __int64 lowDividend, unsigned __int64 divisor, unsigned __int64 *remainder){
    mov rdx, highDividend
    mov rax, lowDividend
    div divisor
    mov [remainder], rdx
    return rax
}

__declspec(inline_asm) void __faststorefence(void){
    lock or dword ptr [rsp], 0
}

__declspec(inline_asm) unsigned short __lzcnt16(unsigned short value){
    lzcnt value, value
    return value
}

__declspec(inline_asm) unsigned int __lzcnt(unsigned int value){
    lzcnt value, value
    return value
}

__declspec(inline_asm) unsigned __int64 __lzcnt64(unsigned __int64 value){
    lzcnt value, value
    return value
}

__declspec(inline_asm) unsigned int _lzcnt_u32(unsigned int value){
    lzcnt value, value
    return value
}

__declspec(inline_asm) unsigned __int64 _lzcnt_u64(unsigned __int64 value){
    lzcnt value, value
    return value
}


//
// GNU intrinsics
//
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

__declspec(inline_asm) int __builtin_expect(int __expression, int expected){
    return __expression
}

__declspec(inline_asm) void __builtin_trap(void){
    bytes{0f 0b} // ud2
}

__declspec(inline_asm) void __builtin_unreachable(void){
    bytes{0f 0b} // ud2
}


//
// SSE intrinsics
//

typedef union __declspec(intrin_type) __declspec(align(16)) __m128 {
    float               m128_f32[4];
    unsigned __int64    m128_u64[2];
    __int8              m128_i8[16];
    __int16             m128_i16[8];
    __int32             m128_i32[4];
    __int64             m128_i64[2];
    unsigned __int8     m128_u8[16];
    unsigned __int16    m128_u16[8];
    unsigned __int32    m128_u32[4];
} __m128;

typedef union __declspec(intrin_type) __declspec(align(16)) __m128i {
    __int8              m128i_i8[16];
    __int16             m128i_i16[8];
    __int32             m128i_i32[4];
    __int64             m128i_i64[2];
    unsigned __int8     m128i_u8[16];
    unsigned __int16    m128i_u16[8];
    unsigned __int32    m128i_u32[4];
    unsigned __int64    m128i_u64[2];
} __m128i;

typedef struct __declspec(intrin_type) __declspec(align(16)) __m128d {
    double              m128d_f64[2];
} __m128d;


__declspec(inline_asm) __m128 _mm_add_ps(__m128 a, __m128 b){
    addps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_add_ss(__m128 a, __m128 b){
    addss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_and_ps(__m128 a, __m128 b){
    andps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_andnot_ps(__m128 a, __m128 b){
    andnps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_cmpeq_ps(__m128 a, __m128 b){
    cmpeqps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpeq_ss(__m128 a, __m128 b){
    cmpeqss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpge_ps(__m128 a, __m128 b){
    cmpgeps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpge_ss(__m128 a, __m128 b){
    cmpgess a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpgt_ps(__m128 a, __m128 b){
    cmpgtps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpgt_ss(__m128 a, __m128 b){
    cmpgtss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmple_ps(__m128 a, __m128 b){
    cmpleps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmple_ss(__m128 a, __m128 b){
    cmpless a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmplt_ps(__m128 a, __m128 b){
    cmpltps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmplt_ss(__m128 a, __m128 b){
    cmpltss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpneq_ps(__m128 a, __m128 b){
    cmpneqps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpneq_ss(__m128 a, __m128 b){
    cmpneqss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpnge_ps(__m128 a, __m128 b){
    cmpngeps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpnge_ss(__m128 a, __m128 b){
    cmpngess a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpngt_ps(__m128 a, __m128 b){
    cmpngtps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpngt_ss(__m128 a, __m128 b){
    cmpngtss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpnle_ps(__m128 a, __m128 b){
    cmpnleps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpnle_ss(__m128 a, __m128 b){
    cmpnless a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpnlt_ps(__m128 a, __m128 b){
    cmpnltps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpnlt_ss(__m128 a, __m128 b){
    cmpnltss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_cmpord_ps(__m128 a, __m128 b){
    cmpordps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpord_ss(__m128 a, __m128 b){
    cmpordss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_cmpunord_ps(__m128 a, __m128 b){
    cmpunordps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_cmpunord_ss(__m128 a, __m128 b){
    cmpunordss a, b
    return a
}

__declspec(inline_asm) int _mm_comieq_ss(__m128 a, __m128 b){
    comiss a, b
    
    sete  al
    setnp cl
    and   al, cl
    movzx eax, al
    return eax
}
__declspec(inline_asm) int _mm_comige_ss(__m128 a, __m128 b){
    comiss a, b
    
    setae al
    movzx eax, al
    return eax
}
__declspec(inline_asm) int _mm_comigt_ss(__m128 a, __m128 b){
    comiss a, b
    
    seta  al
    movzx eax, al
    return eax
}
__declspec(inline_asm) int _mm_comile_ss(__m128 a, __m128 b){
    comiss b, a
    
    setae al
    movzx eax, al
    return eax
}
__declspec(inline_asm) int _mm_comilt_ss(__m128 a, __m128 b){
    comiss b, a
    
    seta  al
    movzx eax, al
    return eax
}
__declspec(inline_asm) int _mm_comineq_ss(__m128 a, __m128 b){
    comiss a, b
    
    setne al
    setp  cl
    or    al, cl
    movzx eax, al
    return eax
}

__declspec(inline_asm) __m128 _mm_cvtsi32_ss(__m128 a, int b){
    cvtsi2ss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_cvt_si2ss(__m128 a, int b){
    cvtsi2ss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_cvtsi64_ss(__m128 a, __int64 b){
    cvtsi2ss a, b
    return a
}

__declspec(inline_asm) float _mm_cvtss_f32(__m128 a){
    movss a, a
    return a
}

__declspec(inline_asm) int _mm_cvtss_si32(__m128 a){
    cvtss2si eax, a
    return eax
}

__declspec(inline_asm) __int64 _mm_cvtss_si64(__m128 a){
    cvtss2si rax, a
    return rax
}

__declspec(inline_asm) int _mm_cvtt_ss2si(__m128 a){
    cvttss2si eax, a
    return eax
}
__declspec(inline_asm) int _mm_cvttss_si32(__m128 a){
    cvttss2si eax, a
    return eax
}

__declspec(inline_asm) __int64 _mm_cvttss_si64(__m128 a){
    cvttss2si rax, a
    return rax
}

__declspec(inline_asm) __m128 _mm_div_ps(__m128 a, __m128 b){
    divps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_div_ss(__m128 a, __m128 b){
    divss a, b
    return a
}


__declspec(inline_asm) __m128 _mm_load_ps(float *mem_addr){
    mov rcx, mem_addr
    movups xmm0, xmmword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128 _mm_load_ps1(float *mem_addr){
    mov rcx, mem_addr
    movss  xmm0, dword ptr [rcx]
    shufps xmm0, xmm0, 0
    return xmm0
}

__declspec(inline_asm) __m128 _mm_load_ss(float const* mem_addr){
    mov rcx, mem_addr
    xorps xmm0, xmm0
    movss xmm0, dword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128 _mm_load1_ps(float const* mem_addr){
    mov    rcx, mem_addr
    movss  xmm0, dword ptr [rcx]
    shufps xmm0, xmm0, 0
    return xmm0
}

__declspec(inline_asm) __m128 _mm_loadr_ps(float const* mem_addr){
    mov rcx, mem_addr
    movups xmm0, xmmword ptr [rcx]
    shufps xmm0, xmm0, 27
    return xmm0
}


__declspec(inline_asm) __m128 _mm_loadu_ps(float *mem_addr){
    mov rdx, mem_addr
    movups xmm0, [rdx]
    return xmm0
}


__declspec(inline_asm) __m128 _mm_max_ps(__m128 a, __m128 b){
    maxps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_max_ss(__m128 a, __m128 b){
    maxss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_min_ps(__m128 a, __m128 b){
    minps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_min_ss(__m128 a, __m128 b){
    minss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_move_ss(__m128 a, __m128 b){
    movss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_movehl_ps(__m128 a, __m128 b){
    movhlps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_movelh_ps(__m128 a, __m128 b){
    movlhps a, b
    return a
}
__declspec(inline_asm) int _mm_movemask_ps(__m128 a){
    movmskps eax, a
    return eax
}

__declspec(inline_asm) __m128 _mm_mul_ps(__m128 a, __m128 b){
    mulps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_mul_ss(__m128 a, __m128 b){
    mulss a, b
    return a
}

__declspec(inline_asm) __m128 _mm_or_ps(__m128 a, __m128 b){
    orps a, b
    return a
}

__declspec(inline_asm) __m128 _mm_rcp_ps(__m128 a){
    rcpps a, a
    return a
}
__declspec(inline_asm) __m128 _mm_rcp_ss(__m128 a){
    rcpss a, a
    return a
}

__declspec(inline_asm) __m128 _mm_rsqrt_ps(__m128 a){
    rsqrtps a, a
    return a
}

__declspec(inline_asm) __m128 _mm_rsqrt_ss(__m128 a){
    rsqrtss a, a
    return a
}

__declspec(inline_asm) __m128 _mm_set_ps(float e3, float e2, float e1, float e0){
    movss xmm0, e0
    movss xmm1, e1
    movss xmm2, e2
    movss xmm3, e3
    
    unpcklps xmm1, xmm0
    unpcklps xmm3, xmm2
    
    movaps  xmm0, xmm3
    movlhps xmm0, xmm1
    
    return xmm0
}

__declspec(inline_asm) __m128 _mm_set_ps1(float a){
    shufps xmm0, a, 0
    return xmm0
}

__declspec(inline_asm) __m128 _mm_set_ss(float a){
    xorps  xmm0, xmm0
    movss  xmm0, a
    return xmm0
}

__declspec(inline_asm) __m128 _mm_set1_ps(float a){
    shufps xmm0, a, 0
    return xmm0
}

#if 0
void _mm_setcsr(unsigned int a){
    __asm__{
        lea rax, a
        ldmxcsr [rax]
    }
}
#endif

__declspec(inline_asm) __m128 _mm_setr_ps(float e3, float e2, float e1, float e0){
    movss xmm0, e3
    movss xmm1, e2
    movss xmm2, e1
    movss xmm3, e0
    
    unpcklps xmm1, xmm0
    unpcklps xmm3, xmm2
    
    movaps  xmm0, xmm3
    movlhps xmm0, xmm1
    
    return xmm0
}

__declspec(inline_asm) __m128 _mm_setzero_ps(void){
    xorps xmm0, xmm0
    return xmm0
}

__declspec(inline_asm) void _mm_sfence(void){
    sfence
}


__declspec(inline_asm) __m128 _mm_shuffle_ps(__m128 a, __m128 b, unsigned int imm8){
    movups xmm0, a
    movups xmm1, b
    shufps xmm0, xmm1, imm8
    return xmm0
}

__declspec(inline_asm) __m128 _mm_sqrt_ps(__m128 a){
    sqrtps a, a
    return a
}

__declspec(inline_asm) __m128 _mm_sqrt_ss(__m128 a){
    sqrtss a, a
    return a
}

__declspec(inline_asm) void _mm_store_ps(float *mem_addr, __m128 a){
    mov rcx, mem_addr
    movdqa xmm0, a
    movaps [rcx], xmm0
}

__declspec(inline_asm) void _mm_store_ps1(float* mem_addr, __m128 a){
    mov rcx, mem_addr
    
    shufps xmm0, a, 0
    movups xmmword ptr[rcx], xmm0
}

__declspec(inline_asm) void _mm_store_ss(float* mem_addr, __m128 a){
    mov rcx, mem_addr
    movss dword ptr [rcx], a
}
__declspec(inline_asm) void _mm_store1_ps(float* mem_addr, __m128 a){
    mov rcx, mem_addr
    shufps xmm0, a, 0
    movups xmmword ptr[rcx], xmm0
}

__declspec(inline_asm) void _mm_storer_ps(float* mem_addr, __m128 a){
    mov rcx, mem_addr
    shufps xmm0, a, 27
    movups xmmword ptr[rcx], xmm0
}

__declspec(inline_asm) void _mm_storeu_ps(float *mem_addr, __m128 a){
    mov rcx, mem_addr
    movdqa xmm0, a
    movups [rcx], xmm0
}

__declspec(inline_asm) __m128 _mm_sub_ps(__m128 a, __m128 b){
    subps a, b
    return a
}
__declspec(inline_asm) __m128 _mm_sub_ss(__m128 a, __m128 b){
    subss a, b
    return a
}

__declspec(inline_asm) int _mm_ucomieq_ss(__m128 a, __m128 b){
    cmpeqss a, b
    
    movd eax, xmm0
    and  eax, 1
    return eax
}
__declspec(inline_asm) int _mm_ucomige_ss(__m128 a, __m128 b){
    ucomiss a, b
    
    setae al
    movzx eax, al
    return eax
}
__declspec(inline_asm) int _mm_ucomigt_ss(__m128 a, __m128 b){
    ucomiss a, b
    
    seta  al
    movzx eax, al
    
    return eax
}
__declspec(inline_asm) int _mm_ucomile_ss(__m128 b, __m128 a){
    ucomiss b, a
    
    setae al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_ucomilt_ss(__m128 a, __m128 b){
    ucomiss b, a
    
    seta  al
    movzx eax, al
    
    return eax
}

__declspec(inline_asm) int _mm_ucomineq_ss(__m128 a, __m128 b){
    cmpneqss a, b
    
    movd eax, xmm0
    and eax, 1
    return eax
}

__declspec(inline_asm) __m128 _mm_undefined_ps(void){
    return xmm0
}


__declspec(inline_asm) __m128 _mm_unpackhi_ps(__m128 a, __m128 b){
    movups xmm0, a
    unpckhps xmm0, b
    return xmm0
}

__declspec(inline_asm) __m128 _mm_unpacklo_ps(__m128 a, __m128 b){
    movups xmm0, a
    unpcklps xmm0, b
    return xmm0
}

__declspec(inline_asm) __m128 _mm_xor_ps(__m128 a, __m128 b){
    xorps a, b
    return a
}

//
// SSE 2?
//


__declspec(inline_asm) __m128i _mm_add_epi8(__m128i a, __m128i b){
    paddb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_add_epi16(__m128i a, __m128i b){
    paddw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_add_epi32(__m128i a, __m128i b){
    paddd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_add_epi64(__m128i a, __m128i b){
    paddq a, b
    return a
}

__declspec(inline_asm) __m128d _mm_add_pd(__m128d a, __m128d b){
    addpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_add_sd(__m128d a, __m128d b){
    addsd a, b
    return a
}


__declspec(inline_asm) __m128i _mm_adds_epi8(__m128i a, __m128i b){
    paddsb a, b
    return a
}


__declspec(inline_asm) __m128i _mm_adds_epi16(__m128i a, __m128i b){
    paddsw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_adds_epu8(__m128i a, __m128i b){
    paddusb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_adds_epu16(__m128i a, __m128i b){
    paddusw a, b
    return a
}

__declspec(inline_asm) __m128d _mm_and_pd(__m128d a, __m128d b){
    andpd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_and_si128(__m128i a, __m128i b){
    pand a, b
    return a
}

__declspec(inline_asm) __m128d _mm_andnot_pd(__m128d a, __m128d b){
    andnpd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_andnot_si128(__m128i a, __m128i b){
    pandn a, b
    return a
}

__declspec(inline_asm) __m128i _mm_avg_epu8(__m128i a, __m128i b){
    pavgb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_avg_epu16(__m128i a, __m128i b){
    pavgw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_bslli_si128(__m128i a, int imm8){
    pslldq a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_bsrli_si128(__m128i a, int imm8){
    psrldq a, imm8
    return a
}

__declspec(inline_asm) __m128  _mm_castpd_ps(__m128d a)    { return a }
__declspec(inline_asm) __m128i _mm_castpd_si128(__m128d a) { return a }
__declspec(inline_asm) __m128d _mm_castps_pd(__m128 a)     { return a }
__declspec(inline_asm) __m128i _mm_castps_si128(__m128 a)  { return a }
__declspec(inline_asm) __m128d _mm_castsi128_pd(__m128i a) { return a }
__declspec(inline_asm) __m128  _mm_castsi128_ps(__m128i a) { return a }

__declspec(inline_asm) void _mm_clflush(void const* p){
    mov rcx, p
    clflush [rcx]
}

__declspec(inline_asm) __m128i _mm_cmpeq_epi8(__m128i a, __m128i b){
    pcmpeqb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cmpeq_epi16(__m128i a, __m128i b){
    pcmpeqw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cmpeq_epi32(__m128i a, __m128i b){
    pcmpeqd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpeq_pd(__m128d a, __m128d b){
    cmpeqpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpeq_sd(__m128d a, __m128d b){
    cmpeqsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpge_pd(__m128d a, __m128d b){
    cmpgepd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpge_sd(__m128d a, __m128d b){
    cmpgesd a, b
    return a
}


__declspec(inline_asm) __m128i _mm_cmpgt_epi8(__m128i a, __m128i b){
    pcmpgtb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cmpgt_epi16(__m128i a, __m128i b){
    pcmpgtw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cmpgt_epi32(__m128i a, __m128i b){
    pcmpgtd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpgt_pd(__m128d a, __m128d b){
    cmpgtpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpgt_sd(__m128d a, __m128d b){
    cmpgtsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmple_pd(__m128d a, __m128d b){
    cmplepd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmple_sd(__m128d a, __m128d b){
    cmplesd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cmplt_epi8(__m128i a, __m128i b){
    pcmpgtb b, a
    return a
}

__declspec(inline_asm) __m128i _mm_cmplt_epi16(__m128i a, __m128i b){
    pcmpgtw b, a
    return a
}

__declspec(inline_asm) __m128i _mm_cmplt_epi32(__m128i a, __m128i b){
    pcmpgtd b, a
    return a
}

__declspec(inline_asm) __m128d _mm_cmplt_pd(__m128d a, __m128d b){
    cmpltpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmplt_sd(__m128d a, __m128d b){
    cmpltsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpneq_pd(__m128d a, __m128d b){
    cmpneqpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpneq_sd(__m128d a, __m128d b){
    cmpneqsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpnge_pd(__m128d a, __m128d b){
    cmpngepd a, b
    return a
}


__declspec(inline_asm) __m128d _mm_cmpnge_sd(__m128d a, __m128d b){
    cmpngesd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpngt_pd(__m128d a, __m128d b){
    cmpngtpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpngt_sd(__m128d a, __m128d b){
    cmpngtsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpnle_pd(__m128d a, __m128d b){
    cmpnlepd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpnle_sd(__m128d a, __m128d b){
    cmpnlesd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpnlt_pd(__m128d a, __m128d b){
    cmpnltpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpnlt_sd(__m128d a, __m128d b){
    cmpnltsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpord_pd(__m128d a, __m128d b){
    cmpordpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpord_sd(__m128d a, __m128d b){
    cmpordsd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpunord_pd(__m128d a, __m128d b){
    cmpunordpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_cmpunord_sd(__m128d a, __m128d b){
    cmpunordsd a, b
    return a
}

__declspec(inline_asm) int _mm_comieq_sd(__m128d a, __m128d b){
    comisd a, b
    
    sete  al
    setnp cl
    and al, cl
    
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_comige_sd(__m128d a, __m128d b){
    comisd a, b
    
    setae al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_comigt_sd(__m128d a, __m128d b){
    comisd a, b
    
    seta al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_comile_sd(__m128d a, __m128d b){
    comisd b, a
    
    setae al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_comilt_sd(__m128d a, __m128d b){
    comisd b, a
    
    seta al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_comineq_sd(__m128d a, __m128d b){
    comisd a, b
    
    setne al
    setp  cl
    or al, cl
    
    movzx eax, al
    return eax
}

__declspec(inline_asm) __m128d _mm_cvtepi32_pd(__m128i a){
    cvtdq2pd a, a
    return a
}

__declspec(inline_asm) __m128 _mm_cvtepi32_ps(__m128i a){
    cvtdq2ps a, a
    return a
}

__declspec(inline_asm) __m128i _mm_cvtpd_epi32(__m128d a){
    cvtpd2dq a, a
    return a
}

__declspec(inline_asm) __m128 _mm_cvtpd_ps(__m128d a){
    cvtpd2ps a, a
    return a
}

__declspec(inline_asm) __m128i _mm_cvtps_epi32(__m128 a){
    cvtps2dq a, a
    return a
}

__declspec(inline_asm) __m128d _mm_cvtps_pd(__m128 a){
    cvtps2pd a, a
    return a
}

__declspec(inline_asm) double _mm_cvtsd_f64(__m128d a){
    movsd a, a
    return a
}

__declspec(inline_asm) __m128i _mm_cvtsi32_si128(int a){
    movd xmm0, a
    return xmm0
}

__declspec(inline_asm) int _mm_cvtsd_si32(__m128d a){
    cvtsd2si eax, a
    return eax
}

__declspec(inline_asm) __int64 _mm_cvtsd_si64(__m128d a){
    cvtsd2si rax, a
    return rax
}

__declspec(inline_asm) __int64 _mm_cvtsd_si64x(__m128d a){
    cvtsd2si rax, a
    return rax
}

__declspec(inline_asm) __m128 _mm_cvtsd_ss(__m128 a, __m128d b){
    cvtsd2ss a, b
    return a
}


__declspec(inline_asm) int _mm_cvtsi128_si32(__m128i a){
    movd eax, a
    return eax
}

__declspec(inline_asm) __int64 _mm_cvtsi128_si64(__m128i a){
    movq rax, a
    return rax
}

__declspec(inline_asm) __int64 _mm_cvtsi128_si64x(__m128i a){
    movq rax, a
    return rax
}

__declspec(inline_asm) __m128d _mm_cvtsi32_sd(__m128d a, int b){
    cvtsi2sd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cvtsi64_si128(__int64 a){
    movq xmm0, a
    return xmm0
}

__declspec(inline_asm) __m128d _mm_cvtsi64x_sd(__m128d a, __int64 b){
    cvtsi2sd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_cvtsi64x_si128(__int64 a){
    movq xmm0, a
    return xmm0
}

// __declspec(inline_asm) __m128d _mm_cvtss_sd(__m128d a, __m128 b){
// cvtss2sd a, b < what is this ?
// }

__declspec(inline_asm) __m128i _mm_cvttpd_epi32(__m128d a){
    cvttpd2dq a, a
    return a
}


__declspec(inline_asm) __m128i _mm_cvttps_epi32(__m128 a){
    cvttps2dq a, a
    return a
}

__declspec(inline_asm) int _mm_cvttsd_si32(__m128d a){
    cvttsd2si eax, a
    return eax
}

__declspec(inline_asm) __int64 _mm_cvttsd_si64(__m128d a){
    cvttsd2si rax, a
    return rax
}

__declspec(inline_asm) __int64 _mm_cvttsd_si64x(__m128d a){
    cvttsd2si rax, a
    return rax
}

__declspec(inline_asm) __m128d _mm_div_pd(__m128d a, __m128d b){
    divpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_div_sd(__m128d a, __m128d b){
    divsd a, b
    return a
}

__declspec(inline_asm) int _mm_extract_epi16(__m128i a, int imm8){
    pextrw eax, a, imm8
    return eax
}

__declspec(inline_asm) __m128i _mm_insert_epi16(__m128i a, int i, int imm8){
    pinsrw a, i, imm8
    return a
}

__declspec(inline_asm) void _mm_lfence(void){
    lfence
}

__declspec(inline_asm) __m128d _mm_load_pd(double const* mem_addr){
    mov rcx, mem_addr
    movupd xmm0, xmmword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128d _mm_load_pd1(double const* mem_addr){
    mov rcx, mem_addr
    movsd xmm0, qword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128d _mm_load_sd(double const* mem_addr){
    mov rcx, mem_addr
    xorps xmm0, xmm0
    movsd xmm0, qword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128i _mm_load_si128(__m128i const* mem_addr){
    mov rcx, mem_addr
    movdqa xmm0, xmmword ptr[rcx]
    return xmm0
}

__declspec(inline_asm) __m128d _mm_load1_pd(double const* mem_addr){
    mov rcx, mem_addr
    movsd xmm0, qword ptr[rcx]
    shufpd xmm0, xmm0, 0
    return xmm0
}

__declspec(inline_asm) __m128d _mm_loadh_pd(__m128d a, double const* mem_addr){
    mov rcx, mem_addr
    movhpd a, qword ptr [rcx]
    return a
}

__declspec(inline_asm) __m128i _mm_loadl_epi64(__m128i const* mem_addr){
    mov rcx, mem_addr
    movq xmm0, qword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128d _mm_loadl_pd(__m128d a, double const* mem_addr){
    mov rcx, mem_addr
    movlpd a, qword ptr [rcx]
    return a
}

__declspec(inline_asm) __m128d _mm_loadr_pd(double const* mem_addr){
    mov rcx, mem_addr
    shufpd xmm0, xmmword ptr [rcx], 1
    return xmm0
}

__declspec(inline_asm) __m128d _mm_loadu_pd(double const* mem_addr){
    mov rcx, mem_addr
    movups xmm0, xmmword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128i _mm_loadu_si128(__m128i const* mem_addr){
    mov rcx, mem_addr
    movdqu xmm0, xmmword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128i _mm_loadu_si16(void const* mem_addr){
    mov rcx, mem_addr
    movzx eax, word ptr[rcx]
    movd xmm0, eax
    return xmm0
}

__declspec(inline_asm) __m128i _mm_loadu_si32(void const* mem_addr){
    mov rcx, mem_addr
    movd xmm0, dword ptr [rcx]
    return xmm0
}

__declspec(inline_asm) __m128i _mm_loadu_si64(void const* mem_addr){
    mov rcx, mem_addr
    movq xmm0, qword ptr [rcx]
    return xmm0
}


__declspec(inline_asm) __m128i _mm_madd_epi16(__m128i a, __m128i b){
    pmaddwd a, b
    return a
}

__declspec(inline_asm) void _mm_maskmoveu_si128(__m128i a, __m128i mask, char* mem_addr){
    // load the 'mem_addr' into the implicit memory operand
    mov rdi, mem_addr
    
    // perform the operation using 'a' and the 'mask'. This will write to [rdi]
    maskmovdqu a, mask
}


__declspec(inline_asm) __m128i _mm_max_epi16(__m128i a, __m128i b){
    pmaxsw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_max_epu8(__m128i a, __m128i b){
    pmaxub a, b
    return a
}

__declspec(inline_asm) __m128d _mm_max_pd(__m128d a, __m128d b){
    maxpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_max_sd(__m128d a, __m128d b){
    maxsd a, b
    return a
}

__declspec(inline_asm) void _mm_mfence(void){
    mfence
}


__declspec(inline_asm) __m128i _mm_min_epu8(__m128i a, __m128i b){
    pminub a, b
    return a
}

__declspec(inline_asm) __m128i _mm_min_epi16(__m128i a, __m128i b){
    pminsw a, b
    return a
}

__declspec(inline_asm) __m128d _mm_min_pd(__m128d a, __m128d b){
    minpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_min_sd(__m128d a, __m128d b){
    minsd a, b
    return a
}


// __m128i _mm_move_epi64(__m128i a){
    // movq I don't get this
// }

__declspec(inline_asm) __m128d _mm_move_sd(__m128d a, __m128d b){
    movaps xmm0, a
    movaps xmm1, b
    movsd xmm0, xmm1
    return xmm0
}

__declspec(inline_asm) int _mm_movemask_epi8(__m128i a){
    pmovmskb eax, a
    return  eax
}

__declspec(inline_asm) int _mm_movemask_pd(__m128d a){
    movmskpd eax, a
    return eax
}

__declspec(inline_asm) __m128i _mm_mul_epu32(__m128i a, __m128i b){
    pmuludq a, b
    return a
}

__declspec(inline_asm) __m128d _mm_mul_pd(__m128d a, __m128d b){
    mulpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_mul_sd(__m128d a, __m128d b){
    mulsd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_mulhi_epi16(__m128i a, __m128i b){
    pmulhw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_mulhi_epu16(__m128i a, __m128i b){
    pmulhuw a, b
    return a
}


__declspec(inline_asm) __m128i _mm_mullo_epi16(__m128i a, __m128i b){
    movdqa xmm0, a
    pmullw xmm0, b
    return xmm0
}


__declspec(inline_asm) __m128d _mm_or_pd(__m128d a, __m128d b){
    orpd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_or_si128(__m128i a, __m128i b){
    por a, b
    return a
}

__declspec(inline_asm) __m128i _mm_packs_epi16(__m128i a, __m128i b){
    packsswb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_packs_epi32(__m128i a, __m128i b){
    packssdw a, b
    return a
}


__declspec(inline_asm) __m128i _mm_packus_epi16(__m128i a, __m128i b){
    packuswb a, b
    return a
}

__declspec(inline_asm) void _mm_pause(void){
    pause
}


__declspec(inline_asm) __m128i _mm_sad_epu8(__m128i a, __m128i b){
    psadbw a, b
    return a
}


__declspec(inline_asm) __m128i _mm_set_epi16(short e7, short e6, short e5, short e4, short e3, short e2, short e1, short e0){
    sub rsp, 24
    mov word ptr[rsp +  0], e0
    mov word ptr[rsp +  2], e1
    mov word ptr[rsp +  4], e2
    mov word ptr[rsp +  6], e3
    mov word ptr[rsp +  8], e4
    mov word ptr[rsp + 10], e5
    mov word ptr[rsp + 12], e6
    mov word ptr[rsp + 14], e7
    
    movdqu xmm0, [rsp]
    add rsp, 24
    
    return xmm0
}

__declspec(inline_asm) __m128i _mm_set_epi32(int e3, int e2, int e1, int e0){
    sub rsp, 24
    mov dword ptr[rsp +  0], e0
    mov dword ptr[rsp +  4], e1
    mov dword ptr[rsp +  8], e2
    mov dword ptr[rsp + 12], e3
    
    movdqu xmm0, [rsp]
    add rsp, 24
    
    return xmm0
}


__declspec(inline_asm) __m128i _mm_set_epi64x(__int64 e1, __int64 e0){
    sub rsp, 24
    mov qword ptr[rsp + 0], e0
    mov qword ptr[rsp + 8], e1
    
    movdqu xmm0, [rsp]
    add rsp, 24
    
    return xmm0
}

__declspec(inline_asm) __m128i _mm_set_epi8(char e15, char e14, char e13, char e12, char e11, char e10, char e9, char e8, char e7, char e6, char e5, char e4, char e3, char e2, char e1, char e0){
    sub rsp, 24
    mov byte ptr[rsp +  0], e0
    mov byte ptr[rsp +  1], e1
    mov byte ptr[rsp +  2], e2
    mov byte ptr[rsp +  3], e3
    mov byte ptr[rsp +  4], e4
    mov byte ptr[rsp +  5], e5
    mov byte ptr[rsp +  6], e6
    mov byte ptr[rsp +  7], e7
    mov byte ptr[rsp +  8], e8
    mov byte ptr[rsp +  9], e9
    mov byte ptr[rsp + 10], e10
    mov byte ptr[rsp + 11], e11
    mov byte ptr[rsp + 12], e12
    mov byte ptr[rsp + 13], e13
    mov byte ptr[rsp + 14], e14
    mov byte ptr[rsp + 15], e15
    
    movdqu xmm0, [rsp]
    add rsp, 24
    
    return xmm0
}


__declspec(inline_asm) __m128d _mm_set_pd(double e1, double e0){
    movsd xmm0, e1
    movsd xmm1, e0
    
    unpcklpd xmm1, xmm0
    return xmm1
}

__declspec(inline_asm) __m128d _mm_set_pd1(double a){
    movsd xmm0, a
    unpcklpd xmm0, xmm0
    return xmm0
}

// @cleanup: I am not sure any of this has to happen?
__declspec(inline_asm) __m128d _mm_set_sd(double a){
    xorps xmm0, xmm0
    movsd xmm0, a
    return xmm0
}


__declspec(inline_asm) __m128i _mm_set1_epi16(short a){
    movsx     eax,  a
    movd      xmm0, eax
    punpcklwd xmm0, xmm0
    pshufd    xmm0, xmm0, 0
    return xmm0
}

__declspec(inline_asm) __m128i _mm_set1_epi32(int a){
    movd    xmm0, a
    pshufd  xmm0, xmm0, 0
    return xmm0
}

__declspec(inline_asm) __m128i _mm_set1_epi64x(__int64 a){
    movq       xmm0, a
    punpcklqdq xmm0, xmm0
    return xmm0
}

__declspec(inline_asm) __m128i _mm_set1_epi8(char a){
    movsx     eax,  a
    movd      xmm0, eax
    punpcklbw xmm0, xmm0
    punpcklwd xmm0, xmm0
    pshufd    xmm0, xmm0, 0
    return xmm0
}

__declspec(inline_asm) __m128d _mm_set1_pd(double a){
    movsd xmm0, a
    unpcklpd xmm0, xmm0
    return xmm0
}

__declspec(inline_asm) __m128i _mm_setr_epi16(short e7, short e6, short e5, short e4, short e3, short e2, short e1, short e0){
    sub rsp, 24
    mov word ptr[rsp +  0], e7
    mov word ptr[rsp +  2], e6
    mov word ptr[rsp +  4], e5
    mov word ptr[rsp +  6], e4
    mov word ptr[rsp +  8], e3
    mov word ptr[rsp + 10], e2
    mov word ptr[rsp + 12], e1
    mov word ptr[rsp + 14], e0
    
    movdqu xmm0, [rsp]
    add rsp, 24
    
    return xmm0
}

__declspec(inline_asm) __m128i _mm_setr_epi32(int e3, int e2, int e1, int e0){
    movd xmm0, e0
    movd xmm1, e1
    movd xmm2, e2
    movd xmm3, e3
    
    punpckldq xmm1, xmm0
    punpckldq xmm3, xmm2
    movdqa xmm0, xmm3
    
    punpcklqdq xmm0, xmm1
    return xmm0
}

__declspec(inline_asm) __m128i _mm_setr_epi8(char e15, char e14, char e13, char e12, char e11, char e10, char e9, char e8, char e7, char e6, char e5, char e4, char e3, char e2, char e1, char e0){
    sub rsp, 24
    mov byte ptr[rsp +  0], e15
    mov byte ptr[rsp +  1], e14
    mov byte ptr[rsp +  2], e13
    mov byte ptr[rsp +  3], e12
    mov byte ptr[rsp +  4], e11
    mov byte ptr[rsp +  5], e10
    mov byte ptr[rsp +  6], e9
    mov byte ptr[rsp +  7], e8
    mov byte ptr[rsp +  8], e7
    mov byte ptr[rsp +  9], e6
    mov byte ptr[rsp + 10], e5
    mov byte ptr[rsp + 11], e4
    mov byte ptr[rsp + 12], e3
    mov byte ptr[rsp + 13], e2
    mov byte ptr[rsp + 14], e1
    mov byte ptr[rsp + 15], e0
    
    movdqu xmm0, [rsp]
    add rsp, 24
    
    return xmm0
}

__declspec(inline_asm) __m128d _mm_setr_pd(double e1, double e0){
    movsd xmm0, e0
    movsd xmm1, e1
    unpcklpd xmm1, xmm0
    return xmm1
}


__declspec(inline_asm) __m128d _mm_setzero_pd(void){
    xorpd xmm0, xmm0
    return xmm0
}


__declspec(inline_asm) __m128i _mm_setzero_si128(void){
    pxor xmm0, xmm0
    return xmm0
}


__declspec(inline_asm) __m128i _mm_shuffle_epi32(__m128i a, int imm8){
    pshufd a, a, imm8
    return a
}


__declspec(inline_asm) __m128d _mm_shuffle_pd(__m128d a, __m128d b, int imm8){
    shufpd a, b, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_shufflehi_epi16(__m128i a, int imm8){
    pshufhw xmm0, a, imm8
    return xmm0
}

__declspec(inline_asm) __m128i _mm_shufflelo_epi16(__m128i a, int imm8){
    pshuflw xmm0, a, imm8
    return xmm0
}

__declspec(inline_asm) __m128i _mm_sll_epi16(__m128i a, __m128i count){
    psllw a, count
    return a
}

__declspec(inline_asm) __m128i _mm_sll_epi32(__m128i a, __m128i count){
    pslld a, count
    return a
}

__declspec(inline_asm) __m128i _mm_sll_epi64(__m128i a, __m128i count){
    psllq a, count
    return a
}


__declspec(inline_asm) __m128i _mm_slli_epi16(__m128i a, int imm8){
    psllw a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_slli_epi32(__m128i a, int imm8){
    pslld a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_slli_epi64(__m128i a, int imm8){
    psllq a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_slli_si128(__m128i a, int imm8){
    pslldq a, imm8
    return a
}

__declspec(inline_asm) __m128d _mm_sqrt_pd(__m128d a){
    sqrtpd a, a
    return a
}

__declspec(inline_asm) __m128d _mm_sqrt_sd(__m128d a, __m128d b){
    sqrtsd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_sra_epi16(__m128i a, __m128i count){
    psraw a, count
    return a
}

__declspec(inline_asm) __m128i _mm_sra_epi32(__m128i a, __m128i count){
    psrad a, count
    return a
}

__declspec(inline_asm) __m128i _mm_srai_epi16(__m128i a, int imm8){
    psraw a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_srai_epi32(__m128i a, int imm8){
    psrad a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_srl_epi16(__m128i a, __m128i count){
    psrlw a, count
    return a
}

__declspec(inline_asm) __m128i _mm_srl_epi32(__m128i a, __m128i count){
    psrld a, count
    return a
}

__declspec(inline_asm) __m128i _mm_srl_epi64(__m128i a, __m128i count){
    psrlq a, count
    return a
}


__declspec(inline_asm) __m128i _mm_srli_epi16(__m128i a, int imm8){
    psrlw  a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_srli_epi32(__m128i a, int imm8){
    psrld a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_srli_epi64(__m128i a, int imm8){
    psrlq a, imm8
    return a
}

__declspec(inline_asm) __m128i _mm_srli_si128(__m128i a, int imm8){
    psrldq a, imm8
    return a
}

__declspec(inline_asm) void _mm_store_pd(double *mem_addr, __m128d a){
    mov rcx, mem_addr
    movups [rcx], a
}


__declspec(inline_asm) void _mm_store_pd1(double* mem_addr, __m128d a){
    shufps a, a, 0
    
    mov rcx, mem_addr
    movups [rcx], a
}

__declspec(inline_asm) void _mm_store_sd(double* mem_addr, __m128d a){
    mov rcx, mem_addr
    movsd qword ptr [rcx], a
}


__declspec(inline_asm) void _mm_store_si128(__m128i *mem_addr, __m128i a){
    mov rcx, mem_addr
    movdqa xmm0, a
    movdqa [rcx], xmm0
}

__declspec(inline_asm) void _mm_store1_pd(double* mem_addr, __m128d a){
    shufpd xmm0, xmm0, 0
    
    mov rcx, mem_addr
    movups xmmword ptr[rcx], xmm0
}

__declspec(inline_asm) void _mm_storeh_pd(double* mem_addr, __m128d a){
    mov rcx, mem_addr
    movhpd qword ptr[rcx], a
}

__declspec(inline_asm) void _mm_storel_epi64(__m128i* mem_addr, __m128i a){
    mov rcx, mem_addr
    movq qword ptr[rcx], a
}


__declspec(inline_asm) void _mm_storel_pd(double* mem_addr, __m128d a){
    mov rcx, mem_addr
    movlpd qword ptr[rcx], a
}

__declspec(inline_asm) void _mm_storer_pd(double* mem_addr, __m128d a){
    shufpd a, a, 1
    
    mov rcx, mem_addr
    movups [rcx], a
}

__declspec(inline_asm) void _mm_storeu_pd(double* mem_addr, __m128d a){
    mov rcx, mem_addr
    movups xmmword ptr [rcx], a
}


__declspec(inline_asm) void _mm_storeu_si128(__m128i *mem_addr, __m128i a){
    mov rcx, mem_addr
    movdqa xmm0, a
    movdqu [rcx], xmm0
}


__declspec(inline_asm) void _mm_storeu_si16(void* mem_addr, __m128i a){
    mov rcx, mem_addr
    movd eax, a
    mov word ptr [rcx], ax
}


__declspec(inline_asm) void _mm_storeu_si32(void* mem_addr, __m128i a){
    mov rcx, mem_addr
    movd eax, a
    mov dword ptr [rcx], eax
}

__declspec(inline_asm) void _mm_storeu_si64(void* mem_addr, __m128i a){
    mov rcx, mem_addr
    movq qword ptr[rcx], a
}

__declspec(inline_asm) void _mm_stream_pd(double* mem_addr, __m128d a){
    mov rcx, mem_addr
    movntpd xmmword ptr[rcx], a
}

__declspec(inline_asm) void _mm_stream_si128(__m128i* mem_addr, __m128i a){
    mov rcx, mem_addr
    movntdq xmmword ptr[rcx], a
}


__declspec(inline_asm) void _mm_stream_si32(int* mem_addr, int a){
    mov rcx, mem_addr
    movnti dword ptr [rcx], a
}

__declspec(inline_asm) void _mm_stream_si64(__int64* mem_addr, __int64 a){
    mov rcx, mem_addr
    movnti qword ptr [rcx], a
}


__declspec(inline_asm) __m128i _mm_sub_epi16(__m128i a, __m128i b){
    movdqa xmm0, a
    psubw  xmm0, b
    return xmm0
}

__declspec(inline_asm) __m128i _mm_sub_epi32(__m128i a, __m128i b){
    psubd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_sub_epi64(__m128i a, __m128i b){
    psubq a, b
    return a
}

__declspec(inline_asm) __m128i _mm_sub_epi8(__m128i a, __m128i b){
    psubb a, b
    return a
}

__declspec(inline_asm) __m128d _mm_sub_pd(__m128d a, __m128d b){
    subpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_sub_sd(__m128d a, __m128d b){
    subsd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_subs_epi16(__m128i a, __m128i b){
    psubsw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_subs_epi8(__m128i a, __m128i b){
    psubsb a, b
    return a
}

__declspec(inline_asm) __m128i _mm_subs_epu16(__m128i a, __m128i b){
    psubusw a, b
    return a
}

__declspec(inline_asm) __m128i _mm_subs_epu8(__m128i a, __m128i b){
    psubusb a, b
    return a
}


__declspec(inline_asm) int _mm_ucomieq_sd(__m128d a, __m128d b){
    cmpeqsd a, b
    movd eax, a
    and eax, 1
    return eax
}

__declspec(inline_asm) int _mm_ucomige_sd(__m128d a, __m128d b){
    ucomisd a, b
    
    setae al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_ucomigt_sd(__m128d a, __m128d b){
    ucomisd a, b
    
    seta al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_ucomile_sd(__m128d a, __m128d b){
    ucomisd b, a
    
    setae al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_ucomilt_sd(__m128d a, __m128d b){
    ucomisd b, a
    
    seta al
    movzx eax, al
    return eax
}

__declspec(inline_asm) int _mm_ucomineq_sd(__m128d a, __m128d b){
    cmpneqsd a, b
    movd eax, a
    and eax, 1
    return eax
}

__declspec(inline_asm) __m128d _mm_undefined_pd(void){
    return xmm0
}

__declspec(inline_asm) __m128i _mm_undefined_si128(void){
    return xmm0
}

__declspec(inline_asm) __m128i _mm_unpackhi_epi16(__m128i a, __m128i b){
    movdqa    xmm0, a
    punpckhwd xmm0, b
    return    xmm0
}


__declspec(inline_asm) __m128i _mm_unpackhi_epi32(__m128i a, __m128i b){
    movdqa    xmm0, a
    punpckhdq xmm0, b
    return    xmm0
}

__declspec(inline_asm) __m128i _mm_unpackhi_epi64(__m128i a, __m128i b){
    punpckhqdq a, b
    return a
}


__declspec(inline_asm) __m128i _mm_unpackhi_epi8(__m128i a, __m128i b){
    punpckhbw a, b
    return a
}


__declspec(inline_asm) __m128d _mm_unpackhi_pd(__m128d a, __m128d b){
    unpckhpd a, b
    return a
}


__declspec(inline_asm) __m128i _mm_unpacklo_epi16(__m128i a, __m128i b){
    punpcklwd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_unpacklo_epi32(__m128i a, __m128i b){
    punpckldq a, b
    return a
}

__declspec(inline_asm) __m128i _mm_unpacklo_epi64(__m128i a, __m128i b){
    punpcklqdq a, b
    return a
}

__declspec(inline_asm) __m128i _mm_unpacklo_epi8(__m128i a, __m128i b){
    punpcklbw a, b
    return a
}

__declspec(inline_asm) __m128d _mm_unpacklo_pd(__m128d a, __m128d b){
    unpcklpd a, b
    return a
}

__declspec(inline_asm) __m128d _mm_xor_pd(__m128d a, __m128d b){
    xorpd a, b
    return a
}

__declspec(inline_asm) __m128i _mm_xor_si128(__m128i a, __m128i b){
    movups xmm0, a
    pxor xmm0, b
    return xmm0
}


//
// SSE 3 ?
//

__declspec(inline_asm) void _mm_prefetch(char *a, int hint){
    mov rcx, a
    prefetch hint, [rcx]
}


__declspec(inline_asm) __m128i _mm_andn_si128(__m128i a, __m128i b){
    movups xmm0, a
    pandn xmm0, b
    return xmm0
}

__declspec(inline_asm) void _mm_stream_ps(float *mem_addr, __m128 a){
    mov rcx, mem_addr
    movdqa xmm0, a
    movntps [rcx], xmm0
}

__declspec(inline_asm) __m128i _mm_shuffle_epi8(__m128i a, __m128i b){
    pshufb a, b
    return a
}


__declspec(inline_asm) __int64 _mm_extract_epi64(__m128i a, const int imm8){
    pextrq rax, a, imm8
    return rax
}

__declspec(inline_asm) int _mm_extract_epi32(__m128i a, const int imm8){
    pextrd eax, a, imm8
    return eax
}


__declspec(inline_asm) __m128i _mm_aesdec_si128(__m128i a, __m128i RoundKey){
    aesdec a, RoundKey
    return a
}

__declspec(inline_asm) __m128i _mm_alignr_epi8(__m128i a, __m128i b, int imm8){
    palignr a, b, imm8
    return a
}

//
// SSE4.2
//
__declspec(inline_asm) int _mm_cmpistri(__m128i a, __m128i b, int imm8){
    movdqa  xmm0, a
    movdqa  xmm1, b
    
    // "Perform a packed comparison of string data with implicit lengths,
    //  generating an index, and storing the result in ECX."
    pcmpistri xmm0, xmm1, imm8
    
    return ecx
}

__declspec(inline_asm) int _mm_cmpestrc(__m128i a, int la, __m128i b, int lb, const int imm8){
    movaps  xmm0, a
    movaps  xmm1, b
    
    // The input length register is EAX/RAX(for xmm1) or EDX/RDX(for xmm2/m128).
    mov     eax, la
    mov     edx, lb
    
    // "Perform a packed comparison of string data with explicit lengths,
    //  generating an index, and storing the result in ECX."
    // "CFlag - Reset if IntRes2 is equal to zero, set otherwise."
    pcmpestri xmm1, xmm0, 1
    
    // "returns 1 if the resulting mask was non-zero, and 0 otherwise."
    mov     eax, 0
    mov     ecx, 1
    cmovb   eax, ecx
    
    return eax
}

//
// __m256 intrinsics
//

typedef union __declspec(intrin_type) __declspec(align(32)) __m256 {
    float m256_f32[8];
} __m256;

typedef struct __declspec(intrin_type) __declspec(align(32)) __m256d {
    double m256d_f64[4];
} __m256d;

typedef union  __declspec(intrin_type) __declspec(align(32)) __m256i {
    __int8              m256i_i8[32];
    __int16             m256i_i16[16];
    __int32             m256i_i32[8];
    __int64             m256i_i64[4];
    unsigned __int8     m256i_u8[32];
    unsigned __int16    m256i_u16[16];
    unsigned __int32    m256i_u32[8];
    unsigned __int64    m256i_u64[4];
} __m256i;


__declspec(inline_asm) __m256 _mm256_add_ps(__m256 a, __m256 b){
    vmovups ymm0, a
    vaddps ymm0, ymm0, b
    return ymm0
}

__declspec(inline_asm) __m256 _mm256_mul_ps(__m256 a, __m256 b){
    vmovups ymm0, a
    vmulps ymm0, ymm0, b
    return ymm0
}

__declspec(inline_asm) __m256 _mm256_blend_ps(__m256 a, __m256 b, int imm8){
    vmovups ymm0, a
    vblendps ymm0, ymm0, b, imm8
    return ymm0
}

__declspec(inline_asm) __m256 _mm256_permute2f128_ps(__m256 a, __m256 b, int imm8){
    vmovups ymm0, a
    vperm2f128 ymm0, ymm0, b, imm8
    return ymm0
}

__declspec(inline_asm) __m256 _mm256_shuffle_ps(__m256 a, __m256 b, int imm8){
    vmovups ymm0, a
    vshufps ymm0, ymm0, b, imm8
    return ymm0
}

__declspec(inline_asm) __m256 _mm256_set1_ps(float a){
    vmovss  xmm0, a
    vshufps xmm0, xmm0, xmm0, 0
    vinsertf128 ymm0, ymm0, xmm0, 1
    return ymm0
}

__declspec(inline_asm) __m256 _mm256_loadu_ps(float const * mem_addr){
    mov rcx, mem_addr
    vmovups ymm0, [rcx]
    return ymm0
}

__declspec(inline_asm) void _mm256_storeu_ps(float *mem_addr, __m256 a){
    mov rcx, mem_addr
    vmovups ymm0, a
    vmovups [rcx], ymm0
}

__declspec(inline_asm) void _mm256_storeu_si256(__m256i * mem_addr, __m256i a){
    mov rcx, mem_addr
    vmovdqu ymm0, a
    vmovdqu [rcx], ymm0
}

__declspec(inline_asm) __m256i _mm256_loadu_si256(__m256i const * mem_addr){
    mov rcx, mem_addr
    vmovdqu ymm0, [rcx]
    return ymm0
}

__declspec(inline_asm) __m256i _mm256_set_epi8(char e31, char e30, char e29, char e28, char e27, char e26, char e25, char e24, char e23, char e22, char e21, char e20, char e19, char e18, char e17, char e16, char e15, char e14, char e13, char e12, char e11, char e10, char e9, char e8, char e7, char e6, char e5, char e4, char e3, char e2, char e1, char e0){
    sub rsp, 40
    mov byte ptr[rsp +  0], e0
    mov byte ptr[rsp +  1], e1
    mov byte ptr[rsp +  2], e2
    mov byte ptr[rsp +  3], e3
    mov byte ptr[rsp +  4], e4
    mov byte ptr[rsp +  5], e5
    mov byte ptr[rsp +  6], e6
    mov byte ptr[rsp +  7], e7
    mov byte ptr[rsp +  8], e8
    mov byte ptr[rsp +  9], e9
    mov byte ptr[rsp + 10], e10
    mov byte ptr[rsp + 11], e11
    mov byte ptr[rsp + 12], e12
    mov byte ptr[rsp + 13], e13
    mov byte ptr[rsp + 14], e14
    mov byte ptr[rsp + 15], e15
    mov byte ptr[rsp + 16], e16
    mov byte ptr[rsp + 17], e17
    mov byte ptr[rsp + 18], e18
    mov byte ptr[rsp + 19], e19
    mov byte ptr[rsp + 20], e20
    mov byte ptr[rsp + 21], e21
    mov byte ptr[rsp + 22], e22
    mov byte ptr[rsp + 23], e23
    mov byte ptr[rsp + 24], e24
    mov byte ptr[rsp + 25], e25
    mov byte ptr[rsp + 26], e26
    mov byte ptr[rsp + 27], e27
    mov byte ptr[rsp + 28], e28
    mov byte ptr[rsp + 29], e29
    mov byte ptr[rsp + 30], e30
    mov byte ptr[rsp + 31], e31
    
    vmovdqu ymm0, [rsp]
    add rsp, 40
    
    return ymm0
}


__declspec(inline_asm) int _mm256_testz_si256(__m256i a, __m256i b){
    vmovdqu ymm0, a
    vmovdqu ymm1, b
    
    vptest ymm0, ymm1
    sete   al
    movzx  eax, al
    return eax
}

__declspec(inline_asm) __m256i _mm256_min_epu8(__m256i a, __m256i b){
    vmovdqu ymm0, a
    vmovdqu ymm1, b
    vpminub ymm0, ymm0, ymm1
    return ymm0
}


__declspec(inline_asm) int _mm256_movemask_epi8(__m256i a){
    vpmovmskb eax, a
    return eax
}


__declspec(inline_asm) __m256i _mm256_set1_epi8(char a){
    movsx   eax,  a
    vmovd   xmm0, eax
    vpxor   xmm1, xmm1, xmm1
    vpshufb xmm0, xmm0, xmm1
    vinsertf128 ymm0, ymm0, xmm0, 1
    return ymm0
}


__declspec(inline_asm)__m256i _mm256_cmpeq_epi8(__m256i a, __m256i b){
    vpcmpeqb ymm0, a, b
    return ymm0
}


