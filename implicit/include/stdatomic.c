
// @cleanup: Are we allowed / supposed to include this?
#include <stdint.h>
#include <stddef.h>

typedef _Atomic(_Bool) atomic_bool;

typedef _Atomic(char)      atomic_schar;
typedef _Atomic(short)     atomic_short;
typedef _Atomic(int)       atomic_int;
typedef _Atomic(long)      atomic_long;
typedef _Atomic(long long) atomic_llong;

typedef _Atomic(unsigned char)      atomic_uchar;
typedef _Atomic(unsigned short)     atomic_ushort;
typedef _Atomic(unsigned int)       atomic_uint;
typedef _Atomic(unsigned long)      atomic_ulong;
typedef _Atomic(unsigned long long) atomic_ullong;

typedef _Atomic(char)      atomic_char;
typedef _Atomic(wchar_t)   atomic_wchar_t;

typedef _Atomic(uint_least16_t) atomic_char16_t;
typedef _Atomic(uint_least32_t) atomic_char32_t;

typedef _Atomic(uint8_t)  atomic_uint8_t;
typedef _Atomic(uint16_t) atomic_uint16_t;
typedef _Atomic(uint32_t) atomic_uint32_t;
typedef _Atomic(uint64_t) atomic_uint64_t;

typedef _Atomic(int8_t)  atomic_int8_t;
typedef _Atomic(int16_t) atomic_int16_t;
typedef _Atomic(int32_t) atomic_int32_t;
typedef _Atomic(int64_t) atomic_int64_t;

typedef _Atomic(int_least8_t)  atomic_int_least8_t;
typedef _Atomic(int_least16_t) atomic_int_least16_t;
typedef _Atomic(int_least32_t) atomic_int_least32_t;
typedef _Atomic(int_least64_t) atomic_int_least64_t;

typedef _Atomic(uint_least8_t)  atomic_uint_least8_t;
typedef _Atomic(uint_least16_t) atomic_uint_least16_t;
typedef _Atomic(uint_least32_t) atomic_uint_least32_t;
typedef _Atomic(uint_least64_t) atomic_uint_least64_t;

typedef _Atomic(int_fast8_t)  atomic_int_fast8_t;
typedef _Atomic(int_fast16_t) atomic_int_fast16_t;
typedef _Atomic(int_fast32_t) atomic_int_fast32_t;
typedef _Atomic(int_fast64_t) atomic_int_fast64_t;

typedef _Atomic(uint_fast8_t)  atomic_uint_fast8_t;
typedef _Atomic(uint_fast16_t) atomic_uint_fast16_t;
typedef _Atomic(uint_fast32_t) atomic_uint_fast32_t;
typedef _Atomic(uint_fast64_t) atomic_uint_fast64_t;

typedef _Atomic(uintptr_t) atomic_uintptr_t;
typedef _Atomic(uintmax_t) atomic_uintmax_t;

typedef _Atomic(intmax_t) atomic_intmax_t;
typedef _Atomic(intptr_t) atomic_intptr_t;

typedef enum{
    memory_order_relaxed,
    memory_order_consume,
    memory_order_acquire,
    memory_order_release,
    memory_order_acq_rel,
    memory_order_seq_cst,
} memory_order;

typedef struct { atomic_bool value; }     atomic_flag;

//_____________________________________________________________________________________________________________________
// Static type definitions to make code more uniform

static typedef long long llong;

static typedef unsigned char      uchar;
static typedef unsigned short     ushort;
static typedef unsigned int       uint;
static typedef unsigned long      ulong;
static typedef unsigned long long ullong;

//_____________________________________________________________________________________________________________________
// Atomic fences

__declspec(inline_asm) void atomic_thread_fence(memory_order){
    lock or dword ptr[rsp], 0
}

__declspec(inline_asm) void atomic_signal_fence(memory_order){
    // noop?
}

//_____________________________________________________________________________________________________________________
// Atomic Flag Functions

__declspec(inline_asm) _Bool atomic_flag_test_and_set(volatile atomic_flag *object){
    mov eax, 1
    xchg al, [object]
    return al
}

__declspec(inline_asm) _Bool atomic_flag_test_and_set_explicit(volatile atomic_flag *object, memory_order){
    mov eax, 1
    xchg al, [object]
    return al
}


__declspec(inline_asm) void atomic_flag_clear(volatile atomic_flag *object){
    mov eax, 0
    xchg al, [object]
}

__declspec(inline_asm) void atomic_flag_clear_explicit(volatile atomic_flag *object, memory_order){
    mov eax, 0
    xchg al, [object]
}

//_____________________________________________________________________________________________________________________
// Atomic Exchange Functions

__declspec(inline_asm) _Bool __atomic_exchange_bool(atomic_bool *object, _Bool desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) char __atomic_exchange_char(atomic_char *object, char desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) short __atomic_exchange_short(atomic_short *object, short desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) int __atomic_exchange_int(atomic_int *object, int desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) long __atomic_exchange_long(atomic_long *object, long desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) llong __atomic_exchange_llong(atomic_llong *object, llong desired){
    xchg [object], desired
    return desired
}


__declspec(inline_asm) uchar __atomic_exchange_uchar(atomic_uchar *object, uchar desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) ushort __atomic_exchange_ushort(atomic_ushort *object, ushort desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) uint __atomic_exchange_uint(atomic_uint *object, uint desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) ulong __atomic_exchange_ulong(atomic_ulong *object, ulong desired){
    xchg [object], desired
    return desired
}

__declspec(inline_asm) ullong __atomic_exchange_ullong(atomic_ullong *object, ullong desired){
    xchg [object], desired
    return desired
}

//_____________________________________________________________________________________________________________________
// Atomic Compare Exchange Functions

__declspec(inline_asm) _Bool __atomic_compare_exchange_bool(atomic_bool *object, _Bool *expected, _Bool desired){
    
    mov al, [expected]
    lock cmpxchg [object], desired
    mov [expected], al
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_char(atomic_char *object, char *expected, char desired){
    
    mov al, [expected]
    lock cmpxchg [object], desired
    mov [expected], al
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_short(atomic_short *object, short *expected, short desired){
    
    mov ax, [expected]
    lock cmpxchg [object], desired
    mov [expected], ax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_int(atomic_int *object, int *expected, int desired){
    
    mov eax, [expected]
    lock cmpxchg [object], desired
    mov [expected], eax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_long(atomic_long *object, long *expected, long desired){
    
    mov eax, [expected]
    lock cmpxchg [object], desired
    mov [expected], eax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_llong(atomic_llong *object, llong *expected, llong desired){
    
    mov rax, [expected]
    lock cmpxchg [object], desired
    mov [expected], rax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_uchar(atomic_uchar *object, uchar *expected, uchar desired){
    
    mov al, [expected]
    lock cmpxchg [object], desired
    mov [expected], al
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_ushort(atomic_ushort *object, ushort *expected, ushort desired){
    
    mov ax, [expected]
    lock cmpxchg [object], desired
    mov [expected], ax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_uint(atomic_uint *object, uint *expected, uint desired){
    
    mov eax, [expected]
    lock cmpxchg [object], desired
    mov [expected], eax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_ulong(atomic_ulong *object, ulong *expected, ulong desired){
    
    mov eax, [expected]
    lock cmpxchg [object], desired
    mov [expected], eax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_ullong(atomic_ullong *object, ullong *expected, ullong desired){
    
    mov rax, [expected]
    lock cmpxchg [object], desired
    mov [expected], rax
    
    sete al
    return al
}

__declspec(inline_asm) _Bool __atomic_compare_exchange_pointer(_Atomic(void *) *object, void *expected, void *desired){
    
    mov rax, [expected]
    lock cmpxchg [object], desired
    mov [expected], rax
    
    sete al
    return al
}

//_____________________________________________________________________________________________________________________
// Atomic Fetch Add

__declspec(inline_asm) char __atomic_fetch_add_char(atomic_char *object, char operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) short __atomic_fetch_add_short(atomic_short *object, short operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) int __atomic_fetch_add_int(atomic_int *object, int operand){
    lock xadd [object], operand
    return operand
}


__declspec(inline_asm) long __atomic_fetch_add_long(atomic_long *object, long operand){
    lock xadd [object], operand
    return operand
}


__declspec(inline_asm) llong __atomic_fetch_add_llong(atomic_llong *object, llong operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) uchar __atomic_fetch_add_uchar(atomic_uchar *object, uchar operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) ushort __atomic_fetch_add_ushort(atomic_ushort *object, ushort operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) uint __atomic_fetch_add_uint(atomic_uint *object, uint operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) ulong __atomic_fetch_add_ulong(atomic_ulong *object, ulong operand){
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) ullong __atomic_fetch_add_ullong(atomic_ullong *object, ullong operand){
    lock xadd [object], operand
    return operand
}

//_____________________________________________________________________________________________________________________
// Atomic Fetch Sub

__declspec(inline_asm) char __atomic_fetch_sub_char(atomic_char *object, char operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) short __atomic_fetch_sub_short(atomic_short *object, short operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) int __atomic_fetch_sub_int(atomic_int *object, int operand){
    neg operand
    lock xadd [object], operand
    return operand
}


__declspec(inline_asm) long __atomic_fetch_sub_long(atomic_long *object, long operand){
    neg operand
    lock xadd [object], operand
    return operand
}


__declspec(inline_asm) llong __atomic_fetch_sub_llong(atomic_llong *object, llong operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) uchar __atomic_fetch_sub_uchar(atomic_uchar *object, uchar operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) ushort __atomic_fetch_sub_ushort(atomic_ushort *object, ushort operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) uint __atomic_fetch_sub_uint(atomic_uint *object, uint operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) ulong __atomic_fetch_sub_ulong(atomic_ulong *object, ulong operand){
    neg operand
    lock xadd [object], operand
    return operand
}

__declspec(inline_asm) ullong __atomic_fetch_sub_ullong(atomic_ullong *object, ullong operand){
    neg operand
    lock xadd [object], operand
    return operand
}

//_____________________________________________________________________________________________________________________
// Atomic Fetch Or

extern inline char __atomic_fetch_or_char(atomic_char *object, char operand){
    char desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_char(object, &expected, desired));
    return expected;
}

extern inline short __atomic_fetch_or_short(atomic_short *object, short operand){
    short desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_short(object, &expected, desired));
    return expected;
}


extern inline int __atomic_fetch_or_int(atomic_int *object, int operand){
    int desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_int(object, &expected, desired));
    return expected;
}

extern inline long __atomic_fetch_or_long(atomic_long *object, long operand){
    long desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_long(object, &expected, desired));
    return expected;
}

extern inline llong __atomic_fetch_or_llong(atomic_llong *object, llong operand){
    llong desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_llong(object, &expected, desired));
    return expected;
}

extern inline uchar __atomic_fetch_or_uchar(atomic_uchar *object, uchar operand){
    uchar desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_uchar(object, &expected, desired));
    return expected;
}

extern inline ushort __atomic_fetch_or_ushort(atomic_ushort *object, ushort operand){
    ushort desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_ushort(object, &expected, desired));
    return expected;
}

extern inline uint __atomic_fetch_or_uint(atomic_uint *object, uint operand){
    uint desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_uint(object, &expected, desired));
    return expected;
}

extern inline ulong __atomic_fetch_or_ulong(atomic_ulong *object, ulong operand){
    ulong desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_ulong(object, &expected, desired));
    return expected;
}

extern inline ullong __atomic_fetch_or_ullong(atomic_ullong *object, ullong operand){
    ullong desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_ullong(object, &expected, desired));
    return expected;
}

//_____________________________________________________________________________________________________________________
// Atomic Fetch Xor

extern inline char __atomic_fetch_xor_char(atomic_char *object, char operand){
    char desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_char(object, &expected, desired));
    return expected;
}

extern inline short __atomic_fetch_xor_short(atomic_short *object, short operand){
    short desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_short(object, &expected, desired));
    return expected;
}


extern inline int __atomic_fetch_xor_int(atomic_int *object, int operand){
    int desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_int(object, &expected, desired));
    return expected;
}

extern inline long __atomic_fetch_xor_long(atomic_long *object, long operand){
    long desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_long(object, &expected, desired));
    return expected;
}

extern inline llong __atomic_fetch_xor_llong(atomic_llong *object, llong operand){
    llong desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_llong(object, &expected, desired));
    return expected;
}

extern inline uchar __atomic_fetch_xor_uchar(atomic_uchar *object, uchar operand){
    uchar desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_uchar(object, &expected, desired));
    return expected;
}

extern inline ushort __atomic_fetch_xor_ushort(atomic_ushort *object, ushort operand){
    ushort desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_ushort(object, &expected, desired));
    return expected;
}

extern inline uint __atomic_fetch_xor_uint(atomic_uint *object, uint operand){
    uint desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_uint(object, &expected, desired));
    return expected;
}

extern inline ulong __atomic_fetch_xor_ulong(atomic_ulong *object, ulong operand){
    ulong desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_ulong(object, &expected, desired));
    return expected;
}

extern inline ullong __atomic_fetch_xor_ullong(atomic_ullong *object, ullong operand){
    ullong desired, expected = *object;
    do{
        desired = operand ^ expected;
    }while(!__atomic_compare_exchange_ullong(object, &expected, desired));
    return expected;
}

//_____________________________________________________________________________________________________________________
// Atomic Fetch And

extern inline char __atomic_fetch_and_char(atomic_char *object, char operand){
    char desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_char(object, &expected, desired));
    return expected;
}

extern inline short __atomic_fetch_and_short(atomic_short *object, short operand){
    short desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_short(object, &expected, desired));
    return expected;
}


extern inline int __atomic_fetch_and_int(atomic_int *object, int operand){
    int desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_int(object, &expected, desired));
    return expected;
}

extern inline long __atomic_fetch_and_long(atomic_long *object, long operand){
    long desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_long(object, &expected, desired));
    return expected;
}

extern inline llong __atomic_fetch_and_llong(atomic_llong *object, llong operand){
    llong desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_llong(object, &expected, desired));
    return expected;
}

extern inline uchar __atomic_fetch_and_uchar(atomic_uchar *object, uchar operand){
    uchar desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_uchar(object, &expected, desired));
    return expected;
}

extern inline ushort __atomic_fetch_and_ushort(atomic_ushort *object, ushort operand){
    ushort desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_ushort(object, &expected, desired));
    return expected;
}

extern inline uint __atomic_fetch_and_uint(atomic_uint *object, uint operand){
    uint desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_uint(object, &expected, desired));
    return expected;
}

extern inline ulong __atomic_fetch_and_ulong(atomic_ulong *object, ulong operand){
    ulong desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_ulong(object, &expected, desired));
    return expected;
}

extern inline ullong __atomic_fetch_and_ullong(atomic_ullong *object, ullong operand){
    ullong desired, expected = *object;
    do{
        desired = operand & expected;
    }while(!__atomic_compare_exchange_ullong(object, &expected, desired));
    return expected;
}

