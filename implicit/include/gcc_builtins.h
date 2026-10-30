
#pragma compilation_unit("gcc_builtins.c")

// Common Predefined Macros

#define __SIZE_TYPE__ long unsigned int
#define __PTRDIFF_TYPE__ long int
#define __WCHAR_TYPE__ int
#define __WINT_TYPE__ unsigned int
#define __INTMAX_TYPE__ long int
#define __UINTMAX_TYPE__ long unsigned int
#define __SIG_ATOMIC_TYPE__ int

#define __INT8_TYPE__ signed char
#define __INT16_TYPE__ short int
#define __INT32_TYPE__ int
#define __INT64_TYPE__ long int

#define __UINT8_TYPE__ unsigned char
#define __UINT16_TYPE__ short unsigned int
#define __UINT32_TYPE__ unsigned int
#define __UINT64_TYPE__ long unsigned int

#define __INT_FAST8_TYPE__ signed char
#define __INT_FAST16_TYPE__ long int
#define __INT_FAST32_TYPE__ long int
#define __INT_FAST64_TYPE__ long int

#define __UINT_FAST8_TYPE__ unsigned char
#define __UINT_FAST16_TYPE__ long unsigned int
#define __UINT_FAST32_TYPE__ long unsigned int
#define __UINT_FAST64_TYPE__ long unsigned int

#define __INT_LEAST8_TYPE__ signed char
#define __INT_LEAST16_TYPE__ short int
#define __INT_LEAST32_TYPE__ int
#define __INT_LEAST64_TYPE__ long int

#define __UINT_LEAST8_TYPE__ unsigned char
#define __UINT_LEAST16_TYPE__ short unsigned int
#define __UINT_LEAST32_TYPE__ unsigned int
#define __UINT_LEAST64_TYPE__ long unsigned int

#define __INTPTR_TYPE__ long int
#define __UINTPTR_TYPE__ long unsigned int

#define __CHAR16_TYPE__ short unsigned int
#define __CHAR32_TYPE__ unsigned int

#define __CHAR_BIT__ 8

#define __SCHAR_MAX__ 0x7f
#define __WCHAR_MAX__ 0x7fffffff

#define __SHRT_MAX__ 0x7fff
#define __INT_MAX__ 0x7fffffff
#define __LONG_MAX__ 0x7fffffffffffffffL
#define __LONG_LONG_MAX__ 0x7fffffffffffffffLL

#define __WINT_MAX__ 0xffffffffU
#define __SIZE_MAX__ 0xffffffffffffffffUL

#define __PTRDIFF_MAX__ 0x7fffffffffffffffL

#define __INTMAX_MAX__ 0x7fffffffffffffffL
#define __UINTMAX_MAX__ 0xffffffffffffffffUL

#define __SIG_ATOMIC_MAX__ 0x7fffffff

#define __INT8_MAX__ 0x7f
#define __INT16_MAX__ 0x7fff
#define __INT32_MAX__ 0x7fffffff
#define __INT64_MAX__ 0x7fffffffffffffffL

#define __UINT8_MAX__ 0xff
#define __UINT16_MAX__ 0xffff
#define __UINT32_MAX__ 0xffffffffU
#define __UINT64_MAX__ 0xffffffffffffffffUL

#define __INT_LEAST8_MAX__ 0x7f
#define __INT_LEAST16_MAX__ 0x7fff
#define __INT_LEAST32_MAX__ 0x7fffffff
#define __INT_LEAST64_MAX__ 0x7fffffffffffffffL

#define __UINT_LEAST8_MAX__ 0xff
#define __UINT_LEAST16_MAX__ 0xffff
#define __UINT_LEAST32_MAX__ 0xffffffffU
#define __UINT_LEAST64_MAX__ 0xffffffffffffffffUL

#define __INT_FAST8_MAX__ 0x7f
#define __INT_FAST16_MAX__ 0x7fffffffffffffffL
#define __INT_FAST32_MAX__ 0x7fffffffffffffffL
#define __INT_FAST64_MAX__ 0x7fffffffffffffffL

#define __UINT_FAST8_MAX__ 0xff
#define __UINT_FAST16_MAX__ 0xffffffffffffffffUL
#define __UINT_FAST32_MAX__ 0xffffffffffffffffUL
#define __UINT_FAST64_MAX__ 0xffffffffffffffffUL

#define __UINTPTR_MAX__ 0xffffffffffffffffUL
#define __INTPTR_MAX__ 0x7fffffffffffffffL

#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)
#define __WINT_MIN__ 0U
#define __SIG_ATOMIC_MIN__ (-__SIG_ATOMIC_MAX__ - 1)

#define __SCHAR_WIDTH__ 8
#define __SHRT_WIDTH__ 16
#define __INT_WIDTH__ 32
#define __LONG_WIDTH__ 64
#define __LONG_LONG_WIDTH__ 64
#define __PTRDIFF_WIDTH__ 64
#define __SIG_ATOMIC_WIDTH__ 32
#define __SIZE_WIDTH__ 64
#define __WCHAR_WIDTH__ 32
#define __WINT_WIDTH__ 32

#define __INT_LEAST8_WIDTH__ 8
#define __INT_LEAST16_WIDTH__ 16
#define __INT_LEAST32_WIDTH__ 32
#define __INT_LEAST64_WIDTH__ 64

#define __INT_FAST8_WIDTH__ 8
#define __INT_FAST16_WIDTH__ 64
#define __INT_FAST32_WIDTH__ 64
#define __INT_FAST64_WIDTH__ 64

#define __INTPTR_WIDTH__ 64
#define __INTMAX_WIDTH__ 64

#define __SIZEOF_SHORT__ 2
#define __SIZEOF_INT__ 4
#define __SIZEOF_LONG__ 8
#define __SIZEOF_LONG_LONG__ 8
#define __SIZEOF_INT128__ 16

#define __SIZEOF_POINTER__ 8

#define __SIZEOF_FLOAT__ 4
#define __SIZEOF_DOUBLE__ 8
#define __SIZEOF_LONG_DOUBLE__ 16
#define __SIZEOF_FLOAT80__ 16
#define __SIZEOF_FLOAT128__ 16

#define __SIZEOF_SIZE_T__ 8
#define __SIZEOF_WCHAR_T__ 4
#define __SIZEOF_WINT_T__ 4
#define __SIZEOF_PTRDIFF_T__ 8

#define __ORDER_LITTLE_ENDIAN__ 1234
#define __ORDER_BIG_ENDIAN__ 4321
#define __ORDER_PDP_ENDIAN__ 3412

#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
#define __FLOAT_WORD_ORDER__ __ORDER_LITTLE_ENDIAN__

#define __FLT32_MIN__ 1.17549435082228750796873653722224568e-38F
#define __FLT32_MAX__ 3.40282346638528859811704183484516925e+38F

#define __FLT64_MIN__ 2.22507385850720138309023271733240406e-308
#define __FLT64_MAX__ 1.79769313486231570814527423731704357e+308


// builtins

#define __builtin_offsetof(type, member) ((long unsigned int)(&((type *)0)->member))
#define __builtin_prefetch(a, ...) __builtin_prefetch__internal(a)

#define __builtin_signbitl(x) ((x) < 0.0L || (1.0L/(x) < 0.0L))
#define __builtin_nanf(x) (0.0f/0.0f)
#define __builtin_nan(x) (0.0/0.0)
#define __builtin_inff() (1.0f/0.0f)
#define __builtin_inf() (1.0/0.0)
#define __builtin_huge_valf() (1.0f/0.0f)
#define __builtin_huge_val() (1.0/0.0)

#define __builtin_isnan(x)      _Generic((x), float: __builtin_isnan_float(x),      double: __builtin_isnan_double(x))
#define __builtin_isinf(x)      _Generic((x), float: __builtin_isinf_float(x),      double: __builtin_isinf_double(x))
#define __builtin_isinf_sign(x) _Generic((x), float: __builtin_isinf_sign_float(x), double: __builtin_isinf_sign_double(x))
#define __builtin_isnormal(x)   _Generic((x), float: __builtin_isnormal_float(x),   double: __builtin_isnormal_double(x))
#define __builtin_isfinite(x)   _Generic((x), float: __builtin_isfinite_float(x),   double: __builtin_isfinite_double(x))

// sync-routines

#pragma compilation_unit("stdatomic.c")

#define __sync_synchronize() atomic_thread_fence(memory_order_seq_cst)

#define __sync_fetch_and_add(object, value, ...) _Generic((object), \
        char *:               __atomic_fetch_add_char,         \
        short *:              __atomic_fetch_add_short,        \
        int *:                __atomic_fetch_add_int,          \
        long *:               __atomic_fetch_add_long,         \
        /*long long *:          __atomic_fetch_add_llong,*/    \
        unsigned char *:      __atomic_fetch_add_char,         \
        unsigned short *:     __atomic_fetch_add_short,        \
        unsigned int *:       __atomic_fetch_add_int,          \
        unsigned long *:      __atomic_fetch_add_long,         \
        /*unsigned long long *: __atomic_fetch_add_llong,*/    \
)((void *)(object), (value))

#define __sync_fetch_and_sub(object, value, ...) _Generic((object), \
        char *:               __atomic_fetch_sub_char,         \
        short *:              __atomic_fetch_sub_short,        \
        int *:                __atomic_fetch_sub_int,          \
        long *:               __atomic_fetch_sub_long,         \
        /*long long *:          __atomic_fetch_sub_llong,*/    \
        unsigned char *:      __atomic_fetch_sub_char,         \
        unsigned short *:     __atomic_fetch_sub_short,        \
        unsigned int *:       __atomic_fetch_sub_int,          \
        unsigned long *:      __atomic_fetch_sub_long,         \
        /*unsigned long long *: __atomic_fetch_sub_llong,*/    \
        )((void *)(object), (value))

#define __sync_fetch_and_and(object, value, ...) _Generic((object), \
        char *:               __atomic_fetch_and_char,         \
        short *:              __atomic_fetch_and_short,        \
        int *:                __atomic_fetch_and_int,          \
        long *:               __atomic_fetch_and_long,         \
        /*long long *:          __atomic_fetch_and_llong,*/    \
        unsigned char *:      __atomic_fetch_and_char,         \
        unsigned short *:     __atomic_fetch_and_short,        \
        unsigned int *:       __atomic_fetch_and_int,          \
        unsigned long *:      __atomic_fetch_and_long,         \
        /*unsigned long long *: __atomic_fetch_and_llong,*/    \
        )((void *)(object), (value))

#define __sync_fetch_and_or(object, value, ...) _Generic((object), \
        char *:               __atomic_fetch_or_char,         \
        short *:              __atomic_fetch_or_short,        \
        int *:                __atomic_fetch_or_int,          \
        long *:               __atomic_fetch_or_long,         \
        /*long long *:          __atomic_fetch_or_llong,*/    \
        unsigned char *:      __atomic_fetch_or_char,         \
        unsigned short *:     __atomic_fetch_or_short,        \
        unsigned int *:       __atomic_fetch_or_int,          \
        unsigned long *:      __atomic_fetch_or_long,         \
        /*unsigned long long *: __atomic_fetch_or_llong,*/    \
        )((void *)(object), (value))

#define __sync_fetch_and_xor(object, value, ...) _Generic((object), \
        char *:               __atomic_fetch_xor_char,         \
        short *:              __atomic_fetch_xor_short,        \
        int *:                __atomic_fetch_xor_int,          \
        long *:               __atomic_fetch_xor_long,         \
        /*long long *:          __atomic_fetch_xor_llong,*/    \
        unsigned char *:      __atomic_fetch_xor_char,         \
        unsigned short *:     __atomic_fetch_xor_short,        \
        unsigned int *:       __atomic_fetch_xor_int,          \
        unsigned long *:      __atomic_fetch_xor_long,         \
        /*unsigned long long *: __atomic_fetch_xor_llong,*/    \
        )((void *)(object), (value))


#define __sync_add_and_fetch(ptr, value, ...) ({ __typeof__(*(ptr)) temp = (value); __sync_fetch_and_add((ptr), temp) + temp; })
#define __sync_sub_and_fetch(ptr, value, ...) ({ __typeof__(*(ptr)) temp = (value); __sync_fetch_and_sub((ptr), temp) - temp; })
#define __sync_or_and_fetch(ptr, value, ...)  ({ __typeof__(*(ptr)) temp = (value); __sync_fetch_and_or((ptr), temp) | temp; })
#define __sync_xor_and_fetch(ptr, value, ...) ({ __typeof__(*(ptr)) temp = (value); __sync_fetch_and_xor((ptr), temp) ^ temp; })
#define __sync_and_and_fetch(ptr, value, ...) ({ __typeof__(*(ptr)) temp = (value); __sync_fetch_and_and((ptr), temp) & temp; })


#define __sync_compare_and_swap__internal(object, expected, desired) _Generic((object), \
        _Bool*:    __atomic_compare_exchange_bool,                                   \
                                                                                     \
        char *:    __atomic_compare_exchange_char,                                   \
        short*:   __atomic_compare_exchange_short,                                   \
        int*:     __atomic_compare_exchange_int,                                     \
        long*:    __atomic_compare_exchange_long,                                    \
        /*atomic_llong*:   __atomic_compare_exchange_llong,*/                        \
                                                                                     \
        unsigned char*:   __atomic_compare_exchange_uchar,                           \
        unsigned short*:  __atomic_compare_exchange_ushort,                          \
        unsigned int*:    __atomic_compare_exchange_uint,                            \
        unsigned long*:   __atomic_compare_exchange_ulong,                           \
        /*atomic_ullong*:  __atomic_compare_exchange_ullong,*/                       \
                                                                                     \
        default:        __atomic_compare_exchange_pointer,                           \
        )((void *)(object), (expected), (desired))


#define __sync_val_compare_and_swap(ptr, expected, desired) ({        \
            __typeof__(*(ptr)) temp = (expected);                     \
            __sync_compare_and_swap__internal((ptr),&temp,(desired)); \
            temp;                                                     \
        })

#define __sync_bool_compare_and_swap(ptr, expected, desired) ({       \
            __typeof__(*(ptr)) temp = (expected);                     \
            __typeof__(temp) original = temp;                         \
            __sync_compare_and_swap__internal((ptr),&temp,(desired)); \
            temp == original;                                         \
        })

#define __sync_lock_test_and_set(ptr, value) _Generic((ptr),    \
        _Bool*:              __atomic_exchange_bool,    \
        char*:               __atomic_exchange_char,    \
        short*:              __atomic_exchange_short,   \
        int*:                __atomic_exchange_int,     \
        long*:               __atomic_exchange_long,    \
        long long*:          __atomic_exchange_llong,   \
        unsigned char*:      __atomic_exchange_uchar,   \
        unsigned short*:     __atomic_exchange_ushort,  \
        unsigned int*:       __atomic_exchange_uint,    \
        unsigned long*:      __atomic_exchange_ulong,   \
        /*unsigned long long*: __atomic_exchange_ullong,*/  \
        )((void *)(ptr), (value))

#define __sync_lock_release(ptr) __sync_lock_test_and_set((ptr), 0)


// Atomic builtins

#define __ATOMIC_RELAXED 0
#define __ATOMIC_CONSUME 1
#define __ATOMIC_ACQUIRE 2
#define __ATOMIC_RELEASE 3
#define __ATOMIC_ACQ_REL 4
#define __ATOMIC_SEQ_CST 5

#define __atomic_add_fetch __sync_add_and_fetch
#define __atomic_sub_fetch __sync_sub_and_fetch
#define __atomic_or_fetch  __sync_or_and_fetch
#define __atomic_xor_fetch __sync_xor_and_fetch
#define __atomic_and_fetch __sync_and_and_fetch

#define __atomic_fetch_add __sync_fetch_and_add
#define __atomic_fetch_sub __sync_fetch_and_sub
#define __atomic_fetch_or  __sync_fetch_and_or
#define __atomic_fetch_xor __sync_fetch_xor
#define __atomic_fetch_and __sync_fetch_and_and

#define __atomic_exchange_n(ptr, value, order) _Generic((ptr), \
        char*:               __atomic_exchange_char((char *)(ptr), (char)(value)), \
        unsigned char*:      __atomic_exchange_uchar((unsigned char *)(ptr), (unsigned char)(value)), \
        short*:              __atomic_exchange_short((short *)(ptr), (short)(value)), \
        unsigned short*:     __atomic_exchange_ushort((unsigned short *)(ptr), (unsigned short)(value)), \
        int*:                __atomic_exchange_int((int *)(ptr), (int)(value)), \
        unsigned int*:       __atomic_exchange_uint((unsigned int *)(ptr), (unsigned int)(value)), \
        long long*:          __atomic_exchange_llong((long long *)(ptr), (long long)(value)), \
        unsigned long long*: __atomic_exchange_ullong((unsigned long long *)(ptr), (unsigned long long)(value)), \
        )

#define __atomic_compare_exchange_n(ptr, expected, desired, weak, success_order, failure_order) _Generic((ptr), \
        char*:               __atomic_compare_exchange_char((char *)(ptr), (char *)(expected), (char)(desired)), \
        unsigned char*:      __atomic_compare_exchange_uchar((unsigned char *)(ptr), (unsigned char *)(expected), (unsigned char)(desired)), \
        short*:              __atomic_compare_exchange_short((short *)(ptr), (short *)(expected), (short)(desired)), \
        unsigned short*:     __atomic_compare_exchange_ushort((unsigned short *)(ptr), (unsigned short *)(expected), (unsigned short)(desired)), \
        int*:                __atomic_compare_exchange_int((int *)(ptr), (int *)(expected), (int)(desired)), \
        unsigned int*:       __atomic_compare_exchange_uint((unsigned int *)(ptr), (unsigned int *)(expected), (unsigned int)(desired)), \
        long long*:          __atomic_compare_exchange_llong((long long *)(ptr), (long long *)(expected), (long long)(desired)), \
        unsigned long long*: __atomic_compare_exchange_ullong((unsigned long long *)(ptr), (unsigned long long *)(expected), (unsigned long long)(desired)), \
        )

#define __atomic_load(ptr, ret, order) ((void)(*(ret) = *(ptr)))
#define __atomic_store(ptr, value, order) ((void)(*(ptr) = *(value)))

#define __atomic_store_n(ptr, value, order) ((void)(*(ptr) = (value)))
#define __atomic_load_n(ptr, order) (*(ptr))

// @cleanup:
#define __atomic_is_lock_free(size, ptr) 1
#define __atomic_thread_fence(order) atomic_thread_fence(order)
#define __atomic_signal_fence(order) atomic_signal_fence(order)
