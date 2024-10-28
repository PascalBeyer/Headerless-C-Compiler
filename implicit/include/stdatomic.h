
#define ATOMIC_BOOL_LOCK_FREE     1
#define ATOMIC_CHAR_LOCK_FREE     1
#define ATOMIC_CHAR16_T_LOCK_FREE 1
#define ATOMIC_CHAR32_T_LOCK_FREE 1
#define ATOMIC_WCHAR_T_LOCK_FREE  1
#define ATOMIC_SHORT_LOCK_FREE    1
#define ATOMIC_INT_LOCK_FREE      1
#define ATOMIC_LONG_LOCK_FREE     1
#define ATOMIC_LLONG_LOCK_FREE    1
#define ATOMIC_POINTER_LOCK_FREE  0

#define ATOMIC_FLAG_INIT {}

// type kill_dependency(type y);
#define kill_dependency(y) (y)

// _Bool atomic_is_lock_free(const volatile atomic_type *object);
#define atomic_is_lock_free(object) _Generic(*(object), \
        atomic_bool:   1, \
        atomic_char:   1, \
        atomic_short:  1, \
        atomic_int:    1, \
        atomic_long:   1, \
        atomic_llong:  1, \
                          \
        atomic_uchar:  1, \
        atomic_ushort: 1, \
        atomic_uint:   1, \
        atomic_ulong:  1, \
        atomic_ullong: 1, \
                          \
        default: 0,       \
)

// void atomic_init(volatile atomic_type *object, non_atomic_type desired);
#define atomic_init(object, desired) (void)(*(object) = (desired))

// void atomic_store(volatile atomic_type *object, non_atomic_type desired);
// void atomic_store_explicit(volatile atomic_type *object, non_atomic_type desired, memory_order order);
#define atomic_store(object, desired) (void)(*(object) = (desired))
#define atomic_store_explicit(object, desired, order) atomic_store(object, desired)

// non_atomic_type atomic_load(volatile *atomic_type *object);
// non_atomic_type atomic_load_explicit(volatile *atomic_type *object, memory_order order);
#define atomic_load(object) *(object)
#define atomic_load_explicit(object, order) atomic_load(object)

// void atomic_exchange(volatile atomic_type *object, non_atomic_type value);
// void atomic_exchange_explicit(volatile atomic_type *object, non_atomic_type value, memory_order order);
#define atomic_exchange(object, value) _Generic(*(object), \
        atomic_char:    __atomic_exchange_bool,            \
                                                           \
        atomic_char:    __atomic_exchange_char,            \
        atomic_short:   __atomic_exchange_short,           \
        atomic_int:     __atomic_exchange_int,             \
        atomic_long:    __atomic_exchange_long,            \
        atomic_llong:   __atomic_exchange_llong,           \
                                                           \
        atomic_uchar:   __atomic_exchange_uchar,           \
        atomic_ushort:  __atomic_exchange_ushort,          \
        atomic_uint:    __atomic_exchange_uint,            \
        atomic_ulong:   __atomic_exchange_ulong,           \
        atomic_ullong:  __atomic_exchange_ullong,          \
)(object, value)
#define atomic_exchange_explicit(object, value, order) atomic_exchange(object, value)

// _Bool atomic_compare_exchange_strong(volatile atomic_type *object, non_atomic_type *expected, non_atomic_type desired);
// _Bool atomic_compare_exchange_strong_explicit(volatile atomic_type *object, non_atomic_type *expected, non_atomic_type desired, memory_order order);
#define atomic_compare_exchange_strong(object, expected, desired) _Generic(*(object), \
        atomic_char:    __atomic_compare_exchange_bool,                               \
                                                                                      \
        atomic_char:    __atomic_compare_exchange_char,                               \
        atomic_short:   __atomic_compare_exchange_short,                              \
        atomic_int:     __atomic_compare_exchange_int,                                \
        atomic_long:    __atomic_compare_exchange_long,                               \
        atomic_llong:   __atomic_compare_exchange_llong,                              \
                                                                                      \
        atomic_uchar:   __atomic_compare_exchange_uchar,                              \
        atomic_ushort:  __atomic_compare_exchange_ushort,                             \
        atomic_uint:    __atomic_compare_exchange_uint,                               \
        atomic_ulong:   __atomic_compare_exchange_ulong,                              \
        atomic_ullong:  __atomic_compare_exchange_ullong,                             \
)(object, desired)

#define atomic_compare_exchange_strong_explicit(object, expected, desired, order) atomic_compare_exchange_strong(object, expected)

// _Bool atomic_compare_exchange_weak(volatile atomic_type *object, non_atomic_type *expected, non_atomic_type desired);
// _Bool atomic_compare_exchange_weak_explicit(volatile atomic_type *object, non_atomic_type *expected, non_atomic_type desired, memory_order order);
#define atomic_compare_exchange_weak(object, expected, desired) atomic_compare_exchange_strong(object, expected)
#define atomic_compare_exchange_weak_explicit(object, expected, desired, order) atomic_compare_exchange_strong(object, expected)

// non_atomic_type atomic_fetch_{add,sub,or,xor,and}(volatile atomic_type *object, non_atomic_type operand);
// non_atomic_type atomic_fetch_{add,sub,or,xor,and}_explicit(volatile atomic_type *object, non_atomic_type operand, memory_order order);
#define atomic_fetch_add(object, operand) _Generic(*(object), \
        atomic_char:    __atomic_fetch_add_char,              \
        atomic_short:   __atomic_fetch_add_short,             \
        atomic_int:     __atomic_fetch_add_int,               \
        atomic_long:    __atomic_fetch_add_long,              \
        atomic_llong:   __atomic_fetch_add_llong,             \
                                                              \
        atomic_uchar:   __atomic_fetch_add_uchar,             \
        atomic_ushort:  __atomic_fetch_add_ushort,            \
        atomic_uint:    __atomic_fetch_add_uint,              \
        atomic_ulong:   __atomic_fetch_add_ulong,             \
        atomic_ullong:  __atomic_fetch_add_ullong,            \
)(object, operand)
#define atomic_fetch_sub(object, operand) _Generic(*(object), \
        atomic_char:    __atomic_fetch_sub_char,              \
        atomic_short:   __atomic_fetch_sub_short,             \
        atomic_int:     __atomic_fetch_sub_int,               \
        atomic_long:    __atomic_fetch_sub_long,              \
        atomic_llong:   __atomic_fetch_sub_llong,             \
                                                              \
        atomic_uchar:   __atomic_fetch_sub_uchar,             \
        atomic_ushort:  __atomic_fetch_sub_ushort,            \
        atomic_uint:    __atomic_fetch_sub_uint,              \
        atomic_ulong:   __atomic_fetch_sub_ulong,             \
        atomic_ullong:  __atomic_fetch_sub_ullong,            \
)(object, operand)
#define atomic_fetch_or(object, operand) _Generic(*(object),  \
        atomic_char:    __atomic_fetch_or_char,               \
        atomic_short:   __atomic_fetch_or_short,              \
        atomic_int:     __atomic_fetch_or_int,                \
        atomic_long:    __atomic_fetch_or_long,               \
        atomic_llong:   __atomic_fetch_or_llong,              \
                                                              \
        atomic_uchar:   __atomic_fetch_or_uchar,              \
        atomic_ushort:  __atomic_fetch_or_ushort,             \
        atomic_uint:    __atomic_fetch_or_uint,               \
        atomic_ulong:   __atomic_fetch_or_ulong,              \
        atomic_ullong:  __atomic_fetch_or_ullong,             \
)(object, operand)
#define atomic_fetch_xor(object, operand) _Generic(*(object), \
        atomic_char:    __atomic_fetch_xor_char,              \
        atomic_short:   __atomic_fetch_xor_short,             \
        atomic_int:     __atomic_fetch_xor_int,               \
        atomic_long:    __atomic_fetch_xor_long,              \
        atomic_llong:   __atomic_fetch_xor_llong,             \
                                                              \
        atomic_uchar:   __atomic_fetch_xor_uchar,             \
        atomic_ushort:  __atomic_fetch_xor_ushort,            \
        atomic_uint:    __atomic_fetch_xor_uint,              \
        atomic_ulong:   __atomic_fetch_xor_ulong,             \
        atomic_ullong:  __atomic_fetch_xor_ullong,            \
)(object, operand)
#define atomic_fetch_and(object, operand) _Generic(*(object), \
        atomic_char:    __atomic_fetch_and_char,              \
        atomic_short:   __atomic_fetch_and_short,             \
        atomic_int:     __atomic_fetch_and_int,               \
        atomic_long:    __atomic_fetch_and_long,              \
        atomic_llong:   __atomic_fetch_and_llong,             \
                                                              \
        atomic_uchar:   __atomic_fetch_and_uchar,             \
        atomic_ushort:  __atomic_fetch_and_ushort,            \
        atomic_uint:    __atomic_fetch_and_uint,              \
        atomic_ulong:   __atomic_fetch_and_ulong,             \
        atomic_ullong:  __atomic_fetch_and_ullong,            \
)(object, operand)

#define atomic_fetch_add_explicit(object, operand, order) atomic_fetch_add(object, operand)
#define atomic_fetch_sub_explicit(object, operand, order) atomic_fetch_sub(object, operand)
#define atomic_fetch_or_explicit(object, operand, order)  atomic_fetch_or(object, operand)
#define atomic_fetch_xor_explicit(object, operand, order) atomic_fetch_xor(object, operand)
#define atomic_fetch_and_explicit(object, operand, order) atomic_fetch_and(object, operand)

#pragma compilation_unit("stdatomic.c")

