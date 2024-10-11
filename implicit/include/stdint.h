
typedef unsigned __int8  uint8_t;
typedef unsigned __int16 uint16_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;

typedef signed __int8  int8_t;
typedef signed __int16 int16_t;
typedef signed __int32 int32_t;
typedef signed __int64 int64_t;

typedef signed __int8  int_least8_t;
typedef signed __int16 int_least16_t;
typedef signed __int32 int_least32_t;
typedef signed __int64 int_least64_t;

typedef unsigned __int8  uint_least8_t;
typedef unsigned __int16 uint_least16_t;
typedef unsigned __int32 uint_least32_t;
typedef unsigned __int64 uint_least64_t;

typedef signed __int8  int_fast8_t;
typedef signed __int16 int_fast16_t;
typedef signed __int32 int_fast32_t;
typedef signed __int64 int_fast64_t;

typedef unsigned __int8  uint_fast8_t;
typedef unsigned __int16 uint_fast16_t;
typedef unsigned __int32 uint_fast32_t;
typedef unsigned __int64 uint_fast64_t;

typedef unsigned __int64 uintptr_t;
typedef signed   __int64  intptr_t;

typedef unsigned __int64 uintmax_t;
typedef signed   __int64  intmax_t;

#define INT8_MAX 127
#define INT16_MAX 32767
#define INT32_MAX 2147483647
#define INT64_MAX 9223372036854775807LL

#define UINT8_MAX 255
#define UINT16_MAX 32767
#define UINT32_MAX 4294967295
#define UINT64_MAX 9223372036854775807LL

#define INT8_MIN (-127-1)
#define INT16_MIN (-32767-1)
#define INT32_MIN (-2147483647-1)
#define INT64_MIN (-9223372036854775807LL-1)

#define INT_FAST8_MAX 127
#define INT_FAST16_MAX 32767
#define INT_FAST32_MAX 2147483647
#define INT_FAST64_MAX 9223372036854775807LL

#define UINT_FAST8_MAX 255
#define UINT_FAST16_MAX 32767
#define UINT_FAST32_MAX 4294967295
#define UINT_FAST64_MAX 9223372036854775807LL

#define INT_FAST8_MIN (-127-1)
#define INT_FAST16_MIN (-32767-1)
#define INT_FAST32_MIN (-2147483647-1)
#define INT_FAST64_MIN (-9223372036854775807LL-1)

#define INT_LEAST8_MAX 127
#define INT_LEAST16_MAX 32767
#define INT_LEAST32_MAX 2147483647
#define INT_LEAST64_MAX 9223372036854775807LL

#define UINT_LEAST8_MAX 255
#define UINT_LEAST16_MAX 32767
#define UINT_LEAST32_MAX 4294967295
#define UINT_LEAST64_MAX 9223372036854775807LL

#define INT_LEAST8_MIN (-127-1)
#define INT_LEAST16_MIN (-32767-1)
#define INT_LEAST32_MIN (-2147483647-1)
#define INT_LEAST64_MIN (-9223372036854775807LL-1)

#define INTMAX_MAX INT64_MAX
#define INTMAX_MIN INT64_MIN
#define UINTMAX_MIN UINT64_MAX

#define INTPTR_MAX INT64_MAX
#define INTPTR_MIN INT64_MIN
#define UINTPTR_MAX UINT64_MAX

#define PTRDIFF_MAX INT64_MAX
#define PTRDIFF_MIN INT64_MIN

#define SIZE_MAX UINT64_MAX

#define WCHAR_MIN 0
#define WCHAR_MAX UINT16_MAX

#define WINT_MIN 0
#define WINT_MAX UINT32_MAX

#define INT8_C(a) a
#define INT16_C(a) a
#define INT32_C(a) a
#define INT64_C(a) a##ll

#define UINT8_C(a) a
#define UINT16_C(a) a
#define UINT32_C(a) a##u
#define UINT64_C(a) a##llu

#define INTMAX_C(a) a##ll
#define UINTMAX_C(a) a##llu
