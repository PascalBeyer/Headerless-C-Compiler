
#if _WIN32
#include_next <stddef.h>
#else

#pragma once

typedef int wchar_t;

typedef __int64 ptrdiff_t;
typedef unsigned __int64 size_t;

#define NULL ((void *)0)
#define offsetof(type, member) (size_t)(&((type *)0)->member)

#endif


typedef union{
    long long   max_align_long_long;
    long double max_align_long_double;
    __float128  max_align_float128;
} max_align_t;

