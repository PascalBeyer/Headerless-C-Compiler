
#if _WIN32
#include_next <stddef.h>
#else

#pragma once

typedef __int64 ptrdiff_t;
typedef unsigned __int64 size_t;

// @incomplete: max_align_t

#define NULL ((void *)0)
#define offsetof(type, member) (size_t)(&((type *)0)->member)

#endif
