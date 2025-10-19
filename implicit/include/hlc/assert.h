
#define assert(expr) ((expr) ? 0 : (__do_assert(__FILE__,  __LINE__, #expr, __FUNCTION__), 1))

#pragma compilation_unit("assert.c")
