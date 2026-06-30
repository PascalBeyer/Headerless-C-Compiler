
#ifdef _WIN32
#include_next <stdalign.h>
#else

#define alignas _Alignas
#define alignof _Alignof
#define __alignas_is_defined 1
#define __alignof_is_defined 1

#endif
