
typedef __int8  s8;
typedef __int16 s16;
typedef __int32 s32;
typedef __int64 s64;

typedef unsigned __int8  u8;
typedef unsigned __int16 u16;
typedef unsigned __int32 u32;
typedef unsigned __int64 u64;

typedef int b32;
typedef s64 smm; 

void _exit(int status);
#define assert(a) (!(a) ? (_exit(1), 0) : 0)

#define true 1
#define false 0
#define null ((void *)0)
#define array_count(a) (sizeof(a)/sizeof(*(a)))