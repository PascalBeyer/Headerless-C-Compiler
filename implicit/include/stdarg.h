

// 
// Windows x64 calling convention has the arguments 
// linearly each in an 8-byte slot. If an argument
// does not "fit" the 8-byte slot, either because it is
// too large or because its size is not a power of two,
// it passed as a pointer.
//    
//    va_start: Get the argument immediately after the 'format' argument.
//    va_arg:   Advance the argument by 8 and use either one or two indirections.
//    va_copy:  Just copy the pointer.
//    va_end:   Do nothing!
// 

typedef struct __va_list{
    __int64 unused;
} *va_list;

#define va_start(ap, parmN) ((ap) = ((va_list)&(parmN) + 1))
#define va_arg(ap, type) ((sizeof(type) > 8 || (sizeof(type) & (sizeof(type)-1))) \
        ? **(type**)(((ap) += 1) - 1) \
        :  *(type *)(((ap) += 1) - 1))
#define va_copy(dest, src) ((dest) = (src))
#define va_end(ap) ((void)(ap))

