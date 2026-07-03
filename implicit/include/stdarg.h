
#if _WIN32
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

#else

typedef struct{
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} va_list[1];

#define va_start(list, ...)  __builtin_va_start(list)
#define va_arg(list, type) *(type *)__builtin_va_arg((list))
#define va_copy(list1, list2) ((list2)[0] = (list1)[0])
#define va_end(list) (void)(list)

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef va_list __gnuc_va_list;
#endif


#endif
