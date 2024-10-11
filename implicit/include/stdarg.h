
typedef struct __va_list{
    __int64 unused;
} *va_list;

#define va_start(ap, parmN) ((ap) = ((va_list)&(parmN) + 1))
#define va_arg(ap, type) *(type*)(((ap) += 1) - 1)
#define va_copy(dest, src) ((dest) = (src))
#define va_end(ap) ((void)(ap))

