// fail "(14,5): Error at '_Format': Undeclared identifier."

typedef struct __va_list{
    __int64 unused;
} *va_list;

#define va_start(ap, parmN) ((ap) = ((va_list)&(parmN) + 1))

#define __crt_va_start va_start

int _start(){
    int _Result;
    va_list _ArgList;
    __crt_va_start(_ArgList, _Format); // This used to fail in the wrong line.
}
