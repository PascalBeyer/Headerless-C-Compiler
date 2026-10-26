// run

#include <stdarg.h>

static int read_varargs(const char *format, int a, int b, int c,
                        int d, int e, int f, ...) {
    va_list args;
    va_start(args, f);
    int first = va_arg(args, int);
    int second = va_arg(args, int);
    va_end(args);
    (void)format;
    (void)a; (void)b; (void)c; (void)d; (void)e; (void)f;
    return first * 10 + second;
}

int main(void) {
    if (read_varargs("x", 1, 2, 3, 4, 5, 6, 7, 8) != 78) return 1;
    return 0;
}
