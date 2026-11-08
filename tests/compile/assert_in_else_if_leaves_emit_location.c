// compile

_Noreturn void __assert_fail(const char *assertion, const char *file,
                             unsigned int line, const char *function);

#define assert(x)                                                     \
    ((void) sizeof((x) ? 1 : 0),                                      \
     ({ if (x) ; else __assert_fail(#x, __FILE__, __LINE__, __func__); }))

static int f(int a)
{
    if (a == 0) {
        return 0;
    } else if (a == 1) {
        assert(1);
        return 0;
    } else {
        return -1;
    }
}

int main(void)
{
    return f(0);
}
