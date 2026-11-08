// compile

_Noreturn void die(void);

static void f(void)
{
    if (0 && ({ die(); (unsigned long)0; })) {
    }
}

int main(void)
{
    f();
    return 0;
}
