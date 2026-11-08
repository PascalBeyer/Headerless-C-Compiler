// run

// A statement expression whose last statement is an if/else must not leak
// the else-branch expression onto the emit-location stack.

int f(int x)
{
    ({ if (x < 8) ; else x = 9; });

    return x;
}

int main(void)
{
    return f(0);
}
