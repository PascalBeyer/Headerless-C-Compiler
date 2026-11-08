// run

int f(void)
{
    int x = 0;

    if (0) {
        x = 1;
        x = 2;
    }

    return x;
}

int main(void)
{
    return f();
}
