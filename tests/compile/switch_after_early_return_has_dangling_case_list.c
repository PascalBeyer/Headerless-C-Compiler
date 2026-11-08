// compile

int f(int mop)
{
    if (1) {
        return 0;
    }

    switch (mop) {
    case 1:
        return 1;
    case 2:
        return 0;
    default:
        return 0;
    }
}

int main(void)
{
    return f(0);
}
