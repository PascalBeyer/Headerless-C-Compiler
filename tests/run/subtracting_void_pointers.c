// run

int main(void)
{
    void *high = (void *)16;
    void *low = (void *)4;

    if (high - low != 12) return 1;
    if (low - high != -12) return 2;

    return 0;
}
