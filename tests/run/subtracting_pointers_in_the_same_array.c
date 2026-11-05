// run

struct S {
    int values[3];
};

int main(void)
{
    int ints[8];
    int *first = &ints[1];
    const int *last = &ints[6];

    if (last - first != 5) return 1;
    if (first - last != -5) return 2;
    if (&ints[8] - &ints[0] != 8) return 3;

    struct S structs[5];
    struct S *sf = &structs[1];
    struct S *sl = &structs[4];

    if (sl - sf != 3) return 4;
    if (sf - sl != -3) return 5;

    if (sizeof(last - first) != sizeof(long long)) return 6;

    int *low = (int *)4;
    int *high = (int *)16;
    if (high - low != 3) return 7;
    if (low - high != -3) return 8;

    return 0;
}
