// run

static int classify(int value)
{
    switch (value) {
    case -3 ... -1: return 1;
    case 0 ... 4:   return 2;
    case 'a' ... 'z': return 3;
    default:        return 4;
    }
}

static int classify_large(unsigned int value)
{
    switch (value) {
    case 0x100000u ... 0x1fffffu: return 1;
    default: return 0;
    }
}

int main(void)
{
    if (classify(-2) != 1) return 1;
    if (classify(3) != 2) return 2;
    if (classify('q') != 3) return 3;
    if (classify(200) != 4) return 4;

    if (classify_large(0x0fffffu) != 0) return 5;
    if (classify_large(0x100000u) != 1) return 6;
    if (classify_large(0x1fffffu) != 1) return 7;
    if (classify_large(0x200000u) != 0) return 8;

    return 0;
}
