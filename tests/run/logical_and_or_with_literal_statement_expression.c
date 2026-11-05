// run

// A statement expression can end in an integer literal while still having
// side effects before it. Logical and/or must not drop those side effects.

static int called;

static int side_effect(void)
{
    called += 1;
    return 1;
}

int main(void)
{
    int value;

    called = 0;
    value = 0 && ({ side_effect(); 1; });
    if (value != 0) return 1;
    if (called != 0) return 2;

    called = 0;
    value = 1 && ({ side_effect(); 1; });
    if (value != 1) return 3;
    if (called != 1) return 4;

    called = 0;
    value = 1 || ({ side_effect(); 0; });
    if (value != 1) return 5;
    if (called != 0) return 6;

    called = 0;
    value = 0 || ({ side_effect(); 1; });
    if (value != 1) return 7;
    if (called != 1) return 8;

    return 0;
}
