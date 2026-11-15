// run

// GNU C allows casting an expression to a union type. The member whose type
// matches the operand is initialized (all union members are at offset 0).
// This is used by QEMU: hw/net/can/ctucan_core.c casts a uint32_t to a union.

union u {
    int i;
    unsigned u;
    long long ll;
};

struct s {
    int x;
};

union su {
    struct s s;
    long long ll;
};

union up {
    void *p;
    int i;
};

int main(void)
{
    union u a = (union u)42;
    union u b;
    union u c;
    union su d;
    union up e;

    if (a.i != 42) return 1;

    b = (union u)7u;
    if (b.u != 7u) return 2;

    c = (union u)1234567890123LL;
    if (c.ll != 1234567890123LL) return 3;

    d = (union su)(struct s){ .x = 99 };
    if (d.s.x != 99) return 4;

    e = (union up)(void *)0;
    if (e.p != 0) return 5;

    return 0;
}
