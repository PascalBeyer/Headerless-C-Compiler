// run

struct suite {
    unsigned short kem;
    unsigned short kdf;
    unsigned short aead;
};

struct base {
    struct suite suite;
};

static int callee(int first, int second, struct suite value) {
    (void)first;
    (void)second;
    if (value.kem != 1) return 0;
    if (value.kdf != 2) return 0;
    if (value.aead != 3) return 0;
    return 1;
}

static int caller(struct base *base) {
    return callee(0, 0, base->suite);
}

int main(void) {
    struct base value = { { 1, 2, 3 } };
    return caller(&value) ? 0 : 1;
}
