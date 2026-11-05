// run
// os linux

typedef enum Kind {
    KIND_ZERO,
    KIND_ONE,
    KIND_TWO,
    KIND_THREE,
    KIND_FOUR,
} Kind;

typedef struct S {
    unsigned char a;
    unsigned char b;
    unsigned char c;
    unsigned char d;
    Kind kind:3;
} S;

int main(void)
{
    S s = {0};
    s.kind = KIND_FOUR;
    if (s.kind != KIND_FOUR) return 1;
    if ((int)s.kind != 4) return 2;
    return 0;
}
