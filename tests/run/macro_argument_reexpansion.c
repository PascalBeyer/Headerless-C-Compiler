// run

#define WRAP2(x) (x)
#define WRAP(x) WRAP2(x)

struct S {
    union {
        struct {
            int member;
        } inner;
    } outer;
};

#define member outer.inner.member

int main(void)
{
    struct S s;

    WRAP(s.member) = 7;

#undef member
    if (s.outer.inner.member != 7) return 1;
    return 0;
}
