// run
// os linux

// An enumerator without '=' takes the previous value plus one. Its type stays
// the type of the previous enumerator, so increments must not silently wrap or
// widen the type.

enum A {
    A0 = 0x7ffffffe,
    A1
};

enum B {
    B0 = 0xfffffffeu,
    B1
};

enum D {
    D0 = -2,
    D1,
    D2
};

enum I {
    I0 = 0x100000000ULL,
    I1
};

// The value of U0 fits into an unsigned int, but its expression type is
// unsigned long. The implicit increment is therefore allowed to use the full
// unsigned-long range, making the enum unsigned long.
enum U {
    U0 = 0xffffffffUL,
    U1
};

// Here the expression is unsigned long as well, but both values still fit into
// an unsigned int, so the enum stays 32-bit.
enum V {
    V0 = 0x80000000UL,
    V1
};

// A positive long expression whose value does not fit into an int also keeps
// its wider range for implicit increments.
enum W {
    W0 = 0x100000000L,
    W1
};

// The first implicit value still fits into an unsigned int, but subsequent
// implicit values must keep using the original unsigned-long increment type.
enum P {
    P0 = 0xfffffffeul,
    P1,
    P2
};

// The same applies to a signed long expression whose value does not fit into
// an int.
enum Q {
    Q0 = 0xfffffffel,
    Q1,
    Q2
};

// The same increment-width behavior must be preserved when the enum also has
// a negative member, forcing the enum to a signed underlying type.
enum PN {
    PN_NEG = -1,
    PN0 = 0xfffffffeul,
    PN1,
    PN2
};

enum QN {
    QN_NEG = -1,
    QN0 = 0xfffffffel,
    QN1,
    QN2
};

// A negative enumerator must not make a later large unsigned enumerator look
// negative when determining the enum's underlying type.
enum M {
    M_NEG = -1,
    M_BIG = 0xffffffffffffffffULL
};

// Incrementing an unsigned enum must be able to cross the signed 64-bit
// boundary without overflowing or wrapping.
enum N {
    N0 = 0x7ffffffffffffffeULL,
    N1,
    N2
};

#define IS_INT(x)   _Generic((x), int: 1, default: 0)
#define IS_UINT(x)  _Generic((x), unsigned int: 1, default: 0)
#define IS_LONG(x)  _Generic((x), long: 1, default: 0)
#define IS_ULONG(x) _Generic((x), unsigned long: 1, default: 0)

#define CHECK(condition, code) do { if(!(condition)) return code; } while(0)

int main(void)
{
    CHECK(sizeof(enum A) == 4, 1);
    CHECK(A0 == 0x7ffffffe && A1 == 0x7fffffff, 2);
    CHECK(IS_INT(A0) && IS_INT(A1), 3);

    CHECK(sizeof(enum B) == 4, 4);
    CHECK(B0 == 0xfffffffeu && B1 == 0xffffffffu, 5);
    CHECK(IS_UINT(B0) && IS_UINT(B1), 6);

    CHECK(sizeof(enum D) == 4, 7);
    CHECK(D0 == -2 && D1 == -1 && D2 == 0, 8);
    CHECK(IS_INT(D0) && IS_INT(D1) && IS_INT(D2), 9);

    CHECK(sizeof(enum I) == 8, 10);
    CHECK(I0 == 0x100000000ULL && I1 == 0x100000001ULL, 11);
    CHECK(IS_ULONG(I0) && IS_ULONG(I1), 12);

    CHECK(sizeof(enum U) == 8, 13);
    CHECK(U0 == 0xffffffffUL && U1 == 0x100000000UL, 14);
    CHECK(IS_ULONG(U0) && IS_ULONG(U1), 15);

    CHECK(sizeof(enum V) == 4, 16);
    CHECK(V0 == 0x80000000UL && V1 == 0x80000001UL, 17);
    CHECK(IS_UINT(V0) && IS_UINT(V1), 18);

    CHECK(sizeof(enum W) == 8, 19);
    CHECK(W0 == 0x100000000L && W1 == 0x100000001L, 20);
    CHECK(IS_ULONG(W0) && IS_ULONG(W1), 21);

    CHECK(sizeof(enum M) == 8, 22);
    CHECK(M_NEG == -1 && (unsigned long)M_BIG == 0xffffffffffffffffUL, 23);
    CHECK(IS_INT(M_NEG) && IS_LONG(M_BIG), 24);

    CHECK(sizeof(enum N) == 8, 25);
    CHECK(N0 == 0x7ffffffffffffffeULL && N1 == 0x7fffffffffffffffULL &&
          N2 == 0x8000000000000000ULL, 26);
    CHECK(IS_ULONG(N0) && IS_ULONG(N1) && IS_ULONG(N2), 27);

    CHECK(sizeof(enum P) == 8, 28);
    CHECK(P0 == 0xfffffffeul && P1 == 0xfffffffful &&
          P2 == 0x100000000ul, 29);
    CHECK(IS_ULONG(P0) && IS_ULONG(P1) && IS_ULONG(P2), 30);

    CHECK(sizeof(enum Q) == 8, 31);
    CHECK(Q0 == 0xfffffffel && Q1 == 0xffffffffl &&
          Q2 == 0x100000000l, 32);
    CHECK(IS_ULONG(Q0) && IS_ULONG(Q1) && IS_ULONG(Q2), 33);

    CHECK(sizeof(enum PN) == 8, 34);
    CHECK(PN_NEG == -1 && PN0 == 0xfffffffeul && PN1 == 0xfffffffful &&
          PN2 == 0x100000000ul, 35);
    CHECK(IS_INT(PN_NEG) && IS_LONG(PN0) && IS_LONG(PN1) && IS_LONG(PN2), 36);

    CHECK(sizeof(enum QN) == 8, 37);
    CHECK(QN_NEG == -1 && QN0 == 0xfffffffel && QN1 == 0xffffffffl &&
          QN2 == 0x100000000l, 38);
    CHECK(IS_INT(QN_NEG) && IS_LONG(QN0) && IS_LONG(QN1) && IS_LONG(QN2), 39);

    return 0;
}
