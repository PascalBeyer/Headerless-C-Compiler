// os linux
// run

// Regression test for System V AMD64 ABI handling of floating point
// aggregates and atomic integer arguments/returns.

typedef struct { float x; } F1;
typedef struct { float x, y; } F2;
typedef struct { float x, y, z; } F3;
typedef struct { float x, y, z, w; } F4;
typedef struct { double x; } D1;
typedef struct { double x, y; } D2;
typedef struct { float x; double y; } FD;
typedef struct { int i; float f; } IF;
typedef struct { float a[3]; } FA3;
typedef struct { F2 a, b; } FN;

typedef _Atomic(int) AtomicInt;
typedef _Atomic(long) AtomicLong;
typedef _Atomic(_Bool) AtomicBool;

F1 mk_f1(float x) { F1 r = { x }; return r; }
F2 mk_f2(float x, float y) { F2 r = { x, y }; return r; }
F3 mk_f3(float x, float y, float z) { F3 r = { x, y, z }; return r; }
F4 mk_f4(float x, float y, float z, float w) { F4 r = { x, y, z, w }; return r; }
D1 mk_d1(double x) { D1 r = { x }; return r; }
D2 mk_d2(double x, double y) { D2 r = { x, y }; return r; }
FD mk_fd(float x, double y) { FD r = { x, y }; return r; }
IF mk_if(int i, float f) { IF r = { i, f }; return r; }
FA3 mk_fa3(float x, float y, float z) { FA3 r = { { x, y, z } }; return r; }
FN mk_fn(float x, float y, float z, float w) { FN r = { { x, y }, { z, w } }; return r; }

// Returning an aggregate from an indexed global used to leak the index
// register in the System V return path.
static F3 f3_array[2] = {
    { 1.0f, 2.0f, 3.0f },
    { 4.0f, 5.0f, 6.0f },
};

F3 get_f3(int i) { return f3_array[i]; }

F3 get_f3_or_default(int i) {
    if(i >= 2) return mk_f3(7.0f, 8.0f, 9.0f);
    return f3_array[i];
}

float take_f1(F1 v) { return v.x; }
float take_f2(F2 v) { return v.x + v.y; }
float take_f3(F3 v) { return v.x + v.y + v.z; }
float take_f4(F4 v) { return v.x + v.y + v.z + v.w; }
double take_d1(D1 v) { return v.x; }
double take_d2(D2 v) { return v.x + v.y; }
double take_fd(FD v) { return v.x + v.y; }
float take_if(IF v) { return v.i + v.f; }
float take_fa3(FA3 v) { return v.a[0] + v.a[1] + v.a[2]; }
float take_fn(FN v) { return v.a.x + v.a.y + v.b.x + v.b.y; }

float ret_float(float x) { return x; }
double ret_double(double x) { return x; }

AtomicInt ret_atomic_int(AtomicInt x) { return x; }
AtomicLong ret_atomic_long(AtomicLong x) { return x; }
AtomicBool ret_atomic_bool(AtomicBool x) { return x; }

int take_atomic_int(AtomicInt x) { return x; }
long take_atomic_long(AtomicLong x) { return x; }
int take_atomic_bool(AtomicBool x) { return x; }

// The seventh integer argument (and everything after it) is passed on the
// stack in the System V ABI. Make sure scalar arguments in that position,
// including null pointers, are stored rather than treated as aggregates.
int take_many_args(int a, int b, int c, int d, int e, int f, void *p, int g) {
    if(p != 0) return -1;
    return a + b + c + d + e + f + g;
}

int take_many_args_pointer(int a, int b, int c, int d, int e, int f, int *p, int g) {
    (void)a; (void)b; (void)c; (void)d; (void)e; (void)f;
    return *p + g;
}

#define CHECK(cond, code) do { if(!(cond)) return (code); } while(0)

int main(void) {
    CHECK(ret_float(7.5f) == 7.5f, 1);
    CHECK(ret_double(8.5) == 8.5, 2);

    F1 f1 = mk_f1(1.0f);
    CHECK(f1.x == 1.0f, 3);
    CHECK(take_f1(f1) == 1.0f, 4);
    CHECK(take_f1(mk_f1(1.5f)) == 1.5f, 5);

    F2 f2 = mk_f2(1.0f, 2.0f);
    CHECK(f2.x == 1.0f && f2.y == 2.0f, 6);
    CHECK(take_f2(f2) == 3.0f, 7);
    CHECK(take_f2(mk_f2(1.5f, 2.5f)) == 4.0f, 8);

    F3 f3 = mk_f3(1.0f, 2.0f, 3.0f);
    CHECK(f3.x == 1.0f && f3.y == 2.0f && f3.z == 3.0f, 9);
    CHECK(take_f3(f3) == 6.0f, 10);
    CHECK(take_f3(mk_f3(1.5f, 2.5f, 3.5f)) == 7.5f, 11);

    F4 f4 = mk_f4(1.0f, 2.0f, 3.0f, 4.0f);
    CHECK(f4.x == 1.0f && f4.y == 2.0f && f4.z == 3.0f && f4.w == 4.0f, 12);
    CHECK(take_f4(f4) == 10.0f, 13);
    CHECK(take_f4(mk_f4(1.5f, 2.5f, 3.5f, 4.5f)) == 12.0f, 14);

    D1 d1 = mk_d1(1.5);
    CHECK(d1.x == 1.5, 15);
    CHECK(take_d1(d1) == 1.5, 16);
    CHECK(take_d1(mk_d1(2.5)) == 2.5, 17);

    D2 d2 = mk_d2(1.5, 2.5);
    CHECK(d2.x == 1.5 && d2.y == 2.5, 18);
    CHECK(take_d2(d2) == 4.0, 19);
    CHECK(take_d2(mk_d2(2.5, 3.5)) == 6.0, 20);

    FD fd = mk_fd(1.0f, 2.5);
    CHECK(fd.x == 1.0f && fd.y == 2.5, 21);
    CHECK(take_fd(fd) == 3.5, 22);
    CHECK(take_fd(mk_fd(1.5f, 3.5)) == 5.0, 23);

    IF iff = mk_if(3, 2.5f);
    CHECK(iff.i == 3 && iff.f == 2.5f, 24);
    CHECK(take_if(iff) == 5.5f, 25);
    CHECK(take_if(mk_if(4, 1.5f)) == 5.5f, 26);

    FA3 fa3 = mk_fa3(1.0f, 2.0f, 3.0f);
    CHECK(fa3.a[0] == 1.0f && fa3.a[1] == 2.0f && fa3.a[2] == 3.0f, 27);
    CHECK(take_fa3(fa3) == 6.0f, 28);
    CHECK(take_fa3(mk_fa3(1.5f, 2.5f, 3.5f)) == 7.5f, 29);

    FN fn = mk_fn(1.0f, 2.0f, 3.0f, 4.0f);
    CHECK(fn.a.x == 1.0f && fn.a.y == 2.0f && fn.b.x == 3.0f && fn.b.y == 4.0f, 30);
    CHECK(take_fn(fn) == 10.0f, 31);
    CHECK(take_fn(mk_fn(1.5f, 2.5f, 3.5f, 4.5f)) == 12.0f, 32);

    AtomicInt ai = 11;
    AtomicLong al = 22;
    AtomicBool ab = 1;
    CHECK(ret_atomic_int(ai) == 11, 33);
    CHECK(ret_atomic_long(al) == 22, 34);
    CHECK(ret_atomic_bool(ab) == 1, 35);
    CHECK(take_atomic_int(ai) == 11, 36);
    CHECK(take_atomic_long(al) == 22, 37);
    CHECK(take_atomic_bool(ab) == 1, 38);

    CHECK(take_many_args(1, 2, 3, 4, 5, 6, 0, 7) == 28, 39);
    int stack_pointer_value = 35;
    CHECK(take_many_args_pointer(1, 2, 3, 4, 5, 6, &stack_pointer_value, 7) == 42, 40);

    F3 indexed = get_f3(1);
    CHECK(indexed.x == 4.0f && indexed.y == 5.0f && indexed.z == 6.0f, 41);

    F3 indexed_default = get_f3_or_default(5);
    CHECK(indexed_default.x == 7.0f && indexed_default.y == 8.0f && indexed_default.z == 9.0f, 42);

    F3 indexed_element = get_f3_or_default(0);
    CHECK(indexed_element.x == 1.0f && indexed_element.y == 2.0f && indexed_element.z == 3.0f, 43);

    return 0;
}
