// run
// os linux

// System V aggregate classification, especially intrinsic members and arrays.
// These shapes exercise the member and array classification paths, including
// the SSEUP handling that must not write past the four-eightbyte table.

#include <emmintrin.h>
#include <immintrin.h>

typedef struct {
    __m128 x;
    __m128 y;
} Pair;

typedef struct {
    __m128 v[2];
} VecArray;

typedef struct {
    __m128 v[1][2];
} NestedArray;

typedef struct {
    float v[4];
} FloatArray;

typedef struct {
    int v[4];
} IntArray;

typedef union {
    __m128 v;
    float f[4];
} VecUnion;

typedef struct {
    __m256 v[1];
} Vec256Array;

typedef struct {
    Pair p[1];
} PairArray;

static Pair pass_pair(Pair p){
    return p;
}

static VecArray pass_vec_array(VecArray a){
    return a;
}

static NestedArray pass_nested_array(NestedArray a){
    return a;
}

static FloatArray pass_float_array(FloatArray a){
    return a;
}

static IntArray pass_int_array(IntArray a){
    return a;
}

static VecUnion pass_union(VecUnion a){
    return a;
}

static __m256 pass_m256(__m256 v){
    return v;
}

static Vec256Array pass_vec256_array(Vec256Array a){
    return a;
}

static PairArray pass_pair_array(PairArray a){
    return a;
}

int main(void){
    float out[8];

    Pair p;
    p.x = _mm_set1_ps(1.0f);
    p.y = _mm_set1_ps(2.0f);
    Pair q = pass_pair(p);
    _mm_storeu_ps(out, _mm_add_ps(q.x, q.y));
    if(out[0] != 3.0f) return 1;
    if(out[3] != 3.0f) return 2;

    VecArray va;
    va.v[0] = _mm_set1_ps(4.0f);
    va.v[1] = _mm_set1_ps(5.0f);
    VecArray vr = pass_vec_array(va);
    _mm_storeu_ps(out, _mm_add_ps(vr.v[0], vr.v[1]));
    if(out[0] != 9.0f) return 3;
    if(out[3] != 9.0f) return 4;

    NestedArray na;
    na.v[0][0] = _mm_set1_ps(6.0f);
    na.v[0][1] = _mm_set1_ps(7.0f);
    NestedArray nr = pass_nested_array(na);
    _mm_storeu_ps(out, _mm_add_ps(nr.v[0][0], nr.v[0][1]));
    if(out[0] != 13.0f) return 5;

    FloatArray fa = {{1.0f, 2.0f, 3.0f, 4.0f}};
    FloatArray fr = pass_float_array(fa);
    if(fr.v[0] != 1.0f || fr.v[1] != 2.0f || fr.v[2] != 3.0f || fr.v[3] != 4.0f) return 6;

    IntArray ia = {{5, 6, 7, 8}};
    IntArray ir = pass_int_array(ia);
    if(ir.v[0] != 5 || ir.v[1] != 6 || ir.v[2] != 7 || ir.v[3] != 8) return 7;

    VecUnion vu;
    vu.v = _mm_set1_ps(9.0f);
    VecUnion ur = pass_union(vu);
    _mm_storeu_ps(out, ur.v);
    if(out[0] != 9.0f) return 8;

    __m256 mv = _mm256_set1_ps(11.0f);
    __m256 mr = pass_m256(mv);
    _mm256_storeu_ps(out, mr);
    if(out[0] != 11.0f) return 9;

    Vec256Array v256a;
    v256a.v[0] = _mm256_set1_ps(13.0f);
    Vec256Array v256r = pass_vec256_array(v256a);
    _mm256_storeu_ps(out, v256r.v[0]);
    if(out[0] != 13.0f) return 10;

    PairArray pa;
    pa.p[0].x = _mm_set1_ps(15.0f);
    pa.p[0].y = _mm_set1_ps(17.0f);
    PairArray pr = pass_pair_array(pa);
    _mm_storeu_ps(out, _mm_add_ps(pr.p[0].x, pr.p[0].y));
    if(out[0] != 32.0f) return 11;

    return 0;
}
