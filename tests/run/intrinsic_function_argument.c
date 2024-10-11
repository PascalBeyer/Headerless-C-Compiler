// compile -intrinsic
// run

#define assert(a) (!(a) ? __fastfail(1) : (void)0)

int one(int a){
    assert(a == 1);
}

static void assert_m128(__m128i m128){
    one(1);
    _mm_extract_epi32(m128, 0);
    assert(_mm_extract_epi32(m128, 1) == 1);
    assert(_mm_extract_epi32(m128, 2) == 2);
    assert(_mm_extract_epi32(m128, 3) == 3);
}


int main(){
    __m128i m128 = _mm_set_epi32(3, 2, 1, 0);
    assert(_mm_extract_epi32(m128, 0) == 0);
    assert(_mm_extract_epi32(m128, 1) == 1);
    assert(_mm_extract_epi32(m128, 2) == 2);
    assert(_mm_extract_epi32(m128, 3) == 3);
    
    assert_m128(m128);
    
    return 0;
}
