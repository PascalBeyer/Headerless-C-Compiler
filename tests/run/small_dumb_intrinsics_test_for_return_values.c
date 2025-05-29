// run

#include <intrin.h>

int _start(){
    
    float f = 1.0f;
    __m128 m = _mm_load_ps1(&f);
    f = _mm_cvtss_f32(m);
    if(f != 1.0f) return 1;
    
    __int64 a = __rdtsc();
    
    return 0;
}
