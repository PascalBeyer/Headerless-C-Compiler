// fail
// broken

#include <intrin.h>

__declspec(inline_asm) __m128 _mm_set1_ps_arst(float a){
    shufps a, a, 0
    return a
}



void _start(){
    __m128 mask = _mm_set1_ps_arst( 10.0f );
}

