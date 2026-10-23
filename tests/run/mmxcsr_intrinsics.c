// os linux
// run

// Regression test for the ldmxcsr/stmxcsr inline assembly instructions used by
// _mm_setcsr/_mm_getcsr.

#include <xmmintrin.h>

int main() {
    unsigned int old = _mm_getcsr();
    unsigned int new_value = old ^ 0x6000;
    _mm_setcsr(new_value);

    unsigned int check = _mm_getcsr();
    _mm_setcsr(old);

    if(check != new_value) return 1;

    return 0;
}
