// run
// os linux

// GNU builtins used directly by third-party C libraries.

int main(void){
    if(__builtin_popcount(0xFFu) != 8) return 1;
    if(__builtin_popcountll(0xF0F0ULL) != 8) return 2;
    if(__builtin_ctz(8u) != 3) return 3;
    if(__builtin_clz(1u) != 31) return 4;
    if(__builtin_ctzll(0x100000000ULL) != 32) return 5;
    if(__builtin_clzll(1ULL) != 63) return 6;

    int value = 1;
    __builtin_prefetch(&value, 0, 3);

    return 0;
}
