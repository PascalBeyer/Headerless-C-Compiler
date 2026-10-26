// run

#include <stddef.h>

struct bitfield_value {
    size_t length : 30;
};

struct split_bitfield {
    size_t low : 8;
    size_t high : 30;
};

struct bitfield_32 {
    size_t low : 8;
    size_t value : 32;
};

struct bitfield_40 {
    size_t low : 8;
    size_t value : 40;
};

struct signed_bitfield_32 {
    int value : 32;
};

static size_t max_bitfield(struct bitfield_value *value) {
    return value->length > 1024 ? value->length : 1024;
}

int main(void) {
    struct bitfield_value bits = { .length = 1 };
    if (max_bitfield(&bits) != 1024) return 1;

    bits.length = 2048;
    if (max_bitfield(&bits) != 2048) return 2;

    struct split_bitfield split = { .low = 0x12, .high = 0x1234567 };
    if (split.low != 0x12) return 3;
    if (split.high != 0x1234567) return 4;

    struct bitfield_32 bits32 = { .low = 0x5a, .value = 0xdeadbeefU };
    if (bits32.value != 0xdeadbeefU) return 5;

    struct bitfield_40 bits40 = { .low = 0x5a, .value = 0x123456789aULL };
    if (bits40.value != 0x123456789aULL) return 6;

    struct signed_bitfield_32 signed_bits = { .value = -5 };
    if (signed_bits.value != -5) return 7;

    return 0;
}
