// run
// os linux

// GCC gives an enumerator the type 'int' when its value fits in int.
// Otherwise it uses the enum's compatible integer type. Check int, unsigned
// int, signed long, and unsigned long enums with constants that nominally
// need 8, 16, 32, and 64 bits.

enum IntEnum {
    IE_8 = 0x7f,
    IE_16 = 0x7fff,
    IE_32 = 0x7fffffff,
    IE_NEG = -1
};

enum UIntEnum {
    UE_8 = 0x7f,
    UE_16 = 0xffff,
    UE_32 = 0x7fffffff,
    UE_U32 = 0xf0000000u
};

enum LongEnum {
    LE_NEG = -1,
    LE_8 = 0x7f,
    LE_16 = 0x7fff,
    LE_32 = 0x7fffffff,
    LE_MID = 0xf0000000,
    LE_64 = 0x100000000LL
};

enum ULongEnum {
    ULE_8 = 0x7f,
    ULE_16 = 0xffff,
    ULE_32 = 0x7fffffff,
    ULE_MID = 0xf0000000u,
    ULE_64 = 0x8000000000000000ULL
};

#define IS_INT(x)       _Generic((x), int: 1, default: 0)
#define IS_UINT(x)      _Generic((x), unsigned int: 1, default: 0)
#define IS_LONG(x)      _Generic((x), long: 1, default: 0)
#define IS_ULONG(x)     _Generic((x), unsigned long: 1, default: 0)

#define CHECK(condition, code) do { if(!(condition)) return code; } while(0)

int main(void)
{
    enum IntEnum int_variable = IE_8;
    enum UIntEnum uint_variable = UE_U32;
    enum LongEnum long_variable = LE_64;
    enum ULongEnum ulong_variable = ULE_64;

    CHECK(sizeof(enum IntEnum) == 4, 1);
    CHECK(IS_INT(IE_8), 2);
    CHECK(IS_INT(IE_16), 3);
    CHECK(IS_INT(IE_32), 4);
    CHECK(IS_INT(IE_NEG), 5);

    CHECK(sizeof(enum UIntEnum) == 4, 6);
    CHECK(IS_INT(UE_8), 7);
    CHECK(IS_INT(UE_16), 8);
    CHECK(IS_INT(UE_32), 9);
    CHECK(IS_UINT(UE_U32), 10);
    CHECK(IS_UINT(~UE_U32), 11);

    CHECK(sizeof(enum LongEnum) == 8, 12);
    CHECK(IS_INT(LE_8), 13);
    CHECK(IS_INT(LE_16), 14);
    CHECK(IS_INT(LE_32), 15);
    CHECK(IS_INT(LE_NEG), 16);
    CHECK(IS_LONG(LE_MID), 17);
    CHECK(IS_LONG(LE_64), 18);
    CHECK(IS_LONG(~LE_64), 19);
    CHECK(LE_64 == 0x100000000LL, 20);

    CHECK(sizeof(enum ULongEnum) == 8, 21);
    CHECK(IS_INT(ULE_8), 22);
    CHECK(IS_INT(ULE_16), 23);
    CHECK(IS_INT(ULE_32), 24);
    CHECK(IS_ULONG(ULE_MID), 25);
    CHECK(IS_ULONG(ULE_64), 26);
    CHECK(IS_ULONG(~ULE_64), 27);
    CHECK(ULE_64 == 0x8000000000000000ULL, 28);

    CHECK(IS_INT(int_variable), 29);
    CHECK(IS_UINT(uint_variable), 30);
    CHECK(IS_LONG(long_variable), 31);
    CHECK(IS_ULONG(ulong_variable), 32);

    return 0;
}
