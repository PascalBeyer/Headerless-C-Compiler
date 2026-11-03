// check "(29,8): Error at '/': Integer division by 0."
// check "(30,9): Error at '/': Integer division by 0."
// check "(31,9): Error at '/': Integer division by 0."
// check "(32,9): Error at '/': Integer division by 0."
// check "(34,8): Error at '/': Integer division by 0."
// check "(35,9): Error at '/': Integer division by 0."
// check "(36,9): Error at '/': Integer division by 0."
// check "(37,9): Error at '/': Integer division by 0."
// check "(43,8): Error at '%': Integer modulation by 0."
// check "(44,9): Error at '%': Integer modulation by 0."
// check "(45,9): Error at '%': Integer modulation by 0."
// check "(46,9): Error at '%': Integer modulation by 0."
// check "(48,8): Error at '%': Integer modulation by 0."
// check "(49,9): Error at '%': Integer modulation by 0."
// check "(50,9): Error at '%': Integer modulation by 0."
// check "(51,9): Error at '%': Integer modulation by 0."
// fail

typedef __int8 s8;
typedef unsigned __int8 u8;
typedef __int16 s16;
typedef unsigned __int16 u16;
typedef __int32 s32;
typedef unsigned __int32 u32;
typedef __int64 s64;
typedef unsigned __int64 u64;

#define test(type) type a_##type = ((type) 1)/((type)0)
test(s8);
test(s16);
test(s32);
test(s64);

test(u8);
test(u16);
test(u32);
test(u64);
#undef test



#define test(type) type b_##type = ((type) 1)%((type)0)
test(s8);
test(s16);
test(s32);
test(s64);

test(u8);
test(u16);
test(u32);
test(u64);
#undef test
