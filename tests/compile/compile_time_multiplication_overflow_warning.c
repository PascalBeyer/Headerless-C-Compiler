// reject "(73,16): Warning 3 at '*': Compile time overflow: (s32)(2 * 3) = 6."
// reject "(74,16): Warning 3 at '*': Compile time overflow: (s32)(0 * 2147483647) = 0."
// reject "(75,16): Warning 3 at '*': Compile time overflow: (s32)(1 * 2147483647) = 2147483647."
// reject "(76,17): Warning 3 at '*': Compile time overflow: (s32)(-1 * 2147483647) = -2147483647."
// reject "(77,20): Warning 3 at '*': Compile time overflow: (s32)(46340 * 46340) = 2147395600."
// check "(78,20): Warning 3 at '*': Compile time overflow: (s32)(46341 * 46341) = -2147479015."
// check "(79,21): Warning 3 at '*': Compile time overflow: (s32)(-46341 * 46341) = 2147479015."
// reject "(80,21): Warning 3 at '*': Compile time overflow: (s32)(-46340 * 46340) = -2147395600."
// reject "(81,17): Warning 3 at '*': Compile time overflow: (s32)(-2 * -3) = 6."
// reject "(82,17): Warning 3 at '*': Compile time overflow: (s32)(-1 * 1) = -1."
// reject "(83,17): Warning 3 at '*': Compile time overflow: (s32)(1 * -1) = -1."
// reject "(84,33): Warning 3 at '*': Compile time overflow: (s32)(-2147483648 * 1) = -2147483648."
// check "(85,33): Warning 3 at '*': Compile time overflow: (s32)(-2147483648 * -1) = -2147483648."
// reject "(86,23): Warning 3 at '*': Compile time overflow: (u32)(0x2 * 0x3) = 0x6."
// reject "(87,23): Warning 3 at '*': Compile time overflow: (u32)(0x0 * 0xffffffff) = 0x0."
// reject "(88,23): Warning 3 at '*': Compile time overflow: (u32)(0x1 * 0xffffffff) = 0xffffffff."
// reject "(89,32): Warning 3 at '*': Compile time overflow: (u32)(0xffffffff * 0x1) = 0xffffffff."
// reject "(90,27): Warning 3 at '*': Compile time overflow: (u32)(0x10000 * 0xffff) = 0xffff0000."
// check "(91,27): Warning 3 at '*': Compile time overflow: (u32)(0x10000 * 0x10000) = 0x0."
// check "(92,32): Warning 3 at '*': Compile time overflow: (u32)(0x80000000 * 0x2) = 0x0."
// check "(93,32): Warning 3 at '*': Compile time overflow: (u32)(0xffffffff * 0x2) = 0xfffffffe."
// check "(94,32): Warning 3 at '*': Compile time overflow: (u32)(0xffffffff * 0xffffffff) = 0x1."
// reject "(95,23): Warning 3 at '*': Compile time overflow: 2 * 3 = 6."
// reject "(96,23): Warning 3 at '*': Compile time overflow: 0 * 9223372036854775807 = 0."
// reject "(97,23): Warning 3 at '*': Compile time overflow: 1 * 9223372036854775807 = 9223372036854775807."
// reject "(98,24): Warning 3 at '*': Compile time overflow: -1 * 9223372036854775807 = -9223372036854775807."
// reject "(99,32): Warning 3 at '*': Compile time overflow: 3037000499 * 3037000499 = 9223372030926249001."
// check "(100,32): Warning 3 at '*': Compile time overflow: 3037000500 * 3037000500 = -9223372036709301616."
// reject "(101,24): Warning 3 at '*': Compile time overflow: -2 * -3 = 6."
// reject "(102,24): Warning 3 at '*': Compile time overflow: -1 * 1 = -1."
// reject "(103,23): Warning 3 at '*': Compile time overflow: 1 * -1 = -1."
// reject "(104,23): Warning 3 at '*': Compile time overflow: 3 * -4 = -12."
// reject "(105,48): Warning 3 at '*': Compile time overflow: -9223372036854775808 * 1 = -9223372036854775808."
// check "(106,48): Warning 3 at '*': Compile time overflow: -9223372036854775808 * -1 = -9223372036854775808."
// check "(107,24): Warning 3 at '*': Compile time overflow: -1 * -9223372036854775808 = -9223372036854775808."
// reject "(108,41): Warning 3 at '*': Compile time overflow: 4611686018427387904 * 1 = 4611686018427387904."
// check "(109,41): Warning 3 at '*': Compile time overflow: 4611686018427387904 * 2 = -9223372036854775808."
// check "(110,41): Warning 3 at '*': Compile time overflow: 4611686018427387904 * 4 = 0."
// reject "(111,41): Warning 3 at '*': Compile time overflow: 4611686018427387904 * -2 = -9223372036854775808."
// check "(112,41): Warning 3 at '*': Compile time overflow: 4611686018427387904 * -4 = 0."
// check "(113,41): Warning 3 at '*': Compile time overflow: 2305843009213693952 * 2305843009213693952 = 0."
// check "(114,42): Warning 3 at '*': Compile time overflow: -2305843009213693952 * 2305843009213693952 = 0."
// check "(115,41): Warning 3 at '*': Compile time overflow: 2305843009213693952 * -2305843009213693952 = 0."
// reject "(116,33): Warning 3 at '*': Compile time overflow: 0x2 * 0x3 = 0x6."
// reject "(117,33): Warning 3 at '*': Compile time overflow: 0x0 * 0xffffffffffffffff = 0x0."
// reject "(118,33): Warning 3 at '*': Compile time overflow: 0x1 * 0xffffffffffffffff = 0xffffffffffffffff."
// reject "(119,52): Warning 3 at '*': Compile time overflow: 0xffffffffffffffff * 0x1 = 0xffffffffffffffff."
// reject "(120,42): Warning 3 at '*': Compile time overflow: 0x100000000 * 0xffffffff = 0xffffffff00000000."
// check "(121,42): Warning 3 at '*': Compile time overflow: 0x100000000 * 0x100000000 = 0x0."
// reject "(122,51): Warning 3 at '*': Compile time overflow: 0x8000000000000000 * 0x1 = 0x8000000000000000."
// check "(123,51): Warning 3 at '*': Compile time overflow: 0x8000000000000000 * 0x2 = 0x0."
// check "(124,52): Warning 3 at '*': Compile time overflow: 0xffffffffffffffff * 0x2 = 0xfffffffffffffffe."
// check "(125,52): Warning 3 at '*': Compile time overflow: 0xffffffffffffffff * 0xffffffffffffffff = 0x1."
// check "(126,27): Warning 3 at '*': Compile time overflow: (u32)(0x10000 * 0x10000) = 0x0."
// check "(127,32): Warning 3 at '*': Compile time overflow: (u32)(0xffffffff * 0xffffffff) = 0x1."
// reject "(128,26): Warning 3 at '*': Compile time overflow: 65536 * 4294967296 = 281474976710656."
// check "(129,42): Warning 3 at '*': Compile time overflow: 0x100000000 * 0x7fffffffffffffff = 0xffffffff00000000."
// check "(130,51): Warning 3 at '*': Compile time overflow: 0x8000000000000000 * 0x2 = 0x0."

// Compile time overflow checks for the constant folding of '*'.
//
// The type of the multiplication is decided by the usual arithmetic conversions of
// the two operands, so we use suffixes to select the type we want to test:
//
//     123      -> s32
//     123u     -> u32
//     123ll    -> s64
//     123ull   -> u64
//
// Every case is checked twice: the ones that overflow have to produce a warning,
// the ones that do not overflow must not produce one ('reject').
int main(){
    int v0 = 2 * 3;
    int v1 = 0 * 2147483647;
    int v2 = 1 * 2147483647;
    int v3 = -1 * 2147483647;
    int v4 = 46340 * 46340;
    int v5 = 46341 * 46341;
    int v6 = -46341 * 46341;
    int v7 = -46340 * 46340;
    int v8 = -2 * -3;
    int v9 = -1 * 1;
    int v10 = 1 * -1;
    int v11 = (-2147483647 - 1) * 1;
    int v12 = (-2147483647 - 1) * -1;
    unsigned v13 = 2u * 3u;
    unsigned v14 = 0u * 4294967295u;
    unsigned v15 = 1u * 4294967295u;
    unsigned v16 = 4294967295u * 1u;
    unsigned v17 = 65536u * 65535u;
    unsigned v18 = 65536u * 65536u;
    unsigned v19 = 2147483648u * 2u;
    unsigned v20 = 4294967295u * 2u;
    unsigned v21 = 4294967295u * 4294967295u;
    __int64 v22 = 2ll * 3ll;
    __int64 v23 = 0ll * 9223372036854775807ll;
    __int64 v24 = 1ll * 9223372036854775807ll;
    __int64 v25 = -1ll * 9223372036854775807ll;
    __int64 v26 = 3037000499ll * 3037000499ll;
    __int64 v27 = 3037000500ll * 3037000500ll;
    __int64 v28 = -2ll * -3ll;
    __int64 v29 = -1ll * 1ll;
    __int64 v30 = 1ll * -1ll;
    __int64 v31 = 3ll * -4ll;
    __int64 v32 = (-9223372036854775807ll - 1) * 1ll;
    __int64 v33 = (-9223372036854775807ll - 1) * -1ll;
    __int64 v34 = -1ll * (-9223372036854775807ll - 1);
    __int64 v35 = 4611686018427387904ll * 1ll;
    __int64 v36 = 4611686018427387904ll * 2ll;
    __int64 v37 = 4611686018427387904ll * 4ll;
    __int64 v38 = 4611686018427387904ll * -2ll;
    __int64 v39 = 4611686018427387904ll * -4ll;
    __int64 v40 = 2305843009213693952ll * 2305843009213693952ll;
    __int64 v41 = -2305843009213693952ll * 2305843009213693952ll;
    __int64 v42 = 2305843009213693952ll * -2305843009213693952ll;
    unsigned __int64 v43 = 2ull * 3ull;
    unsigned __int64 v44 = 0ull * 18446744073709551615ull;
    unsigned __int64 v45 = 1ull * 18446744073709551615ull;
    unsigned __int64 v46 = 18446744073709551615ull * 1ull;
    unsigned __int64 v47 = 4294967296ull * 4294967295ull;
    unsigned __int64 v48 = 4294967296ull * 4294967296ull;
    unsigned __int64 v49 = 9223372036854775808ull * 1ull;
    unsigned __int64 v50 = 9223372036854775808ull * 2ull;
    unsigned __int64 v51 = 18446744073709551615ull * 2ull;
    unsigned __int64 v52 = 18446744073709551615ull * 18446744073709551615ull;
    unsigned v53 = 65536u * 65536;
    unsigned v54 = 4294967295u * -1;
    __int64 v55 = 65536u * 4294967296ll;
    unsigned __int64 v56 = 4294967296ull * 9223372036854775807ll;
    unsigned __int64 v57 = 9223372036854775808ull * 2;
    return (int)(v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v14 + v15 + v16 + v17 + v18 + v19 + v20 + v21 + v22 + v23 + v24 + v25 + v26 + v27 + v28 + v29 + v30 + v31 + v32 + v33 + v34 + v35 + v36 + v37 + v38 + v39 + v40 + v41 + v42 + v43 + v44 + v45 + v46 + v47 + v48 + v49 + v50 + v51 + v52 + v53 + v54 + v55 + v56 + v57);
}
