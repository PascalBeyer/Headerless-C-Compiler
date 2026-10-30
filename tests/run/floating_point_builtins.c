// os linux
// run

static double double_from_bits(unsigned long long bits){
    union{
        unsigned long long bits;
        double value;
    } result;
    result.bits = bits;
    return result.value;
}

static float float_from_bits(unsigned int bits){
    union{
        unsigned int bits;
        float value;
    } result;
    result.bits = bits;
    return result.value;
}

#define CHECK(expected, expression) do{ \
    if((expression) != (expected)) return __LINE__; \
}while(0)

#define SIGNBITF_NEGATIVE (-2147483647 - 1)
#define SIGNBITF_POSITIVE 0

static int check_double(unsigned long long bits, int expected_isnan, int expected_isinf, int expected_sign, int expected_isnormal, int expected_isfinite, int expected_signbit){
    double d = double_from_bits(bits);
    
    CHECK(expected_isnan,   __builtin_isnan(d));
    CHECK(expected_isinf,   __builtin_isinf(d));
    CHECK(expected_sign,    __builtin_isinf_sign(d));
    CHECK(expected_isnormal,__builtin_isnormal(d));
    CHECK(expected_isfinite,__builtin_isfinite(d));
    CHECK(expected_signbit, __builtin_signbit(d));
    return 0;
}

static int check_float(unsigned int bits, int expected_isnan, int expected_isinf, int expected_sign, int expected_isnormal, int expected_isfinite, int expected_signbit){
    float f = float_from_bits(bits);
    
    CHECK(expected_isnan,   __builtin_isnan(f));
    CHECK(expected_isinf,   __builtin_isinf(f));
    CHECK(expected_sign,    __builtin_isinf_sign(f));
    CHECK(expected_isnormal,__builtin_isnormal(f));
    CHECK(expected_isfinite,__builtin_isfinite(f));
    CHECK(expected_signbit, __builtin_signbitf(f));
    return 0;
}

int main(void){
    CHECK(0, check_double(0x0000000000000000ULL, 0, 0, 0, 0, 1, 0));
    CHECK(0, check_double(0x8000000000000000ULL, 0, 0, 0, 0, 1, 1));
    CHECK(0, check_double(0x7fefffffffffffffULL, 0, 0, 0, 1, 1, 0));
    CHECK(0, check_double(0xffefffffffffffffULL, 0, 0, 0, 1, 1, 1));
    CHECK(0, check_double(0x0010000000000000ULL, 0, 0, 0, 1, 1, 0));
    CHECK(0, check_double(0x8010000000000000ULL, 0, 0, 0, 1, 1, 1));
    CHECK(0, check_double(0x0000000000000001ULL, 0, 0, 0, 0, 1, 0));
    CHECK(0, check_double(0x8000000000000001ULL, 0, 0, 0, 0, 1, 1));
    CHECK(0, check_double(0x7ff0000000000000ULL, 0, 1, 1, 0, 0, 0));
    CHECK(0, check_double(0xfff0000000000000ULL, 0, 1, -1, 0, 0, 1));
    CHECK(0, check_double(0x7ff8000000000000ULL, 1, 0, 0, 0, 0, 0));
    CHECK(0, check_double(0xfff8000000000000ULL, 1, 0, 0, 0, 0, 1));
    
    CHECK(0, check_float(0x00000000U, 0, 0, 0, 0, 1, SIGNBITF_POSITIVE));
    CHECK(0, check_float(0x80000000U, 0, 0, 0, 0, 1, SIGNBITF_NEGATIVE));
    CHECK(0, check_float(0x7f7fffffU, 0, 0, 0, 1, 1, SIGNBITF_POSITIVE));
    CHECK(0, check_float(0xff7fffffU, 0, 0, 0, 1, 1, SIGNBITF_NEGATIVE));
    CHECK(0, check_float(0x00800000U, 0, 0, 0, 1, 1, SIGNBITF_POSITIVE));
    CHECK(0, check_float(0x80800000U, 0, 0, 0, 1, 1, SIGNBITF_NEGATIVE));
    CHECK(0, check_float(0x00000001U, 0, 0, 0, 0, 1, SIGNBITF_POSITIVE));
    CHECK(0, check_float(0x80000001U, 0, 0, 0, 0, 1, SIGNBITF_NEGATIVE));
    CHECK(0, check_float(0x7f800000U, 0, 1, 1, 0, 0, SIGNBITF_POSITIVE));
    CHECK(0, check_float(0xff800000U, 0, 1, -1, 0, 0, SIGNBITF_NEGATIVE));
    CHECK(0, check_float(0x7fc00000U, 1, 0, 0, 0, 0, SIGNBITF_POSITIVE));
    CHECK(0, check_float(0xffc00000U, 1, 0, 0, 0, 0, SIGNBITF_NEGATIVE));
    
    return 0;
}
