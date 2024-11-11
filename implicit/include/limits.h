#define CHAR_BIT 8
#define MB_LEN_MAX 5

#define SCHAR_MAX 127
#define CHAR_MAX 127
#define SHRT_MAX 32767
#define INT_MAX 2147483647
#define LLONG_MAX 9223372036854775807LL

#define SCHAR_MIN (-127-1)
#define CHAR_MIN  (-127-1)
#define SHRT_MIN (-32767-1)
#define INT_MIN (-2147483647-1)
#define LLONG_MIN (-9223372036854775807LL-1)

#define UCHAR_MAX 255ui8
#define USHRT_MAX 65535ui16
#define UINT_MAX 4294967295ui32
#define ULLONG_MAX 18446744073709551615ui64

// @note: For now assume windows and we don't care about int vs long.
#define LONG_MIN INT_MIN
#define LONG_MAX INT_MAX
#define ULONG_MAX UINT_MAX

// Microsoft specific
//   see https://learn.microsoft.com/en-us/cpp/c-runtime-library/data-type-constants?view=msvc-170
#define _I8_MAX   127i8
#define _I8_MIN (-127i8-1)

#define _I16_MAX   32767i16
#define _I16_MIN (-32767i16-1)

#define _I32_MAX   2147483647i32
#define _I32_MIN (-2147483647i32-1)

#define _I64_MAX   9223372036854775807
#define _I64_MIN (-9223372036854775807-1)

#define _UI8_MAX  0xffui8
#define _UI16_MAX 0xffffui16
#define _UI32_MAX 0xffffffffui32
#define _UI64_MAX 0xffffffffffffffffui64
