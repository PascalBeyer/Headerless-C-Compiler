#pragma once

#if _WIN32
#include_next <float.h>
#else 

#define DBL_MAX 1.7976931348623158E+308
#define FLT_MAX 3.402823466E+38F

#define FLT_MIN 1.175494351e-38F
#define DBL_MIN 2.2250738585072014E-308

#define FLT_EPSILON 1.192092896e-07F
#define DBL_EPSILON 2.2204460492503131e-16

#define FLT_RADIX 2
#define FLT_MANT_DIG 24
#define DBL_MANT_DIG 53
#define FLT_DIG 6
#define DBL_DIG 15
#define FLT_MIN_EXP (-125)
#define DBL_MIN_EXP (-1021)
#define FLT_MAX_EXP 128
#define DBL_MAX_EXP 1024
#define FLT_MIN_10_EXP (-37)
#define DBL_MIN_10_EXP (-307)
#define FLT_MAX_10_EXP 38
#define DBL_MAX_10_EXP 308

#endif
