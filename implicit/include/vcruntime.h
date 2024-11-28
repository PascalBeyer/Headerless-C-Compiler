
#pragma once

#define __CRTDECL

#include <sal.h>
#include <stdarg.h>
#include <stdint.h>

#define _CRT_BEGIN_C_HEADER
#define _CRT_END_C_HEADER

#define _CRT_INSECURE_DEPRECATE(...)
#define _CRT_DEPRECATE_TEXT(...)
#define _CRT_INSECURE_DEPRECATE_MEMORY(...)

#define __crt_va_start va_start
#define __crt_va_end va_end

#define _CRT_WIDE_INTERNAL(a) L##a
#define _CRT_WIDE(a) _CRT_WIDE_INTERNAL(a)

#ifdef __HLC_COMPILE_TO_OBJECT__

#ifdef _DLL
#define _VCRTIMP __declspec(dllimport)
#else 
#define _VCRTIMP
#endif

#else // !__HLC_COMPILE_TO_OBJECT__

// We want to import all things when we compiling to an exe.
// Otherwise, let the normal crt code paths decide.
#define _ACRTIMP __declspec(dllimport)
#define _VCRTIMP __declspec(dllimport)
#define _DCRTIMP __declspec(dllimport)
#endif

#define __crt_countof(array) (sizeof(array) / sizeof((array)[0]))

typedef unsigned short wchar_t;
typedef unsigned __int64 size_t;
typedef __int64 ptrdiff_t;
typedef __int64 intptr_t;

