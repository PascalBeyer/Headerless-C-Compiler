
#define __CRTDECL
#define _VCRTIMP __declspec(dllimport)

#include <sal.h>
#include <stddef.h>
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

#define _CRT_DECLARE_NONSTDC_NAMES 1
