
#pragma once

#pragma compilation_unit("cpuid.c")

#define __cpuid(leaf, eax, ebx, ecx, edx) __cpuid_gnu((leaf), &(eax), &(ebx), &(ecx), &(edx))
#define __cpuid_count(leaf, count, eax, ebx, ecx, edx) __cpuid_count((leaf), (count), &(eax), &(ebx), &(ecx), &(edx))


