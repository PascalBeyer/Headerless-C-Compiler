// run

// Basic coverage for the implicit <cpuid.h>:
//   __cpuid, __get_cpuid_max, and the leaf-1 feature bit constants.

#include <cpuid.h>

int main(void)
{
    unsigned int eax, ebx, ecx, edx;
    unsigned int max;

    max = __get_cpuid_max(0, 0);
    if (max < 1) return 1;

    __cpuid(0, eax, ebx, ecx, edx);
    if (eax != max) return 2;

    __cpuid(1, eax, ebx, ecx, edx);
    if (!(edx & bit_FPU)) return 3;
    if (!(edx & bit_TSC)) return 4;

    return 0;
}
