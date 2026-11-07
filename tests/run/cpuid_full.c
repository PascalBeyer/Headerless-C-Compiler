// run

// __cpuid_count and __get_cpuid coverage for the implicit <cpuid.h>.

#include <cpuid.h>

int main(void)
{
    unsigned int eax, ebx, ecx, edx;

    __cpuid_count(7, 0, eax, ebx, ecx, edx);

    if (!__get_cpuid(1, &eax, &ebx, &ecx, &edx)) return 1;

    return 0;
}
