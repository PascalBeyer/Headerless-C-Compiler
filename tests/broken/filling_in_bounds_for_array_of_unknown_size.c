// broken
#include <stdio.h>

int (*x[])[];
int (*x[])[5];
int (*x[4])[];
int main(void)
{
    printf("Length of x = %llu\n", sizeof(x)/sizeof(x[0]));
    printf("Length of *x[0] = %llu\n", sizeof(*x[0])/sizeof((*x[0])[0]));
    return 0;
}

