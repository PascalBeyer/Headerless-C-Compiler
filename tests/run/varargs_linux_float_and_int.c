// os linux
// run
// check "Arg 0: 1 (gp_offset: 24)"
// check "Arg 0: 1.000000 (fp_offset: 64)"
// check "Arg 1: 2 (gp_offset: 32)"
// check "Arg 1: 2.000000 (fp_offset: 80)"
// check "Arg 2: 3 (gp_offset: 40)"
// check "Arg 2: 3.000000 (fp_offset: 96)"
// check "Arg 3: 4 (gp_offset: 48)"
// check "Arg 3: 4.000000 (fp_offset: 112)"
// check "Arg 4: 5 (gp_offset: 48)"
// check "Arg 4: 5.000000 (fp_offset: 128)"
// check "Arg 5: 6 (gp_offset: 48)"
// check "Arg 5: 6.000000 (fp_offset: 144)"
// check "Arg 6: 7 (gp_offset: 48)"
// check "Arg 6: 7.000000 (fp_offset: 160)"
// check "Arg 7: 8 (gp_offset: 48)"
// check "Arg 7: 8.000000 (fp_offset: 176)"
// check "Arg 8: 9 (gp_offset: 48)"
// check "Arg 8: 9.000000 (fp_offset: 176)"
// check "Arg 9: 10 (gp_offset: 48)"
// check "Arg 9: 10.000000 (fp_offset: 176)"


#include <stdio.h>

#define offsetof(type, member) &args->member

void analyze_args(int int_count, int float_count, ...) {
    va_list args;
    va_start(args, float_count);
    
    printf("Size %lu\n", sizeof(args));
   
    printf("Initial gp_offset: %d %p\n", args->gp_offset, offsetof(va_list, gp_offset));
    printf("Initial fp_offset: %d %p\n", args->fp_offset, offsetof(va_list, fp_offset));
    printf("Initial arg_area: %p %p\n",  args->overflow_arg_area, offsetof(va_list, overflow_arg_area));
    printf("args->reg_save_area %p %p\n", args->reg_save_area, offsetof(va_list, reg_save_area));
    
    for (int i = 0; i < int_count; i++) {
        int value = va_arg(args, int);
        printf("Arg %d: %d (gp_offset: %d)\n", i, value, args->gp_offset);
	
	double value2 = va_arg(args, double);
        printf("Arg %d: %f (fp_offset: %d)\n", i, value2, args->fp_offset);
    }
    
    va_end(args);
}

int main(){
    analyze_args(10, 10, 1, 1.0, 2, 2.0, 3, 3.0, 4, 4.0, 5, 5.0, 6, 6.0, 7, 7.0, 8, 8.0, 9, 9.0, 10, 10.0);
}
