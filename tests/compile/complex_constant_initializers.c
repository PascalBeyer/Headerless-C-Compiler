
#ifdef INCLUDED

STORAGE_TYPE int global_array[] = { 1, 2, 3, 4, 5 };
STORAGE_TYPE int *add_for_global_array = global_array + 1;
STORAGE_TYPE int *sub_for_global_array = global_array - 1;
STORAGE_TYPE int global_variable = 1;
STORAGE_TYPE int *add_for_global_variable = &global_variable + 1;
STORAGE_TYPE int *sub_for_global_variable = &global_variable - 1;
STORAGE_TYPE int *initialized_by_array_literal = (int []){1, 2, 3};
STORAGE_TYPE int *initialized_by_member_of_struct_literal = (struct {int array[10];}){0}.array;
STORAGE_TYPE int *initialized_by_indexed_array_literal = &(int []){1, 2, 3}[0];

#else

#define STORAGE_TYPE
#define INCLUDED

#include "complex_constant_initializers.c"

#undef STORAGE_TYPE


int main(){
    
#define STORAGE_TYPE static
#include "complex_constant_initializers.c"
    
}

#endif
