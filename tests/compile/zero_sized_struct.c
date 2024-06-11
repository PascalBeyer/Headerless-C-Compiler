

typedef struct {} zero_sized_typedef;


#ifdef __HLC__
#define ZERO_SIZE_STRUCT struct{}
#else
#define ZERO_SIZE_STRUCT zero_sized_typedef
#endif


ZERO_SIZE_STRUCT zero_sized_global;

struct {
    ZERO_SIZE_STRUCT zero_sized_field;
} zero_sized_struct_with_member;

ZERO_SIZE_STRUCT array_of_zero_sized_struct[1];

ZERO_SIZE_STRUCT function_that_takes_zero_sized_struct_and_returns_zero_sized_struct(ZERO_SIZE_STRUCT argument){
    return argument;
}


int main(){
    ZERO_SIZE_STRUCT zero_sized_local;
    
    // zero_sized assignment and type match
    zero_sized_global = zero_sized_local;
    
    zero_sized_local = zero_sized_struct_with_member.zero_sized_field;
    
    ZERO_SIZE_STRUCT *zero_sized_pointer;
    
    *zero_sized_pointer = zero_sized_local;
    
    zero_sized_pointer = &zero_sized_global;
    
    zero_sized_local = *zero_sized_pointer;
    
    zero_sized_local = function_that_takes_zero_sized_struct_and_returns_zero_sized_struct(zero_sized_local);
    
    zero_sized_local = array_of_zero_sized_struct[0];
    
    return 0;
}

