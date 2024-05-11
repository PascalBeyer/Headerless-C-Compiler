
unresolved *pointer_to_unresolved_type;
//unresolved (*pointer_to_unresolved_array)[10]; currently not legal anymore 10.11.2020

struct{
    unresolved *member;
} struct_that_contains_an_unresolved_member;

unresolved *function_returning_pointer_to_unresolved_type(){
    return (void *)0;
}

void function_with_unresolved_argument(unresolved *unresolved_argument){
    (void)unresolved_argument;
}


struct unresolved;
typedef struct unresolved unresolved;

// this actually is not legal I guess...
/*
struct{
    unresolved a;
} *pointer_to_struct_containing_unresolved_member;
*/

int main(){
	function_with_unresolved_argument(function_returning_pointer_to_unresolved_type());
	
    return 0;
}
    
