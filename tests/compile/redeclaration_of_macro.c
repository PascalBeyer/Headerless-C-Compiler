// check "'basic_redef': Redefinition of macro 'basic_redef'."
// check "'basic_redef': ... Here is the previous definition."
// check "'function_redef': Redefinition of macro 'function_redef'."
// check "'function_redef': ... Here is the previous definition."
// check "'basic_to_function': Redefinition of macro 'basic_to_function'."
// check "'basic_to_function': ... Here is the previous definition."
// check "'function_to_varargs': Redefinition of macro 'function_to_varargs'."
// check "'function_to_varargs': ... Here is the previous definition."

int _start(){
    
#define basic_redef 1
#define basic_redef 2
    if(basic_redef != 2) return 1;
    
#define function_redef() 1
#define function_redef() 2
    if(function_redef() != 2) return 1;
    
#define basic_to_function 1
#define basic_to_function() 1
    if(basic_to_function() != 1) return 1;
    
#define function_to_varargs() 1
#define function_to_varargs(...) 1
    if(function_to_varargs(1, 2, 3, 4) != 1) return 1;
    
    return 0;
}
