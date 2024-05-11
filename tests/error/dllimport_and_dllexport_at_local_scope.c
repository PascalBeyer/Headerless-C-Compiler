// check "Error at 'static_uninitialized_dllimport_variable': Variable declared '__declspec(dllimport)' cannot also be declared 'static'."
// check "Error at 'static_uninitialized_dllexport_variable': Variable declared '__declspec(dllexport)' cannot also be declared 'static'."
// check "Error at 'uninitialized_dllexport_variable': Cannot define a variable declared '__declspec(dllexport)' inside of a function. Did you forget to add 'extern'?"
// check "Error at 'initialized_dllexport_variable': Cannot define a variable declared '__declspec(dllexport)' inside of a function. Did you forget to add 'extern'?"
// reject "Error at 'uninitialized_dllimport_variable'"
// reject "Error at 'extern_initialized_dllimport_variable'"
// reject "Error at 'extern_initialized_dllexport_variable'"
// fail

int main(){
    static __declspec(dllimport) int static_uninitialized_dllimport_variable; // Error: cannot be static
    static __declspec(dllexport) int static_uninitialized_dllexport_variable; // Error: cannot be static
    
    __declspec(dllimport) int uninitialized_dllimport_variable; // Okay: dllimport declaration
    __declspec(dllexport) int uninitialized_dllexport_variable; // Error: external definition at local scope.
    
    extern __declspec(dllimport) int extern_initialized_dllimport_variable; // Okay: dllimport declaration
    extern __declspec(dllexport) int extern_initialized_dllexport_variable; // Okay: export declaration (NOT definition)
    
    __declspec(dllexport) int initialized_dllexport_variable = 1; //  Error: external definition at local scope.
}

__declspec(dllexport) int extern_initialized_dllexport_variable;
