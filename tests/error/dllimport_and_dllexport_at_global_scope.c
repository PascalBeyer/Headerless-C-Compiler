// check "Error at 'initialized_dllimport_function': Cannot define a function that is declared '__declspec(dllimport)'."
// check "Error at 'initialized_dllimport_variable': Variable declared '__declspec(dllimport)' cannot be initialized."
// check "Error at 'static_uninitialized_dllimport_variable': Variable declared '__declspec(dllimport)' cannot also be declared 'static'."
// check "Error at 'static_uninitialized_dllexport_variable': Variable declared '__declspec(dllexport)' cannot also be declared 'static'."
// reject "Error at 'initialized_dllexport_function'"
// reject "Error at 'initialized_dllexport_variable'"
// reject "Error at 'uninitialized_dllimport_variable'"
// reject "Error at 'uninitialized_dllexport_variable'"
// reject "Error at 'extern_uninitialized_dllimport_variable'"
// reject "Error at 'extern_uninitialized_dllexport_variable'"
// fail

__declspec(dllimport) int initialized_dllimport_function() { return 1; } // Error: dllimport prohibited on definition
__declspec(dllimport) int initialized_dllimport_variable = 3;            // Error: dllimport prohibited on definition
__declspec(dllexport) int initialized_dllexport_function() { return 1; } // Okay:  export definition
__declspec(dllexport) int initialized_dllexport_variable = 3;            // Okay:  export definition

__declspec(dllimport) int uninitialized_dllimport_variable; // Okay: dllimport declaration
__declspec(dllexport) int uninitialized_dllexport_variable; // Okay: export tentative definition

static __declspec(dllimport) int static_uninitialized_dllimport_variable; // Error: cannot be static
static __declspec(dllexport) int static_uninitialized_dllexport_variable; // Error: cannot be static

extern __declspec(dllimport) int extern_uninitialized_dllimport_variable; // Okay: dllimport declaration
extern __declspec(dllexport) int extern_uninitialized_dllexport_variable; // Okay: export declaration (NOT definition)

