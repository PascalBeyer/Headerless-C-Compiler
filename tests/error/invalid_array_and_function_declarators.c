// check "(6,29): Error at '(': Function returning array is illegal."
// check "(7,32): Error at '(': Function returning function is illegal."
// check "(8,23): Error at '[': Array of functions is illegal"
// fail  "(9,35): Error at '[': Only the most outer array can be of unknown size."

int function_returning_array()[3]{}
int function_returning_function()(){}
int array_of_functions[3]() = {};
int array_of_array_of_unknown_size[3][];
