// check "'return' in function declared as '_Noreturn'."
// check "Control flow reaching the end of '_Noreturn' function."

_Noreturn int noreturn_function_1(){
    return 1;
}

_Noreturn int noreturn_function_2(){}


int main(){
    noreturn_function_1();
    noreturn_function_2();
}
