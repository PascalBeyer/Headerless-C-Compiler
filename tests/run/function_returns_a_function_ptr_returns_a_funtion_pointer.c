// run


int function(void){
    return 1337;
}

int (*(function_returning_a_function)(void))(void){
    return function;
}

int (*(*(function_that_returns_a_function_which_returns_a_function)(void))(void))(void){
    return function_returning_a_function;
}

int main(){
    
    return function_that_returns_a_function_which_returns_a_function()()() == 1337 ? 0 : 1;
}

