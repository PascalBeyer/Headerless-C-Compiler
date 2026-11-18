// run

void function(void){
}

void (*function_array[5])(void) = {
    [0 ... 3] = function,
};

int main(void){
    (function_array[2])();
}
