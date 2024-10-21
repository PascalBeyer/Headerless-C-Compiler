
void (*function_pointer)(void);

int main(){
    function_pointer = main;
    (*function_pointer)();
}
