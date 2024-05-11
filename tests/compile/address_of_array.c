

int array[400];

int main(){
    // right
    int *implicit_pointer = array;
    int (*explicit_array_pointer)[400] = &array;
    
    // wrong
    //int *explicit_pointer = &array;
    //int (*implicit_array_pointer)[400] = array;
}
