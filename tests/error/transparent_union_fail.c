// fail "(14,14): Error at '&': Expression of type 'short*' matches none of the members of __attribute__((transparent_union))-pointer_argument."

union pointer_argument {
    int *int_ptr;
    char *char_ptr;
} __attribute__((transparent_union));

char function(union pointer_argument arg){
    return *arg.char_ptr;
}

int main(){
    short a;
    function(&a);
}
