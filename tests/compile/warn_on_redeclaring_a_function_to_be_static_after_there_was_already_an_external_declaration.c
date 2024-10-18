// check "'function': Redefining declaration from external to static."

int function();
static int function(){
    return 1;
}


int main(){
    return function();
}
