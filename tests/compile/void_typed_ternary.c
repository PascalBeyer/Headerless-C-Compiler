
void function1(){}
void function2(){}

int main(){
    
    int a = 1;
    int b = 0;
    
    a ? function1() : function2();
    b ? function1() : function2();
    
    return 0;
}
