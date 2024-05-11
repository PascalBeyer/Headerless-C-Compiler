// fail "Expression of type void cannot be used as function argument."

void varargs(int count, ...){
    
}

int main(){
    varargs(1, (void)1);
}
