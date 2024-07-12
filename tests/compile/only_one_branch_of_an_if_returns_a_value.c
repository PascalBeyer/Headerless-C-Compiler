// check "must return a value."

int returns_value_in_one_branch_of_if(int a){
    if(a) return 1;
}

int main(){
    return returns_value_in_one_branch_of_if(0);
}
