// check "Error at '4': More than one 'default' association."
// check "Error at '3': ... Here is the previous 'default' association."
// check "Error at 'a': The expression of type 'int' matches two associations."
// check "Error at '1': ... Here is the first match."
// fail "Error at '2': ... Here is the second match."

int other(){
    int a  = 1;
    return _Generic(a,
            default: 3,
            default: 4);
}

int main(){
    
    int a = 1;
    
    return _Generic(a, 
            int: 1,
            int: 2,
            default: 3);
}
