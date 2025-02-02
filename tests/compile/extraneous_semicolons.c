// compile -Wextraneous_semicolon
// check "Extraneous ';' in struct."
// check "Extraneous ';' at global scope."

;

struct hello{
    ;
    
    int a;
};


int main(){
    struct hello arst = {0};
    
    return arst.a;
}

