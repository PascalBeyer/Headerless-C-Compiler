// reject "Warning"


// Not specifying parameter names is permitted for unused parameters.
int function(int, char*[]){ return 0; }

int main(){
    return function(0, 0);
}
