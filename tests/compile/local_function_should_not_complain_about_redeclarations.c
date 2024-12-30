// reject "Warning"

int main(){
    
    int a = 0, b = 1;
    
    
    int function(int a){
        int b = 1;
        return a + b;
    }
    
    return function(a + b);
}
