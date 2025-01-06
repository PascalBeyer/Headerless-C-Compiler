
#define va_macro(a, ...) a, ## __VA_ARGS__
#define non_va_macro(a, b) a, ## b

int function(int a, int b){
    return a + b;
}

int main(){
    int a = va_macro(1);
    int b = va_macro(1,);
    int c = function(va_macro(1, 2));
    int d = function(va_macro(1, 2));
    
    return a + b + c + d;
}
