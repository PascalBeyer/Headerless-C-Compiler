
#define args(func, a, b, c, d, e) func(a, b, c, d, e)


int args(
function, 
int 
a, 
int 
b, 
int 
c, 
int 
d, 
int 
e
){
    return a + b + c + d + e;
}

int main(){
    return function(1, 2, 3, 4, 5);
}

