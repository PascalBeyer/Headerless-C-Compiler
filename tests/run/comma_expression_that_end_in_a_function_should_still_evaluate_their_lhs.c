// run

int a;

int function(){
    return a;
}

int main(){
    return (a = 3, function)() - 3;
}
