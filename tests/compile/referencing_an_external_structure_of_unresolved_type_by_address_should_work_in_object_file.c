// compile /c

extern struct unresolved a;

int *b = (void *)&a;

int main(){
    
    int *b = (void *)&a;
    return *b;
}
