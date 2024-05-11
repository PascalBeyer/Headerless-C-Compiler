// run

#define assert(a) if(!(a)) return 1

int main(){
    static int a = 1337;
    static int *b = &a;
    
    assert(&a == b);
    assert(a == *b);
    
    return 0;
}


