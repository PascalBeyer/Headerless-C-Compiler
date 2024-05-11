// run

struct arst{
    struct { int a; };
    int b;
};

#define assert(a) if(!(a)) return 1;

int main(){
    
    struct arst arst = {1, 2};
    
    assert(arst.a == 1);
    assert(arst.b == 2);
    
    return 0;
}
