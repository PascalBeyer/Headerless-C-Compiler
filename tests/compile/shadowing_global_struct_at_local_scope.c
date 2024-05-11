
struct asd {int a;};


struct asd2 { int a; };


int main(){
    struct asd {float b;} redeclaration;
    
    struct asd2 should_be_4_bytes;
    
    struct asd2{
        int a;
        int b;
    } should_be_8_bytes;
    
#define static_assert(expr) typedef char static_assert_hack[(expr) ? 1 : -1]
    
    static_assert(sizeof(should_be_4_bytes) == 4);
    static_assert(sizeof(should_be_8_bytes) == 8);
    
    return 0;
}
