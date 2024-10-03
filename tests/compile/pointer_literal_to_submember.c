
#define offset_in_type(type, member) (unsigned long long)(&((type *)0)->member)

struct arst{
    struct { int a; } arst;
};


int main(){
    int arst[offset_in_type(struct arst, arst.a) + 1];
    
    return 1;
}
