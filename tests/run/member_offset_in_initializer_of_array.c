// run

#define assert(a) if(!(a)) return 1;

struct asd{
    int member_one;
    int member_two;
};


int offsets[] = {
    (int)(__int64)(&(((struct asd *)0)->member_one)),
    (int)(__int64)(&(((struct asd *)0)->member_two)),
};


int main(){
    
    assert(sizeof(offsets) == 8);
    assert(offsets[0] == 0);
    assert(offsets[1] == 4);
    
    return 0;
}
