// run

#define assert(a) if(!(a)) return -1
#define offset_in_type(type, member) (__int64)(&((type *)0)->member)

int main(){
    struct __declspec(packed) packed{
        __int8  a;
        __int32 b;
        __int16 c;
        __int64 d;
    } packed = {
        .a = 1,
        .b = 2,
        .c = 3,
        .d = 4,
    };
    
    assert(offset_in_type(struct packed, a) == 0);
    assert(offset_in_type(struct packed, b) == 1);
    assert(offset_in_type(struct packed, c) == 1 + 4);
    assert(offset_in_type(struct packed, d) == 1 + 4 + 2);
    assert(sizeof(packed) == 1 + 4 + 2 + 8);
    
    return 0;
}
