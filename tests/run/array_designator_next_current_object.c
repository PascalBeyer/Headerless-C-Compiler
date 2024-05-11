// run

#define assert(a) if(!(a)) return -1

int array[400] = {
    [300] = 1,
    2,
    3,
    [0] = 1337,
    420,
    
    [399] = 0xdead,
    [15] = 0xbeef,
    
};

int main(){
    assert(array[300] == 1);
    assert(array[301] == 2);
    assert(array[302] == 3);
    assert(array[303] == 0);
    assert(array[0] == 1337);
    assert(array[1] == 420);
    assert(array[399] == 0xdead);
    assert(array[15] == 0xbeef);
    return 0;
}
