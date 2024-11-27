

int main(void){
    struct {
        int a :13;
    } bitfield;
    _Atomic int atomic_int;
    
    _Static_assert(_Generic(main, int (*)(void) : 1 ), "");
    _Static_assert(_Generic("abc",  char *: 1), "");    
    _Static_assert(_Generic(bitfield.a,  int: 1), "");    
    _Static_assert(_Generic(atomic_int, int: 1), "");
}

