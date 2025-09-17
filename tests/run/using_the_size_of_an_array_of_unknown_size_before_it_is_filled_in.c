// run

#define assert(a) if(!(a)) return 1;

int array_of_unknown_size[];
int what_size_do_I_have[sizeof(array_of_unknown_size)];
int array_of_unknown_size[4];

int _start(){
    
    assert(sizeof(array_of_unknown_size) == 16);
    assert(sizeof(what_size_do_I_have) == 4 * sizeof(array_of_unknown_size));
    return 0;
}



