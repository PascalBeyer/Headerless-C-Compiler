// run

#define offset_in_type(type, member) (unsigned long long)(&((type *)0)->member)

#define assert(a) if(!(a)) return 1;

int main(){
    
    struct has_array_of_unknown_size{
        int asd;
        int array_of_unknown_size[];
    };
    
    
    assert(offset_in_type(struct has_array_of_unknown_size, array_of_unknown_size) == 4);
    assert(*(&((struct has_array_of_unknown_size *)0)->array_of_unknown_size) == (int *)4);
    
    // int (*asd)[] = &((struct has_array_of_unknown_size *)0)->array_of_unknown_size;
    return 0;
}


