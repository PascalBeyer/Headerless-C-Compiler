// run

typedef unsigned char u8;
struct contains_small_array {u8 a[3], b; };
struct contains_small_array array[] = { { 1 }, 2 };

struct contains_small_anon_struct { struct{u8 a[3];}; u8 b; };
struct contains_small_anon_struct array2[] = { { 1 }, 2 };

#define assert(a) if(!(a)) return 1;

int main(){
    
    assert(sizeof(array)/sizeof(*array) == 2);
    assert(array[0].a[0] == 1 && array[0].a[1] == 0 && array[0].a[2] == 0 && array[0].b == 0);
    assert(array[1].a[0] == 2 && array[1].a[1] == 0 && array[1].a[2] == 0 && array[1].b == 0);
    
    assert(sizeof(array2)/sizeof(*array2) == 2);
    assert(array2[0].a[0] == 1 && array2[0].a[1] == 0 && array2[0].a[2] == 0 && array2[0].b == 0);
    assert(array2[1].a[0] == 2 && array2[1].a[1] == 0 && array2[1].a[2] == 0 && array2[1].b == 0);
    
    struct contains_small_array l_array[] = { { 1 }, 2 };
    struct contains_small_anon_struct l_array2[] = { { 1 }, 2 };
    
    assert(sizeof(l_array)/sizeof(*l_array) == 2);
    assert(l_array[0].a[0] == 1 && l_array[0].a[1] == 0 && l_array[0].a[2] == 0 && l_array[0].b == 0);
    assert(l_array[1].a[0] == 2 && l_array[1].a[1] == 0 && l_array[1].a[2] == 0 && l_array[1].b == 0);
    
    assert(sizeof(l_array2)/sizeof(*l_array2) == 2);
    assert(l_array2[0].a[0] == 1 && l_array2[0].a[1] == 0 && l_array2[0].a[2] == 0 && l_array2[0].b == 0);
    assert(l_array2[1].a[0] == 2 && l_array2[1].a[1] == 0 && l_array2[1].a[2] == 0 && l_array2[1].b == 0);
}
