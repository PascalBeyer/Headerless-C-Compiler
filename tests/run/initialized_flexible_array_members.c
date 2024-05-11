// run

#define assert(a) if(!(a)) return -1

struct has_flexible_array_member{
    char array[];
};

int simple_flex_array(){
    int prev = 1;
    struct has_flexible_array_member flex = {
        .array[0x1000] = 1,
    };
    int post = 1;
    
    memset(flex.array, 0, 0x1000);
    assert(prev == 1);
    assert(post == 1);
    return 0;
}

int flex_array_compound(){
    int prev = 1;
    struct has_flexible_array_member *flex = &(struct has_flexible_array_member){
        .array[0x1000] = 1,
    };
    int post = 1;
    
    memset(flex->array, 0, 0x1000);
    assert(prev == 1);
    assert(post == 1);
    return 0;
}


int static_simple_flex_array(){
    static int prev = 1;
    static struct has_flexible_array_member flex = {
        .array[0x1000] = 1,
    };
    static int post = 1;
    
    memset(flex.array, 0, 0x1000);
    assert(prev == 1);
    assert(post == 1);
    return 0;
}

int static_flex_array_compound(){
    static int prev = 1;
    static struct has_flexible_array_member *flex = &(struct has_flexible_array_member){
        .array[0x1000] = 1,
    };
    static int post = 1;
    
    memset(flex->array, 0, 0x1000);
    assert(prev == 1);
    assert(post == 1);
    return 0;
}


int main(){
    assert(simple_flex_array() == 0);
    assert(flex_array_compound() == 0);
    assert(static_simple_flex_array() == 0);
    assert(static_flex_array_compound() == 0);
    
    return 0;
}


void *memset(void *mem, int val, unsigned __int64  amount){
    char *it = mem;
    for(unsigned __int64 i = 0; i < amount; i++){
        *it++ = (char)val;
    }
    return mem;
}

