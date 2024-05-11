// run

struct arst{
    struct { int anon1; };
    struct { int anon2; };
    struct { int anon3; };
    struct { int anon4; };
    struct { int anon5; };
    struct { int anon6; };
    struct { int anon7; };
    struct { int anon8; };
    struct { int anon9; };
    struct { int anona; };
    struct { int anonb; };
    struct { int anonc; };
    struct { int anond; };
    struct { int anone; };
    struct { int anonf; };
};

struct string{
    union{
        int size;
        int length;
        int amount;
    };
    union{
        char *data;
        char *memory;
    };
};

#define assert(a) if(!(a)) return 1;

int main(){
    
    assert(0x0000000000000000 == (int)&((struct arst *)0)->anon1);
    assert(0x0000000000000004 == (int)&((struct arst *)0)->anon2);
    assert(0x0000000000000008 == (int)&((struct arst *)0)->anon3);
    assert(0x000000000000000C == (int)&((struct arst *)0)->anon4);
    assert(0x0000000000000010 == (int)&((struct arst *)0)->anon5);
    assert(0x0000000000000014 == (int)&((struct arst *)0)->anon6);
    assert(0x0000000000000018 == (int)&((struct arst *)0)->anon7);
    assert(0x000000000000001C == (int)&((struct arst *)0)->anon8);
    assert(0x0000000000000020 == (int)&((struct arst *)0)->anon9);
    assert(0x0000000000000024 == (int)&((struct arst *)0)->anona);
    assert(0x0000000000000028 == (int)&((struct arst *)0)->anonb);
    assert(0x000000000000002C == (int)&((struct arst *)0)->anonc);
    assert(0x0000000000000030 == (int)&((struct arst *)0)->anond);
    assert(0x0000000000000034 == (int)&((struct arst *)0)->anone);
    assert(0x0000000000000038 == (int)&((struct arst *)0)->anonf);
    
    
    assert((int)&((struct string *)0)->size == 0);
    assert((int)&((struct string *)0)->length == 0);
    assert((int)&((struct string *)0)->amount == 0);
    assert((int)&((struct string *)0)->data == 8);
    assert((int)&((struct string *)0)->memory == 8);
    
    
    return 0;
}
    
