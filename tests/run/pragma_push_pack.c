// run

typedef struct __declspec(align(32)){
    int arst[16];
} arst;

// #include <stdio.h>
#include <stddef.h>

struct arst5{
    __declspec(align(2)) char a;
    int b;
};

#pragma pack(push, 1)

struct arst{
    int a;
    arst b;
};

struct arst2{
    char a;
    short b;
    int c;
    __int64 d;
};

struct arst3{
    __declspec(align(16)) int a;
};

struct arst4{
    int a;
    struct arst3 b;
};


struct arst6{
    struct arst5 a;
    int b;
};

#define assert(a) if(!(a)) return 1;

int main(){
    
    // Override alignment takes precedence over pragma pack alignment.
    assert(_Alignof(arst) == 0x20 && sizeof(struct arst) == 0x60 &&  offsetof(struct arst, b) == 0x20);
    
    assert(_Alignof(struct arst2) == 1);
    assert(sizeof(struct arst2) == 0xf);
    assert(offsetof(struct arst2, a) == 0);
    assert(offsetof(struct arst2, b) == 1);
    assert(offsetof(struct arst2, c) == 3);
    assert(offsetof(struct arst2, d) == 7);
    
    assert(_Alignof(struct arst3) == 16);
    
    assert(_Alignof(struct arst4) == 16);
    assert(sizeof(struct arst4) == 32);
    assert(offsetof(struct arst4, b) == 16);
    
    // printf("%d %d %d\n", _Alignof(struct arst6), sizeof(struct arst6), offsetof(struct arst6, b)); // @cleanup: It also keeps track of a minimum alignment or something.
}

//_____________________________________________________________________________________________________________________
// Some tests for the stack behaviour.

#define ALIGNMENT 0x4

#define check_align(n) static_assert(_Alignof((struct{ __int64 a;}){0}) == n)

#pragma pack()
#pragma pack(show) // 16

static_assert(_Alignof((struct{ __int64 a;}){0}) == 8);

check_align(8);

#pragma pack(push, hello, ALIGNMENT)

check_align(4);

#pragma pack(show) // 8 | hello 16

#pragma pack(pop, 2)

check_align(2);

#pragma pack(pop) // @note: fails

check_align(2);

#pragma pack(push, hello, 8)

check_align(8)

#pragma pack(pop, world) // Warning previous one did not push world.

check_align(8)

#pragma pack(push, hello, 1)

check_align(1)

#pragma pack(push, 2)

check_align(2)

#pragma pack(pop, hello)

check_align(8)

