// run

#define assert(a) if(!(a)) return 1;

// 6.3.1.2 "If any scalar value is converted to _Bool the result is 0 if the value compares equal to 0; 
//          otherwise, the result is 1."

// nice little msvc bug
_Bool a = 256;

_Bool function_that_returns_a_bool(){
    return 1337;
}

int function_that_takes_a_bool(_Bool bool){
    assert(bool);
	return 0;
}

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef char      s8;
typedef short     s16;
typedef int       s32;
typedef long long s64;

typedef float f32;
typedef double f64;

int main(){
    assert(a == 1);
    a = 1337;
    assert(a == 1);
    
    a = 0;
    assert(a == 0);
    
    _Bool b = 256;
    assert(b == 1);
    
    assert((b = 256) == 1);
    
    int c = 1337;
    assert((_Bool)c);
    assert((_Bool)c == 1);
    
    assert(c + (_Bool)c == 1338);

    char d = a + b + c;
    assert(d == (char)1338);
    
    assert(function_that_returns_a_bool() == 1);
    
    if(function_that_takes_a_bool(1337)) return 1;
    
    void *pointer = &pointer;
    assert((_Bool)pointer == 1);
    
    pointer = 0;
    assert((_Bool)pointer == 0);
    pointer = (void *)0x1337;
    
    assert((_Bool)((void *)0) == 0);
    assert((_Bool)((void *)0x1337133713371337) == 1);
    
    assert((_Bool)((u8)0) == 0);
    assert((_Bool)((u8)0x1337133713371337) == 1);
    assert((_Bool)((u16)0) == 0);
    assert((_Bool)((u16)0x1337133713371337) == 1);
    assert((_Bool)((u32)0) == 0);
    assert((_Bool)((u32)0x1337133713371337) == 1);
    assert((_Bool)((u64)0) == 0);
    assert((_Bool)((u64)0x1337133713371337) == 1);
    
    assert((_Bool)((s8)0) == 0);
    assert((_Bool)((s8)0x1337133713371337) == 1);
    assert((_Bool)((s16)0) == 0);
    assert((_Bool)((s16)0x1337133713371337) == 1);
    assert((_Bool)((s32)0) == 0);
    assert((_Bool)((s32)0x1337133713371337) == 1);
    assert((_Bool)((s64)0) == 0);
    assert((_Bool)((s64)0x1337133713371337) == 1);
    
    assert((_Bool)((f32)0.0f) == 0);
    assert((_Bool)((f32)1337.0f) == 1);
    assert((_Bool)((f64)0.0f) == 0);
    assert((_Bool)((f64)1337.0f) == 1);
    
    u8  _u8  = 0x13;
    u16 _u16 = 0x1337;
    u32 _u32 = 0x13371337;
    u64 _u64 = 0x1337133713371337;
    
    s8  _s8  = 0x13;
    s16 _s16 = 0x1337;
    s32 _s32 = 0x13371337;
    s64 _s64 = 0x1337133713371337;
    
    f32 _f32 = 1337.0f;
    f64 _f64 = 1337.0f;
    
    _Bool bool = 0;
    assert(bool = _u8);
    assert(bool = _u16);
    assert(bool = _u32);
    assert(bool = _u64);
    
    assert(bool = _s8);
    assert(bool = _s16);
    assert(bool = _s32);
    assert(bool = _s64);
    
    assert(bool = _f32);
    assert(bool = _f64);
    
    assert(bool = pointer);
    
    assert((bool = _u8)     == 1);
    assert((bool = _u16)    == 1);
    assert((bool = _u32)    == 1);
    assert((bool = _u64)    == 1);
    assert((bool = _s8)     == 1);
    assert((bool = _s16)    == 1);
    assert((bool = _s32)    == 1);
    assert((bool = _s64)    == 1);
    assert((bool = _f32)    == 1);
    assert((bool = _f64)    == 1);
    assert((bool = pointer) == 1);
    
    
    _u8  = 0;
    _u16 = 0;
    _u32 = 0;
    _u64 = 0;
    
    _s8  = 0;
    _s16 = 0;
    _s32 = 0;
    _s64 = 0;
    
    _f32 = 0;
    _f64 = 0;
    
    pointer = 0;
    
    assert(!(bool = _u8));
    assert(!(bool = _u16));
    assert(!(bool = _u32));
    assert(!(bool = _u64));
    
    assert(!(bool = _s8));
    assert(!(bool = _s16));
    assert(!(bool = _s32));
    assert(!(bool = _s64));
    
    assert(!(bool = _f32));
    assert(!(bool = _f64));
    
    assert(!(bool = pointer));
    
    assert((bool = _u8)     == 0);
    assert((bool = _u16)    == 0);
    assert((bool = _u32)    == 0);
    assert((bool = _u64)    == 0);
    assert((bool = _s8)     == 0);
    assert((bool = _s16)    == 0);
    assert((bool = _s32)    == 0);
    assert((bool = _s64)    == 0);
    assert((bool = _f32)    == 0);
    assert((bool = _f64)    == 0);
    assert((bool = pointer) == 0);
    
    // @cleanup: Actually make some tests for compound assignments, instead of just testing that they compile and run.
    
    bool |= _u8;
    bool |= _u16;
    bool |= _u32;
    bool |= _u64;
    
    bool |= _s8;
    bool |= _s16;
    bool |= _s32;
    bool |= _s64;
    
    bool &= _u8;
    bool &= _u16;
    bool &= _u32;
    bool &= _u64;
    
    bool &= _s8;
    bool &= _s16;
    bool &= _s32;
    bool &= _s64;
    
    bool ^= _u8;
    bool ^= _u16;
    bool ^= _u32;
    bool ^= _u64;
    
    bool ^= _s8;
    bool ^= _s16;
    bool ^= _s32;
    bool ^= _s64;
    
    return 0;
}
