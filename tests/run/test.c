// compile -stdlib
// run

#include "test.h"

enum enum_test{
    ENUM_zero,
    ENUM_one,
    ENUM_one2 = ENUM_one,
    ENUM_333 = 333,
    ENUM_334,
    ENUM_2p2 = 2 + 2,
    ENUM_5,
    ENUM_10 = 3 * 3  + 1,
    ENUM_minus_one = -1,
    ENUM_hex = 0x12345,
    ENUM_max_int = 0x7fffffff,
    ENUM_overflow = 0x7fffffff + 1, // @cleanup: why does this not print a warning?
    ENUM_way_to_large = 0xffffffff,
};

s32 local_persist_test(){
    static int a;
    return a++;
}

struct type_decl{
    int a;
} and_var_decl;

struct {
    char array[13];
    char array2[13];
    char *string_in_struct;
} global_struct_around_array = { "I'm an array", "I'm an array!", "I am a char *"};

struct{
    unsigned short _byte, _state;
} has_compound_declaration;

struct contains_a_bitfield{
    int a : 1;
    int b : 1;
    int c : 4;
    int d : 6;
    int e : 31;
};

static struct contains_a_bitfield globally_initialize_bitfield_variables = {
    1, 1, 2, 6, 7
};


unsigned short global_array_of_unknown_size_initialized_with_wide_characters[] = L"Hello";


int main(){
    assert(sizeof(0)                        == 4); // int
    assert(sizeof(0xffffffff)               == 4); // unsigned int
    assert(sizeof(4294967295)               == 8); // long long
    assert(sizeof(0x100000000)              == 8); // unsigned long long
    assert(sizeof(-1)                       == 4); // int
    assert(sizeof(2147483647)               == 4); // int 
    assert(sizeof(2147483648)               == 8); // long long
    assert(sizeof((2147483647 + 1))         == 4); // int + 1 = int
    assert(sizeof(-2147483648)              == 8); // -(long long) = long long
    assert(sizeof(-0xffffff)                == 4); // -(unsigned int) = unsigned int
    assert(sizeof(0xffff)                   == 4); // int
    assert(sizeof(0x80000000)               == 4); // unsigned int
    assert(sizeof(0x100000000)              == 8); // long long
    assert(sizeof((0xffffffffffffffff + 1)) == 8); // unsigned long long + 1 = unsigned long long
    
    assert(sizeof(0.0) == 8);
    assert(sizeof(0.0f) == 4);
    
    assert(__HLC__ == 1);
    
    assert(0b1111 == 15);
    
    assert(ENUM_zero == 0);
    assert(ENUM_one == 1);
	assert(ENUM_one2 == 1);
    assert(ENUM_333 == 333);
    assert(ENUM_334 == 334);
    assert(ENUM_2p2 == 4);
    assert(ENUM_5 == 5);
    assert(ENUM_10 == 10);
    assert(ENUM_minus_one == -1);
    assert(ENUM_hex == 0x12345);
    assert(ENUM_max_int == 2147483647);
    assert(ENUM_overflow == -2147483648);
    assert(ENUM_max_int == 0x7fffffff);
    assert(ENUM_overflow == 0x80000000); // @cleanup:  this is correct I guess. (signed -> unsigned int)
    assert(ENUM_overflow == 2147483647 + 1); // right hand side is long long @cleanup: is it?
    assert(ENUM_way_to_large == -1);
    
    static int local_return_10(){ return 10;}
    assert(local_return_10() == 10);
    
    {typedef s32 local_type; local_type a = 1337; assert(a == 1337);}
    s32 local_type = 1337; 
    assert(local_type == 1337);
    
    struct type_decl type_decl;
    type_decl.a = 1;
    assert(type_decl.a == 1);
    assert(and_var_decl.a == 0);
    
#define identity_macro(what_to_put) what_to_put
    assert(identity_macro(1) == 1);
    assert(identity_macro(type_decl.a) == 1);
    
#define times_three(a) (a + a + a)
    int var = 123;
    assert(times_three(var) == 3 * 123);
    assert(times_three(var++) == 3 * 123 + 3);
    assert(times_three((var = 123)) == 3 * 123);
    assert(times_three(--var) == 3 * 123 - 6);
    assert(times_three((var = 111)));
    
    assert((var = 12) + (var = 13) == 25);
    
    // for now the easy test.. see below
    int define_overwrite_test = 111;
#define define_overwrite_test  3
    assert(define_overwrite_test == 3);
    
    /*
    // @hmm: this does not complain in msvc. But I am not sure if it should.
    //       first define_overwrite_test gets expanded during arguments substitution of 'assert'
    //       then  define_overwrite_test gets expanded during the expansion of 'assert'
    //       this might be 6.10.3.4.2 in the spec... read that more careful
        
    int define_overwrite_test = 111;
    #define define_overwrite_test define_overwrite_test + 3
    assert(define_overwrite_test == 114);
        
    */
    
    static s32 local_function_local_persist_test(){
        static int a;
        return a++;
    }
    for(u32 i = 0; i < 10; i++) assert(local_function_local_persist_test() == i);
    
    
    for(u32 i = 0; i < 10; i++) assert(local_persist_test() == i);
    
    struct{
        int a; 
        int b;
        int c;
    } anonymous_struct = {1, 2, 3};
    assert(anonymous_struct.a == 1 && anonymous_struct.b == 2 && anonymous_struct.c == 3);
    struct{
        struct{
            int a;
            int b;
            int c;
        };
    } anonymous_struct2 = {{1, 2, 3}}; // @cleanup: both work on msvc
    // } anonymous_struct2 = {1,2,3};
    assert(anonymous_struct2.a == 1 && anonymous_struct2.b == 2 && anonymous_struct2.c == 3);
    
    u32 u32_array[10] = {
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    };
    
    for(u32 i = 0; i < array_count(u32_array); i++){
        assert(i + 1 == u32_array[i]);
    }
    assert(u32_array[0] <= u32_array[2]);
    
    char array_string[13] = "i'm an array";
    
    assert(sizeof(array_string) == 13);
    assert(sizeof("i'm an array") == 13); 
    
    assert(cstring_match(array_string, "i'm an array"));
    
    struct {
        char array[13];
        char *string_in_struct;
    } struct_around_array = { "I'm an array", "I am a char *"};
    
    assert(sizeof(struct_around_array.array) == 13);
    assert(sizeof(struct_around_array.string_in_struct) == 8);
    assert(sizeof(struct_around_array) == 24);
    
    assert(cstring_match(struct_around_array.array, "I'm an array"));
    assert(cstring_match(struct_around_array.string_in_struct, "I am a char *"));
    
    assert(sizeof(global_struct_around_array.array) == 13);
    assert(sizeof(global_struct_around_array.string_in_struct) == 8);
    assert(sizeof(global_struct_around_array) == 40);
    
    assert(cstring_match(global_struct_around_array.array, "I'm an array"));
    assert(cstring_match(global_struct_around_array.string_in_struct, "I am a char *"));
    
    
    {
        int my_awsome_int_array[3] = {1, 2, 3};
        int sum = 0;
        for(u32 i = 0; i < 3; i++) sum += my_awsome_int_array[i];
        assert(sum == 6);
    }
    
    u32 zero = 0;
    u32 one  = 1;
    u32 two  = 2;
    
    assert(0 || one);
    assert(1 && two);
    assert(1 && 2);
    assert(one && 2);
    assert(one && two);
    assert(one != two);
    
    switch(one){
        case 1: break;
        default: assert(false);
    }
    
    switch(one){
        case 2: assert(false);
        default: break;
    }
    
    assert(-2 == (char)(0 - two));
    // @note: compile time casting
    assert((u8)0x1101 == 1);
    assert((s8)0xffff == -1);
    
    {
        int a = 0;
        switch(one){
            case 1: 
            case 2:
            a = true;
            break;
        }
        assert(a);
    }
    
    static struct{ int a; int b; int c;} return_a_struct(){
        return (struct{ int a; int b; int c;}){1, 2, 3};
    }
    
    struct {int a; int b; int c; } a_struct = return_a_struct();
    assert(a_struct.a == 1 && a_struct.b == 2 && a_struct.c == 3);
    
    u32 i_get_incremented = 0;
    assert(i_get_incremented++ == 0 && ++i_get_incremented == 2 && i_get_incremented == 2);
    assert(--i_get_incremented == 1 && i_get_incremented-- == 1 && i_get_incremented == 0);
    
    {
        int a = 0;
        if(one == two){
            a = 1;
        }
        assert(a == 0);
    }
    
    {
        int a = 0;
        if(one == two); else{
            a = 1;
        }
        assert(a);
    }
    
    u32 three = one + two;
    assert(three == 3);
    
    
    // struct v2{ int x; int y; }; @cleanup:?
    struct v2 i_was_intialized_to_one_two_named = {.x = (int)one, .y = (int)two};
    assert(i_was_intialized_to_one_two_named.x == 1 && i_was_intialized_to_one_two_named.y == 2);
    
    u32 u32_has_braces = {1};
    assert(u32_has_braces == 1);
    
    {
        u32 shift = 1 << 3 << 2;
        assert(shift == 32);
        
        u32 shift_and_or = 1 | 1 << 3 | 1 << 4 | 2;
        assert(shift_and_or == 27);
        
        u32 mult = shift_and_or * shift * 3;
        assert(mult == 2592);
        
        u32 paren_expression = 3 * (one + two);
        assert(paren_expression == 9);
    }
    
    // @cleanup: should this work? I guess they are not closures so probably not...
    // static u32 local_function(){return 1;}
    // static u32 local_function2(){return local_function();}
    
    static u64 varargs_sum_u32(u32 amount, ...){
        va_list va;
        //((void)(__va_start(&va, amount)));
        va = (va_list)((u64 *)&amount + 1);
        
        static u64 sum_u32_va_list(u32 amount, va_list va){
            u64 sum = 0;
            for(u32 i = 0; i < amount; i++){
                sum += *(u32* ) ((va += sizeof(__int64)) - sizeof(__int64));
            }
            return sum;
        }
        
        u64 sum = sum_u32_va_list(amount, va);
        ((void)(va = (va_list)0));
        return sum;
    }
    u64 varargs_sum = varargs_sum_u32(7, 1, 2, 3, 4, 5, 6, 7);
    assert(varargs_sum == 28);
    
    u64 many_args_function_call = varargs_sum_u32(8, 1, 2, 3, 4, 5, 6, 7, varargs_sum_u32(7, 100, 200, 300, 400, 500, 600, 700));
    assert(many_args_function_call == 28 + 28 * 100);
    
    {    
        struct v2 *a = &(struct v2){};
        assert(a->x == 0);
        assert(a->y == 0);
        
        
        struct v2 *b = &(struct v2){1, 2};
        assert(b->x == 1);
        assert(b->y == 2);
    }
    
    assert(global_initialized_u32 == 42);
    assert(global_initialized_v3.x == 1 && global_initialized_v3.y == 2 && global_initialized_v3.z == 3);
    
    struct {struct v3 a; struct v2 b;} i_contain_v3 = {{1,2,3}, {1, 2}};
    assert(i_contain_v3.a.x == 1 && i_contain_v3.a.y == 2 && i_contain_v3.a.z == 3);
    assert(i_contain_v3.b.x == 1 && i_contain_v3.b.y == 2);
    
    struct {struct v3 a; struct v2 b;} i_contain_v3_2 = {.a.x = 1, .a.z = 3, .b.y = i_contain_v3.a.x};
    assert(i_contain_v3_2.a.x == 1 && i_contain_v3_2.a.z == 3 && i_contain_v3_2.b.y == 1);
    
    static void my_awsome_local_function(int a){
        assert(a == 2);
    }
    void (*my_awesome_function_ptr)(int a); 
    my_awesome_function_ptr = my_awsome_function;
    my_awesome_function_ptr(1);
    my_awesome_function_ptr = my_awsome_local_function;
    my_awesome_function_ptr(2);
    
    static u32 im_a_static_do_you_know_where_i_live;
    u32 *yes_i_know_where_you_live = &im_a_static_do_you_know_where_i_live;
    *yes_i_know_where_you_live = 123;
    assert(im_a_static_do_you_know_where_i_live == 123);
    
    {
        char c = 1;
        short s = 1;
        int i = 1;
        long long ll = 1;
        
        unsigned char uc = 1;
        unsigned short us = 1;
        unsigned int ui = 1;
        unsigned long long ull = 1;
        
        float f;
        assert((f = c) == 1.0f);
        assert((f = s) == 1.0f);
        assert((f = i) == 1.0f);
        assert((f = ll) == 1.0f);
        assert((f = uc) == 1.0f);
        assert((f = us) == 1.0f);
        assert((f = ui) == 1.0f);
        assert((f = ull) == 1.0f);
        
        double d;
        assert((d = c) == 1.0);
        assert((d = s) == 1.0);
        assert((d = i) == 1.0);
        assert((d = ll) == 1.0);
        assert((d = uc) == 1.0);
        assert((d = us) == 1.0);
        assert((d = ui) == 1.0);
        assert((d = ull) == 1.0);
        
        assert((c = f) == 1);
        assert((s = f) == 1);
        assert((i = f) == 1);
        assert((ll = f) == 1);
        assert((uc = f) == 1);
        assert((us = f) == 1);
        assert((ui = f) == 1);
        assert((ull = f) == 1);
        
        assert((c = d) == 1);
        assert((s = d) == 1);
        assert((i = d) == 1);
        assert((ll = d) == 1);
        assert((uc = d) == 1);
        assert((us = d) == 1);
        assert((ui = d) == 1);
        assert((ull = d) == 1);
        
        int a = (int)1.4f;
        assert(a == 1);
        
        int b = (int)1.4;
        assert(b == 1);
        
        signed long long bits = 215328880000001312;
        d = (double)bits;
        assert(d == 215328880000001312.0);
    }    
    
    {
        float a = 0.f;
        float b = -0.f;
        assert(a == 0.f);
        assert(b == 0.f);
        assert(a == -0.f);
        assert(b == -0.f);
        assert(a == b);
        assert(0.f == -0.f);
        assert(*(u32 *)&a != *(u32 *)&b);
    }
    
    {
        double a = 0.0;
        double b = -0.0;
        assert(a == 0.0);
        assert(b == 0.0);
        assert(a == -0.0);
        assert(b == -0.0);
        assert(a == b);
        assert(0.0 == -0.0);
        assert(*(u64 *)&a != *(u64 *)&b);
    }
    
#define hlc_decorate(a) hlc_##a
    
    u32 hlc_decorate(asd) = 12;
    assert(hlc_asd == 12);
    
    {
        static struct{
            u64 a;
            u64 b;
        } array_of_structs[10] = {{1, 2}, {2, 2}};
        
        struct{
            u64 a;
            u64 b;
        } asd = array_of_structs[0];
        assert(asd.a == 1 && asd.b == 2);
        assert(array_of_structs[1].a == 2 && array_of_structs[1].b == 2);
        assert(array_of_structs[2].a == 0 && array_of_structs[2].b == 0);
    }
    
    
    enum my_enum{
        MY_ENUM_zero,
        MY_ENUM_one,
        MY_ENUM_two,
        MY_ENUM_twelve = 12,
    } enum_variable = MY_ENUM_one;
    assert(enum_variable == 1);
    
    enum_variable = hlc_asd;
    assert(enum_variable == MY_ENUM_twelve);
    u64 enum_to_u64 = enum_variable;
    assert(enum_to_u64 == 12);
    
    {
        // const propagated
        0 && assert(false);
        1 || assert(false);
        
        // dynamic
        zero && assert(false);
        one  || assert(false);
    }
    
    {
        u32 *ptr = (u32 *)1337;
        assert(ptr++ == (u32 *)1337);
        assert(ptr-- == (u32 *)1341);
        assert(ptr   == (u32 *)1337);
        
        assert(++ptr == (u32 *)1341);
        assert(--ptr == (u32 *)1337);
        assert(ptr   == (u32 *)1337);
    }
    
    {   // test that 'for(;;)' works
        b32 did_something = false;
        for(;;){
            did_something = true;
            break;
        }
        assert(did_something);
    }
    
    static void i_take_the_float_argument_one(float one){
        assert(one == 1.0f);
    }
    
    i_take_the_float_argument_one(1.0f);
    
    static void i_take_the_double_argument_one(double one){
        assert(one == 1.0);
    }
    
    i_take_the_double_argument_one(1.0);
    
#if 1
#elif 0
#elif 1
    assert(false);
#else
    assert(false);
#endif
    
    struct contains_a_member_function struct_that_contains_a_member_function;
    static u32 the_member_function(struct contains_a_member_function s, u32 depth){
        if(depth < 10){
            return s.member_function(s, depth + 1);
        }
        return 1;
    }
    struct_that_contains_a_member_function.member_function = the_member_function;
    
    assert(the_member_function(struct_that_contains_a_member_function, 0));
    
    {
#define pair(type_a, type_b) struct { type_a first; type_b second; }
        pair(int, int) a = { .first = 1, .second = 3 };
        pair(int, int) b = a;
        assert(b.first == 1 && b.second == 3);
    }
    
    {
        struct five_byte_struture a = {{1, 2, 3, 4, 5}};
        i_have_a_5_byte_argument(a);
    }
    
    struct contains_a_bitfield bitfield;
    bitfield.a = 1;
    bitfield.b = 1;
    bitfield.c = 13;
    bitfield.d = 52;
    bitfield.e = 13371337;
    assert(bitfield.a == -1); 
    assert(bitfield.b == -1);
    assert(bitfield.c == -3);
    assert(bitfield.d == -0b1100);
    assert(bitfield.e == 13371337);
    
    
    struct contains_a_bitfield bitfield_with_initializer_list = {
        1, 0, 6, -0b1100, 13371337,
    };
    
    assert(bitfield_with_initializer_list.a == -1); 
    assert(bitfield_with_initializer_list.b == 0);
    assert(bitfield_with_initializer_list.c == 6);
    assert(bitfield_with_initializer_list.d == -12);
    assert(bitfield_with_initializer_list.e == 13371337);
    
    
    {
        char *char_pointer = null;
        char_pointer += 5;
        assert(char_pointer == (char *)5);
        char_pointer -= 1;
        assert(char_pointer == (char *)4);
        
        int *int_pointer = null;
        int_pointer += 5;
        assert(int_pointer == (int *)20);
        int_pointer -= 1;
        assert(int_pointer == (int *)16);
    }
    
    
    {
        unsigned int val = factorial(10);
        assert(val == 3628800); 
    }
    
    {
        int to_shadow = 666;
        {
            int to_shadow = 1337;
            assert(to_shadow == 1337);
        }
        assert(to_shadow == 666);
    }
    
    {
        int array[4];
        assert(sizeof(array) == 16);
        assert(sizeof(&array) == 8);
        assert(sizeof(*(&array)) == 16);
        assert(sizeof(*array) == 4);
        assert(sizeof(&(*array)) == 8);
        assert(sizeof(*(&(*array))) == 4);
    }
    
    {
        int double_array[2][1];
        assert(array_count(double_array) == 2);
        assert(array_count(double_array[0]) == 1);
        
        int tripple_array[3][2][1];
        assert(array_count(tripple_array) == 3);
        assert(array_count(*tripple_array) == 2);
        assert(array_count(**tripple_array) == 1);
        
        int quad_array[1][2][3][4];
        assert(array_count(quad_array) == 1);
        assert(array_count(quad_array[0]) == 2);
        assert(array_count(*quad_array[0]) == 3);
        assert(array_count((*quad_array[0])[0]) == 4);
    }
    
    {   // test 'integer + pointer'
        int *pointer = (int *)0x1337;
        int  integer = 0x1000;
        assert(pointer + integer == integer + pointer);
    }
    
    {   // conditional expressions
        int easy = one < two ? 1337 : 420;
        assert(easy == 1337);
        
        int medium = one > two ? one, two : 69;
        assert(medium == 69);
        
        int hard_one = one < two ? one ? 666 : two : one < two ? 420 : 69;
        assert(hard_one == 666);
        
        int hard_too = one > two ? one ? one : two : one < two ? 420 : 69;
        assert(hard_too == 420);
    }
    
    
    //
    // Maybe not the right spot for these, but here are some bug repro generated by generating expressions
    //
    {
        // negating double was negating float
        double var_3 = (-172704380.13);
        double expr_11 = (-(var_3));
        assert(expr_11 == 172704380.13);
    }
    
    {
        // casting u64 to float was casting u64 to double
        unsigned long long var_7 = (3323963600ui64);
        float var_15 = (1179495698.13f);
        float expr_2 = (var_15 = (var_7));
        assert(expr_2 == 3323963648.000000);
    }
    
    {
        assert((float)0x80ui8 ==  128.f);
        assert((float)0x80i8  == -128.f);
        assert((float)0x8000ui16 ==  32768.f);
        assert((float)0x8000i16  == -32768.f);
        assert((float)0x80000000ui32 ==  2147483648.f);
        assert((float)0x80000000i32  == -2147483648.f);
        assert((float)0x8000000000000000ui64 ==  9223372036854775808.f);
        assert((float)0x8000000000000000i64  == -9223372036854775808.f);
        
        assert((double)0x80ui8 ==  128.0);
        assert((double)0x80i8  == -128.0);
        assert((double)0x8000ui16 ==  32768.0);
        assert((double)0x8000i16  == -32768.0);
        assert((double)0x80000000ui32 ==  2147483648.0);
        assert((double)0x80000000i32  == -2147483648.0);
        assert((double)0x8000000000000000ui64 ==  9223372036854775808.0);
        assert((double)0x8000000000000000i64  == -9223372036854775808.0);
        
        u8 _u8 = 0x80ui8;
        s8 _s8 = 0x80i8;
        u16 _u16 = 0x8000ui16;
        s16 _s16 = 0x8000i16;
        u32 _u32 = 0x80000000ui32;
        s32 _s32 = 0x80000000i32;
        u64 _u64 = 0x8000000000000000ui64;
        s64 _s64 = 0x8000000000000000i64;
        
        float f;
        f = _u8;
        assert(f ==  128.f);
        f = _s8;
        assert(f == -128.f);
        f = _u16;
        assert(f ==  32768.f);
        f = _s16;
        assert(f == -32768.f);
        f = _u32;
        assert(f ==  2147483648.f);
        f = _s32;
        assert(f == -2147483648.f);
        f = _u64;
        assert(f ==  9223372036854775808.f);
        f = _s64;
        assert(f == -9223372036854775808.f);
        
        double d;
        d = _u8;
        assert(d ==  128.0);
        d = _s8;
        assert(d == -128.0);
        d = _u16;
        assert(d ==  32768.0);
        d = _s16;
        assert(d == -32768.0);
        d = _u32;
        assert(d ==  2147483648.0);
        d = _s32;
        assert(d == -2147483648.0);
        d = _u64;
        assert(d ==  9223372036854775808.0);
        d = _s64;
        assert(d == -9223372036854775808.0);
    }
    
    
    {
        double var_00 = (-1183312983.13);
        float var_08 = (-282911132.13f);
        var_08 -= var_00;
        // print("var_08: %f\n", var_08); @cleanup: what was this supposed to test?
    }
    
    
    {
        char a = -1;
        int b = 0x100;
        a %= b; // this should not crash, which is the worry here!
        assert(a == -1);
    }
    
    
    {
        int i = 0x4000000;
        i += 1.0f;
        assert(i == 0x4000000);
        
        
        float f = (float)i;
        assert(f == 67108864.f);
        f += 1;
        assert(f == 67108864.f);
        
        i = 1;
        i += f;
        assert(i == 67108864);
    }
    
    {
        int i = 1000;
        float f = 1.234f;
        
        i *= f;
        assert(i == 1234);
    }
    
    {
        unsigned long long var_00 = 0x10000;
        short expr = 1;
        expr /= var_00;
        
        assert(expr == 0);
    }
    
    {
        unsigned long long var_00 = 0x10100;
        short expr = -1;
        expr /= var_00;
        
        assert(expr == -256);
    }
    
    {
        double var_01 = 3715022438.0;
        unsigned int result = var_01;
        
        assert(result == 3715022438);
    }
    
    {
        static int other_array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        static int *array[1] = {&other_array[9]}; // this used to be out of bounds
        
        assert(*array[0] == 10);
    }
    
    {
        int *int_pointer = 0;
        float *float_pointer = 0;
        
        assert(int_pointer == float_pointer);
        
        float_pointer = (float *)1;
        
        assert(int_pointer != float_pointer);
    }
    
    {
        // @note: this was bugged and subtracted from an int instead of char.
        struct{
            char a;
            char b;
            char c;
            char d;
        } characters = {0};
        characters.a -= 1;
        assert(characters.a == -1);
        assert(characters.b == 0);
        assert(characters.c == 0);
        assert(characters.d == 0);
    }
    
    {
        
        struct asd{
            unsigned char array[8];
            char b;
        };
        
        struct asd asd = {
            .b = 1,
            .array = "12345678",
        };
        assert(asd.b == 1);
        
        static struct asd static_asd = {
            .b = 1,
            .array = "12345678",
        };
        assert(static_asd.b == 1);
        
        
        static struct asd small_init = {
            "asd"
        };
        
        assert(memory_is_zero((u8 *)&small_init + 4, sizeof(small_init) - 4));
        
        char init_array[0x100] = "asd";
        
        assert(memory_is_zero(init_array + 3, sizeof(init_array) - 3));
    }
    
    {
        // @note: this broke at some point because we did not emit patches.
        
        static const char *strings[] = {
            "hello",
            "my",
            "friend",
        };
        
        assert(memcmp(strings[0], "hello", sizeof("hello")) == 0);
        assert(memcmp(strings[1], "my", sizeof("my")) == 0);
        assert(memcmp(strings[2], "friend", sizeof("friend")) == 0);
    }
    
    assert((!0x13701DE3194642FALL) == 0);
    
    
    {
        struct {
            unsigned f0 : 1;
            u64 f1 : 2;
            u64 f2 : 33;
        } unsigned_bitfield = {
            0, 
        };
        
        assert(-1 < unsigned_bitfield.f0); // This used to fail, because the 'f0' was loaded as an 'u32'.
        assert(-1 > unsigned_bitfield.f2); // On the other hand, in this case clang/gcc seem to agree on it being a u64.
        
        assert(sizeof(+unsigned_bitfield.f0) == 4);
        assert(sizeof(+unsigned_bitfield.f1) == 4);
        assert(sizeof(+unsigned_bitfield.f2) == 8);
    }
    
    {
        struct struct_that_memcopies arg1 = {
            1, 2, 3
        };
        struct struct_that_memcopies arg2 = {
            4, 5, 6
        };
        
        function_that_does_a_bunch_of_struct_memcopies(arg1, arg2);
    }
    
    {
        union float_int{
            float f;
            unsigned int i;
        };
        
        float a = 0.0f;
        float b = -a;
        
        union float_int asd = {
            .f = b,
        };
        
        assert(asd.i == 0x80000000);
        
        union double_int64{
            double f;
            unsigned __int64 i;
        };
        
        double c = 0.0;
        double d = -c;
        
        union double_int64 asd2 = {
            .f = d,
        };
        
        assert(asd2.i == 0x8000000000000000);
    }
    
    assert((1ll << 63) >> 63 == -1);
    assert((1 << 31) >> 31 == -1);
    assert((1ull << 63) >> 63 == 1);
    assert((1u << 31) >> 31 == 1);
    
    assert(sizeof(char) << 63 >> 63 == 1);
    assert(_Alignof(char) << 63 >> 63 == 1);
    
    assert(4 == sizeof(!(char)0));
    assert(4 == sizeof(!(long long)0));
    
    {
        struct {
            int asd;
        } array_of_structures[1] = {3};
        assert(array_of_structures->asd == 3);
    }
    
    {
        struct {
            char array[3];
            char should_not_be_zero;
        } contains_array_and_char = { .should_not_be_zero = 1, .array = "123", };
        assert(contains_array_and_char.should_not_be_zero == 1);
        
        static struct {
            char array[3];
            char should_not_be_zero;
        } static_contains_array_and_char = { .should_not_be_zero = 1, .array = "123", };
        assert(static_contains_array_and_char.should_not_be_zero == 1);
        
        struct {
            char go_back;
            char array[3];
            char should_not_be_zero;
        } current_object_contains_array_and_char = { .should_not_be_zero = 1, .go_back = 2, "123", };
        assert(current_object_contains_array_and_char.should_not_be_zero == 1);
        
        static struct {
            char go_back;
            char array[3];
            char should_not_be_zero;
        } current_object_static_contains_array_and_char = { .should_not_be_zero = 1, .go_back = 2, "123", };
        assert(current_object_static_contains_array_and_char.should_not_be_zero == 1);
    }
    
    {
        
        struct asd{
            int asd :8;
        } asd;
        
        (int)asd.asd; // used to not compile.
        
        int asd2 = asd.asd ? asd.asd : 1; // used to not compile.
        
        struct asd2{
            int asd :8, :0, asd2 :10;
        } asd2;
    }
    
    assert((int){7} == 7);
    assert((int){} == 0); // This is an extension.
    
    assert(sizeof(global_array_of_unknown_size_initialized_with_wide_characters) == 12);
    assert(memcmp(global_array_of_unknown_size_initialized_with_wide_characters, 
            (u8[]){'H', 0, 'e', 0, 'l', 0, 'l', 0, 'o', 0, 0, 0}, 12) == 0);
    
    {
        unsigned short local_array_of_unknown_size_initialized_with_wide_characters[] = L"Hello";
        assert(sizeof(local_array_of_unknown_size_initialized_with_wide_characters) == 12);
        assert(memcmp(local_array_of_unknown_size_initialized_with_wide_characters, 
                (u8[]){'H', 0, 'e', 0, 'l', 0, 'l', 0, 'o', 0, 0, 0}, 12) == 0);
    }
    
    {
        static unsigned short static_array_of_unknown_size_initialized_with_wide_characters[] = L"Hello";
        assert(sizeof(static_array_of_unknown_size_initialized_with_wide_characters) == 12);
        assert(memcmp(static_array_of_unknown_size_initialized_with_wide_characters, 
                (u8[]){'H', 0, 'e', 0, 'l', 0, 'l', 0, 'o', 0, 0, 0}, 12) == 0);
    }
    
    
    assert(memcmp(string_literal_with_backslash, "Hel        lo", sizeof("Hel        lo")) == 0);
    
    return 0;
}


char string_literal_with_backslash[] = "Hel\
        lo";
        

struct struct_that_memcopies{
    u32 a;
    u32 b;
    u32 c;
};

void function_that_does_a_bunch_of_struct_memcopies(struct struct_that_memcopies arg1, struct struct_that_memcopies arg2){
    static struct struct_that_memcopies static1, static2;
    
    struct struct_that_memcopies local1 = {0}, local2 = {0};
    
    arg1 = local1 = static1 = static2 = local2 = arg2 = arg1 = local2 = static2 = arg1;
}

int memory_is_zero(void *_memory, u64 size){
    u8 *memory = _memory;
    for(smm i = 0; i < size; i++){
        if(memory[i] != 0) return false;
    }
    return true;
}


int memcmp(void *_string1, void *_string2, u64 amount){
    
    if(amount == 0) return 0;
    
    char *string1 = _string1;
    char *string2 = _string2;
    
    while(--amount && *string1 == *string2){
        string1 += 1;
        string2 += 1;
    }
    
    return *(unsigned char *)string1 - *(unsigned char *)string2;
}

static unsigned int factorial(unsigned int a){
    if(a == 0) return 1;
    return a * factorial(a - 1);
}


struct five_byte_struture{
    char a[5];
};

char i_have_a_5_byte_argument(struct five_byte_struture a){
    return a.a[1];
}


struct contains_a_member_function{
    u32 (*member_function)(struct contains_a_member_function context, u32 depth);
};


struct predefined_type_that_never_gets_filled_in;
typedef struct predefined_type_that_never_gets_filled_in typedef_of_type_that_never_gets_filled_in;


void function_that_uses_a_pointer_to_a_type_that_never_gets_filled_in(struct predefined_type_that_never_gets_filled_in *type);


void my_awsome_function(int a){
    assert(a == 1);
}

u32 global_initialized_u32 = 42;
struct v2{ int x; int y; };
struct v3{ int x; int y; int z;};
struct v3 global_initialized_v3 = {1, 2, 3};
typedef char *va_list;



b32 cstring_match(char *a, char *b){
    while(*a && *b){
        if(*a++ != *b++) return false;
    }
    
    return (*a == *b);
}
