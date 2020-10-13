

// A dumping place for small regression tests.
// Warnings are intended, they test implicit conversions.


#include "test_common.c"

enum enum_test{
    ENUM_zero,
    ENUM_one,
    // @cleanup:
    //ENUM_one2 = ENUM_one,
    ENUM_333 = 333,
    ENUM_334,
    ENUM_2p2 = 2 + 2,
    ENUM_5,
    ENUM_10 = 3 * 3  + 1,
    ENUM_minus_one = -1,
    ENUM_hex = 0x12345,
    ENUM_max_int = 0x7fffffff,
    ENUM_overflow = 0x7fffffff + 1, // @cleanup: why does this not print a warning?
    ENUM_way_to_large = 0xffffffffff, // @cleanup: this should warn probably?
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
    char *string_in_struct;
} global_struct_around_array = { "I'm an array", "I am a char *"};

struct{
    unsigned short _byte, _state;
}has_compound_declaration;

int _start(){
    
    print_line("starting tests");
    
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
    
    assert(ENUM_zero == 0);
    assert(ENUM_one == 1);
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
        //} anonymous_struct2 = {{1, 2, 3}}; @cleanup: both work on msvc
    } anonymous_struct2 = {1,2,3};
    assert(anonymous_struct2.a == 1 && anonymous_struct2.b == 2 && anonymous_struct2.c == 3);
    
    u32 u32_array[10] = {
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    };
    
    for(u32 i = 0; i < array_count(u32_array); i++){
        assert(i + 1 == u32_array[i]);
    }
    assert(u32_array[0] <= u32_array[2]);
    
    char array[13] = "i'm an array";
    
    assert(sizeof(array) == 13);
    assert(sizeof("i'm an array") == 13); 
    
    assert(cstring_match(array, "i'm an array"));
    
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
    assert(sizeof(global_struct_around_array) == 24);
    
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
    
    
    //struct v2{ int x; int y; }; @cleanup:?
    struct v2 i_was_intialized_to_one_two_named = {.x =(int) one, .y = (int)two};
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
    
    static u64 varargs_sum_u64(u32 amount, ...){
        va_list va;
        //((void)(__va_start(&va, amount)));
        va = (va_list)((u64 *)&amount + 1);
        
        static u64 sum_u64_va_list(u32 amount, va_list va){
            u64 sum = 0;
            for(u32 i = 0; i < amount; i++){
                sum += *(u64* ) ((va += sizeof(__int64)) - sizeof(__int64));
            }
            return sum;
        }
        
        u64 sum = sum_u64_va_list(amount, va);
        ((void)(va = (va_list)0));
        return sum;
    }
    u64 varargs_sum = varargs_sum_u64(7, 1, 2, 3, 4, 5, 6, 7);
    assert(varargs_sum == 28);
    
    u64 many_args_function_call = varargs_sum_u64(8, 1, 2, 3, 4, 5, 6, 7, varargs_sum_u64(7, 100, 200, 300, 400, 500, 600, 700));
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
    }    
    
#define pbc_decorate(a) pbc_##a
    
    u32 pbc_decorate(asd) = 12;
    assert(pbc_asd == 12);
    
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
        
        // @cleanup: why does this compile....?
        //asd = (struct{struct {u64 a; u64 b;} asd;}){{3, 2}}; 
        //assert(asd.a == 3 && asd.b == 2);
    }
    
    
    enum my_enum{
        MY_ENUM_zero,
        MY_ENUM_one,
        MY_ENUM_two,
        MY_ENUM_twelve = 12,
    } enum_variable = MY_ENUM_one;
    assert(enum_variable == 1);
    
    enum_variable = pbc_asd;
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
        struct __m128 a = {.e[0] = 0.0, .e[1] = 1.0};
        a = _mm_add_sd(a, a);
        assert(a.e[0] == 0.0 && a.e[1] == 2.0);
    }
    
#define pair(type_a, type_b) struct { type_a first; type_b second; }
    pair(int, int) a = { .first = 1, .second = 3 };
    pair(int, int) b = a;
    assert(b.first == 1 && b.second == 3);
    
    
    return 0;
}


struct __m128{
    double e[2];
};
struct __m128 _mm_add_sd(struct __m128, struct __m128);


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
