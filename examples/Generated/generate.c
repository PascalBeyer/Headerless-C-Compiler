

// gnerates a 'large' program that I used to check that integer operations
// work as the spec intends (by cross checking what MSVC outputs).
// To increase or decrease the size you can play around with this number
#define AMOUNT_PER_ASSERT 400
// 400  ~ 175k LOC
// 2300 ~ 1M   LOC

#include "std.c"
#include "windows.c"

typedef s64 d64;

func void scope_open(struct txt_context *context){
   txt_print(context, "{");
   context->indentation_level++;
}

func void scope_close(struct txt_context *context){
   context->indentation_level--;
   txt_print(context, "}");
}

int main(int argc, char **argv){
    (void)argc;
    (void)argv;
    
    struct memory_arena scratch = zero_struct;
    struct txt_context *context = &(struct txt_context)zero_struct;
    context->scratch = &scratch;
    
    
    
    txt_print(context, "\n\n#include \"generated_include.c\"");
    txt_print(context, "s64 errors;");
    
    smm function_id = 0;
    
    smm seed = __rdtsc();
    struct random_series *series = &(struct random_series)zero_struct;
    series->val = seed;
    
#define assert_op(op, amount) \
    for(smm i = 0; i < amount; i++){ \
        smm first = (smm)((u32)rand(series) & 0xffffffff); \
        smm second = (smm)((u32)rand(series) & 0xffffffff); \
        txt_print(context, "assert((%lldll " #op " %lldll) == %lld);", first, second, (smm)(first op second)); \
    }
    
#define assert_op_shift(op, amount) \
    for(smm i = 0; i < amount; i++){ \
        smm first = rand(series); \
        smm second = rand(series) & 63; \
        txt_print(context, "assert((%lld " #op " %lld) == %lld);", first, second, (smm)(first op second)); \
    }
    
    
#define assert_op_mod(amount) \
    for(smm i = 0; i < amount; i++){ \
        smm first = rand(series); \
        smm second = rand(series); \
        txt_print(context, "assert((%lld %%  %lld) == %lld);", first, second, (smm)(first % second)); \
    }
    smm amount = AMOUNT_PER_ASSERT;
    
#if 0
    assert_op(+);
    assert_op(-);
    assert_op(*);
    assert_op_mod(1);
    assert_op(/);
    assert_op(^);
    assert_op(|);
    assert_op(&);
    
    assert_op(>);
    assert_op(<);
    assert_op(>=);
    assert_op(<=);
    assert_op(==);
    assert_op(!=);
    
    assert_op(&&);
    assert_op(||);
    
    assert_op_shift(>>);
    assert_op_shift(<<);
#endif
    
#define assert_op2(type1, type2, a, op) \
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type1 first = (type1)rand(series);\
            type2 second = (type2)rand(series);\
            txt_print(context, #type1 " first = %ll" #a ";", (a##64)first);\
            txt_print(context, #type2 " second = %ll" #a ";", (a##64)second);\
            txt_print(context, "assert((first " #op " second) == %ll" #a ");", (a##64)(first op second)); \
        }scope_close(context);\
    }
    
#define non_static_op(op) \
    assert_op2(u8,  u8, u, op);\
    assert_op2(u16, u8, u, op);\
    assert_op2(u32, u8, u, op);\
    assert_op2(u64, u8, u, op);\
    assert_op2(s8,  u8, d, op);\
    assert_op2(s16, u8, d, op);\
    assert_op2(s32, u8, d, op);\
    assert_op2(s64, u8, d, op);\
    assert_op2(u8,  u16, u, op);\
    assert_op2(u16, u16, u, op);\
    assert_op2(u32, u16, u, op);\
    assert_op2(u64, u16, u, op);\
    assert_op2(s8,  u16, d, op);\
    assert_op2(s16, u16, d, op);\
    assert_op2(s32, u16, d, op);\
    assert_op2(s64, u16, d, op);\
    assert_op2(u8,  u32, u, op);\
    assert_op2(u16, u32, u, op);\
    assert_op2(u32, u32, u, op);\
    assert_op2(u64, u32, u, op);\
    assert_op2(s8,  u32, d, op);\
    assert_op2(s16, u32, d, op);\
    assert_op2(s32, u32, d, op);\
    assert_op2(s64, u32, d, op);\
    assert_op2(u8,  u64, u, op);\
    assert_op2(u16, u64, u, op);\
    assert_op2(u32, u64, u, op);\
    assert_op2(u64, u64, u, op);\
    assert_op2(s8,  u64, d, op);\
    assert_op2(s16, u64, d, op);\
    assert_op2(s32, u64, d, op);\
    assert_op2(s64, u64, d, op);\
    assert_op2(u8,  s8, u, op);\
    assert_op2(u16, s8, u, op);\
    assert_op2(u32, s8, u, op);\
    assert_op2(u64, s8, u, op);\
    assert_op2(s8,  s8, d, op);\
    assert_op2(s16, s8, d, op);\
    assert_op2(s32, s8, d, op);\
    assert_op2(s64, s8, d, op);\
    assert_op2(u8,  s16, u, op);\
    assert_op2(u16, s16, u, op);\
    assert_op2(u32, s16, u, op);\
    assert_op2(u64, s16, u, op);\
    assert_op2(s8,  s16, d, op);\
    assert_op2(s16, s16, d, op);\
    assert_op2(s32, s16, d, op);\
    assert_op2(s64, s16, d, op);\
    assert_op2(u8,  s32, u, op);\
    assert_op2(u16, s32, u, op);\
    assert_op2(u32, s32, u, op);\
    assert_op2(u64, s32, u, op);\
    assert_op2(s8,  s32, d, op);\
    assert_op2(s16, s32, d, op);\
    assert_op2(s32, s32, d, op);\
    assert_op2(s64, s32, d, op);\
    assert_op2(u8,  s64, u, op);\
    assert_op2(u16, s64, u, op);\
    assert_op2(u32, s64, u, op);\
    assert_op2(u64, s64, u, op);\
    assert_op2(s8,  s64, d, op);\
    assert_op2(s16, s64, d, op);\
    assert_op2(s32, s64, d, op);\
    assert_op2(s64, s64, d, op);\
    
#define assert_op2_mod(type, a, op) \
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type first = (type)rand(series);\
            type second = (type)rand(series);\
            second = second ? second : 1;\
            txt_print(context, #type " first = %ll" #a ";", (a##64)first);\
            txt_print(context, #type " second = %ll" #a ";", (a##64)second);\
            txt_print(context, "assert((first %s second) == %ll" #a ");", #op, (a##64)(first op second)); \
        }scope_close(context);\
    }
    
#define non_static_op_mod(op) \
    assert_op2_mod(u8, u, op);\
    assert_op2_mod(u16, u, op);\
    assert_op2_mod(u32, u, op);\
    assert_op2_mod(u64, u, op);\
    assert_op2_mod(s8, d, op);\
    assert_op2_mod(s16, d, op);\
    assert_op2_mod(s32, d, op);\
    assert_op2_mod(s64, d, op);\
    
    
#define assert_op3(type, a, op) \
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type first = (type)rand(series);\
            type second = (type)rand(series);\
            txt_print(context, #type " first = %ll" #a ";", (a##64)first);\
            txt_print(context, "assert((first " #op " %ll" #a ") == %ll" #a ");", (a##64)second, (a##64)(first op second)); \
        }scope_close(context);\
    }
    
#define non_static_lit_op(op) \
    assert_op3(u8, u, op);\
    assert_op3(u16, u, op);\
    assert_op3(u32, u, op);\
    assert_op3(u64, u, op);\
    assert_op3(s8, d, op);\
    assert_op3(s16, d, op);\
    assert_op3(s32, d, op);\
    assert_op3(s64, d, op);\
    
#define assert_op3_mod(type, a, op) \
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type first = (type)rand(series);\
            type second = (type)rand(series);\
            second = second ? second : 1;\
            txt_print(context, #type " first = %ll" #a ";", (a##64)first);\
            txt_print(context, "assert((first %s %ll" #a ") == %ll" #a ");", #op, (a##64)second, (a##64)(first op second)); \
        }scope_close(context);\
    }
    
#define non_static_lit_op_mod(op) \
    assert_op3_mod(u8, u, op);\
    assert_op3_mod(u16, u, op);\
    assert_op3_mod(u32, u, op);\
    assert_op3_mod(u64, u, op);\
    assert_op3_mod(s8, d, op);\
    assert_op3_mod(s16, d, op);\
    assert_op3_mod(s32, d, op);\
    assert_op3_mod(s64, d, op);\
    
#define assert_op3_shift(type, a, op) \
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type first = (type)rand(series);\
            u8 second = (u8)(rand(series) & (sizeof(type)*8 - 1)) ;\
            txt_print(context, #type " first = %ll" #a ";", (a##64)first);\
            txt_print(context, "assert((first " #op " %ll" #a ") == %ll" #a ");", (a##64)second, (a##64)(first op second)); \
        }scope_close(context);\
    }
    
#define non_static_lit_op_shift(op) \
    assert_op3_shift(u8, u, op);\
    assert_op3_shift(u16, u, op);\
    assert_op3_shift(u32, u, op);\
    assert_op3_shift(u64, u, op);\
    assert_op3_shift(s8, d, op);\
    assert_op3_shift(s16, d, op);\
    assert_op3_shift(s32, d, op);\
    assert_op3_shift(s64, d, op);\
    
    non_static_op(+);
#if 0
    non_static_op(-);
    non_static_op(*);
    non_static_op_mod(%);
    non_static_op_mod(/);
    non_static_op(^);
    non_static_op(|);
    non_static_op(&);
    non_static_op(>>);
    non_static_op(<<);
    non_static_op(>);
    non_static_op(<);
    
    non_static_op(>=);
    non_static_op(<=);
    non_static_op(==);
    non_static_op(!=);
    non_static_op(&&);
    non_static_op(||);
    
    non_static_op(+=);
    non_static_op(-=);
    non_static_op(*=);
    non_static_op_mod(%=);
    non_static_op_mod(/=);
    non_static_op(^=);
    non_static_op(|=);
    non_static_op(&=);
    non_static_op(>>=);
    non_static_op(<<=);
    non_static_op(>=);
    non_static_op(<=);
    
    
    non_static_lit_op(+);
    non_static_lit_op(-);
    non_static_lit_op(*);
    non_static_lit_op_mod(%);
    non_static_lit_op_mod(/);
    non_static_lit_op(^);
    non_static_lit_op(|);
    non_static_lit_op(&);
    non_static_lit_op_shift(>>);
    non_static_lit_op_shift(<<);
    non_static_lit_op(>);
    non_static_lit_op(<);
    
    non_static_lit_op(>=);
    non_static_lit_op(<=);
    non_static_lit_op(==);
    non_static_lit_op(!=);
    non_static_lit_op(&&);
    non_static_lit_op(||);
    
    non_static_lit_op(+=);
    non_static_lit_op(-=);
    non_static_lit_op(*=);
    non_static_lit_op_mod(%=);
    non_static_lit_op_mod(/=);
    non_static_lit_op(^=);
    non_static_lit_op(|=);
    non_static_lit_op(&=);
    non_static_lit_op_shift(>>=);
    non_static_lit_op_shift(<<=);
    non_static_lit_op(>=);
    non_static_lit_op(<=);
    
    
#define assert_2_op(type, a, op1, op2)\
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type first = (type)rand(series);\
            type second = (type)rand(series);\
            type third = (type)rand(series);\
            txt_print(context, #type " first = %ll" #a ";", (a##64)first);\
            txt_print(context, #type " second = %ll" #a ";", (a##64)second);\
            txt_print(context, #type " third = %ll" #a ";", (a##64)third);\
            txt_print(context, "assert((" #a "64)(first %s (second %s third)) == %ll" #a ");", #op1, #op2,(a##64)(first op1 (second op2 third))); \
        }scope_close(context);\
    }
    
#define assert_2(op1, op2)\
    assert_2_op(u8, u, op1, op2)\
    assert_2_op(u16, u, op1, op2)\
    assert_2_op(u32, u, op1, op2)\
    assert_2_op(u64, u, op1, op2)\
    assert_2_op(s8, d, op1, op2)\
    assert_2_op(s16, d, op1, op2)\
    assert_2_op(s32, d, op1, op2)\
    assert_2_op(s64, d, op1, op2)\
    
    assert_2(+, +);
    assert_2(+, -);
    assert_2(+, *);
    assert_2(+, |);
    assert_2(+, ^);
    assert_2(+, &);
    
    assert_2(-, +);
    assert_2(-, -);
    assert_2(-, *);
    assert_2(-, |);
    assert_2(-, ^);
    assert_2(-, &);
    
    
    assert_2(*, +);
    assert_2(*, -);
    assert_2(*, *);
    assert_2(*, |);
    assert_2(*, ^);
    assert_2(*, &);
    
    
    assert_2(|, +);
    assert_2(|, -);
    assert_2(|, *);
    assert_2(|, |);
    assert_2(|, ^);
    assert_2(|, &);
    
    assert_2(^, +);
    assert_2(^, -);
    assert_2(^, *);
    assert_2(^, |);
    assert_2(^, ^);
    assert_2(^, &);
    
    assert_2(&, +);
    assert_2(&, -);
    assert_2(&, *);
    assert_2(&, |);
    assert_2(&, ^);
    assert_2(&, &);
    
    assert_2(=, +);
    assert_2(=, -);
    assert_2(=, *);
    assert_2(=, |);
    assert_2(=, ^);
    assert_2(=, &);
    
    assert_2(+=, +);
    assert_2(+=, -);
    assert_2(+=, *);
    assert_2(+=, |);
    assert_2(+=, ^);
    assert_2(+=, &);
    
    assert_2(-=, +);
    assert_2(-=, -);
    assert_2(-=, *);
    assert_2(-=, |);
    assert_2(-=, ^);
    assert_2(-=, &);
    
    assert_2(*=, +);
    assert_2(*=, -);
    assert_2(*=, *);
    assert_2(*=, |);
    assert_2(*=, ^);
    assert_2(*=, &);
    
    assert_2(|=, +);
    assert_2(|=, -);
    assert_2(|=, *);
    assert_2(|=, |);
    assert_2(|=, ^);
    assert_2(|=, &);
    
    assert_2(^=, +);
    assert_2(^=, -);
    assert_2(^=, *);
    assert_2(^=, |);
    assert_2(^=, ^);
    assert_2(^=, &);
    
    assert_2(&=, +);
    assert_2(&=, -);
    assert_2(&=, *);
    assert_2(&=, |);
    assert_2(&=, ^);
    assert_2(&=, &);
    
    
    assert_2(=, =);
    assert_2(=, +=);
    assert_2(=, -=);
    assert_2(=, *=);
    assert_2(=, |=);
    assert_2(=, ^=);
    assert_2(=, &=);
    
    assert_2(+=, =);
    assert_2(+=, +=);
    assert_2(+=, -=);
    assert_2(+=, *=);
    assert_2(+=, |=);
    assert_2(+=, ^=);
    assert_2(+=, &=);
    
    assert_2(-=, =);
    assert_2(-=, +=);
    assert_2(-=, -=);
    assert_2(-=, *=);
    assert_2(-=, |=);
    assert_2(-=, ^=);
    assert_2(-=, &=);
    
    assert_2(*=, =);
    assert_2(*=, +=);
    assert_2(*=, -=);
    assert_2(*=, *=);
    assert_2(*=, |=);
    assert_2(*=, ^=);
    assert_2(*=, &=);
    
    assert_2(|=, =);
    assert_2(|=, +=);
    assert_2(|=, -=);
    assert_2(|=, *=);
    assert_2(|=, |=);
    assert_2(|=, ^=);
    assert_2(|=, &=);
    
    assert_2(^=, =);
    assert_2(^=, +=);
    assert_2(^=, -=);
    assert_2(^=, *=);
    assert_2(^=, |=);
    assert_2(^=, ^=);
    assert_2(^=, &=);
    
    assert_2(&=, =);
    assert_2(&=, +=);
    assert_2(&=, -=);
    assert_2(&=, *=);
    assert_2(&=, |=);
    assert_2(&=, ^=);
    assert_2(&=, &=);
    
    
#define assert_2_op_logical(type, a, op1, op2)\
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            u8 first = (u8)(rand(series) & 1);\
            type second = (type)rand(series);\
            type third = (type)rand(series);\
            txt_print(context, "u8 first = %llu;", (u64)first);\
            txt_print(context, #type " second = %ll" #a ";", (a##64)second);\
            txt_print(context, #type " third = %ll" #a ";", (a##64)third);\
            txt_print(context, "assert((u64)(first %s (second %s third)) == %llu);", #op1, #op2,(u64)(first op1 (second op2 third))); \
        }scope_close(context);\
    }
    
#define assert_op_logical(op)\
    assert_2_op_logical(u8, u, +, op);\
    assert_2_op_logical(u16, u, +, op);\
    assert_2_op_logical(u32, u, +, op);\
    assert_2_op_logical(u64, u, +, op);\
    assert_2_op_logical(s8, d, +, op);\
    assert_2_op_logical(s16, d, +, op);\
    assert_2_op_logical(s32, d, +, op);\
    assert_2_op_logical(s64, d, +, op);\
    assert_2_op_logical(u8, u, -, op);\
    assert_2_op_logical(u16, u, -, op);\
    assert_2_op_logical(u32, u, -, op);\
    assert_2_op_logical(u64, u, -, op);\
    assert_2_op_logical(s8, d, -, op);\
    assert_2_op_logical(s16, d, -, op);\
    assert_2_op_logical(s32, d, -, op);\
    assert_2_op_logical(s64, d, -, op);\
    assert_2_op_logical(u8, u, *, op);\
    assert_2_op_logical(u16, u, *, op);\
    assert_2_op_logical(u32, u, *, op);\
    assert_2_op_logical(u64, u, *, op);\
    assert_2_op_logical(s8, d, *, op);\
    assert_2_op_logical(s16, d, *, op);\
    assert_2_op_logical(s32, d, *, op);\
    assert_2_op_logical(s64, d, *, op);\
    assert_2_op_logical(u8, u, >>, op);\
    assert_2_op_logical(u16, u, >>, op);\
    assert_2_op_logical(u32, u, >>, op);\
    assert_2_op_logical(u64, u, >>, op);\
    assert_2_op_logical(s8, d, >>, op);\
    assert_2_op_logical(s16, d, >>, op);\
    assert_2_op_logical(s32, d, >>, op);\
    assert_2_op_logical(s64, d, >>, op);\
    assert_2_op_logical(u8, u, <<, op);\
    assert_2_op_logical(u16, u, <<, op);\
    assert_2_op_logical(u32, u, <<, op);\
    assert_2_op_logical(u64, u, <<, op);\
    assert_2_op_logical(s8, d, <<, op);\
    assert_2_op_logical(s16, d, <<, op);\
    assert_2_op_logical(s32, d, <<, op);\
    assert_2_op_logical(s64, d, <<, op);\
    assert_2_op_logical(u8, u, |=, op);\
    assert_2_op_logical(u16, u, |=, op);\
    assert_2_op_logical(u32, u, |=, op);\
    assert_2_op_logical(u64, u, |=, op);\
    assert_2_op_logical(s8, d, |=, op);\
    assert_2_op_logical(s16, d, |=, op);\
    assert_2_op_logical(s32, d, |=, op);\
    assert_2_op_logical(s64, d, |=, op);\
    assert_2_op_logical(u8, u, *=, op);\
    assert_2_op_logical(u16, u, *=, op);\
    assert_2_op_logical(u32, u, *=, op);\
    assert_2_op_logical(u64, u, *=, op);\
    assert_2_op_logical(s8, d, *=, op);\
    assert_2_op_logical(s16, d, *=, op);\
    assert_2_op_logical(s32, d, *=, op);\
    assert_2_op_logical(s64, d, *=, op);\
    assert_2_op_logical(u8, u, ||, op);\
    assert_2_op_logical(u16, u, ||, op);\
    assert_2_op_logical(u32, u, ||, op);\
    assert_2_op_logical(u64, u, ||, op);\
    assert_2_op_logical(s8, d, ||, op);\
    assert_2_op_logical(s16, d, ||, op);\
    assert_2_op_logical(s32, d, ||, op);\
    assert_2_op_logical(s64, d, ||, op);\
    assert_2_op_logical(u8, u, &&, op);\
    assert_2_op_logical(u16, u, &&, op);\
    assert_2_op_logical(u32, u, &&, op);\
    assert_2_op_logical(u64, u, &&, op);\
    assert_2_op_logical(s8, d, &&, op);\
    assert_2_op_logical(s16, d, &&, op);\
    assert_2_op_logical(s32, d, &&, op);\
    assert_2_op_logical(s64, d, &&, op);\
    assert_2_op_logical(u8, u, +, op);\
    assert_2_op_logical(u16, u, +, op);\
    assert_2_op_logical(u32, u, +, op);\
    assert_2_op_logical(u64, u, +, op);\
    assert_2_op_logical(s8, d, +, op);\
    assert_2_op_logical(s16, d, +, op);\
    assert_2_op_logical(s32, d, +, op);\
    assert_2_op_logical(s64, d, +, op);\
    assert_2_op_logical(u8, u, -, op);\
    assert_2_op_logical(u16, u, -, op);\
    assert_2_op_logical(u32, u, -, op);\
    assert_2_op_logical(u64, u, -, op);\
    assert_2_op_logical(s8, d, -, op);\
    assert_2_op_logical(s16, d, -, op);\
    assert_2_op_logical(s32, d, -, op);\
    assert_2_op_logical(s64, d, -, op);\
    assert_2_op_logical(u8, u, *, op);\
    assert_2_op_logical(u16, u, *, op);\
    assert_2_op_logical(u32, u, *, op);\
    assert_2_op_logical(u64, u, *, op);\
    assert_2_op_logical(s8, d, *, op);\
    assert_2_op_logical(s16, d, *, op);\
    assert_2_op_logical(s32, d, *, op);\
    assert_2_op_logical(s64, d, *, op);\
    assert_2_op_logical(u8, u, >>, op);\
    assert_2_op_logical(u16, u, >>, op);\
    assert_2_op_logical(u32, u, >>, op);\
    assert_2_op_logical(u64, u, >>, op);\
    assert_2_op_logical(s8, d, >>, op);\
    assert_2_op_logical(s16, d, >>, op);\
    assert_2_op_logical(s32, d, >>, op);\
    assert_2_op_logical(s64, d, >>, op);\
    assert_2_op_logical(u8, u, <<, op);\
    assert_2_op_logical(u16, u, <<, op);\
    assert_2_op_logical(u32, u, <<, op);\
    assert_2_op_logical(u64, u, <<, op);\
    assert_2_op_logical(s8, d, <<, op);\
    assert_2_op_logical(s16, d, <<, op);\
    assert_2_op_logical(s32, d, <<, op);\
    assert_2_op_logical(s64, d, <<, op);\
    assert_2_op_logical(u8, u, |=, op);\
    assert_2_op_logical(u16, u, |=, op);\
    assert_2_op_logical(u32, u, |=, op);\
    assert_2_op_logical(u64, u, |=, op);\
    assert_2_op_logical(s8, d, |=, op);\
    assert_2_op_logical(s16, d, |=, op);\
    assert_2_op_logical(s32, d, |=, op);\
    assert_2_op_logical(s64, d, |=, op);\
    assert_2_op_logical(u8, u, *=, op);\
    assert_2_op_logical(u16, u, *=, op);\
    assert_2_op_logical(u32, u, *=, op);\
    assert_2_op_logical(u64, u, *=, op);\
    assert_2_op_logical(s8, d, *=, op);\
    assert_2_op_logical(s16, d, *=, op);\
    assert_2_op_logical(s32, d, *=, op);\
    assert_2_op_logical(s64, d, *=, op);\
    assert_2_op_logical(u8, u, ||, op);\
    assert_2_op_logical(u16, u, ||, op);\
    assert_2_op_logical(u32, u, ||, op);\
    assert_2_op_logical(u64, u, ||, op);\
    assert_2_op_logical(s8, d, ||, op);\
    assert_2_op_logical(s16, d, ||, op);\
    assert_2_op_logical(s32, d, ||, op);\
    assert_2_op_logical(s64, d, ||, op);\
    assert_2_op_logical(u8, u, &&, op);\
    assert_2_op_logical(u16, u, &&, op);\
    assert_2_op_logical(u32, u, &&, op);\
    assert_2_op_logical(u64, u, &&, op);\
    assert_2_op_logical(s8, d, &&, op);\
    assert_2_op_logical(s16, d, &&, op);\
    assert_2_op_logical(s32, d, &&, op);\
    assert_2_op_logical(s64, d, &&, op);\
    assert_2_op_logical(u8, u, !=, op);\
    assert_2_op_logical(u16, u, !=, op);\
    assert_2_op_logical(u32, u, !=, op);\
    assert_2_op_logical(u64, u, !=, op);\
    assert_2_op_logical(s8, d, !=, op);\
    assert_2_op_logical(s16, d, !=, op);\
    assert_2_op_logical(s32, d, !=, op);\
    assert_2_op_logical(s64, d, !=, op);\
    assert_2_op_logical(u8, u, <=, op);\
    assert_2_op_logical(u16, u, <=, op);\
    assert_2_op_logical(u32, u, <=, op);\
    assert_2_op_logical(u64, u, <=, op);\
    assert_2_op_logical(s8, d, <=, op);\
    assert_2_op_logical(s16, d, <=, op);\
    assert_2_op_logical(s32, d, <=, op);\
    assert_2_op_logical(s64, d, <=, op);\
    assert_2_op_logical(u8, u, >=, op);\
    assert_2_op_logical(u16, u, >=, op);\
    assert_2_op_logical(u32, u, >=, op);\
    assert_2_op_logical(u64, u, >=, op);\
    assert_2_op_logical(s8, d, >=, op);\
    assert_2_op_logical(s16, d, >=, op);\
    assert_2_op_logical(s32, d, >=, op);\
    assert_2_op_logical(s64, d, >=, op);\
    assert_2_op_logical(u8, u, <, op);\
    assert_2_op_logical(u16, u, <, op);\
    assert_2_op_logical(u32, u, <, op);\
    assert_2_op_logical(u64, u, <, op);\
    assert_2_op_logical(s8, d, <, op);\
    assert_2_op_logical(s16, d, <, op);\
    assert_2_op_logical(s32, d, <, op);\
    assert_2_op_logical(s64, d, <, op);\
    assert_2_op_logical(u8, u, >, op);\
    assert_2_op_logical(u16, u, >, op);\
    assert_2_op_logical(u32, u, >, op);\
    assert_2_op_logical(u64, u, >, op);\
    assert_2_op_logical(s8, d, >, op);\
    assert_2_op_logical(s16, d, >, op);\
    assert_2_op_logical(s32, d, >, op);\
    assert_2_op_logical(s64, d, >, op);\
    assert_2_op_logical(u8, u, ==, op);\
    assert_2_op_logical(u16, u, ==, op);\
    assert_2_op_logical(u32, u, ==, op);\
    assert_2_op_logical(u64, u, ==, op);\
    assert_2_op_logical(s8, d, ==, op);\
    assert_2_op_logical(s16, d, ==, op);\
    assert_2_op_logical(s32, d, ==, op);\
    assert_2_op_logical(s64, d, ==, op);\
    
    assert_op_logical(==);
    assert_op_logical(>=);
    assert_op_logical(<=);
    assert_op_logical(!=);
    assert_op_logical(>);
    assert_op_logical(<);
    
#define assert_type_assign(type1, a1, type2, a2)\
    for(smm i = 0; i < amount; i++){ \
        txt_print(context, "static void test_%d()", function_id++);\
        scope_open(context);{\
            type1 first = (type1) rand(series);\
            txt_print(context, #type1 " first = %ll" #a1 ";", (a1##64)first);\
            txt_print(context, #type2 " second = (" #type2 ")first;");\
            txt_print(context, "assert(second == %ll" #a2 ");", (a2##64)(type2)first); \
        }scope_close(context);\
    }
    
    assert_type_assign(u8, u, u8, u);
    assert_type_assign(u8, u, u16, u);
    assert_type_assign(u8, u, u32, u);
    assert_type_assign(u8, u, u64, u);
    
    assert_type_assign(u16, u, u8, u);
    assert_type_assign(u16, u, u16, u);
    assert_type_assign(u16, u, u32, u);
    assert_type_assign(u16, u, u64, u);
    
    assert_type_assign(u32, u, u8, u);
    assert_type_assign(u32, u, u16, u);
    assert_type_assign(u32, u, u32, u);
    assert_type_assign(u32, u, u64, u);
    
    assert_type_assign(u64, u, u8, u);
    assert_type_assign(u64, u, u16, u);
    assert_type_assign(u64, u, u32, u);
    assert_type_assign(u64, u, u64, u);
    
    
    assert_type_assign(u8, u, s8, d);
    assert_type_assign(u8, u, s16, d);
    assert_type_assign(u8, u, s32, d);
    assert_type_assign(u8, u, s64, d);
    
    assert_type_assign(u16, u, s8, d);
    assert_type_assign(u16, u, s16, d);
    assert_type_assign(u16, u, s32, d);
    assert_type_assign(u16, u, s64, d);
    
    assert_type_assign(u32, u, s8, d);
    assert_type_assign(u32, u, s16, d);
    assert_type_assign(u32, u, s32, d);
    assert_type_assign(u32, u, s64, d);
    
    assert_type_assign(u64, u, s8, d);
    assert_type_assign(u64, u, s16, d);
    assert_type_assign(u64, u, s32, d);
    assert_type_assign(u64, u, s64, d);
    
    
    assert_type_assign(s8, d, u8, u);
    assert_type_assign(s8, d, u16, u);
    assert_type_assign(s8, d, u32, u);
    assert_type_assign(s8, d, u64, u);
    
    assert_type_assign(s16, d, u8, u);
    assert_type_assign(s16, d, u16, u);
    assert_type_assign(s16, d, u32, u);
    assert_type_assign(s16, d, u64, u);
    
    assert_type_assign(s32, d, u8, u);
    assert_type_assign(s32, d, u16, u);
    assert_type_assign(s32, d, u32, u);
    assert_type_assign(s32, d, u64, u);
    
    assert_type_assign(s64, d, u8, u);
    assert_type_assign(s64, d, u16, u);
    assert_type_assign(s64, d, u32, u);
    assert_type_assign(s64, d, u64, u);
    
    assert_type_assign(s8, d, s8, d);
    assert_type_assign(s8, d, s16, d);
    assert_type_assign(s8, d, s32, d);
    assert_type_assign(s8, d, s64, d);
    
    assert_type_assign(s16, d, s8, d);
    assert_type_assign(s16, d, s16, d);
    assert_type_assign(s16, d, s32, d);
    assert_type_assign(s16, d, s64, d);
    
    assert_type_assign(s32, d, s8, d);
    assert_type_assign(s32, d, s16, d);
    assert_type_assign(s32, d, s32, d);
    assert_type_assign(s32, d, s64, d);
    
    assert_type_assign(s64, d, s8, d);
    assert_type_assign(s64, d, s16, d);
    assert_type_assign(s64, d, s32, d);
    assert_type_assign(s64, d, s64, d);
    
#if 0
    u32 maze_indices[1 << 12];
    u32 maze_size = array_count(maze_indices);
    assert(is_power_of_two(maze_size));
    for(u32 i = 0; i < maze_size; i++) maze_indices[i] = i;
    
    for(u32 i = 0; i < maze_size; i++){
        smm j = rand(series) % (maze_size - i);
        smm index = maze_indices[j];
        
        txt_print(context, "int maze_test_%d(int depth)", index);
        scope_open(context);{
            txt_print(context, "if(depth > 20) return %d;", index);
            while(rand(series) & 3){
                smm choice = rand(series) & (maze_size - 1);
                txt_print(context, "maze_test_%d(depth + 1);", choice);
            }
            txt_print(context, "return 0;");
        }scope_close(context);
        
        if(index != (maze_size - i - 1)){
            maze_indices[j] = maze_indices[maze_size - i - 1];
        }
    }
#endif
    
    txt_print(context, "int if_equals_test()");
    scope_open(context);{
        txt_print(context, "if(%llu == %lld) assert(0);", rand(series), rand(series));
        u8 first = (u8)rand(series);
        u8 second = (u8)rand(series);
        if(rand(series) & 1) second = first;
        txt_print(context, "u8 first = %u;", (u64)first);
        txt_print(context, "u8 second = %u;", (u64)second);
        
        if(first == second){
            txt_print(context, "if(first == second); else assert(false);");
        }else{
            txt_print(context, "if(first == second) assert(false);");
        }
        
        if(first == second){
            txt_print(context, "if(first != second) assert(false);");
        }else{
            txt_print(context, "if(first != second); else assert(false);");
        }
        
        txt_print(context, "return 0;");
    }scope_close(context);
    
#endif
    
    //txt_print(context, "#if __clang__");
    //txt_print(context, "int main()");
    //txt_print(context, "#else");
    txt_print(context, "int _start()");
    //txt_print(context, "#endif");
    scope_open(context);{
        txt_print(context, "print_string(\"test suite start!\\n\");");
        for(smm i = 0; i < function_id; i++){
            txt_print(context, "test_%d();", i);
        }
        //txt_print(context, "maze_test_0(0);");
        txt_print(context, "print_string(\"test suite done!\\n\");");
        txt_print(context, "return (int)errors;");
    }scope_close(context);
    
    
    struct string ret = txt_flatten(&scratch, context);
    os_write_file("generated.c", ret.data, ret.amount);
}

