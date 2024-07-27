
#include <stdarg.h>

#ifdef __GNUC__
#include <emmintrin.h>
#endif

#if defined(__clang__) || defined(__GNUC__)


#include <stdint.h>
#include <stddef.h>

typedef int8_t s8;
typedef uint8_t u8;
typedef int16_t s16;
typedef uint16_t u16;
typedef int32_t s32;
typedef uint32_t u32;
typedef int64_t s64;
typedef uint64_t u64;

typedef uintptr_t umm;
typedef uintptr_t umem;
typedef intptr_t  smm;
typedef intptr_t  smem;


#define NO_RETURN
#define PRINTLIKE 
#define ALIGNED(n) __attribute__((aligned (n)))

#elif defined(_MSC_VER) || defined(__HLC__)

#include <intrin.h>

typedef __int8 s8;
typedef unsigned __int8 u8;
typedef __int16 s16;
typedef unsigned __int16 u16;
typedef __int32 s32;
typedef unsigned __int32 u32;
typedef __int64 s64;
typedef unsigned __int64 u64;

typedef unsigned __int64 umm;
typedef __int64 smm;

#if defined(__HLC__)
#define PRINTLIKE __declspec(printlike)
#else
#define PRINTLIKE 
#endif

#define NO_RETURN __declspec(noreturn)
#define ALIGNED(n) __declspec(align(n))

#else
#error unknown compiler
#endif

#if defined(__clang__) || defined(__HLC__)
#define zero_struct {}
#elif defined(_MSC_VER) || defined(__GNUC__)
#define zero_struct {0}
#endif


#define alignof(a) _Alignof(a)

typedef s8 b8;
typedef s16 b16;
typedef s32 b32;
typedef s64 b64;
typedef float f32;
typedef double f64;

#define true 1
#define false 0
#define null ((void *)0)
#define max_u8  (u8)(-1)
#define max_u16 (u16)(-1)
#define max_u32 (u32)(-1)
#define max_u64 (u64)(-1)

#define u8_max (u8)(-1)
#define u16_max (u16)(-1)
#define u32_max (u32)(-1)
#define u64_max (u64)(-1)

#define s8_max (+127)
#define s8_min (-128)

#define max_s8 (+127)
#define min_s8 (-128)

#define min_s16 (-32768)
#define max_s16 (+32767)

#define s16_min (-32768)
#define s16_max (+32767)

#define s32_max (+2147483647)
#define s32_min (-(s32)2147483648)

#define max_s32 (+2147483647)
#define min_s32 (-(s32)2147483648)

#define max_s64 (+9223372036854775807)
#define s64_max (+9223372036854775807)
#define min_s64 (-(s64)9223372036854775808ull)
#define s64_min (-(s64)9223372036854775808ull)

#define min_u8 (u8)(0)
#define u8_min (u8)(0)

#define min_u16 (u16)(0)
#define u16_min (u16)(0)

#define min_u32 (u32)(0)
#define u32_min (u32)(0)

#define min_u64 (u64)(0)
#define u64_min (u64)(0)

#define func static
// #define func inline

//////////////////////////////////////////////

// @cleanup: this is garbage, os_reserve doen not know commit os_commit_memory does not know reserve
//           maybe commit should take the buffer
struct os_virtual_buffer{
    union{
        u8 *memory;
        u8 *base;
        u8 *data;
    };
    smm committed;
    smm reserved;
};

struct memory_buffer{
    u8 *memory;
    smm size;
};

struct os_file{
    b32 file_does_not_exist;
    
    // time given in number 100-nanosecond intervals elapsed since 00:00 hours, Jan 1, 1970 UTC
    // I.e a more precise version of unix time stamps
    u64 access_time;
    u64 modification_time;  // @cleanup: check that these actually agree in windows and linux
    u64 creation_time;
    
    union{
        u8 *memory;
        u8 *data;
    };
    union{
        smm amount;
        u64 size;
    };
};


static struct os_virtual_buffer os_reserve_memory(void *desired_base, smm reserve_size);
static struct os_virtual_buffer os_commit_memory(void *desired_base, smm commit_size);
static void *                   os_allocate_memory(smm size){
    struct os_virtual_buffer buf = os_reserve_memory(0, size);
    buf = os_commit_memory(buf.memory, buf.reserved);
    return buf.memory;
}

static void           os_free_memory(void *memory_to_free);
static struct os_file os_load_file(char *file_name, void *buffer, smm buffer_size); // @sigh cstrings
static b32            os_write_file(char *file_name, void *buffer, smm buffer_size); // zero terminated
static NO_RETURN void os_panic(u32 exit_code);
static u32            os_print_string(char *string, smm length);
static void           os_debug_break(void);
static f64            os_get_time_in_seconds(void);


PRINTLIKE static u32 print(char *format, ...);

//////////////////////////////////////////////

#define cast(type) (type)
#define offset_in_type(type, member) (u64)(&((type *)0)->member)

#define kilo_bytes(a) ((a) * 1024ULL)
#define mega_bytes(a) ((kilo_bytes(a)) * 1024ULL)
#define giga_bytes(a) ((mega_bytes(a)) * 1024ULL)

#if defined(_Debug)

#ifdef FUZZING
#define __do_assert(file, line, expr, function) ((*((u32 *)0x1337133713371337) = 0), os_panic(0))

#else
static NO_RETURN void __do_assert(const char *file, int line, const char *expression, const char *function){
    
    print("%s(%i): ASSERT FIRED: '%s' in function %s.\n", file, line, expression, function);
    os_debug_break();
    os_panic(1337);
}
#endif

#define assert(expr) ((expr) ? 0 : (__do_assert(__FILE__,  __LINE__, #expr, __FUNCTION__), 1))

#define debug_only(a) a
#define not_implemented (__do_assert(__FILE__,  __LINE__, "not implemented!", __FUNCTION__))
#define invalid_code_path (__do_assert(__FILE__, __LINE__, "invalid code path", __FUNCTION__))
#define invalid_default_case(...) default:{ assert(!"invalid default case"); __VA_ARGS__; } break
#define prevent_usage(ident) struct { int asd; } ident; (void)ident;

#else // _Debug

#define assert(a) 0
#define debug_only(a)
#define not_implemented
#define invalid_code_path
#define invalid_default_case(...) default: __VA_ARGS__; break
#define prevent_usage(...)

#endif // _Debug


#ifdef __clang__
#define static_assert(expr) _Static_assert(expr, "")

#elif defined(__HLC__)

#define zero_struct {}
// @cleanup: I guess we should allow the above as well?
#define M_concat2__internal(a, b) a ## b
#define M_concat2(a, b) M_concat2__internal(a, b)
#define static_assert(expr) static u8 M_concat2(static_assert, __LINE__)[(expr) ? 1 : -1]

#elif defined(_MSC_VER)

#define M_concat2__internal(a, b) a ## b
#define M_concat2(a, b) M_concat2__internal(a, b)
#define static_assert(expr) static u8 M_concat2(static_assert, __LINE__)[(expr) ? 1 : -1]

#endif
//////////////////////////////////

#define array_count(a) (sizeof(a) / sizeof(*(a)))
#define is_power_of_two(a) ((a) && !((a) & ((a) - 1)))

#define align_up(to_align, to) (assert(is_power_of_two(to)), ((((to_align) - 1) | ((to) - 1)) + 1))

// #define align_up(to_align, to) (assert(to), ((to_align) - ((to_align) & ((to) - 1))) + (((to_align) & ((to) - 1)) ? (to) : 0))
#define max_of(a, b) ((a) > (b) ? (a) : (b))
#define min_of(a, b) ((a) < (b) ? (a) : (b))
#define clamp(a, min, max) min_of(max, max_of(min, a))


#define sll_is_empty(list) ((list).first == null)
#define sll_clear(list) {(list).first = null; (list).last = null;}


#define sll_push_back(list, node) \
{\
    if((list).first){\
        (list).last->next = (node);\
        (list).last = (node);\
        (node)->next = null;\
    }else{\
        (list).first = (node);\
        (list).last = (node);\
        (node)->next = null;\
    }\
}


#define sll_push_front(list, node)\
{\
    if((list).first){\
        (node)->next = (list).first;\
        (list).first = node;\
    }else{\
        (list).first = (node);\
        (list).last = (node);\
        (node)->next = null;\
    }\
}


#define sll_push_front_list(list, to_prepend)\
{\
    if((to_prepend).first){\
        (to_prepend).last->next = (list).first;\
        (list).first = (to_prepend).first;\
        if(!(list).last) (list).last = (to_prepend).last;\
    }\
}



#define sll_push_back_list(list, to_append) {\
    if((to_append).first){\
        if((list).first){\
            (list).last->next = (to_append).first;\
            (list).last = (to_append).last;\
        }else{\
            (list).first = (to_append).first;\
            (list).last = (to_append).last;\
        }\
    }\
}


#define sll_pop_front(list) {\
    if((list).first){\
        (list).first = (list).first->next;\
        if(!(list).first) (list).last = null;\
    }\
}

#define sll_remove(list, node, prev)\
{\
    if(prev){\
        assert(prev->next == node);\
        (prev)->next = (node)->next;\
    }else{\
        (list).first = (node)->next;\
    }\
    if(!(node)->next) (list).last = prev;\
}\


struct sll_sort_node{
    struct sll_sort_node *next;
    struct{
        void *first;
        void *last;
    } list;
};

#define sll_sort(list_to_sort, scratch, smaller_function_or_define)                        \
if((list_to_sort).first != (list_to_sort).last){                                           \
    \
    struct{                                                                                \
        void *first;                                                                       \
        void *last;                                                                        \
    } out = zero_struct;                                                                   \
    \
    struct{                                                                                \
        struct sll_sort_node *first;                                                       \
        struct sll_sort_node *last;                                                        \
    } stack = zero_struct;                                                                 \
    \
    struct sll_sort_node *initial_node = &(struct sll_sort_node)zero_struct;               \
    initial_node->list.first = (void *)(list_to_sort).first;                               \
    initial_node->list.last  = (void *)(list_to_sort).last;                                \
    sll_push_front(stack, initial_node);                                                   \
    \
    /* we saved the list so we can now clear it */                                         \
    sll_clear(list_to_sort);                                                               \
    \
    /* we will continouisly use '(list_to_sort)' to make things typesafe */                \
    while(!sll_is_empty(stack)){                                                           \
        struct sll_sort_node *current_node = stack.first;                                  \
        sll_pop_front(stack);                                                              \
        \
        /* if there is only one node append it to 'out'. */                                \
        if(current_node->list.first == current_node->list.last){                           \
            void *node = current_node->list.first;                                         \
            ((list_to_sort).first = node)->next = null;                                    \
            \
            if(out.first){                                                                 \
                ((list_to_sort).last = out.last)->next = node;                             \
            }else{                                                                         \
                out.first = node;                                                          \
            }                                                                              \
            out.last = node;                                                               \
            continue;                                                                      \
        }                                                                                  \
        struct{                                                                            \
            void *first;                                                                   \
            void *last;                                                                    \
        } list = zero_struct;                                                              \
        list.first = current_node->list.first;                                             \
        list.last = current_node->list.last;                                               \
        \
        /* choose the front of the list as pivot and pop it */                             \
        void *piveot = list.first;                                                         \
        list.first = ((list_to_sort).first = piveot)->next;                                \
        if(!list.first) list.last = null;                                                  \
        \
        struct{                                                                            \
            void *first;                                                                   \
            void *last;                                                                    \
        } smaller = zero_struct;                                                           \
        smm smaller_count = 0;                                                             \
        \
        struct{                                                                            \
            void *first;                                                                   \
            void *last;                                                                    \
        } bigger = zero_struct;                                                            \
        smm bigger_count = 0;                                                              \
        \
        for(void *it = list.first; it; ){                                                  \
            void *next = ((list_to_sort).first = it)->next;                                \
            if(smaller_function_or_define(((list_to_sort).first = it), ((list_to_sort).last = piveot))){ \
                if(smaller.first){                                                         \
                    ((list_to_sort).first = it)->next = smaller.first;                     \
                    smaller.first = it;                                                    \
                }else{                                                                     \
                    ((list_to_sort).first = it)->next = null;                              \
                    smaller.first = it;                                                    \
                    smaller.last  = it;                                                    \
                }                                                                          \
                smaller_count += 1;                                                        \
            }else{                                                                         \
                if(bigger.first){                                                          \
                    ((list_to_sort).first = it)->next = bigger.first;                      \
                    bigger.first = it;                                                     \
                }else{                                                                     \
                    ((list_to_sort).first = it)->next = null;                              \
                    bigger.first = it;                                                     \
                    bigger.last  = it;                                                     \
                }                                                                          \
                bigger_count += 1;                                                         \
            }                                                                              \
            it = next;                                                                     \
        }                                                                                  \
        \
        if(bigger_count == 0){                                                             \
            /* do nothing */                                                               \
        }else{                                                                             \
            struct sll_sort_node *new_node = push_struct(scratch, struct sll_sort_node);   \
            new_node->list.first = bigger.first;                                           \
            new_node->list.last  = bigger.last;                                            \
            sll_push_front(stack, new_node);                                               \
        }                                                                                  \
        \
        if(smaller_count == 0){                                                            \
            ((list_to_sort).first = piveot)->next = null;                                  \
            if(out.first){                                                                 \
                ((list_to_sort).first = out.last)->next = piveot;                          \
            }else{                                                                         \
                out.first = piveot;                                                        \
            }                                                                              \
            out.last  = piveot;                                                            \
        }else if(smaller_count == 1){                                                      \
            void *node = smaller.first;                                                    \
            ((list_to_sort).first = node)->next = piveot;                                  \
            ((list_to_sort).first = piveot)->next = null;                                  \
            \
            if(out.first){                                                                 \
                ((list_to_sort).first = out.last)->next = node;                            \
            }else{                                                                         \
                out.first = node;                                                          \
            }                                                                              \
            out.last  = piveot;                                                            \
        }else{                                                                             \
            /* @note: push 'back' here, as we want to 'rotate' through piveots */          \
            ((list_to_sort).first = piveot)->next = null;                                  \
            ((list_to_sort).first = smaller.last)->next = piveot;                          \
            smaller.last = piveot;                                                         \
            \
            struct sll_sort_node *new_node = push_struct(scratch, struct sll_sort_node);   \
            new_node->list.first = smaller.first;                                          \
            new_node->list.last  = smaller.last;                                           \
            sll_push_front(stack, new_node);                                               \
        }                                                                                  \
    }                                                                                      \
    \
    (list_to_sort).first = out.first;                                                      \
    (list_to_sort).last  = out.last;                                                       \
}


#define dll_is_empty(list) ((list).first == null)
#define dll_clear(list) {(list).first = null; (list).last = null;}


#define dll_push_back(list, node) {\
    if((list).first){\
        (node)->prev = (list).last;\
        (list).last->next = (node);\
        (list).last = (node);\
        (node)->next = null;\
    }else{\
        (list).first = (node);\
        (list).last = (node);\
        (node)->prev = null;\
        (node)->next = null;\
    }\
}

#define dll_push_back_list(list, list2) {\
    if((list2).first){\
        if((list).first){\
            (list2).first->prev = (list).last;\
            (list).last->next = (list2).first;\
            (list).last = (list2).last;\
        }else{\
            (list) = (list2);\
        }\
    }\
}


#define dll_push_front(list, node) {if((list).first){(node)->next = (list).first; (list).first->prev = (node); (list).first = (node); (node)->prev = null;} else {(list).first = (node); (list).last = (node); (node)->next = null; (node)->prev = null;}}

#define dll_remove(list, node){\
    if((list).first == (node)){\
        (list).first = (node)->next;\
        if((list).first){\
            (list).first->prev = null;\
        }else{\
            (list).last = null;\
        }\
    }else if((list).last == (node)){\
        (list).last = (node)->prev;\
        if((list).last){\
            (list).last->next = null;\
        }else{\
            (list).first = null;\
        }\
    }else{\
        (node)->prev->next = (node)->next;\
        (node)->next->prev = (node)->prev;\
    }\
}

func u8  to_u8(smm number)  { assert(u8_min  <=  (number) && (number) <= u8_max);  return (u8)(number);  }
func u16 to_u16(smm number) { assert(u16_min <=  (number) && (number) <= u16_max); return (u16)(number); }
func u32 to_u32(smm number) { assert(u32_min <=  (number) && (number) <= u32_max); return (u32)(number); }
func u64 to_u64(smm number) { assert(u64_min <=  (number) && (number) <= u64_max); return (u64)(number); }


func s8  to_s8(smm number)  { assert(s8_min  <=  (number) && (number) <= s8_max);  return (s8)(number);  }
func s16 to_s16(smm number) { assert(s16_min <=  (number) && (number) <= s16_max); return (s16)(number); }
func s32 to_s32(smm number) { assert(s32_min <=  (number) && (number) <= s32_max); return (s32)(number); }
func s64 to_s64(smm number) { assert(s64_min <=  (number) && (number) <= s64_max); return (s64)(number); }


///////////////////////////////////////////////////////////////

#if defined(__clang__) || defined(__HLC__)

extern __int64 _InterlockedExchangeAdd64(__int64 volatile * _Addend, __int64 _Value);
extern void _mm_pause();

void *memset(void *mem, int val, size_t amount){
    u8 *it = mem;
    for(size_t i = 0; i < amount; i++){
        *it++ = (u8)val;
    }
    return mem;
}

void *memcpy(void *dest, const void *source, size_t amount){
    u8 *it  = dest;
    const u8 *it2 = source;
    for(size_t i = 0; i < amount; i++){
        *it++ = *it2++;
    }
    
    return dest;
}

int memcmp(void *_string1, void *_string2, size_t amount){
    
    if(amount == 0) return 0;
    
    char *string1 = _string1;
    char *string2 = _string2;
    
    while(--amount && *string1 == *string2){
        string1 += 1;
        string2 += 1;
    }
    
    return *(unsigned char *)string1 - *(unsigned char *)string2;
}

int _fltused;


#elif defined(__GNUC__)
void *memset(void *mem, int val, size_t amount);
void *memcpy(void *dest, const void *source, size_t amount);

#elif defined(_MSC_VER)

// 
// CRT Stuff.
// 

int _fltused;

#pragma function(memset)
void *memset(void *mem, int val, size_t amount){
    
    if(!amount) return mem;
    __stosb(mem, (unsigned char)val, amount);
    return mem;
}


#pragma function(memcpy)
void *memcpy(void *dest, const void *source, size_t amount){
    if(!amount) return dest;
    __movsb(dest, source, amount);
    return dest;
}

#pragma function(memcmp)
int memcmp(void *_string1, void *_string2, size_t amount){
    
    if(amount == 0) return 0;
    
    char *string1 = _string1;
    char *string2 = _string2;
    
    while(--amount && *string1 == *string2){
        string1 += 1;
        string2 += 1;
    }
    
    return *(unsigned char *)string1 - *(unsigned char *)string2;
}

#endif




// @note: returns the initial value
func s64   atomic_add(s64 *val, s64 to_add);

func void *atomic_compare_and_swap(void *dest, void *source, void *comparand);
func smm   atomic_compare_and_swap_smm(smm *dest, smm source, smm comparand);


func b32 memory_is_zero(void *_memory, smm size){
    u8 *memory = _memory;
    for(smm i = 0; i < size; i++){
        if(memory[i] != 0) return false;
    }
    return true;
}

#if defined(__clang__) || defined(__GNUC__)

#define count_leading_zeros32 __builtin_clz
#define count_leading_zeros64 __builtin_clzll
#define count_trailing_zeros32 __builtin_ctz
#define count_trailing_zeros64 __builtin_ctzll

#elif defined(_MSC_VER)

func u32 count_leading_zeros32(u32 a){
    unsigned long ret;
    if(!_BitScanReverse(&ret, a)){
        return 32;
    }
    return 31 - ret;
}

func u32 count_leading_zeros64(u64 a){
    unsigned long ret;
    if(!_BitScanReverse64(&ret, a)){
        return 64;
    }
    return 63 - ret;
}

func u32 count_trailing_zeros32(u32 a){
    unsigned long ret;
    if(!_BitScanForward(&ret, a)){
        ret = 0;
    }
    return ret;
}

func u32 count_trailing_zeros64(u64 a){
    unsigned long ret;
    if(!_BitScanForward64(&ret, a)){
        ret = 0;
    }
    return ret;
}
#endif


///////////////////////////////////////////////

struct memory_arena{
    //
    // @note: @WARNING: All arenas are fixed size now. This is used in the tokenizer!
    //
    
    u8 *base;
    u8 *current;
    u8 *end_of_committed_memory;
    smm reserved;
    
    f32 exponential_growth_factor;// @incomplete:
    u32 constant_grow_amount;
    
    char *out_of_memory_string;
    
    debug_only(s32 temp_count;)
    debug_only(u64 committed_size;) // should be the same as (end_of_arena - arena_base)
};

func void arena_reset(struct memory_arena *arena){
    memset(arena->base, 0, arena->current - arena->base);
    arena->current = arena->base;
}

static u8 *global_dynamic_memory_base;
static smm global_dynamic_memory_reserved;
static const smm global_dynamic_memory_size = giga_bytes(1024);
static const smm default_arena_size = giga_bytes(8);

func struct memory_arena create_memory_arena(smm size, f32 exp_grow, u32 constant_grow){
    
    if(!global_dynamic_memory_base){
        // @cleanup: somehow ensure that this is only done once?
        global_dynamic_memory_base = os_reserve_memory(null, global_dynamic_memory_size).memory;
        assert(global_dynamic_memory_base);
        global_dynamic_memory_reserved = 0x1000; // start a bit into it, just if someone checks for 0 offset
    }
    
    u8 *base = global_dynamic_memory_base + atomic_add(&global_dynamic_memory_reserved, size);
    
    if(base + size > global_dynamic_memory_base + global_dynamic_memory_size){
        print("Memory error!\n");
        os_panic(1);
    }
    
    struct memory_arena ret = zero_struct;
    ret.current = base;
    ret.base    = base;
    ret.end_of_committed_memory = base;
    ret.reserved = size;
    
    ret.exponential_growth_factor = exp_grow;
    ret.constant_grow_amount      = constant_grow;
    
#if defined(_Debug)
    ret.committed_size = 0;
    ret.temp_count     = 0;
#endif
    
    return ret;
}

#define push_uninitialized_struct(arena, type) ((type *)push_struct_(arena, sizeof(type), alignof(type)))
#define push_struct(arena, type) ((type*)memset(push_uninitialized_struct(arena, type), 0, sizeof(type)))

#define push_uninitialized_data(arena, type, amount)   ((type *)push_struct_(arena, (amount) * sizeof(type), alignof(type)))
#define push_data(arena, type, amount) ((type *)memset(push_uninitialized_data(arena, type, amount), 0, sizeof(type) * (amount)))

#define push_array_copy(arena, type, array, amount) ((type *)memcpy(push_uninitialized_data(arena, type, sizeof(type) * (amount)), array, (amount) * sizeof(type)))

// @note: this _should_ take a pointer as x because then we get an error if we pass a structure. If it takes a structure and we do &x, we loose this error checking.
#define block_zero(x) memset(x, 0, sizeof(*x))
#define block_copy(source, dest) (assert(sizeof(*source) == sizeof(*dest)), memcpy(x, y, sizeof(*x)))
#define push_struct_copy(arena, type, x) ((type *)memcpy(push_struct(arena, type), (type *)x, sizeof(type)))

#define dynarray_maybe_grow(type, arena, array, amount, capacity){ \
    if(*&(amount) == *&(capacity)){                                \
        capacity <<= 1;                                            \
        type *new_array = push_uninitialized_data(arena, type, capacity); \
        memcpy(new_array, array, (amount) * sizeof(*array));       \
        array = new_array;                                         \
    }                                                              \
}

func u8 *arena_current(struct memory_arena *arena){
    
    if(!arena->reserved){
        //
        // This arena was not initialized yet. Do that!
        //
        
        *arena = create_memory_arena(default_arena_size, 2.0f, mega_bytes(1));
    }
    
    return arena->current;
}

func void push_align_initialized_to_specific_value(struct memory_arena *arena, smm alignment, u8 value){
    assert(alignment <= 0x1000); // at most page align, otherwise I would have to check more stuff here.
    assert(is_power_of_two(alignment));
    
    if((smm)arena->current & (alignment - 1)){
        u8 *aligned_up = (u8 *)align_up((smm)arena->current, alignment);
        
        memset(arena_current(arena), value, (aligned_up - arena_current(arena)));
        arena->current = aligned_up;
    }
}

func void push_zero_align(struct memory_arena *arena, smm alignment){
    push_align_initialized_to_specific_value(arena, alignment, 0);
}

func u8 *push_align(struct memory_arena *arena, u32 alignment){
    
    if(!arena->reserved){
        //
        // This arena was not initialized yet. Do that!
        //
        
        *arena = create_memory_arena(default_arena_size, 2.0f, mega_bytes(1));
    }
    
    
    assert(alignment <= 0x1000); // at most page align, otherwise I would have to check more stuff here.
    assert(is_power_of_two(alignment));
    
    u8 *ret = arena->current;
    arena->current = (u8 *)align_up((smm)arena->current, alignment);
    return ret;
}


// 
// Don't inline this routine as it should not be called a lot.
//
__declspec(noinline) func void grow_arena(struct memory_arena *arena, smm size){
    
    if(!arena->reserved){
        //
        // This arena was not initialized yet. Do that!
        //
        
        *arena = create_memory_arena(default_arena_size, 2.0f, mega_bytes(1));
    }
    
    smm current_arena_size = (smm)(arena->end_of_committed_memory - (u8 *)arena->base);
    
    //
    // Calculate the arena size specified by the growth factors
    //
    smm exponential_growth_factor = (smm)(current_arena_size * max_of(1.0f, arena->exponential_growth_factor));
    smm constant_growth_factor    = current_arena_size + max_of(arena->constant_grow_amount, mega_bytes(1));
    
    smm grow_to = max_of(exponential_growth_factor, constant_growth_factor);
    grow_to = align_up(grow_to, 0x1000);
    
    //
    // make sure 'grow_to' does not exceed the reserved memory
    //
    grow_to = min_of(grow_to, arena->reserved);
    
    //
    // make sure that at least the initial allocation fits
    //
    smm aligned_size = align_up(size, 0x1000);
    grow_to = max_of(current_arena_size + aligned_size, grow_to);
    
    if(grow_to > arena->reserved){
        char *out_of_memory_string = arena->out_of_memory_string;
        if(!out_of_memory_string) out_of_memory_string = "Error: Ran out of memory!\n";
        print("%s", out_of_memory_string);
        os_debug_break();
        os_panic(1);
    }
    
    struct os_virtual_buffer buffer = os_commit_memory(arena->base, grow_to);
    assert(buffer.base == arena->base);
    assert(buffer.committed == grow_to);
    
    arena->end_of_committed_memory = buffer.memory + buffer.committed;
}

func void *push_struct_(struct memory_arena *arena, smm size, smm alignment){
    
    assert(is_power_of_two(alignment));
    assert(alignment <= 0x1000); // align at most to a page, this is as our size should be page aligned
    
    //
    // Align up 'arena->current' by the required amount.
    //
    smm mask = alignment - 1;
    arena->current = (u8 *)(((smm)arena->current + mask) & ~mask);
    
    //
    // This should be guarantied as we align up at most by a page.
    //
    assert(arena->current <= arena->end_of_committed_memory);
    
    smm rest_size = (smm)(arena->end_of_committed_memory - arena->current);
    
    if(size > rest_size){
        grow_arena(arena, size);
    }
    
    void *ret = arena->current;
    arena->current += size;
    return ret;
}

//_____________________________________________________________________________________________________________________
// Temporary Memory

struct temporary_memory{
    struct memory_arena *arena;
    u8 *saved_current; // @WARNING: if the arena was only zero-initialized so far this will be zero
    
    debug_only(smm temp_index;)
};

func struct temporary_memory begin_temporary_memory(struct memory_arena *arena){
    struct temporary_memory ret = zero_struct;
    ret.arena = arena;
    ret.saved_current = arena_current(arena);
    debug_only(ret.temp_index = ++arena->temp_count);
    return ret;
}

func void end_temporary_memory(struct temporary_memory temp){
    assert(temp.arena->temp_count == temp.temp_index);
    debug_only(temp.arena->temp_count--);
    
    u8 *reset_to = temp.saved_current;
    
    if(!temp.saved_current){
        // 
        // We did 'begin_temporary_memory' on a zero initialized memory arena.
        // @note: This also works if the arena is still zero initialized.
        // 
        reset_to = temp.arena->base;
    }
    
    assert((u64)temp.saved_current <= (u64)temp.arena->current);
    temp.arena->current = reset_to;
}

func void solidify_temporary_memory(struct temporary_memory temp){
    assert(temp.arena->temp_count == temp.temp_index);
    debug_only(temp.arena->temp_count--);
}

////////////////////////////////////////////////////////////////

// @cleanup flag is_zero_terminated?
// u32 type: ascii, utf8, utf16, utf32
// u32 flags: is_zero_terminated, ...

func smm cstring_length(char *c_string){
    if(!c_string) return 0;
    u32 ret = 0;
    while(*c_string++) ret++;
    return ret;
}

func b32 u8_is_alpha(u8 c){
    return ('z' >= c && c >= 'a') || ('Z' >= c && c >= 'A') || (c == '_');
}

func b32 u8_is_alpha_numeric(u8 c){
    return u8_is_alpha(c) || ('9' >= c && c >= '0');
}

func b32 u8_is_whitespace(u8 c){
    return (c ==  '\v') || (c ==  '\t') || (c ==  '\f') || (c ==  ' ');
}

func b32 u8_is_newline(u8 c){
    return (c == '\n') || (c == '\r');
}

func b32 u8_is_whitespace_or_newline(u8 c){
    return u8_is_whitespace(c) || u8_is_newline(c);
}

func b32 u8_is_number(u8 c){
    return ('9' >= c && c >= '0');
}

func b32 u8_is_hex_number(u8 c){
    return u8_is_number(c) || ('f' >= (c|32) && (c|32) >= 'a');
}

func b32 u8_is_printable(u8 c){
    return (c >= 32) && (c < 127);
}

func u8 u8_parse_hex_to_binary(u8 hex){
    if('9' >= hex && hex >= '0') return hex - '0';
    if('f' >= hex && hex >= 'a') return hex - 'a' + 10;
    if('F' >= hex && hex >= 'A') return hex - 'A' + 10;
    return 0;
}
////////////////////////////////

// @note: we can print these using %.*s... maybe not its undefined behavior sigh!
struct string{
    union{
        smm size;
        smm length;
        smm amount;
    };
    union{
        u8 *data;
        u8 *memory;
    };
};

#define string(a) ((struct string){.data = (u8 *)a, .amount = (sizeof(a) - 1)})
#define const_string(a) {.data = (u8 *)a, .amount = (sizeof(a) - 1)}

func struct string create_string(u8 *data, smm size){
    struct string ret;
    ret.data = data;
    ret.size = size;
    return ret;
}

func b32 cstring_match(char *a, char *b){
    while(*a && *b){
        if(*a++ != *b++) return false;
    }
    
    return (*a == *b);
}

// @note: oring by 32 converts to lowercase

func b32 cstring_match_case_insensitive(char *a, char *b){
    while(*a && *b){
        if((*a++ | 32) != (*b++ | 32)) return false;
    }
    
    return (*a == *b); // make sure they are both 0
}

func b32 string_match(struct string a, struct string b){
    if(a.length != b.length) return false;
    
    return memcmp(a.data, b.data, a.size) == 0;
}

func b32 string_match_case_insensitive(struct string a, struct string b){
    if(a.length != b.length) return false;
    
    for(u32 i = 0; i < a.length; i++){
        if((a.data[i] | 32) != (b.data[i] | 32)) return false;
    }
    
    return true;
}

func b32 string_front_match(struct string a, char *c_string){
    u8 *it = cast(u8 *)c_string;
    for(smm i = 0; i < a.amount; i++, it++){
        if(*it == 0) return true;
        if(a.data[i] != *it) return false;
    }
    
    return (*it == 0);
}

func b32 string_front_match_eat(struct string *a, char *c_string){
    u8 *it = cast(u8 *)c_string;
    for(smm i = 0; i < a->amount; i++, it++){
        if(*it == 0){
            a->data += i;
            a->size -= i;
            return true;
        }
        if(a->data[i] != *it) return false;
    }
    
    if(*it){
        return false;
    }else{
        a->data += a->size;
        a->size = 0;
        return true;
    }
}

//@note: this should probably return 'front'
func struct string string_eat_front(struct string *a, smm amount){
    struct string ret = create_string(a->data, amount);
    
    assert(a->amount >= amount);
    a->data   += amount;
    a->amount -= amount;
    return ret;
}

func b32 string_eat_newline(struct string *string){
    if(!string->size) return 0;
    
    if(string->data[0] == '\n'){
        string_eat_front(string, 1);
        return 1;
    }
    
    if(string->data[0] == '\r'){
        string_eat_front(string, 1);
        if(string->data[0] == '\n'){
            string_eat_front(string, 1);
        }
        return 1;
    }
    return 0;
}

func b32 string_front_match_case_insensitive(struct string a, char *c_string){
    u8 *it = cast(u8 *)c_string;
    for(smm i = 0; i < a.amount; i++, it++){
        if(*it == 0) return true;
        if((a.data[i] | 32) != (*it | 32)) return false;
    }
    
    return (*it == 0);
}

func b32 string_lexically_smaller(struct string a, struct string b){
    smm size = min_of(a.size, b.size);
    for(smm i = 0; i < size; i++){
        if(a.data[i] < b.data[i]) return true;
        if(a.data[i] > b.data[i]) return false;
    }
    
    if(a.size < b.size) return true;
    if(a.size > b.size) return false;
    return false;
}

func int string_compare_lexically(struct string a, struct string b){
    smm size = min_of(a.size, b.size);
    for(smm i = 0; i < size; i++){
        if(a.data[i] < b.data[i]) return -1;
        if(a.data[i] > b.data[i]) return +1;
    }
    
    if(a.size < b.size) return -1;
    if(a.size > b.size) return +1;
    return 0;
}


func b32 string_lexically_smaller_case_insensitive(struct string a, struct string b){
    smm size = min_of(a.size, b.size);
    for(smm i = 0; i < size; i++){
        if((a.data[i] | 32) < (b.data[i] | 32)) return true;
        if((a.data[i] | 32) > (b.data[i] | 32)) return false;
    }
    
    if(a.size < b.size) return true;
    if(a.size > b.size) return false;
    return false;
}

func struct string string_eat_characters_front(struct string *string, char *characters){
    smm index = 0;
    for(; index < string->size; index++){
        char *s = characters;
        while(*s && *s != string->data[index]){
            s++;
        }
        if(!*s) break;
    }
    return string_eat_front(string, index);
}

func struct string string_eat_until_characters_front(struct string *string, char *characters){
    smm index = 0;
    for(; index < string->size; index++){
        char *s = characters;
        while(*s && *s != string->data[index]){
            s++;
        }
        if(*s) break;
    }
    return string_eat_front(string, index);
}

func struct string eat_whitespaces(struct string *a){
    smm i = 0;
    for(; i < a->length; i++){
        if(!u8_is_whitespace(a->data[i])) break;
    }
    struct string ret = create_string(a->data, i);
    string_eat_front(a, i);
    return ret;
}

struct string string_strip_whitespace(struct string string){
    
    while(string.size && string.data[string.size-1] == ' '){
        string.size -= 1;
    }
    
    while(string.size && string.data[0] == ' '){
        string.size -= 1;
        string.data += 1;
    }
    
    return string;
}

func struct string eat_whitespaces_and_newlines(struct string *a){
    smm i = 0;
    for(; i < a->length; i++){
        if(!u8_is_whitespace(a->data[i]) && !u8_is_newline(a->data[i])) break;
    }
    struct string ret = create_string(a->data, i);
    string_eat_front(a, i);
    return ret;
}

// @hmm: This also eats numbers?
func struct string eat_identifier(struct string *a){
    smm i = 0;
    for(; i < a->length; i++){
        int is_ascii_letter = ('a' <= a->data[i] && a->data[i] < 'z') || ('A' <= a->data[i] && a->data[i] < 'Z');
        int is_number       = ('0' <= a->data[i] && a->data[i] < '9');
        int is_underscore   = a->data[i] == '_';
        int is_unicode      = a->data[i] >= 128;
        
        if(!is_ascii_letter && !is_number && !is_underscore && !is_unicode) break;
    }
    struct string ret = create_string(a->data, i);
    string_eat_front(a, i);
    return ret;
}

func b32 string_ends_with(struct string a, struct string end){
    if(end.size > a.size) return false;
    
    smm diff = a.amount - end.amount;
    struct string end_of_a = create_string(a.data + diff, end.amount);
    return string_match(end_of_a, end);
}

func struct string eat_until_char(struct string *a, u8 eat_until, b32 eat_delimiter){
    smm i = 0;
    for(; i < a->amount; i++){
        if(a->data[i] == eat_until) break;
    }
    smm eat_extension = eat_delimiter && (i + eat_delimiter <= a->amount);
    
    struct string ret = create_string(a->data, i + eat_extension);
    string_eat_front(a, i + eat_extension);
    return ret;
}

func struct string string_eat_back(struct string *a, smm amount){
    assert(a->amount >= amount);
    a->amount -= amount;
    return *a;
}

func struct string string_from_cstring(char *data){
    smm length = cstring_length(data);
    struct string ret;
    ret.data   = cast(u8 *)data;
    ret.length = length;
    return ret;
}
#define cstring_to_string string_from_cstring


func void replace_characters(struct string string, char *replace, char replace_with){
    for(smm i = 0; i < string.amount; i++){
        char *iter = replace;
        while(*iter){
            if(string.data[i] == *iter){
                string.data[i] = (u8)replace_with;
                break;
            }
            iter++;
        }
    }
}

func char *push_cstring_from_string(struct memory_arena *arena, struct string str){
    char *memory = push_uninitialized_data(arena, char, str.size + 1);
    memcpy(memory, str.memory, str.amount);
    memory[str.size] = 0;
    return memory;
}

func b32 string_is_printable(struct string s){
    for(smm i = 0; i < s.amount; i++){
        if(!u8_is_printable(s.data[i])) return false;
    }
    return true;
}

func struct string push_string_copy(struct memory_arena *arena, struct string str){
    struct string ret = zero_struct;
    ret.data = push_uninitialized_data(arena, u8, str.amount);
    ret.amount = str.amount;
    memcpy(ret.data, str.data, str.amount);
    return ret;
}

func struct string push_zero_terminated_string_copy(struct memory_arena *arena, struct string string){
    return create_string((u8 *)push_cstring_from_string(arena, string), string.size);
}

func u64 string_djb2_hash(struct string str){
    
    u64 hash = 5381;
    for(smm i = 0; str.size > i; i++){
        hash = (hash << 5) + hash + str.data[i];
    }
    return hash;
}

#define STB_SPRINTF_IMPLEMENTATION
#define STB_SPRINTF_STATIC
#define STB_SPRINTF_DECORATE(name) name
#include "stb_sprintf.h"
#undef STB_SPRINTF_DECORATE
#undef STB_SPRINTF_IMPLEMENTATION

// automatically zero terminated
func struct string push_format_string_va_list(struct memory_arena *arena, char *format, va_list va){
    
    va_list copied;
    va_copy(copied, va);
    
    int length = vsnprintf(0, 0, format, va) + 1;
    char *data = push_uninitialized_data(arena, char, length);
    vsnprintf(data, length, format, copied);
    
    return create_string((u8 *)data, length - 1);
}

func struct string strip_quotes(struct string string){
    assert(string.size >= 2);
    assert(string.data[0] == string.data[string.size - 1]);
    string.data += 1;
    string.size -= 2;
    return string;
}

func struct string push_format_string(struct memory_arena *arena, char *format, ...){
    va_list va;
    va_start(va, format);
    struct string ret = push_format_string_va_list(arena, format, va);
    va_end(va);
    return ret;
}

static u32 print_args_list(char *format, va_list va){
    // @note asserting anything in console_print is not a good idear, as assert calls console print
    char buffer[4096];
    u32 length = (u32)vsnprintf(buffer, sizeof(buffer), format, va);
    return os_print_string(buffer, length);
}

PRINTLIKE static u32 print(char *format, ...){
    va_list va;
    va_start(va, format);
    u32 chars_written = print_args_list(format, va);
    va_end(va);
    return chars_written;
}

func struct string string_concatenate(struct memory_arena *arena, struct string s1, struct string s2){
    struct string ret;
    ret.amount = s1.amount + s2.amount;
    ret.data = push_uninitialized_data(arena, u8, ret.amount + 1);
    memcpy(ret.data, s1.data, s1.amount);
    memcpy(ret.data + s1.amount, s2.data, s2.amount);
    ret.data[ret.amount] = 0; // zero terminate because why not
    return ret;
}

struct string_list_node{
    struct string_list_node *next;
    struct string_list_node *prev;
    struct string string;
};

struct string_list{
    struct{
        struct string_list_node *first;
        struct string_list_node *last;
    } list;
    
    smm amount_of_strings;
    smm total_size;
};

func void string_list_prefix(struct string_list *list, struct memory_arena *arena, struct string string){
    if(string.size == 0) return;
    
    struct string_list_node *node = push_struct(arena, struct string_list_node);
    node->string = push_string_copy(arena, string);
    dll_push_front(list->list, node);
    list->total_size += string.length;
    list->amount_of_strings += 1;
}

func void string_list_postfix(struct string_list *list, struct memory_arena *arena, struct string string){
    if(string.size == 0) return;
    
    struct string_list_node *node = push_struct(arena, struct string_list_node);
    node->string = push_string_copy(arena, string);
    dll_push_back(list->list, node);
    list->total_size += string.length;
    list->amount_of_strings += 1;
}

func b32 string_list_add_uniquely(struct string_list *list, struct memory_arena *arena, struct string string){
    for(struct string_list_node *node = list->list.first; node; node = node->next){
        if(string_match(node->string, string)){
            return 1;
        }
    }
    
    string_list_postfix(list, arena, string);
    return 0;
}



// zero terminates because why not
func struct string string_list_flatten(struct string_list list, struct memory_arena *arena){
    u8 *data = push_uninitialized_data(arena, u8, list.total_size + 1);
    data[list.total_size] = 0;
    u8 *at = data;
    for(struct string_list_node *it = list.list.first; it; it = it->next){
        memcpy(at, it->string.data, it->string.length);
        at += it->string.length;
    }
    return create_string(data, list.total_size);
}

// @note: this trashes front and back.
func struct string_list string_list_concatenate(struct string_list front, struct string_list back){
    struct string_list ret;
    ret.list = front.list;
    
    dll_push_back_list(ret.list, back.list);
    ret.amount_of_strings = front.amount_of_strings + back.amount_of_strings;
    ret.total_size = front.total_size + back.total_size;
    
    return ret;
}


func void print_byte_range(u8 *start, smm amount){
    
    for(smm row_index = 0; row_index < (amount/16); row_index++){
        smm byte_index = row_index * 16;
        print("0x%.8X | ", (int)byte_index);
        u8 *at = start + byte_index;
        print("%.2X %.2X %.2X %.2X ", at[0],  at[1],  at[2],  at[3]);
        print("%.2X %.2X %.2X %.2X ", at[4],  at[5],  at[6],  at[7]);
        print("%.2X %.2X %.2X %.2X ", at[8],  at[9],  at[10], at[11]);
        print("%.2X %.2X %.2X %.2X ", at[12], at[13], at[14], at[15]);
        print("| ");
        for(u32 i = 0; i < 16; i++){
            u8 c = u8_is_printable(at[i]) ? at[i] : '.';
            print("%c", c);
        }
        print("\n");
    }
    
    smm bytes_printed = 16 * (amount / 16);
    if(bytes_printed == amount) return;
    
    print("0x%.8X | ", (int)bytes_printed);
    
    for(smm i = 0; i < 16; i++){
        smm byte_index = bytes_printed + i;
        
        if(byte_index < amount){
            u8 *at = start + byte_index;
            print("%.2X ", *at);
        }else{
            print("   ");
        }
    }
    
    print("| ");
    u8 *at = start + bytes_printed;
    for(u32 i = 0; i < (amount - bytes_printed); i++){
        u8 c = u8_is_printable(at[i]) ? at[i] : '.';
        print("%c", c);
    }
    print("\n");
}

////////////////////////////////

func b32 path_is_absolute(struct string path){
    if(path.size < 2) return false;
    return u8_is_alpha(path.data[0]) && path.data[1] == ':';
}

func b32 path_is_relative(struct string path){
    return !path_is_absolute(path);
}

func struct string canonicalize_slashes(struct string path){
    for(smm i = 0; i < path.amount; i++){
        if(path.data[i] == '\\') path.data[i] = '/';
    }
    return path;
}

func void hacky_canonicalize_file_for_case_insensitivity(struct string *path){
    for(smm i = 0; i < path->amount; i++){
        if(path->data[i] == '\\') path->data[i] = '/';
        if('A' <= path->data[i] && path->data[i] <= 'Z') path->data[i] |= 32;
    }
    
}

func b32 path_contains_wildcard(struct string path){
    for(smm i = 0; i < path.size; i++){
        if(path.data[i] == '*') return true;
    }
    return false;
}

func struct string pop_one_directory(struct string path){
    if(path.size < 2) return string("");
    
    smm one_past_last_slash = 0;
    for(smm i = path.size - 2; i >= 0; i--){
        if(path.data[i] == '/' || path.data[i] == '\\'){
            one_past_last_slash = i + 1;
            break;
        }
    }
    
    path.amount = one_past_last_slash;
    return path;
}

// @incomplete: care about all of the file path things: https://docs.microsoft.com/en-us/dotnet/standard/io/file-path-formats
// @incomplete: this should care about all of these different kinds. also maybe this is platform specific?
// maybe this should be os_file_path

// always zero_terminated
func struct string concatenate_file_paths(struct memory_arena *arena, struct string absolute, struct string relative){
    
    if(string_front_match(relative, "./")){
        string_eat_front(&relative, 2);
    }
    
    while(string_front_match(relative, "../")){
        string_eat_front(&relative, 3);
        absolute = pop_one_directory(absolute);
    }
    
    if(string_match(relative, string(".."))){
        relative = string("");
        absolute = pop_one_directory(absolute);
    }
    
    if(absolute.amount && absolute.data[absolute.amount - 1] == '/'){
        string_eat_back(&absolute, 1);
    }
    
    if(relative.size && relative.data[0] == '/'){
        return string_concatenate(arena, absolute, relative);
    }
    
    struct string ret;
    ret.amount = absolute.amount + relative.amount + 1;
    ret.data = push_uninitialized_data(arena, u8, ret.amount + 1);
    ret.data[ret.amount] = 0; // zero terminate
    memcpy(ret.data, absolute.data, absolute.amount);
    ret.data[absolute.amount] = '/';
    memcpy(ret.data + absolute.amount + 1, relative.data, (u64)relative.amount);
    return ret;
}

func struct string strip_file_extension(struct string file_name){
    for(smm i = file_name.amount - 1; i >= 0; i--){
        if(file_name.data[i] == '/' || file_name.data[i] == '\\') break; // no file extension
        if(file_name.data[i] == '.'){
            file_name.amount = i;
            break;
        }
    }
    return file_name;
}

func struct string get_file_extension(struct string file_name){
    for(smm i = file_name.amount - 1; i >= 0; i--){
        if(file_name.data[i] == '/' || file_name.data[i] == '\\') break; // no file extension
        if(file_name.data[i] == '.'){
            return create_string(file_name.data + i, file_name.size - i);
        }
    }
    return string("");
}

func struct string strip_file_path(struct string path){
    smm one_past_last_slash = 0;
    for(smm i = path.amount - 1; i >= 0; i--){
        if(path.data[i] == '/' || path.data[i] == '\\'){
            one_past_last_slash = i + 1;
            break;
        }
    }
    
    path.data   += one_past_last_slash;
    path.amount -= one_past_last_slash;
    return path;
}
#define get_file_name(path) strip_file_path(path)

func struct string strip_file_name(struct string path){
    smm one_past_last_slash = 0;
    for(smm i = path.amount - 1; i >= 0; i--){
        if(path.data[i] == '/' || path.data[i] == '\\'){
            one_past_last_slash = i + 1;
            break;
        }
    }
    
    path.amount = one_past_last_slash;
    return path;
}
#define get_file_path(path) strip_file_name(path)

//_____________________________________________________________________________________________________________________

func smm print_string(struct string string){
    return os_print_string((char *)string.data, string.amount);
}

//_____________________________________________________________________________________________________________________

// @cleanup: this is about the most lazy version
func u64 string_to_u64(struct string str, b32 *_out_success){
    
    u64 ret = 0;
    if(str.size > 2 && str.data[0] == '0' && (str.data[1]|32) == 'x'){
        for(smm i = 0; i < str.length; i++){
            ret *= 16;
            ret += u8_parse_hex_to_binary(str.data[i]);
        }
    }else{
        for(smm i = 0; i < str.length; i++){
            u64 val = str.data[i] - '0';
            if(val > 10){
                *_out_success = false;
                return 0;
            }
            ret = ret*10 + val;
        }
    }
    return ret;
}

// @cleanup: Even lazier...
u64 string_parse_u64(struct string string){
    u64 ret = 0;
    for(smm index = 0; index < string.size; index++){
        ret *= 10;
        ret += (string.data[index] - '0');
    }
    return ret;
}


//_____________________________________________________________________________________________________________________


func u32 u32_round_up_to_next_power_of_two(u32 v){
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;
    return v;
}

func u64 u64_round_up_to_next_power_of_two(u64 v){
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    v++;
    return v;
}

/////////////////////////////////////////////////


typedef  union ALIGNED(16){
    struct{
        s64 ptr1;
        s64 ptr2;
    };
    u32 _u32[4];
    u8 _u8[16];
} m128;

// returns 1 on success
// returns 0 on fail and overrides *comparand with *dest
func b32 atomic_compare_and_swap_128(m128 *dest, m128 source, m128 *comparand);

// implements 'i++;'
func s64 atomic_postincrement(s64 *val);

// implements 'i--;'
func s64 atomic_postdecrement(s64 *val);

// implements '++i;'
func s64 atomic_preincrement(s64 *val);

// implements '--i;'
func s64 atomic_predecrement(s64 *val);

struct ticket_spinlock{
    s64 tickets_given_out;
    s64 ticket_in_work;
};

#if defined(__clang__)
#define atomic_load(type, value) cast (type)*(cast(volatile type *)(&value))
#define atomic_store(type, address, value) *(cast(volatile type *)(&address)) = (value)
#else
#define atomic_load(type, value) (*&(type)zero_struct = value, cast(type)*(cast(volatile type *)(&value)))
#define atomic_store(type, address, value) *(cast(volatile type *)(&address)) = (*&(type)zero_struct = value, (value))
#endif

// @cleanup: think of some asserts
func void ticket_spinlock_lock(struct ticket_spinlock *mutex){
    s64 my_ticket = atomic_postincrement(&mutex->tickets_given_out);
    while(atomic_load(s64, mutex->ticket_in_work) != my_ticket){
        _mm_pause();
    }
}

func void ticket_spinlock_unlock(struct ticket_spinlock *mutex){
    atomic_preincrement(&mutex->ticket_in_work);
}

//_____________________________________________________________________________________________________________________

// @sigh this takes a cstring as I don't want to leak
func struct os_file load_file_into_arena(char *file_name, struct memory_arena *arena){
    struct os_file file = os_load_file(file_name, 0, 0);
    assert(file.data == null);
    u8 *buf = push_uninitialized_data(arena, u8, file.amount + 1);
    buf[file.amount] = 0;
    file = os_load_file(file_name, buf, file.amount + 1);
    return file;
}


//_____________________________________________________________________________________________________________________

func u64 xor_shift64(u64 x){
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    return x;
}

struct random_series{
    u64 val;
};

func struct random_series random_series_from_seed(u64 seed){
    struct random_series ret;
    ret.val = seed;
    return ret;
}

func u32 random_u32(struct random_series *series){
    u64 val = xor_shift64(series->val);
    series->val = val;
    return (u32)val;
}

func u64 random_u64(struct random_series *series){
    u64 val = xor_shift64(series->val);
    series->val = val;
    return val;
}

/////////////////////////////////////////////////////

func s32 save_truncate_smm_to_s32(smm pointer_diff){
    assert(pointer_diff <= s32_max);
    assert(pointer_diff >= s32_min);
    
    return cast(s32)pointer_diff;
}


func u32 save_truncate_smm_to_u32(smm pointer_diff){
    assert(pointer_diff <= u32_max);
    assert(pointer_diff >= 0);
    
    return cast(u32)pointer_diff;
}


func u16 save_truncate_smm_to_u16(smm pointer_diff){
    assert(pointer_diff <= u16_max);
    assert(pointer_diff >= 0);
    
    return cast(u16)pointer_diff;
}

func u8 save_truncate_smm_to_u8(smm pointer_diff){
    assert(pointer_diff >= 0);
    assert(pointer_diff <= u8_max);
    
    return cast(u8)pointer_diff;
}

//////////////////////////////////////////////////////

func void print_m128(m128 to_print){
    print("0x");
    for(u32 i = 0; i < sizeof(m128); i++){
        print("%.2x", to_print._u8[i]);
    }
}

func m128 hash_md5_inplace(u8 *msg, smm initial_length, smm max_length){
    
    smm length = align_up(initial_length + 1 + 8, 64);
    assert(max_length >= length);
    
    memset(msg + initial_length, 0, length - initial_length);
    msg[initial_length] = (1 << 7); // only the top bit set
    
    smm bit_length = 8 * initial_length;
    memcpy(msg + length - 8, &bit_length, 8); // copy in the length
    
    
    // constant stuff:
    static const u32 shifts[] = {
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
        4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
        6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
    };
    
    // Use binary integer part of the sines of integers (in radians) as constants
    static const u32 sines[] = {
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
    };
    
    u32 hash_0 = 0x67452301;
    u32 hash_1 = 0xefcdab89;
    u32 hash_2 = 0x98badcfe;
    u32 hash_3 = 0x10325476;
    
    // go through in blocks of 512 bits or 64 bytes
    for(smm block_it = 0; block_it < length; block_it += 64){
        u8 *block = (msg + block_it);
        u32 *words = (u32 *)block;
        
        u32 block_hash_0 = hash_0;
        u32 block_hash_1 = hash_1;
        u32 block_hash_2 = hash_2;
        u32 block_hash_3 = hash_3;
        
        for(u32 i = 0; i < 64; i++){
            u32 mix;
            u32 word_index;
            
            if(i < 16){
                mix = block_hash_3 ^ (block_hash_1 & (block_hash_2 ^ block_hash_3));
                word_index = i;
            }else if(i < 32){
                mix = block_hash_2 ^ (block_hash_3 & (block_hash_1 ^ block_hash_2));
                word_index = (5 * i + 1) & 15;
            }else if(i < 48){
                mix = block_hash_1 ^ block_hash_2 ^ block_hash_3;
                word_index = (3 * i + 5) & 15;
            }else{
                mix = block_hash_2 ^ (block_hash_1 | (~block_hash_3));
                word_index = (7 * i) & 15;
            }
            
            u32 temp_hash = block_hash_3;
            block_hash_3 = block_hash_2;
            block_hash_2 = block_hash_1;
            
            u32 to_rotate = (block_hash_0 + mix + sines[i] + words[word_index]);
            u32 rotate_by = shifts[i];
            block_hash_1 = block_hash_1 + ((to_rotate << rotate_by) | (to_rotate >> (32 - rotate_by)));
            
            block_hash_0 = temp_hash;
        }
        
        hash_0 += block_hash_0;
        hash_1 += block_hash_1;
        hash_2 += block_hash_2;
        hash_3 += block_hash_3;
    }
    
    m128 ret;
    ret._u32[0] = hash_0;
    ret._u32[1] = hash_1;
    ret._u32[2] = hash_2;
    ret._u32[3] = hash_3;
    return ret;
}

func m128 hash_md5(struct memory_arena *scratch, u8 *data, smm initial_length){
    struct temporary_memory temp = begin_temporary_memory(scratch);
    
    // Prepare the message:
    
    // message layout:
    // | original layout | (1 << 7) | pad zeroes | length of the message (64 bit) |
    // message bit-length has to be divisible by 512, or the message byte length  has to be divisible by 64
    smm length = align_up(initial_length + 1 + 8, 64);
    
    u8 *msg = push_uninitialized_data(scratch, u8, length);
    memcpy(msg, data, initial_length);
    
    m128 ret = hash_md5_inplace(data, initial_length, length);
    
    end_temporary_memory(temp);
    
    return ret;
}


static void quicksort_u32_array(struct memory_arena *scratch, u32 *evaluations, smm amount){
    
    struct temporary_memory temp_mem = begin_temporary_memory(scratch);
    
    smm *sort_stack = push_uninitialized_data(scratch, smm, amount);
    
    smm left = 0;
    smm right = amount - 1;
    
    smm sort_stack_at = 0;
    while(true){
        
        if(left < right){
            smm piveot = evaluations[left];
            
            smm right_it = right;
            smm left_it  = left;
            
            while(left_it < right_it){
                while(evaluations[left_it] <= piveot && left_it < right_it) left_it++;
                
                while(evaluations[right_it] > piveot) right_it--;
                
                if(left_it < right_it){
                    {
                        //
                        // swap the evaluations
                        //
                        u32 temp = evaluations[left_it];
                        evaluations[left_it] = evaluations[right_it];
                        evaluations[right_it] = temp;
                    }
                }
            }
            
            //
            // Swap the piveot element to the middle!
            //
            {
                u32 temp = evaluations[left];
                evaluations[left] = evaluations[right_it];
                evaluations[right_it] = temp;
            }
            
            //
            // At this point we know:
            //    1) everything after  'right_it' is >  piveot
            //    2) everything before 'left_it'  is <= piveot
            //    3) evaluations[right_it] == piveot
            
            // We need to sort:
            //    1) [left, left_it - 1]   = [left, right_it]
            //    2) [right_it + 1, right] = [left_it, right]
            //     3) but actually 'evaluations[right_it]' is known to be correct
            
            if(left_it < right){
                sort_stack[sort_stack_at++] = right;
            }
            
            if(left < right_it - 1){
                sort_stack[sort_stack_at++] = right_it - 1;
            }else{
                left++;
            }
        }
        
        if(sort_stack_at == 0) break;
        
        right = sort_stack[--sort_stack_at];
    }
    
    end_temporary_memory(temp_mem);
}

////////////////////////////////////////////////////////////////////////////////////

// utility for dumping out indented "txt" files
struct txt_context{
    struct string_list list;
    struct memory_arena *scratch;
    u32 indentation_level;
    u32 indentation_disabled;
    u32 newline_disabled;
};

func void txt_print_no_indent(struct txt_context *file, char *format, ...){
    va_list va;
    va_start(va, format);
    string_list_postfix(&file->list, file->scratch, push_format_string_va_list(file->scratch, format, va));
    va_end(va);
}

func void txt_print(struct txt_context *file, char *format, ...){
    // we dont do anything
    if(file->indentation_disabled && file->newline_disabled && !*format) return;
    va_list va;
    va_start(va, format);
    if(!file->indentation_disabled){
        for(u32 i = 0; i < file->indentation_level; i++){
            string_list_postfix(&file->list, file->scratch, string("\t"));
        }
    }
    string_list_postfix(&file->list, file->scratch, push_format_string_va_list(file->scratch, format, va));
    va_end(va);
    if(!file->newline_disabled) string_list_postfix(&file->list, file->scratch, string("\n"));
}

func void txt_print_bytes(struct txt_context *txt, u8 *bytes, smm amount, b32 skip_indent){
    u32 prev_indent = txt->indentation_level++;
    if(skip_indent) txt->indentation_level = 0;
    txt_print(txt, "bytes:");
    txt->indentation_level = 0;
    for(u32 i = 0; i < amount; i++){
        txt_print(txt, " %.2x", bytes[i]);
    }
    txt_print(txt, "\n");
    txt->indentation_level = prev_indent;
}

func void txt_print_printables(struct txt_context *txt, u8 *bytes, smm amount, b32 skip_indent){
    u32 prev_indent = txt->indentation_level++;
    if(skip_indent) txt->indentation_level = 0;
    txt_print(txt, "printables:");
    txt->indentation_level = 0;
    for(u32 i = 0; i < amount; i++){
        txt_print(txt, "%c", u8_is_printable(bytes[i]) ? bytes[i] : '.');
    }
    txt_print(txt, "\n");
    txt->indentation_level = prev_indent;
}

func struct string txt_flatten(struct memory_arena *arena, struct txt_context *txt){
    return string_list_flatten(txt->list, arena);
}

func b32 write_txt_to_file(struct txt_context *txt, char *file_name){
    struct string buf = txt_flatten(txt->scratch, txt);
    return os_write_file(file_name, buf.data, buf.size);
}



