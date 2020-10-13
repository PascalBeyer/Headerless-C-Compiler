
/*
#ifdef __PBC__
typedef char int8_t;
typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;

typedef uint64_t uintptr_t;
typedef int64_t intptr_t;
typedef int64_t ptrdiff_t;

typedef int64_t size_t;
typedef char *va_list;
#define NULL ((void *)0)




#else*/
#ifdef __PBC__
// Defined as the integer literal value 100 for compilations that target x64 processors. Otherwise, undefined.
#define _M_AMD64 100
// Defined as the integer literal value 100 for compilations that target x64 processors. Otherwise, undefined.
#define _M_X64 100
// Defined as 1 when the compilation target is 64-bit ARM or x64. Otherwise, undefined.
#define __pragma(...)
#define _WIN64 1
#endif

#include <stdint.h>
#include <stdarg.h>
//#endif
//#include <stdalign.h>
#define alignof(a) _Alignof(a)

typedef int8_t s8;
typedef uint8_t u8;
typedef int16_t s16;
typedef uint16_t u16;
typedef int32_t s32;
typedef uint32_t u32;
typedef int64_t s64;
typedef uint64_t u64;
typedef s8 b8;
typedef s16 b16;
typedef s32 b32;
typedef s64 b64;
typedef float f32;
typedef double f64;
typedef uintptr_t umm;
typedef uintptr_t umem;
typedef intptr_t  smm;
typedef intptr_t  smem;
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
#define min_s64 (-(s64)9223372036854775808)
#define s64_min (-(s64)9223372036854775808)

#define min_u8 (u8)(0)
#define u8_min (u8)(0)

#define min_u16 (u16)(0)
#define u16_min (u16)(0)

#define min_u32 (u32)(0)
#define u32_min (u32)(0)

#define min_u64 (u64)(0)
#define u64_min (u64)(0)

#define global static
#define proc static
#define func static
// #define func inline

#ifdef __clang__
#define zero_struct {}
#elif defined(_MSC_VER)
#define zero_struct {0}
#endif

//////////////////////////////////////////////

struct os_virtual_buffer{
    union{
        u8 *memory;
        u8 *base;
    };
    smm commited;
    smm reserved;
};

struct memory_buffer{
    u8 *memory;
    smm size;
};

struct os_file{
    b32 file_does_not_exist;
    
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

static void                     os_free_memory(void *memory_to_free);
static struct os_file           os_load_file(char *file_name, void *buffer, umm buffer_size); // @sigh cstrings
static b32                      os_write_file(char *file_name, void *buffer, smm buffer_size); // zero terminated
static __declspec(noreturn) void os_panic(u32 exit_code);
static u32                      os_print_string(char *string, smm length);
static void                     os_debug_break(void);

static f64                     os_get_time_in_seconds(void);

// @cleanup: this is not part of the os layer anymore.
static u32 print(char *format, ...);

//////////////////////////////////////////////

#define cast(type) (type)

#define kilo_bytes(a) ((a) * 1024ULL)
#define mega_bytes(a) ((kilo_bytes(a)) * 1024ULL)
#define giga_bytes(a) ((mega_bytes(a)) * 1024ULL)

#if defined(_Debug)
proc  __declspec(noreturn) void __do_assert(const char *file, int line, const char *expression, const char *function){
    
    print("%s:%i: ASSERT FIRED: '%s' in function %s.\n", file, line, expression, function);
    os_debug_break();
    os_panic(1);
}
#define assert(expr) ((expr) ? 0 : (__do_assert(__FILE__,  __LINE__, #expr, __FUNCTION__), 1))
//#define assert(expr) ((expr) ? 0 : (*((u32 *)0x1337133713371337) = 0))
#define debug_only(a) a
#define not_implemented assert(!"not implemented!")
#define invalid_code_path __do_assert(__FILE__, __LINE__, "invalid code path", __FUNCTION__)
#define invalid_default_case(...) default:{ assert(!"invalid default case"); __VA_ARGS__; } break

#else // _Debug

#define assert(a) 0
#define debug_only(a)
#define not_implemented
#define invalid_code_path
#define invalid_default_case(...) default: __VA_ARGS__; break

//__buildin_assume()?

#endif // _Debug



#ifdef __clang__
#define static_assert(expr) _Static_assert(expr, "")
#elif defined(_MSC_VER)
#define static_assert(expr) static u8 static_assert_hack[(expr) ? 1 : -1]

#elif defined(__PBC__)
#define __va_start(va, format) (void)(*(va) = (((char *)&format) + 8))
#define zero_struct {}
#define __unaligned
// @cleanup: I guess we should allow the above as well?
#define M_concat2__internal(a, b) a ## b
#define M_concat2(a, b) M_concat2__internal(a, b)
#define static_assert(expr) static u8 M_concat2(static_assert, __LINE__)[(expr) ? 1 : -1]

/*
#define va_start(va, format) (void)((va) = (((char *)&format) + 8))
#define va_end(va) (void)((va) = (va_list)0)
#define va_arg(va, type) *(type *)((va += sizeof(__int64)) - sizeof(__int64))
*/

int count_trailing_zeros32(u32 a){
    int zeroes = 0;
    for(u32 i = 0; i < 32; i++){
        u32 bit = (1 << i);
        if(a & bit){
            zeroes = i;
            break;
        }
    }
    return zeroes;
}

int count_leading_zeros32(u32 a){
    int zeroes = 0;
    for(int i = 31; i >= 0; i--){
        u32 bit = (1 << i);
        if(a & bit){
            zeroes = i;
            break;
        }
    }
    return zeroes;
}

int count_leading_zeros64(u64 a){
    int zeroes = 0;
    for(int i = 63; i >= 0; i--){
        u64 bit = (1ull << i);
        if(a & bit){
            zeroes = i;
            break;
        }
    }
    return zeroes;
}

unsigned char _InterlockedCompareExchange128(__int64 volatile * _Destination, __int64 _ExchangeHigh, __int64 _ExchangeLow, __int64 * _ComparandResult);
__int64 _InterlockedCompareExchange64(s64 volatile *dest, s64 source, s64 comparand);

__int64 _InterlockedExchangeAdd64(__int64 * dest, __int64 Value);
__int64 _InterlockedIncrement64(__int64 volatile *inc);

__int64 _InterlockedDecrement64(__int64 volatile *dec);

unsigned __int64 __rdtsc();

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



#if 1
#define to_u8(number)  ((assert(u8_min <=  (number) && (number) <= u8_max)),  (u8)(number))
#define to_s8(number)  ((assert(s8_min <=  (number) && (number) <= s8_max)),  (s8)(number))
#define to_u16(number) ((assert(u16_min <= (number) && (number) <= u16_max)), (u16)(number))
#define to_s16(number) ((assert(s16_min <= (number) && (number) <= s16_max)), (s16)(number))
#define to_u32(number) ((assert(u32_min <= (number) && (number) <= u32_max)), (u32)(number))
#define to_s32(number) ((assert(s32_min <= (number) && (number) <= s32_max)), (s32)(number))
#define to_u64(number) ((assert(u64_min <= (number) && (number) <= u64_max)), (u64)(number))
#define to_s64(number) ((assert(s64_min <= (number) && (number) <= s64_max)), (s64)(number))
#endif


///////////////////////////////////////////////////////////////
struct bucket_array{
    struct bucket_array *next;
    smm capacity;
    smm size;
};

#define bucket_get_array(bucket) (void *)(bucket + 1)

func struct bucket_array *bucket_array_allocate_page(smm element_size){
    smm mem_size = align_up(128 * element_size + sizeof(struct bucket_array), 0x1000);
    smm capacity = (mem_size - sizeof(struct bucket_array)) / element_size;
    struct bucket_array *bucket_array = os_allocate_memory(mem_size);
    bucket_array->capacity = capacity;
    return bucket_array;
}

#define bucket_array(type)                                                                 \
struct{                                                                                    \
    struct bucket_array *first;                                                             \
    struct bucket_array *last;                                                              \
    type *array;                                                                            \
    smm count;                                                                              \
}                                                                                          \

#define bucket_array_add(arr, element)                                                     \
{                                                                                          \
    if(!arr.array){                                                                         \
        struct bucket_array *bucket_array = bucket_array_allocate_page(sizeof(element));     \
        arr.first = bucket_array;                                                            \
        arr.last = bucket_array;                                                             \
        arr.array = (void *)((u8 *) bucket_array + sizeof(struct bucket_array));                \
    }                                                                                       \
    if(arr.last->capacity <= arr.last->size + 1){                                           \
        struct bucket_array *new_bucket_array = bucket_array_allocate_page(sizeof(element)); \
        arr.last->next = new_bucket_array;                                                   \
        arr.last = new_bucket_array;                                                         \
        arr.array = (void *)((u8 *) new_bucket_array + sizeof(struct bucket_array));                \
    }                                                                                       \
    arr.array[arr.last->size++] = element;                                              \
    arr.count++; \
}                                                                                          \

///////////////////////////////////////////////////////////////

int _fltused;

#if defined(__clang__) || defined(__PBC__)

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
#endif

#ifdef __clang__
// @cleanup: take a look at these after -O3, is this just rep movsb?

#define count_leading_zeros32 __builtin_clz
#define count_leading_zeros64 __builtin_clzll
#define count_trailing_zeros32 __builtin_ctz
#define count_trailing_zeros64 __builtin_ctzll

#define memset __builtin_memset
#define memcpy __builtin_memcpy
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

func b32 memory_is_zero(void *_memory, smm size){
    u8 *memory = _memory;
    for(smm i = 0; i < size; i++){
        if(memory[i] != 0) return false;
    }
    return true;
}

///////////////////////////////////////////////

struct memory_arena_block{
    struct memory_arena_block *next;
    struct memory_arena_block *prev;
    
    u8 *base;
    u8 *current;
    u8 *end_of_arena; // end of commited memory
    smm reserved_size;
};

struct memory_arena{ // @cleanup: @speed: think of a way to get the current block out.
    struct{
        struct memory_arena_block *first;
        struct memory_arena_block *last;
    }block_list;
    
    struct memory_arena_block *current_block;
    
    f32 exponential_growth_factor;// @incomplete:
    u32 constant_grow_amount;
    
    s32 overwrite_alignment;
    b32 is_fixed_size;
    
    debug_only(s32 temp_count;)
        debug_only(u64 commited_size;) // should be the same as (end_of_arena - arena_base)
};

struct tempoary_memory{
    struct memory_arena *arena;
    u8 *saved_base;
    struct memory_arena_block *memory_block;
    
    debug_only(smm temp_index;)
    
};

func struct tempoary_memory begin_tempoary_memory(struct memory_arena *arena){
    struct tempoary_memory ret = zero_struct;
    ret.arena = arena;
    if(arena->current_block){
        ret.memory_block = arena->current_block;
        ret.saved_base = arena->current_block->current;
    }
    debug_only(ret.temp_index = ++arena->temp_count);
    return ret;
}

func void end_tempoary_memory(struct tempoary_memory temp){
    assert(temp.arena->temp_count == temp.temp_index);
    debug_only(temp.arena->temp_count--);
    
    if(temp.memory_block){
        temp.arena->current_block = temp.memory_block;
        temp.arena->current_block->current = temp.saved_base;
    }else{
        if(temp.arena->current_block){
            struct memory_arena *arena = temp.arena;
            arena->current_block = arena->block_list.first;
            arena->current_block->current = arena->current_block->base;
        }
    }
}

func void solidify_tempoary_memory(struct tempoary_memory temp){
    assert(temp.arena->temp_count == temp.temp_index);
    
    debug_only(temp.arena->temp_count--);
}

func struct memory_arena create_memory_arena(struct os_virtual_buffer buffer, f32 exp_grow, u32 constant_grow){
    assert(buffer.reserved);
    if(!buffer.commited){
        assert(constant_grow >= kilo_bytes(4));
        smm size_to_commit = min_of((smm)constant_grow, buffer.reserved);
        buffer.commited = os_commit_memory(buffer.base, size_to_commit).commited;
    }
    
    struct memory_arena_block *new_block = cast(struct memory_arena_block *)buffer.base;
    memset(new_block, 0, sizeof(*new_block));
    new_block->base          = (buffer.base + sizeof(struct memory_arena_block));
    new_block->current       = (buffer.base + sizeof(struct memory_arena_block));
    new_block->end_of_arena  = (buffer.base + buffer.commited);
    new_block->reserved_size = buffer.reserved;
    
    
    struct memory_arena ret = zero_struct;
    ret.current_block             = new_block;
    ret.exponential_growth_factor = exp_grow;
    ret.constant_grow_amount      = constant_grow;
    
#if defined(_Debug)
    ret.commited_size = 0;
    ret.temp_count    = 0;
#endif
    
    return ret;
}

#define push_uninit_struct(arena, type) ((type *)push_struct_(arena, sizeof(type), alignof(type)))
#define push_struct(arena, type) ((type *)memset(push_uninit_struct(arena, type), 0, sizeof(type)))

#define push_data(arena, type, amount)   ((type *)push_struct_(arena, (amount) * sizeof(type), alignof(type)))
#define push_zero_data(arena, type, amount) ((type *)memset(push_data(arena, type, sizeof(type) * (amount)), 0, sizeof(type) * (amount)))

#define push_array_copy(arena, type, array, amount) ((type *)memcpy(push_data(arena, type, sizeof(type) * (amount)), array, (amount) * sizeof(type)))

// @note: this _should_ take a pointer as x because then we get an error if we pass a structure. If it takes a structure and we do &x, we loose this error checking.
#define block_zero(x) memset(x, 0, sizeof(*x))
#define block_copy(source, dest) (assert(sizeof(*source) == sizeof(*dest)), memcpy(x, y, sizeof(*x)))
#define push_struct_copy(arena, type, x) ((type *)memcpy(push_struct(arena, type), (type *)x, sizeof(type)))

#define dynarray_maybe_grow(type, arena, array, amount, capacity){ \
    if(*&(amount) == *&(capacity)){                                \
        capacity <<= 1;                                            \
        type *new_array = push_data(arena, type, capacity);        \
        memcpy(new_array, array, (amount) * sizeof(*array));       \
        array = new_array;                                         \
    }                                                              \
}
    
func u8 *arena_current(struct memory_arena *arena){
    if(!arena->current_block) return null;
    return arena->current_block->current;
}

// @cleanup: these should test some stuff
func void push_align_initialized_to_specific_value(struct memory_arena *arena, smm alignment, u8 byte){
    assert(!(alignment & (alignment - 1))); // is power of two
    assert(alignment > 0);
    u8 *aligned_up = cast(void *)align_up(cast(smm)arena_current(arena), alignment);
    if(aligned_up != arena_current(arena)){
        memset(arena_current(arena), byte, (aligned_up - arena_current(arena)));
    }
    arena->current_block->current = aligned_up;
}

func void push_zero_align(struct memory_arena *arena, smm alignment){
    push_align_initialized_to_specific_value(arena, alignment, 0);
}

func void push_align(struct memory_arena *arena, u32 alignment){
    assert(!(alignment & (alignment - 1))); // is power of two
    arena->current_block->current = cast(void *)align_up(cast(smm)arena_current(arena), alignment);
}

// @note: this has a bunch of hardcoded constants
func void *push_struct_(struct memory_arena *arena, smm size, u32 alignment){
    assert(!(alignment & (alignment - 1))); // is power of two
    assert(alignment <= 0x1000); // align at most to a page, this is as our size should be page aligned
    assert(alignment > 0);
    
    if(arena->overwrite_alignment > 0) alignment = (u32)arena->overwrite_alignment;
    
    struct memory_arena_block *block = arena->current_block;
    
    smm grow_to;
    if(!block){
        grow_to = max_of((smm)mega_bytes(1), size);
        goto allocate_new_block;
    }
    
    block->current = cast(void *)align_up(cast(umm)block->current, alignment);
    assert(block->current <= block->end_of_arena); // this should be garatied as we align up at most by a page
    
    smm rest_size = cast(smm)(block->end_of_arena - block->current);
    
    if(rest_size < size + (smm)alignment){
        smm current_arena_size = cast(smm)(block->end_of_arena - cast(u8 *)block->base);
        smm exponential = cast(smm)((f32)current_arena_size * max_of(1.0f, arena->exponential_growth_factor));
        grow_to = (smm)(exponential + max_of(arena->constant_grow_amount, kilo_bytes(16)));
        
        smm aligned_size = align_up(size, 0x1000);
        grow_to = max_of(current_arena_size + aligned_size, grow_to);
        if(grow_to > block->reserved_size){
            if(arena->is_fixed_size){
                // @cleanup: make this string changeable!
                print("Error: memory failiure!\n");
                os_debug_break();
                os_panic(1);
            }
            
            if(block->next){
                arena->current_block = block->next;
                arena->current_block->current = arena->current_block->base; // if it is after us it is free
                return push_struct_(arena, size, alignment);
            }
            
            allocate_new_block:;
            smm memory_reserve = max_of(grow_to + sizeof(struct memory_arena_block), mega_bytes(1));
            smm memory_commit  = max_of(grow_to + sizeof(struct memory_arena_block), kilo_bytes(16));
            
            struct os_virtual_buffer reserved = os_reserve_memory(0, memory_reserve);
            struct os_virtual_buffer commited = os_commit_memory(reserved.base, memory_commit);
            if(!reserved.base || !commited.base){
                print("Error: allocation failiure!\n");
                os_debug_break();
                os_panic(1);
            }
            
            struct memory_arena_block *new_block = cast(struct memory_arena_block *)commited.base;
            new_block->base = (commited.base + sizeof(struct memory_arena_block));
            new_block->current = (commited.base + sizeof(struct memory_arena_block));
            new_block->end_of_arena = (commited.base + commited.commited);
            new_block->reserved_size = commited.reserved;
            
            dll_push_back(arena->block_list, new_block);
            
            arena->current_block = new_block;
            block = new_block;
            goto end;
            
        }
        struct os_virtual_buffer buffer = os_commit_memory(block->base, grow_to);
        assert(buffer.memory);
        block->end_of_arena = buffer.memory + buffer.commited;
    }
    
    end:;
    void *ret = block->current;
    block->current += size;
    return ret;
}

func struct memory_arena push_arena(struct memory_arena *arena, smm size){
    struct memory_arena_block *block = arena->current_block;
    smm truncated_size = min_of(block->reserved_size, size);
    
    struct os_virtual_buffer buf;
    buf.base     = block->current;
    buf.commited = min_of(cast(smm)(block->end_of_arena - block->current), truncated_size);
    buf.reserved = truncated_size;
    
    struct memory_arena ret = create_memory_arena(buf, arena->exponential_growth_factor, arena->constant_grow_amount);
    
    block->current += truncated_size;
    block->end_of_arena = max_of(block->end_of_arena, block->current);
    if(block->next && block->current == block->base + block->reserved_size){
        block = block->next;
    }
    
    return ret;
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
    return u8_is_number(c) || ('f' >= c && c >= 'a') || ('F' >= c && c >= 'A');
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
        u8 *mem;
        u8 *memory;
        u8 *str;
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
    
    for(u32 i = 0; i < a.length; i++){
        if(a.str[i] != b.str[i]) return false;
    }
    
    return true;
}

func b32 string_match_case_insensitive(struct string a, struct string b){
    if(a.length != b.length) return false;
    
    for(u32 i = 0; i < a.length; i++){
        if((a.str[i] | 32) != (b.str[i] | 32)) return false;
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

//@note: this should probably return 'front'
func struct string string_eat_front(struct string *a, smm amount){
    assert(a->amount >= amount);
    a->data   += amount;
    a->amount -= amount;
    return *a;
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

func b32 string_ends_with(struct string a, struct string end){
    if(end.size > a.size) return false;
    
    smm diff = a.amount - end.amount;
    struct string end_of_a = create_string(a.data + diff, end.amount);
    return string_match(end_of_a, end);
}

func struct string eat_until_char(struct string *a, u8 eat_until){
    smm i = 0;
    for(; i < a->amount; i++){
        if(a->data[i] == eat_until) break;
    }
    struct string ret = create_string(a->data, i);
    string_eat_front(a, i);
    return ret;
}

func struct string eat_until_whitespace(struct string *a){
    smm i = 0;
    for(; i < a->length; i++){
        if(u8_is_whitespace(a->data[i])) break;
    }
    struct string ret = create_string(a->data, i);
    string_eat_front(a, i);
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
    char *memory = push_data(arena, char, str.size + 1);
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
    ret.data = push_data(arena, u8, str.amount);
    ret.amount = str.amount;
    memcpy(ret.data, str.data, str.amount);
    return ret;
}

#define STB_SPRINTF_IMPLEMENTATION
#define STB_SPRINTF_STATIC
#define STB_SPRINTF_DECORATE(name) name
#include "stb_sprintf.h"
#undef STB_SPRINTF_DECORATE
#undef STB_SPRINTF_IMPLEMENTATION

// automatically zero terminated
func struct string push_format_string_va_list(struct memory_arena *arena, char *format, va_list va){
    int length = vsnprintf(0, 0, format, va) + 1;
    char *data = push_data(arena, char, length);
    vsnprintf(data, length, format, va);
    
    return create_string((u8 *)data, length - 1);
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

static u32 print(char *format, ...){
    va_list va;
    va_start(va, format);
    u32 chars_written = print_args_list(format, va);
    va_end(va);
    return chars_written;
}

func struct string string_concatinate(struct memory_arena *arena, struct string s1, struct string s2){
    struct string ret;
    ret.amount = s1.amount + s2.amount;
    ret.data = push_data(arena, u8, ret.amount + 1);
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
    struct string_list_node *node = push_struct(arena, struct string_list_node);
    node->string = push_string_copy(arena, string);
    dll_push_front(list->list, node);
    list->total_size += string.length;
    list->amount_of_strings += 1;
}

func void string_list_postfix(struct string_list *list, struct memory_arena *arena, struct string string){
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
    u8 *data = push_data(arena, u8, list.total_size + 1);
    u8 *at = data;
    for(struct string_list_node *it = list.list.first; it; it = it->next){
        memcpy(at, it->string.data, it->string.length);
        at += it->string.length;
    }
    return create_string(data, list.total_size);
}

// @note: this trashes front and back.
func struct string_list string_list_concatinate(struct string_list front, struct string_list back){
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
        print("0x%.8X | ", byte_index);
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
    
    print("0x%.8X | ", bytes_printed);
    
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

func void canonicalize_slashes(struct string path){
    for(u32 i = 0; i < path.amount; i++){
        if(path.data[i] == '\\') path.data[i] = '/';
    }
}

// @incomplete: care about all of the file path things: https://docs.microsoft.com/en-us/dotnet/standard/io/file-path-formats
// @incomplete: this should care about all of these differant kinds. also maybe this is platform specific?
// maybe this should be os_file_path

// always zero_terminated
func struct string concatinate_file_paths(struct memory_arena *arena, struct string absolute, struct string relative){
    assert(absolute.amount);
    if(absolute.data[absolute.amount - 1] == '/'){
        string_eat_back(&absolute, 1);
    }
    
    // linux paths
    if(string_front_match(relative, "./")){
        string_eat_front(&relative, 2);
    }
    
    while(string_front_match(relative, "../")){
        string_eat_front(&relative, 3);
        for(smm i = absolute.amount; --i;){
            if(absolute.data[i] == '/'){
                absolute.amount = i;
                break;
            }
        }
    }
    
    if(relative.data[0] == '/'){
        return string_concatinate(arena, absolute, relative);
    }
    
    struct string ret;
    ret.amount = absolute.amount + relative.amount + 1;
    ret.data = push_data(arena, u8, ret.amount + 1);
    ret.data[ret.amount] = 0; // zero terminate
    memcpy(ret.data, absolute.data, absolute.amount);
    ret.data[absolute.amount] = '/';
    memcpy(ret.data + absolute.amount + 1, relative.data, (u64)relative.amount);
    return ret;
}

func struct string strip_file_extension(struct string file_name){
    for(smm i = file_name.amount; --i;){
        if(file_name.data[i] == '.'){
            file_name.amount = i;
            break;
        }
    }
    return file_name;
}

func struct string strip_file_path(struct string path){
    smm one_past_last_slash = 0;
    for(smm i = path.amount; --i;){
        if(path.data[i] == '/' || path.data[i] == '\\'){
            one_past_last_slash = i + 1;
            break;
        }
    }
    
    path.data   += one_past_last_slash;
    path.amount -= one_past_last_slash;
    return path;
}


func struct string strip_file_name(struct string path){
    smm one_past_last_slash = 0;
    for(smm i = path.amount; --i;){
        if(path.data[i] == '/' || path.data[i] == '\\'){
            one_past_last_slash = i + 1;
            break;
        }
    }
    
    path.amount = one_past_last_slash;
    return path;
}

////////////////////////////////

typedef struct string * unique_string;


// @note this does not copy the strings, maybe It should have a flag for this?
struct string_intern_entry{
    struct string value;
    u64 hash;
};

struct string_intern_table{
    struct string_intern_entry *entries;
    struct string zero_string;
    
    u64 size;
    u64 log2_capacity;
    
    b32 copy_and_zero_terminate;
    b32 pad;
    struct memory_arena copy_arena;
    
    debug_only(
        u64 amount_of_hash_collisions;
        u64 amount_of_hash_accesses;
        )
};

proc u64 string_intern_hash(struct string str){
    u64 hash   = 0;
    for(smm i = 0; str.size > i; i++){
        hash = 301 * hash  + str.data[i];
    }
    return hash;
}

proc unique_string string_intern_table_intern(struct string_intern_table *table, struct string string){
    if(string.size == 0) return &table->zero_string; // we use the zero size string as invalid entry
    u64 hash       = string_intern_hash(string);
    umem capacity   = (1ull << table->log2_capacity);
    if(table->size > capacity / 2){
        not_implemented; // @note: this is impossible with this setup as we return '&entry->value'.
        return &table->zero_string;
    }
    u64 index = hash & (capacity - 1);
    struct string_intern_entry *entry = table->entries + index;
    while(entry->value.size && (entry->hash != hash ||  !string_match(entry->value, string))){
        debug_only(table->amount_of_hash_collisions++);
        entry++;
    }
    
    if(cast(umem)(entry - table->entries) >= capacity){
        entry = table->entries;
        while(entry->value.size && (entry->hash != hash ||  !string_match(entry->value, string))){
            debug_only(table->amount_of_hash_collisions++);
            entry++;
        }
    }
    
    if(!entry->value.size){
        entry->value = string;
        if(table->copy_and_zero_terminate){
            entry->value.data = cast(u8 *)push_cstring_from_string(&table->copy_arena, string);
        }
        entry->hash  = hash;
        table->size++;
    }
    
    return &entry->value;
}


proc void string_intern_table_print(struct string_intern_table *table){
    umem capacity = (1ull << table->log2_capacity);
    for(umem i    = 0; capacity > i; i++){
        struct string_intern_entry val = table->entries[i];
        if(val.value.size){
            print("%u: '%.*s'\n", val.hash, val.value.size, val.value.str);
        }
    }
}

func smm print_string(struct string string){
    return os_print_string((char *)string.data, string.amount);
}

///////////////////////////////////

// @cleanup: this is about the most lazy version
func u64 string_to_u64(struct string str, b32 *success){
    u64 ret = 0;
    for(u32 i = 0; i < str.length; i++){
        u64 val = str.data[i] - '0';
        if(val > 10){
            *success = false;
            return 0;
        }
        ret = ret*10 + val;
    }
    return ret;
}

//////////////////////////////////////////////



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



// @cleanup: these could be defines... maybe they should be in windows.c....
// @note: return the initial value
func s64 atomic_add(s64 *val, s64 to_add){
    //__int64 _InterlockedExchangeAdd64(__int64 volatile * _Addend, __int64 _Value);
    return _InterlockedExchangeAdd64(val, to_add);
}

//func s64 atomic_subtract(s64 *val, s64 to_add)
//{
//return _InterlockedExchangeSub64(val, to_add);
//}

func void *atomic_compare_and_swap(void *dest, void *source, void *comparand){
    return (void *)_InterlockedCompareExchange64((s64 *)dest, (s64)source, (s64)comparand);
}


typedef  union __declspec(align(16)){
    struct{
        __int64 ptr1;
        __int64 ptr2;
    };
    u32 _u32[4];
    u8 _u8[16];
} m128;


unsigned char _InterlockedCompareExchange128(__int64 volatile * _Destination, __int64 _ExchangeHigh, __int64 _ExchangeLow, __int64 * _ComparandResult);
// returns 1 on success
// returns 0 on fail and overrides *comparand with *dest
func b32 atomic_compare_and_swap_128(m128 *dest, m128 source, m128 *comparand){
    assert(((umm)dest & 15) == 0);
    return _InterlockedCompareExchange128((__int64 *)dest, source.ptr2, source.ptr1, (__int64 *)comparand);
}



// implements 'i++;'
func s64 atomic_postincrement(s64 *val){
    //__int64 _InterlockedIncrement64(__int64 volatile * _Addend);
    return _InterlockedIncrement64(cast(long long *)val) - 1;
}

// implements 'i--;'
func s64 atomic_postdecrement(s64 *val){
    //__int64 _InterlockedDecrement64(__int64 volatile * _Addend);
    return _InterlockedDecrement64(cast(long long *)val) + 1;
}

// implements '++i;'
func s64 atomic_preincrement(s64 *val){
    //__int64 _InterlockedIncrement64(__int64 volatile * _Addend);
    return _InterlockedIncrement64(cast(long long *)val);
}

// implements '--i;'
func s64 atomic_predecrement(s64 *val){
    //__int64 _InterlockedDecrement64(__int64 volatile * _Addend);
    return _InterlockedDecrement64(cast(long long *)val);
}

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
proc void ticket_spinlock_lock(struct ticket_spinlock *mutex){
    s64 my_ticket = atomic_postincrement(&mutex->tickets_given_out);
    while(atomic_load(s64, mutex->ticket_in_work) != my_ticket){
        _mm_pause();
    }
}

proc void ticket_spinlock_unlock(struct ticket_spinlock *mutex){
    atomic_preincrement(&mutex->ticket_in_work);
}



///////////////////////////////////////////////
// @sigh this takes a cstring as I don't want to leak
proc struct os_file load_file_into_arena(char *file_name, struct memory_arena *arena){
    struct os_file file = os_load_file(file_name, 0, 0);
    assert(file.data == null);
    u8 *buf = push_data(arena, u8, file.amount + 1);
    buf[file.amount] = 0;
    file = os_load_file(file_name, buf, file.amount + 1);
    return file;
}


/////////////////////////////////////////////////

func u32 xor_shift32(u32 x32){
    x32 ^= x32 << 13;
    x32 ^= x32 >> 17;
    x32 ^= x32 << 5;
    return x32;
}

func u64 xor_shift64(u64 x){
	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	return x;
}

proc u64 cheap_and_dirty_hash(u64 hash, u32 number_of_bits_to_return){
    u64 mask  = (1 << number_of_bits_to_return) - 1;
    u64 index = (hash ^ (hash >> 24));
    index = xor_shift64(index);
    index    &= mask;
    
    return index;
}

struct hash_map_entry{
    void *value;
    u64 hash;
};

struct hash_map{
    struct hash_map_entry *entries; // this is zero terminated.
    u32 size;
    u32 log2_capacity;
    debug_only(u64 amount_of_hash_collisions;)
        debug_only(u64 amount_of_hash_accesses;)
};

proc u64 hash_map_get_capacity(struct hash_map *hash_map){
    return (1ull << hash_map->log2_capacity);
}

// @cleanup: this is only a lowerbound... bad name
proc u32 u32_base_2_integer_logarithm(u32 a){
    assert(is_power_of_two(a));
    return (u32)count_trailing_zeros32(a);
}

proc struct hash_map hash_map_create(u64 capacity, struct hash_map_entry *entries){
    assert(capacity);
    assert(is_power_of_two(capacity));
    assert(capacity <= (1u << 31u));
    struct hash_map ret = {0};
    ret.log2_capacity = u32_base_2_integer_logarithm(cast(u32)capacity);
    ret.entries = entries;
    return ret;
}

// returns a valid place if it does not exist
proc struct hash_map_entry* hash_map_get_internal(struct hash_map *hash_map, u64 hash){
    debug_only(hash_map->amount_of_hash_accesses++);
    
    u64 index    = cheap_and_dirty_hash(hash, hash_map->log2_capacity);
    u64 capacity = (1ull << hash_map->log2_capacity);
    assert(index < capacity);
    struct hash_map_entry *entry = hash_map->entries + index;
    while(entry->value && entry->hash != hash)
    {
        debug_only(hash_map->amount_of_hash_collisions++);
        entry++;
    }
    
    // if we are outside of the range we reset to 0, and start again
    if(cast(u64) (entry -  hash_map->entries) >= capacity)
    {
        debug_only(hash_map->amount_of_hash_collisions++);
        entry= hash_map->entries;
        while(entry->value && entry->hash != hash)
        {
            debug_only(hash_map->amount_of_hash_collisions++);
            entry++;
        }
    }
    
    return entry;
}

proc void *hash_map_get(struct hash_map *hash_map, u64 hash){
    struct hash_map_entry *entry = hash_map_get_internal(hash_map, hash);
    assert(!entry->value || entry->hash == hash);
    
    return entry->value;
}

func void hash_map_insert(struct hash_map *hash_map, void *value, u64 hash){
    struct hash_map_entry *entry = hash_map_get_internal(hash_map, hash);
    
    assert(!entry->value || entry->hash == hash);
    if(entry->value){
        assert(!"hash_map_insert: hash was allready used in hash_map!");
    }else{
        entry->value = value;
        entry->hash  = hash;
    }
}

func void hash_map_change_or_create(struct hash_map *hash_map, void *value, u64 hash){
    struct hash_map_entry *location = hash_map_get_internal(hash_map, hash);
    
    if(!location->value){
        hash_map->size++;
        location->hash = hash;
    }
    else{
        assert(hash == location->hash);
    }
    
    location->value = value;
    assert(hash_map->size < (1ull << (hash_map->log2_capacity - 1)));
}

func void *hash_map_delete_and_get(struct hash_map *hash_map, u64 hash){
    struct hash_map_entry *location = hash_map_get_internal(hash_map, hash);
    
    if(!location->value) return null;
    
    void *save = location->value;
    
    location->value = 0;
    location->hash = 0;
    return save;
}

/////////////////////////////////////////////////////

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

func u64 rand(struct random_series *series){
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

func m128 hash_md5(struct memory_arena *scratch, u8 *data, smm initial_length){
    struct tempoary_memory temp = begin_tempoary_memory(scratch);
    
    // Prepare the message:
    
    // message layout:
    // | original layout | (1 << 7) | pad zeroes | length of the message (64 bit) |
    // message bit-length has to be divisible by 512, or the message byte length  has to be divisible by 64
    smm length = align_up(initial_length + 1 + 8, 64);
    
    u8 *msg = push_data(scratch, u8, length);
    memcpy(msg, data, initial_length);
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
    
    end_tempoary_memory(temp);
    
    m128 ret;
    ret._u32[0] = hash_0;
    ret._u32[1] = hash_1;
    ret._u32[2] = hash_2;
    ret._u32[3] = hash_3;
    return ret;
}

////////////////////////////////////////////////////////////////////////////////////

// @note: I am sort of confused whether or not this is the actual crc32 or whether this is only the inner part
//        and we would need reflection / xoring with 0xffffffff. But this is what I needed for the pdb-hash
func u32 crc32(u32 initial_crc, u8 *data, smm amount){
    // crc works by using polynomial division over F_2.
    // the i-th bit corresponds to X^i.
    
    // for simplicity 'amount = n = 100'
    // msg: [b99:b98:...: b0] -> b99 X^99 + b98 X^98 + ... + b0
    
    // CRC32 uses the 'generating polynomial':
    //    X^32 + X^26 + X^23 + X^22 + X^16 + X^12 + X^11 + X^10 + X^8 + X^7 + X^5 + X^4 + X^2 + X + 1
    // or 100000100110000010001110110110111 = 0x104c11db7, we usually omit the first one.
    
    // ACTUALLY: everything uses 'reflected' values
    /*
        // 'reflect' the entry i.e swap all bits
        func u32 reflect(u32 entry){
            for(u32 i = 0; i < 16; i++){
                u32 j = 31 - i;
                u32 bit1 = ((1 << i) & entry);
                u32 bit2 = ((1 << j) & entry);
                
                entry ^= bit1 | bit2 | ((bit1 >> i) << j) | ((bit2 >> j) << i));
            }
            return entry;
        }
    */
    // so the reflected polynomial is 0xedb88320
    
    // the crc of a message is the remainder after long division by the generating polynomial.
    
    // this table maps 'byte' -> ('byte' * X^32 mod g(X)).
    static const u32 crc32_table[0x100] = {
        0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
        0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
        0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
        0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
        0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
        0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
        0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
        0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924, 0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
        0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
        0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
        0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
        0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
        0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
        0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
        0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
        0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
        0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
        0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
        0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
        0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
        0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
        0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
        0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236, 0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
        0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
        0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
        0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
        0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
        0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
        0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
        0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
        0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
        0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
    };
    // The crc_table is generated by this code:
    /*
    for(u32 entry_index = 0; entry_index < 0x100; entry_index++){
        u32 entry = entry_index;
        for(u32 bit_index = 0; bit_index < 8; bit_index++){
            // perform polynomial division.
            // if the top bit is set, we subtract (xor) the (reflected) generating polynomial.
            entry = (entry & 1) ? ((entry >> 1) ^ reflect(0x04c11db7)) : (entry >> 1);
        }
        // after we are done, 'entry' is the remainder of polynomial division over F_2 of 'i * X^32'
        // store this in the table
        
        crc_table[entry_index] = entry;
    }
    */
    
    // assume we have a message and a last byte
    // [msg?,...,msg0] | [lb7,...,lb0]
    // and we have calculated the remainder of 'msg * X^32' after division by g(X) to be 'crc'
    // i.e:  msg * X^32 + crc = 0 mod g(X)
    // thus we calculate
    //     crc' = (msg||lb) * X^32       mod g(X)
    //          = msg * X^40 + lb * X^32 mod g(X)
    //          = crc * X^8  + lb * X^32 mod g(X)
    //          = (crc[31:8] << 8) + (crc[7:0] + lb) * X^32
    // note the reflection on crc.
    // finally the line in the for below is this equation for the crc' using the table above
    //     crc' = (crc[31:8] << 8) + ((crc[7:0] + lb) * X^32 mod g(X))
    
    u32 crc = initial_crc;
    for(smm i = 0; i < amount; i++){
        crc = (crc >> 8) ^ crc32_table[(crc & 0xff) ^ data[i]];
    }
    return crc;
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



