// run

#include <intrin.h>

#define assert(a) if(!(a)) return 1

int main(){
    __int8  s8  = -8;
    __int16 s16 = -16;
    __int32 s32 = -32;
    __int64 s64 = -64;
    
    unsigned __int8  u8  = 8;
    unsigned __int16 u16 = 16;
    unsigned __int32 u32 = 32;
    unsigned __int64 u64 = 64;
    
    void *ptr = &ptr;
    
    // __debugbreak();
    __rdtsc();
    
    __rdtscp(&u32);
    
    u8 = _addcarry_u8(u8,  u8,  u8,  &u8);
    u8 = _addcarry_u16(u8, u16, u16, &u16);
    u8 = _addcarry_u32(u8, u32, u32, &u32);
    u8 = _addcarry_u64(u8, u64, u64, &u64);
    
    __addgsbyte(0,  u8);
    __addgsword(0,  u16);
    __addgsdword(0, u32);
    __addgsqword(0, u64);
    __incgsbyte(0);
    __incgsword(0);
    __incgsdword(0);
    __incgsqword(0);
    
    u8 = _BitScanForward(&u32, u32);
    u8 = _BitScanReverse(&u32, u32);
    u8 = _BitScanForward64(&u32, u64);
    u8 = _BitScanReverse64(&u32, u64);
    
    u8 = _bittest(&s32, s32);
    u8 = _bittest64(&s64, s64);
    
    u8 = _bittestandcomplement(&s32, s32 & 31);
    u8 = _bittestandcomplement64(&s64, s64 & 63);
    
    u8 = _bittestandreset(&s32, s32 & 31);
    u8 = _bittestandreset64(&s64, s64 & 63);
    
    u8 = _bittestandset(&s32, s32 & 31);
    u8 = _bittestandset64(&s64, s64 & 64);
    
    s8  = _InterlockedCompareExchange8(&s8, s8, s8);
    s16 = _InterlockedCompareExchange16(&s16, s16, s16);
    s32 = _InterlockedCompareExchange(&s32, s32, s32);
    s64 = _InterlockedCompareExchange64(&s64, s64, s64);
        
    __declspec(align(16)) __int64 array[2] = {0};
    u8 = _InterlockedCompareExchange128(array, s64, s64, array);
    
    ptr = _InterlockedCompareExchangePointer(&ptr, ptr, ptr);
    
    s8  = _InterlockedExchangeAdd8(&s8, s8);
    s16 = _InterlockedExchangeAdd16(&s16, s16);
    s32 = _InterlockedExchangeAdd(&s32, s32);
    s64 = _InterlockedExchangeAdd64(&s64, s64);
    
    s8  = _InterlockedIncrement8(&s8);
    s16 = _InterlockedIncrement16(&s16);
    s32 = _InterlockedIncrement(&s32);
    s64 = _InterlockedIncrement64(&s64);
    
    s8  = _InterlockedDecrement8(&s8);
    s16 = _InterlockedDecrement16(&s16);
    s32 = _InterlockedDecrement(&s32);
    s64 = _InterlockedDecrement64(&s64);
    
    __stosb(&u8,  0x13,               1);
    __stosw(&u16, 0x1337,             1);
    __stosd(&u32, 0x13371337,         1);
    __stosq(&u64, 0x1337133713371337, 1);
    
    __movsb(&u8,  (unsigned __int8  *)&s8,  1);
    __movsw(&u16, (unsigned __int16 *)&s16, 1);
    __movsd(&u32, (unsigned __int32 *)&s32, 1);
    __movsq(&u64, (unsigned __int64 *)&s64, 1);
    
    
    u16 = __popcnt16(u16);
    u32 = __popcnt(u32);
    u64 = __popcnt64(u64);
    
    _mm_pause();
    
    _InterlockedExchange(&u32, u32);
    
    __m128 mask = _mm_set1_ps( 10.0f );
    assert(mask.m128_f32[0] == 10.0f && mask.m128_f32[1] == 10.0f && mask.m128_f32[2] == 10.0f && mask.m128_f32[3] == 10.0f);
    
    return 0;
}
