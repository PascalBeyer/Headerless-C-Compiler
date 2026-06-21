// run

#include <intrin.h>

int main()
{
    unsigned __int64 u64 = 0x0102030405060708;
    unsigned long ul = 0x01020304;
    unsigned short us = 0x0102;
    
    if(_byteswap_uint64(u64) !=0x807060504030201) return 1;

#ifdef _WIN32
    if(_byteswap_ulong(ul) != 0x4030201) return 1;
#else
    if(_byteswap_ulong(ul) != 0x403020100000000) return 1;
#endif
    if(_byteswap_ushort(us) != 0x201) return 1;
}
