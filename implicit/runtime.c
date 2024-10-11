
// 
// To link to the ucrt.dll with MSVC you are supposed to link to 3 libraries:
//     1) ucrt.lib
//     2) vcruntime.lib
//     3) msvcrt.lib
// 
// The ucrt.lib is included with the Windows Software Development Kit.
// Hence, I think for now we can rely on that existing.
// On the other hand, vcruntime.lib and msvcrt.lib ship with MSVC
// and we don't want to require other people to install MSVC to use the compiler.
// Hence, we cannot link to the other two libraries.
// 
// Fortunally, there is not a lot of code in those.
// 
// vcruntime.lib contains functions, that are somehow "compiler specific", 
// for example exception handling, C++ run-time type information etc.
// For us, we only care about the following functions:
// 
//     wcsstr
//     wcsrchr
//     wcschr
//     strstr
//     strrchr
//     strchr
//     memset
//     memmove
//     memcpy
//     memcmp
//     memchr
//     longjmp
//     _setjmpex (technically __intrinsic_setjmpex, but that is used by msvcrt.lib's _setjmpex)
//     _setjmp   (technically __intrinsic_setjmp,   but that is used by msvcrt.lib's _setjmp)
//     
// msvcrt.lib contains functions, that are related to security features, like 
// 
//     `__guard_dispatch_icall` (Control Flow Guards)
//     `__security_cookie`
//     `__chkstk`
//     ...
//     
// Since we currently do not support any security features (and would not use Microsofts code anyway),
// We only have to implement the list of functions specified above.
// 

//_____________________________________________________________________________________________________________________
// Memory functions.


// @cleanup: How to handle types in "implicit" filesB.
#define wchar_t unsigned short
#define size_t  unsigned __int64


void *memset(void *mem, int val, unsigned __int64 amount){
    unsigned char *it = mem;
    for(unsigned __int64 i = 0; i < amount; i++){
        *it++ = (unsigned char)val;
    }
    return mem;
}

void *memcpy(void *dest, const void *source, unsigned __int64 amount){
    unsigned char *it  = dest;
    unsigned char *it2 = source;
    for(unsigned __int64 i = 0; i < amount; i++){
        *it++ = *it2++;
    }
    
    return dest;
}

void *memmove(void *dest, const void *source, unsigned __int64 amount){
    
    if(source >= dest || (char *)source + amount < (char *)dest){
        // There is no overlapp, or source is bigger than dest:
        // 
        // <---- dest ---->
        //        <--- source --->
        // 
        // or 
        // <--- source ---> 
        //                      <---- dest ---->
        // 
        // We can copy forward:
        // 
        
        unsigned char *it  = dest;
        unsigned char *it2 = source;
        
        for(unsigned __int64 i = 0; i < amount; i++){
            *it++ = *it2++;
        }
    }else{
        // 
        // There is overlapp and source is before dest.
        // 
        // <--- source --->
        //     <---- dest ---->
        // 
        // We need to copy backwards.
        // 
        
        unsigned char *it  = (unsigned char *)dest   + amount;
        unsigned char *it2 = (unsigned char *)source + amount;
        
        for(unsigned __int64 i = 0; i < amount; i++){
            *--it = *--it2;
        }
    }
    
    return dest;
}

int memcmp(void *_string1, void *_string2, unsigned __int64 amount){
    
    if(amount == 0) return 0;
    
    char *string1 = _string1;
    char *string2 = _string2;
    
    while(--amount && *string1 == *string2){
        string1 += 1;
        string2 += 1;
    }
    
    return *(unsigned char *)string1 - *(unsigned char *)string2;
}

void *memchr(void *mem, int c, size_t count){
    char *it = mem;
    for(size_t index = 0; index < count; index++){
        if(it[index] == (char)c) return it + index;
    }
    return 0;
}

//_____________________________________________________________________________________________________________________
// String functions.

char *strstr(char *haystack, char *needle){
    if(*needle) return haystack;
    
    while(*haystack){
        char *string1 = haystack;
        char *string2 = needle;
        
        // @note: We cannot overrun string1, as the zero-terminator would cause us to break.
        while(*string2){
            if(*string1++ != *string2++) break;
        }
        
        if(*string2) return haystack;
        
        haystack++;
    }
    
    return 0;
}

char *strchr(char *haystack, int needle){
    while(*haystack != needle) haystack++;
    return *haystack ? haystack : 0;
}

char *strrchr(char *haystack, int needle){
    char *end = haystack;
    while(*end++){};
    while(end >= haystack && *end != needle) end--;
    return end >= haystack ? end : 0;
}

//_____________________________________________________________________________________________________________________
// Wide-Character String functions

wchar_t *wcsstr(wchar_t *haystack, wchar_t *needle){
    if(*needle) return haystack;
    
    while(*haystack){
        wchar_t *string1 = haystack;
        wchar_t *string2 = needle;
        
        // @note: We cannot overrun string1, as the zero-terminator would cause us to break.
        while(*string2){
            if(*string1++ != *string2++) break;
        }
        
        if(*string2) return haystack;
        
        haystack++;
    }
    
    return 0;
}

wchar_t *wcschr(wchar_t *haystack, wchar_t needle){
    while(*haystack != needle) haystack++;
    return *haystack ? haystack : 0;
}

wchar_t *wcsrchr(wchar_t *haystack, wchar_t needle){
    wchar_t *end = haystack;
    while(*end++){};
    while(end >= haystack && *end != needle) end--;
    return end >= haystack ? end : 0;
}

//_____________________________________________________________________________________________________________________
// Setjmp or longjmp.

