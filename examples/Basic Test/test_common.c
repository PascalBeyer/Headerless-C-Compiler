


#define assert(expr) ((expr) ? 0 : (do_assert(#expr, __FILE__, __FUNCTION__, __LINE__), 0))

void do_assert(char *expr, char *file, char *function, int line){
    print_string(file);
    print_string("(");
    print_number(line);
    print_string(",1): ");
    //print_string(function); @cleanup: I guess this is busted again?
    print_string("assert fired in expression \"");
    print_string(expr);
    print_string("\"\n");
    
}

typedef signed char      s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef float f32;
typedef double f64;
typedef s32 b32;
#define null (void *)0
#define array_count(a) (sizeof(a)/sizeof(*(a)))
#define offset_in_struct(s,member) (s64)(&((struct s *)null)->member)
#define true 1
#define false 0

__declspec(dllimport) void **GetStdHandle(u32 nStdHandle);
__declspec(dllimport) s32 WriteFile(void **hFile, char *lpBuffer, u32 nNumberOfBytesToWrite,u32 *lpNumberOfBytesWritten, void *lpOverlapped);


void print_line(char *a){
    u32 stdout = 0xFFFFFFF5;
    void **handle = GetStdHandle(stdout);
    u32 length = (u32)strlen(a);
    u32 ignored;
    WriteFile(handle, a, length, &ignored, null);
    char *newline = "\n";
    WriteFile(handle, newline, 1, &ignored, null);
}

void print_string(char *a){
    u32 stdout = 0xFFFFFFF5;
    void **handle = GetStdHandle(stdout);
    u32 length = (u32)strlen(a);
    u32 ignored;
    WriteFile(handle, a, length, &ignored, null);
}

u64 strlen(char *a){
    u64 ret = 0;
    while(*a){ // @cleanup: post and preincrement
        a = a + 1;
        ret = ret + 1; 
    }
    return ret;
}

void print_char(char a){
    u32 stdout = 0xFFFFFFF5;
    void **handle = GetStdHandle(stdout);
    u32 amount_written;
    WriteFile(handle, &a, 1, &amount_written, (void *)0);
}

void print_number(u32 n){
    if(n == 0){
        print_char('0');
        return;
    }
    u32 asd = n;
    u32 log = 1;
    while(asd){
        log = log * 10;
        asd = asd / 10;
    }
    log = log / 10;
    
    while(log){
        u8 c = (u8)((n / log) % 10) + '0';
        print_char((char)c);
        log = log / 10;
    }
}

b32 cstring_match(char *a, char *b){
    while(*a && *b){
        if(*a++ != *b++) return false;
    }
    
    return (*a == *b);
}

b32 memory_is_equal(u8 *a, u8 *b, u64 size){
    for(u64 i = 0; i < size; i++){
        if(a[i] != b[i]) return false;
    }
    return true;
}

