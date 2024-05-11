
typedef __int32 DWORD, LONG;
typedef __int64 LONGLONG;

typedef union _LARGE_INTEGER {
    struct {
        DWORD LowPart;
        LONG HighPart;
    };
    
    struct {
        DWORD LowPart;
        LONG HighPart;
    } u;
    LONGLONG QuadPart;
} LARGE_INTEGER;

LARGE_INTEGER integer = {{0, 0}};


int main(){}
