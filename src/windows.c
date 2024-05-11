
#ifdef proc
#undef proc
#endif

#if 0
#include <windows.h>
#else

#define MAX_PATH 32767

#define func static
// @cleanup: I should probably get rid of all of these types

typedef unsigned char UBYTE;
typedef int BOOL;
typedef unsigned short WORD;
typedef unsigned long DWORD, *PDWORD;
typedef void *LPVOID, *PVOID;
typedef unsigned __int64 ULONG_PTR;
typedef ULONG_PTR DWORD_PTR, SIZE_T;
typedef void *HANDLE;
typedef __int64 LONG_PTR;
typedef DWORD *LPDWORD;

typedef unsigned __int64 DWORD64, *PDWORD64;
typedef char *LPCSTR;
typedef char CHAR;
typedef short SHORT;
typedef long LONG, *LPLONG;
typedef __int64 LONGLONG;
typedef unsigned __int64 ULONGLONG;
typedef int INT;
typedef unsigned int UINT;
typedef unsigned long ULONG;
typedef unsigned char UCHAR;
typedef unsigned short USHORT;

typedef unsigned char BYTE;
typedef BYTE BOOLEAN;

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

typedef union _ULARGE_INTEGER {
    struct {
        DWORD LowPart;
        DWORD HighPart;
    };
    struct {
        DWORD LowPart;
        DWORD HighPart;
    } u;
    ULONGLONG QuadPart;
} ULARGE_INTEGER;


struct HINSTANCE__{int unused;}; typedef struct HINSTANCE__ *HINSTANCE;
typedef HINSTANCE HMODULE;
/////// GENERAL

__declspec(dllimport) u32 GetCurrentProcessId(void);
__declspec(dllimport) u32 GetProcessId(HANDLE Process);
__declspec(dllimport) void __stdcall Sleep(DWORD dwMilliseconds);
__declspec(dllimport) BOOL __stdcall QueryPerformanceCounter(LARGE_INTEGER * lpPerformanceCount);
__declspec(dllimport) BOOL __stdcall QueryPerformanceFrequency(LARGE_INTEGER * lpFrequency);

// GetCurrentDirectory with 0 returns the size of the buffer including the null terminator
__declspec(dllimport) DWORD __stdcall GetCurrentDirectoryA(DWORD nBufferLength, u8* lpBuffer);
__declspec(dllimport) HMODULE __stdcall GetModuleHandleA(LPCSTR lpModuleName);
__declspec(dllimport) DWORD GetModuleFileNameA(HMODULE hModule, char *lpFilename, DWORD nSize);

typedef struct _SYSTEM_INFO {
    union {
        DWORD dwOemId;
        struct {
            WORD wProcessorArchitecture;
            WORD wReserved;
        } DUMMYSTRUCTNAME;
    } DUMMYUNIONNAME;
    DWORD     dwPageSize;
    LPVOID    lpMinimumApplicationAddress;
    LPVOID    lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD     dwNumberOfProcessors;
    DWORD     dwProcessorType;
    DWORD     dwAllocationGranularity;
    WORD      wProcessorLevel;
    WORD      wProcessorRevision;
} SYSTEM_INFO, *LPSYSTEM_INFO;
__declspec(dllimport) void __stdcall GetSystemInfo(LPSYSTEM_INFO lpSystemInfo);
__declspec(dllimport) CHAR * __stdcall GetCommandLineA(void);
__declspec(dllimport) BOOL __stdcall CloseHandle(HANDLE hObject);
__declspec(dllimport) __declspec(noreturn) void __stdcall ExitProcess(UINT uExitCode);

////// ALLOC
__declspec(dllimport) void * __stdcall GlobalAlloc(UINT uFlags,SIZE_T dwBytes);
#define GMEM_FIXED 0x0000
__declspec(dllimport) LPVOID __stdcall VirtualAlloc(LPVOID lpAddress, SIZE_T dwSize, DWORD flAllocationType, DWORD flProtect);

__declspec(dllimport) BOOL __stdcall VirtualProtect(LPVOID lpAddress, SIZE_T dwSize, DWORD flNewProtect,PDWORD lpflOldProtect);

__declspec(dllimport) BOOL __stdcall VirtualFree(LPVOID lpAddress,SIZE_T dwSize,DWORD dwFreeType);

#define MEM_COMMIT     0x00001000
#define MEM_RESERVE    0x00002000
#define PAGE_READWRITE 0x04
#define PAGE_EXECUTE   0x10
#define PAGE_EXECUTE_READ 0x20
#define PAGE_EXECUTE_READWRITE 0x40

////// ERROR
__declspec(dllimport) void __stdcall SetLastError(DWORD dwErrCode);
__declspec(dllimport) DWORD __stdcall GetLastError(void);
#define ERROR_SUCCESS 0
#define ERROR_INVALID_FUNCTION 1
#define ERROR_FILE_NOT_FOUND 2
#define ERROR_ACCESS_DENIED 5
#define ERROR_INVALID_HANDLE 6
#define ERROR_INVALID_PARAMETER 87

////// CONSOLE

__declspec(dllimport) HANDLE __stdcall GetStdHandle(DWORD nStdHandle);
#define INVALID_HANDLE_VALUE ((HANDLE)(LONG_PTR)-1)
#define STD_INPUT_HANDLE  (DWORD)-10
#define STD_OUTPUT_HANDLE (DWORD)-11
#define STD_ERROR_HANDLE  (DWORD)-12
//__declspec(dllimport) BOOL __stdcall AllocConsole(void);
//__declspec(dllimport) BOOL __stdcall AttachConsole(DWORD dwProcessId);
//#define ATTACH_PARENT_PROCESS (DWORD)-1
//__declspec(dllimport) BOOL __stdcall GetConsoleMode(HANDLE hConsoleHandle,LPDWORD lpMode);
//__declspec(dllimport) BOOL __stdcall SetConsoleMode(HANDLE hConsoleHandle,DWORD dwMode);

/////// FILE
typedef struct _SECURITY_ATTRIBUTES {
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    BOOL bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;
__declspec(dllimport) HANDLE __stdcall CreateFileA(char *FileName, DWORD dwDesiredAccess,DWORD dwShareMode,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD dwCreationDisposition,DWORD dwFlagsAndAttributes,HANDLE hTemplateFile);
#define FILE_SHARE_READ   0x00000001
#define FILE_SHARE_WRITE  0x00000002
#define FILE_SHARE_DELETE 0x00000004
#define CREATE_NEW 1
#define CREATE_ALWAYS 2
#define OPEN_EXISTING 3
#define OPEN_ALWAYS 4
#define TRUNCATE_EXISTING 5
#define FILE_ATTRIBUTE_NORMAL 128
#define GENERIC_READ                     (0x80000000L)
#define GENERIC_WRITE                    (0x40000000L)
#define GENERIC_EXECUTE                  (0x20000000L)
#define GENERIC_ALL                      (0x10000000L)

__declspec(dllimport) BOOL __stdcall GetFileSizeEx(HANDLE hFile, LARGE_INTEGER *lpFileSize);

typedef struct _OVERLAPPED {
    ULONG_PTR Internal;
    ULONG_PTR InternalHigh;
    union {
        struct {
            DWORD Offset;
            DWORD OffsetHigh;
        } ;
        PVOID Pointer;
    } ;
    
    HANDLE hEvent;
} OVERLAPPED, *LPOVERLAPPED;
__declspec(dllimport) BOOL __stdcall ReadFile(HANDLE hFile,LPVOID lpBuffer,DWORD nNumberOfBytesToRead,LPDWORD lpNumberOfBytesRead,LPOVERLAPPED lpOverlapped);

__declspec(dllimport) BOOL __stdcall WriteFile(HANDLE hFile,LPVOID lpBuffer,DWORD nNumberOfBytesToWrite,
        LPDWORD lpNumberOfBytesWritten,LPOVERLAPPED lpOverlapped);


typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME;

typedef struct _WIN32_FIND_DATAA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR cFileName[ 260 ];
    CHAR cAlternateFileName[ 14 ];
} WIN32_FIND_DATAA;

__declspec(dllimport) HANDLE __stdcall FindFirstFileA(char *lpFileName, WIN32_FIND_DATAA *lpFindFileData);
__declspec(dllimport) BOOL __stdcall FindNextFileA(HANDLE hFindFile, WIN32_FIND_DATAA *lpFindFileData);
__declspec(dllimport) BOOL __stdcall FindClose(HANDLE hFindFile);

typedef struct _SYSTEMTIME {
    WORD wYear;
    WORD wMonth;
    WORD wDayOfWeek;
    WORD wDay;
    WORD wHour;
    WORD wMinute;
    WORD wSecond;
    WORD wMilliseconds;
} SYSTEMTIME, *PSYSTEMTIME;


__declspec(dllimport) void GetSystemTime(PSYSTEMTIME lpSystemTime);
__declspec(dllimport) ULONGLONG GetTickCount64(void);
/////////////////////////////////////////////////////////////////////////////////////////////////////////

// File iterator (USAGE):  // @cleanup: make sure this code actually compiles
// for(struct os_file_iterator it = os_file_iterator_initialize("C:/*s");
//     os_file_iterator_valid(&it); os_file_iterator_next(&it)){
//     struct string file_name = os_file_iterator_get(&it);
//     print(".*s\n", file_name.size, file_name.data);
// }

struct os_file_iterator{
    WIN32_FIND_DATAA find_data;
    HANDLE handle;
};

struct os_file_iterator os_file_iterator_initialize(char *search_string){
    struct os_file_iterator ret = zero_struct;
    ret.handle = FindFirstFileA(search_string, &ret.find_data);
    return ret;
}

// returns a file_iterator that will return 'false' on 'os_file_iterator_valid'
// and can later be initialized by just overwriting it
struct os_file_iterator os_file_iterator_invalid(void){
    struct os_file_iterator ret = zero_struct;
    ret.handle = INVALID_HANDLE_VALUE;
    return ret;
}

b32 os_file_iterator_valid(struct os_file_iterator *iterator){
    return (iterator->handle != INVALID_HANDLE_VALUE);
}

struct string os_file_iterator_get(struct os_file_iterator *iterator){
    return string_from_cstring(iterator->find_data.cFileName);
}

void os_file_iterator_free(struct os_file_iterator *iterator){
    CloseHandle(iterator->handle); // @cleanup: "When the search handle is no longer needed, close it by using the FindClose function, not CloseHandle."
    iterator->handle = INVALID_HANDLE_VALUE;
}

// @cleanup: should maybe return an 'os_file' structure not sure
// returns the file_name, i.e 'file' for 'C:/path/to/file'
void os_file_iterator_next(struct os_file_iterator *iterator){
    if(!FindNextFileA(iterator->handle, &iterator->find_data)){
        os_file_iterator_free(iterator);
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////

__declspec(dllimport) DWORD GetFileAttributesA(char *lpFileName);


__declspec(dllimport) BOOL __stdcall IsDebuggerPresent(void);
__declspec(dllimport) void __stdcall DebugBreak(void);

typedef DWORD (__stdcall *LPTHREAD_START_ROUTINE)(LPVOID lpThreadParameter);
__declspec(dllimport) HANDLE __stdcall CreateThread(LPSECURITY_ATTRIBUTES lpThreadAttributes, SIZE_T dwStackSize, LPTHREAD_START_ROUTINE lpStartAddress,LPVOID lpParameter, DWORD dwCreationFlags,LPDWORD lpThreadId);



typedef __int64 ( __stdcall *FARPROC)(void);
__declspec(dllimport) FARPROC __stdcall GetProcAddress(HMODULE hModule, char *lpProcName);

__declspec(dllimport) HMODULE __stdcall LoadLibraryA(char * lpLibFileName);


///////// THREADING

__declspec(dllimport) DWORD __stdcall GetCurrentThreadId(void);
#define INFINITE            0xFFFFFFFF  // Infinite timeout
__declspec(dllimport) DWORD __stdcall WaitForSingleObject(HANDLE hHandle,DWORD dwMilliseconds);
__declspec(dllimport) DWORD __stdcall WaitForMultipleObjects(DWORD nCount,HANDLE * lpHandles,BOOL bWaitAll,DWORD dwMilliseconds);
__declspec(dllimport) HANDLE __stdcall CreateEventA(LPSECURITY_ATTRIBUTES lpEventAttributes, BOOL bManualReset, BOOL bInitialState, LPCSTR lpName);
__declspec(dllimport) BOOL __stdcall SetEvent(HANDLE hEvent);
__declspec(dllimport) BOOL __stdcall ResetEvent(HANDLE hEvent);

__declspec(dllimport) HANDLE __stdcall CreateSemaphoreA(LPSECURITY_ATTRIBUTES lpSemaphoreAttributes,LONG lInitialCount,LONG lMaximumCount,LPCSTR lpName);
__declspec(dllimport) BOOL __stdcall ReleaseSemaphore(HANDLE hSemaphore,LONG lReleaseCount,LPLONG lpPreviousCount);

///// EXCEPTIONS
typedef struct _RUNTIME_FUNCTION {
    DWORD BeginAddress;
    DWORD EndAddress;
    union {
        DWORD UnwindInfoAddress;
        DWORD UnwindData;
    };
} RUNTIME_FUNCTION;
__declspec(dllimport) BOOLEAN __cdecl RtlAddFunctionTable(RUNTIME_FUNCTION *FunctionTable,DWORD EntryCount,DWORD64 BaseAddress);

typedef enum _EXCEPTION_DISPOSITION
{
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind
} EXCEPTION_DISPOSITION;

typedef struct _EXCEPTION_RECORD {
    DWORD ExceptionCode;
    DWORD ExceptionFlags;
    struct _EXCEPTION_RECORD *ExceptionRecord;
    PVOID ExceptionAddress;
    DWORD NumberParameters;
    ULONG_PTR ExceptionInformation[15];
} EXCEPTION_RECORD, *PEXCEPTION_RECORD;

#if 0
#define UNW_FLAG_EHANDLER  0x01
#define UNW_FLAG_UHANDLER  0x02
#define UNW_FLAG_CHAININFO 0x04

typedef EXCEPTION_DISPOSITION __stdcall EXCEPTION_ROUTINE(struct _EXCEPTION_RECORD *ExceptionRecord,PVOID EstablisherFrame, struct _CONTEXT *ContextRecord,PVOID DispatcherContext);

typedef EXCEPTION_ROUTINE *PEXCEPTION_ROUTINE;
#endif


typedef struct __declspec(align(16)) _M128A {
    unsigned __int64 Low;
    __int64 High;
} M128A, *PM128A;

typedef struct __declspec(align(16)) _XSAVE_FORMAT {
    WORD ControlWord;
    WORD StatusWord;
    BYTE TagWord;
    BYTE Reserved1;
    WORD ErrorOpcode;
    DWORD ErrorOffset;
    WORD ErrorSelector;
    WORD Reserved2;
    DWORD DataOffset;
    WORD DataSelector;
    WORD Reserved3;
    DWORD MxCsr;
    DWORD MxCsr_Mask;
    M128A FloatRegisters[8];
    
    M128A XmmRegisters[16];
    BYTE Reserved4[96];
    
} XSAVE_FORMAT, *PXSAVE_FORMAT;

typedef struct __declspec(align(16)) _CONTEXT {
    
    DWORD64 P1Home;
    DWORD64 P2Home;
    DWORD64 P3Home;
    DWORD64 P4Home;
    DWORD64 P5Home;
    DWORD64 P6Home;
    
    DWORD ContextFlags;
    DWORD MxCsr;
    
    WORD SegCs;
    WORD SegDs;
    WORD SegEs;
    WORD SegFs;
    WORD SegGs;
    WORD SegSs;
    DWORD EFlags;
    
    DWORD64 Dr0;
    DWORD64 Dr1;
    DWORD64 Dr2;
    DWORD64 Dr3;
    DWORD64 Dr6;
    DWORD64 Dr7;
    
    DWORD64 Rax;
    DWORD64 Rcx;
    DWORD64 Rdx;
    DWORD64 Rbx;
    DWORD64 Rsp;
    DWORD64 Rbp;
    DWORD64 Rsi;
    DWORD64 Rdi;
    DWORD64 R8;
    DWORD64 R9;
    DWORD64 R10;
    DWORD64 R11;
    DWORD64 R12;
    DWORD64 R13;
    DWORD64 R14;
    DWORD64 R15;
    
    DWORD64 Rip;
    
    union {
        XSAVE_FORMAT FltSave;
        struct {
            M128A Header[2];
            M128A Legacy[8];
            M128A Xmm0;
            M128A Xmm1;
            M128A Xmm2;
            M128A Xmm3;
            M128A Xmm4;
            M128A Xmm5;
            M128A Xmm6;
            M128A Xmm7;
            M128A Xmm8;
            M128A Xmm9;
            M128A Xmm10;
            M128A Xmm11;
            M128A Xmm12;
            M128A Xmm13;
            M128A Xmm14;
            M128A Xmm15;
        } ;
    } ;
    
    M128A VectorRegister[26];
    DWORD64 VectorControl;
    
    DWORD64 DebugControl;
    DWORD64 LastBranchToRip;
    DWORD64 LastBranchFromRip;
    DWORD64 LastExceptionToRip;
    DWORD64 LastExceptionFromRip;
} CONTEXT, *PCONTEXT;


typedef struct _EXCEPTION_POINTERS {
    PEXCEPTION_RECORD ExceptionRecord;
    PCONTEXT ContextRecord;
} EXCEPTION_POINTERS, *PEXCEPTION_POINTERS;


typedef LONG (__stdcall *LPTOP_LEVEL_EXCEPTION_FILTER)(struct _EXCEPTION_POINTERS *ExceptionInfo);
__declspec(dllimport) LPTOP_LEVEL_EXCEPTION_FILTER __stdcall SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER lpTopLevelExceptionFilter);

//////////////////////

#endif // #else of if 0 #include <windows.h>

struct os_thread_info{
    smm id; // @cleanup: how is this initialized?
    void *handle;
};

typedef u32 (__stdcall *os_thread_proc)(void *param);

static struct os_thread_info os_create_thread(os_thread_proc proc, void *data){
    struct os_thread_info info = zero_struct;
    
    info.handle = CreateThread(0, 0, cast(LPTHREAD_START_ROUTINE)proc, data, 0, (unsigned long *)&info.id);
    return info;
}

void __debugbreak(void);

static void os_debug_break(void){
    if(IsDebuggerPresent()){
        __debugbreak();
    }
}

static f64 os_get_time_in_seconds(void){
    LARGE_INTEGER performance_frequency;
    QueryPerformanceFrequency(&performance_frequency);
    
    LARGE_INTEGER time;
    QueryPerformanceCounter(&time);
    
    return (f64)time.QuadPart / (f64)performance_frequency.QuadPart;
}

static struct os_virtual_buffer os_reserve_memory(void *_desired_base, smm reserve_size){
    u8 *desired_base = _desired_base;
    
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info); // this cannot fail apperantly
    
    u64 granularity = system_info.dwAllocationGranularity;
    u64 page_size = system_info.dwPageSize;
    
    assert(desired_base == 0 || ((desired_base > cast(u8 *)system_info.lpMinimumApplicationAddress) && (desired_base < cast(u8 *)system_info.lpMaximumApplicationAddress)));
    assert(is_power_of_two(granularity));
    assert(is_power_of_two(page_size));
    
    DWORD allocation_flags = PAGE_READWRITE;
    
    void *mem1 = desired_base;
    if(reserve_size){
        if(!desired_base){
            // If the lpAddress parameter is NULL, this value is rounded up to the next page boundary.
            smm rest = reserve_size & ((smm)page_size - 1);
            reserve_size     = (reserve_size - rest) + (rest ? (smm)page_size : 0);
        }else{
            
            // If the memory is being reserved, the specified address is rounded down to the nearest multiple of the allocation granularity.
            desired_base -=  (granularity - 1) & (umm)desired_base;
        }
        
        mem1 = VirtualAlloc(desired_base, (SIZE_T)reserve_size, MEM_RESERVE, allocation_flags);
        if(!mem1){
            struct os_virtual_buffer ret = zero_struct;
            return ret;
        }
    }
    
    struct os_virtual_buffer virtual_buffer;
    virtual_buffer.base     = mem1;
    virtual_buffer.committed = 0;
    virtual_buffer.reserved = reserve_size;
    
    return virtual_buffer;
}


// one can pass any of these as 0 to not do that part
static struct os_virtual_buffer os_commit_memory(void *_desired_base, smm commit_size){
    u8 *desired_base = _desired_base;
    
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info); // this cannot fail apperantly
    
    u64 granularity = system_info.dwAllocationGranularity;
    u64 page_size = system_info.dwPageSize;
    
    assert(desired_base == 0 || ((desired_base > cast(u8 *)system_info.lpMinimumApplicationAddress) && (desired_base < cast(u8 *)system_info.lpMaximumApplicationAddress)));
    assert(is_power_of_two(granularity));
    assert(is_power_of_two(page_size));
    
    DWORD allocation_flags = PAGE_READWRITE;
    
    void *mem2 = VirtualAlloc(desired_base, (SIZE_T)commit_size, MEM_COMMIT, allocation_flags);
    if(!mem2){
        struct os_virtual_buffer ret = zero_struct;
        return ret;
    }
    
    struct os_virtual_buffer virtual_buffer;
    virtual_buffer.base     = mem2;
    virtual_buffer.committed = commit_size;
    virtual_buffer.reserved = 0; // @cleanup: this is kinda terrible
    
    return virtual_buffer;
}


static void os_free_memory(void *memory_to_free){
    DWORD MEM_RELEASE = 0x00008000;
    VirtualFree(memory_to_free, 0, MEM_RELEASE);
}

///////////////////////////////////////////////////////////////////////////

__int64 _InterlockedCompareExchange64(__int64 volatile *_Destination, __int64 Exchange, __int64 Comparand);
unsigned char _InterlockedCompareExchange128(__int64 volatile * _Destination, __int64 _ExchangeHigh, __int64 _ExchangeLow, __int64 * _ComparandResult);
__int64 _InterlockedIncrement64(__int64 volatile * _Addend);
__int64 _InterlockedDecrement64(__int64 volatile * _Addend);
__int64 _InterlockedIncrement64(__int64 volatile * _Addend);
__int64 _InterlockedDecrement64(__int64 volatile * _Addend);
__int64 _InterlockedExchangeAdd64(__int64 volatile * _Addend, __int64 _Value);

// @note: return the initial value
func s64 atomic_add(s64 *val, s64 to_add){
    return _InterlockedExchangeAdd64(val, to_add);
}

//func s64 atomic_subtract(s64 *val, s64 to_add)
//{
//return _InterlockedExchangeSub64(val, to_add);
//}

func void *atomic_compare_and_swap(void *dest, void *source, void *comparand){
    return (void *)_InterlockedCompareExchange64((s64 *)dest, (s64)source, (s64)comparand);
}

func smm atomic_compare_and_swap_smm(smm *dest, smm source, smm comparand){
    return _InterlockedCompareExchange64((s64 *)dest, (s64)source, (s64)comparand);
}

// returns 1 on success
// returns 0 on fail and overrides *comparand with *dest
func b32 atomic_compare_and_swap_128(m128 *dest, m128 source, m128 *comparand){
    assert(((umm)dest & 15) == 0);
    return _InterlockedCompareExchange128((__int64 *)dest, source.ptr2, source.ptr1, (__int64 *)comparand);
}

// implements 'i++;'
func s64 atomic_postincrement(s64 *val){
    return _InterlockedIncrement64(cast(long long *)val) - 1;
}

// implements 'i--;'
func s64 atomic_postdecrement(s64 *val){
    return _InterlockedDecrement64(cast(long long *)val) + 1;
}

// implements '++i;'
func s64 atomic_preincrement(s64 *val){
    return _InterlockedIncrement64(cast(long long *)val);
}

// implements '--i;'
func s64 atomic_predecrement(s64 *val){
    return _InterlockedDecrement64(cast(long long *)val);
}


// implements '++i;'
func u32 u32_atomic_preincrement(u32 *val){
    return _InterlockedIncrement(cast(long *)val);
}

///////////////////////////////////////////////////////////////////////////

#define FUZZ_ME_BUFFER_SIZE mega_bytes(16)

#ifdef FUZZING
__declspec(dllexport) u8 fuzz_me_buffer[FUZZ_ME_BUFFER_SIZE];

static struct os_file os_load_file(char *file_name, void *buffer, smm buffer_size){
    struct os_file result = {.data = buffer, .size = 0};
    static b32 not_the_first_time;
    if(not_the_first_time) return result;
    
    result.size = FUZZ_ME_BUFFER_SIZE;
    for(int i = 0; i < FUZZ_ME_BUFFER_SIZE; i++){
        if(!fuzz_me_buffer[i]){
            result.size = i;
            break;
        }
    }
    
    if(!buffer_size) return result; // just say the file does always exist
    
    assert(buffer_size >= result.size);
    memcpy(buffer, fuzz_me_buffer, result.size);
    not_the_first_time = true;
    result.memory = buffer;
    
    return result;
}
#else
// @hmm maybe we should have a unified os_memory_range_return, that has error information.
// filename needs to be zero_terminated
// you can pass in a zero buffer to find out the size

typedef struct _BY_HANDLE_FILE_INFORMATION {
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    dwVolumeSerialNumber;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
    DWORD    nNumberOfLinks;
    DWORD    nFileIndexHigh;
    DWORD    nFileIndexLow;
} BY_HANDLE_FILE_INFORMATION;

__declspec(dllimport)
BOOL GetFileInformationByHandle(HANDLE hFile, BY_HANDLE_FILE_INFORMATION *lpFileInformation);

u64 file_time_to_percise_unix_time(FILETIME *ft){
    ULARGE_INTEGER ull;
    ull.LowPart  = ft->dwLowDateTime;
    ull.HighPart = ft->dwHighDateTime;
    return ull.QuadPart - 116444736000000000ULL;
}

static struct os_file os_load_file(char *file_name, void *buffer, smm buffer_size){
    struct os_file result = zero_struct;
    
    // @cleanup this should do the W-thing
    HANDLE file_handle = CreateFileA(file_name, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if(file_handle == INVALID_HANDLE_VALUE){
        result.file_does_not_exist = true;
        return result;
    }
    
    BY_HANDLE_FILE_INFORMATION file_information;
    if(!GetFileInformationByHandle(file_handle, &file_information)){
        CloseHandle(file_handle);
        // @reserche, when can this happen?
        return result;
    }
    
    LARGE_INTEGER _file_size = { 
        .LowPart = file_information.nFileSizeLow, 
        .HighPart = file_information.nFileSizeHigh,
    };
    
    s64 file_size = _file_size.QuadPart;
    result.size   = cast(u64) file_size;
    result.access_time       = file_time_to_percise_unix_time(&file_information.ftLastAccessTime);
    result.creation_time     = file_time_to_percise_unix_time(&file_information.ftCreationTime);
    result.modification_time = file_time_to_percise_unix_time(&file_information.ftLastWriteTime);
    
    if(file_size > buffer_size){
        CloseHandle(file_handle);
        return result; // the result.size is set
    }
    
    DWORD bytes_read;
    if(file_size && (!ReadFile(file_handle, buffer, cast(DWORD) file_size, &bytes_read, 0) || file_size != bytes_read)){
        CloseHandle(file_handle);
        return result;
    }
    
    CloseHandle(file_handle);
    
    result.size   = (u64)file_size;
    result.memory = buffer;
    
    return result;
}
#endif

func b32 path_is_directory(char *path){
    u32 INVALID_FILE_ATTRIBUTES = u32_max;
    u32 FILE_ATTRIBUTE_DIRECTORY = 0x10;
    u32 file_attributes = GetFileAttributesA(path);
    return (file_attributes != INVALID_FILE_ATTRIBUTES) && (file_attributes & FILE_ATTRIBUTE_DIRECTORY);
}

// @cleanup: for now file_name has to be zero terminated maybe we can alloca or something
static b32 os_write_file(char *file_name, void *buffer, smm buffer_size){
    HANDLE file_handle = CreateFileA(file_name, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, NULL);
    if (file_handle == INVALID_HANDLE_VALUE) return false;
    
    // ignore FILE_ALLREADY_EXISTS
    if(GetLastError() == 183) SetLastError(0);
    
    b32 result = false;
    DWORD bytes_written;
    if (WriteFile(file_handle, buffer, save_truncate_smm_to_u32(buffer_size), &bytes_written, 0) && (bytes_written == buffer_size)){
        result = true;
    }
    
    CloseHandle(file_handle);
    
    //SetLastError(0);
    return result;
}


static __declspec(noreturn) void os_panic(u32 exit_code){
    ExitProcess(exit_code);
}

static u32 os_print_string(char *string, smm length){
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
#if 0
    if(stdout == INVALID_HANDLE_VALUE){
        win32_init_console();
        stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    }
#endif
    
    DWORD chars_written;
    if(length > 0 && length < u32_max){
        WriteFile(stdout, string, (u32)length, &chars_written, 0);
    }else{
        return 0;
    }
    
    return chars_written;
}


struct parsed_command_line{
    int argc;
    char **argv;
};

// arguments:
//    'command_line'      = the unprocessed command line received by calling 'GetCommandLineA'
//    'command_line_size' = the size of the command line, not including the zero_terminator
//    'out_buffer'        = a buffer that is at least as long as the command line,
//                          which will recieve the processed command line as serial zero_terminated strings
//     the return value is the amount of arguments we got.

func smm windows_parse_command_line__internal(char *command_line, smm command_line_size, char *out_buffer){
    
    // According to msdn:
    //   1) Arguments are delimited by white space, which is either a space or tab.
    //   2) The first argument must be valid and is the program name. Can be in quotes.
    //      Everything else does not apply.
    //   3) Quotes give rise to arguements that conain spaces.
    //      Double quotes ("") in qotes give rise to a single quote.
    //      If the command line ends before ending the last argument then all character read so far are
    //      the last argument
    //   4) \" is just ".
    //   5) \ is just \ if it does not precedes a ".
    //   6) if there are an even number of \ preceeding ", then they get halved and the " is a delimiter
    //   7) if there are an add number of \ preceeding ", then they get halved and the last \"  is just ".
    
    
    // Examples:                      argv[1]       argv[2]          argv[3]
    //   1) "abc"      d       e   ->  abc            d                e
    //   2) a\\b     d"e f"g   h   ->  a\\b         de fg              h
    //   3) a\\\"b     c       d   ->  a\"b           c                d
    //   4) a\\\\"b c"  d          ->  a\\           b c               d
    
    char *at = out_buffer;
    
    // if 'in_quotes' we are also 'in_argument'
    b32 in_quotes     = false;
    b32 in_argument   = false;
    
    smm amount_of_arguments = 0;
    
    for(smm i = 0; i < command_line_size; ){
        // @note: accsessing command_line[i + 1] is save because of zero termination
        assert(!in_quotes || in_argument);
        
        if(command_line[i] == '\\'){
            if(!in_argument){
                in_argument = true;
                amount_of_arguments++;
            }
            
            smm amount_of_slashes = 0;
            for(; i < command_line_size; i++){
                if(command_line[i] != '\\') break;
                amount_of_slashes += 1;
            }
            
            if(command_line[i] == '"'){
                // emit one slash for every pair of slashes
                for(smm s = 0; s < amount_of_slashes/2; s++) *at++ = '\\';
                
                if(amount_of_slashes & 1){
                    *at++ = '"'; // it was escaped
                    i++; // eat the '"'
                }else{
                    continue;
                }
            }else{
                // just emit all the slashes
                for(smm s = 0; s < amount_of_slashes; s++) *at++ = '\\';
            }
        }else if(command_line[i] == '"'){
            if(!in_quotes){
                i++; // skip the '"'
                in_quotes = true;
                
                if(!in_argument){
                    in_argument = true; // if the argument started with quotes ("asd"bcd -> asdbcd)
                    amount_of_arguments++;
                }
            }else{
                if(command_line[i + 1] == '"'){
                    i += 2;
                    *at++ = '"';
                }else{
                    i++;
                    in_quotes = false;
                }
            }
        }else if(u8_is_whitespace_or_newline((u8)command_line[i])){
            if(in_quotes){
                *at++ = command_line[i];
            }else if(in_argument){
                in_argument = false; // end the argument
                *at++ = 0;
            }else{
                // do nothing we are currently in whitespace
            }
            i++;
        }else{
            *at++ = command_line[i]; // always just output the character
            i++;
            if(!in_argument){
                in_argument = true;
                amount_of_arguments++;
            }
        }
    }
    *at++ = 0; // zero terminate
    
    return amount_of_arguments;
}

func struct parsed_command_line windows_parse_command_line(char *command_line){
    smm command_line_size = cstring_length(command_line);

    char *preped_command_line = (char *)GlobalAlloc(GMEM_FIXED, command_line_size + 1);
    
    smm amount_of_arguments = windows_parse_command_line__internal(
                                  command_line, command_line_size, preped_command_line);
    
    char **argv = (char **)GlobalAlloc(GMEM_FIXED, (amount_of_arguments + 1) * sizeof(char *));

    
    char *at = preped_command_line;
    for(smm i = 0; i < amount_of_arguments; i++){
        argv[i] = at;
        while(*at++); // skip to past the next zero_terminator
    }
    argv[amount_of_arguments] = 0;
    
    struct parsed_command_line ret;
    ret.argc = (int)amount_of_arguments;
    ret.argv = argv;
    return ret;
}


int main(int argument_count, char **argument_values);

__declspec(noreturn) void _start(void) {
    CHAR * command_line = GetCommandLineA();
    
    struct parsed_command_line parsed_command_line = windows_parse_command_line(command_line);
    
    int exit_code = main(parsed_command_line.argc, parsed_command_line.argv);
    ExitProcess((u32)exit_code);
}

