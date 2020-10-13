#undef proc

#if 0
#include <windows.h>
#else

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
    } ;
    struct {
        DWORD LowPart;
        LONG HighPart;
    } u;
    LONGLONG QuadPart;
} LARGE_INTEGER;


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

__declspec(dllimport) DWORD GetFileAttributesA(char *lpFileName);
__declspec(dllimport) HANDLE __stdcall FindFirstFileA(char *lpFileName, WIN32_FIND_DATAA *lpFindFileData);
__declspec(dllimport) BOOL __stdcall FindNextFileA(HANDLE hFindFile, WIN32_FIND_DATAA *lpFindFileData);
__declspec(dllimport) BOOL __stdcall FindClose(HANDLE hFindFile);

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
    smm id;
    void *handle;
};

typedef u32 (__stdcall *os_thread_proc)(void *param);

static_assert(sizeof(u32) == sizeof(unsigned long));

static struct os_thread_info os_create_thread(b32 run_immediatly, os_thread_proc proc, void *data){
    struct os_thread_info info;
    
    info.handle = CreateThread(0, 0, cast(LPTHREAD_START_ROUTINE)proc, data, run_immediatly ? 0 : 0x4, (unsigned long *)&info.id);
    return info;
}

static void os_debug_break(void){
    if(IsDebuggerPresent()){
        DebugBreak();
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
    
    DWORD allocation_flags = PAGE_EXECUTE_READWRITE;
    
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
        
        // @cleanup : remove PAGE_EXECUTE
        mem1 = VirtualAlloc(desired_base, (SIZE_T)reserve_size, MEM_RESERVE, allocation_flags);
        if(!mem1){
            struct os_virtual_buffer ret = zero_struct;
            return ret;
        }
    }
    
    struct os_virtual_buffer virtual_buffer;
    virtual_buffer.base     = mem1;
    virtual_buffer.commited = 0;
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
    
    DWORD allocation_flags = PAGE_EXECUTE_READWRITE;
    
    // @cleanup : remove PAGE_EXECUTE
    void *mem2 = VirtualAlloc(desired_base, (SIZE_T)commit_size, MEM_COMMIT, allocation_flags);
    if(!mem2){
        struct os_virtual_buffer ret = zero_struct;
        return ret;
    }
    
    struct os_virtual_buffer virtual_buffer;
    virtual_buffer.base     = mem2;
    virtual_buffer.commited = commit_size;
    virtual_buffer.reserved = 0;
    
    return virtual_buffer;
}


static void os_free_memory(void *memory_to_free){
    DWORD MEM_RELEASE = 0x00008000;
    VirtualFree(memory_to_free, 0, MEM_RELEASE);
}

#if 0
static void win32_init_console(void){
    // @warning:@warning:@warning:@warning:@warning:@warning:@warning:
    // @note: This whole thing did not seem to work for now. We are now compiling with
    // the linker switch /SUBSYSTEM:console instead of /SUBSYSTEM:windows
    // this make us always be passed a console handle (???) but we should eventually make this work
    // with /SUBSYSTEM:windows as well, if we want to make this into more of a _library_.
    
    SetLastError(0); // do we need this?
    if(!AttachConsole(ATTACH_PARENT_PROCESS)){
        
        DWORD error = GetLastError();
        switch(error){
            case ERROR_SUCCESS:
            {
                // we should never get here
            }break;
            
            case ERROR_ACCESS_DENIED:
            {
                // we allready have a console.
                
            }break;
            case ERROR_INVALID_HANDLE:
            {
                // the parent process does not have a console
                AllocConsole();
            }break;
            case ERROR_INVALID_PARAMETER:
            {
                // parent process does not exist, should never happen
                AllocConsole();
            }break;
            default:
            {
                // not sure....
                AllocConsole();
            }
            
            //invalid_default_case;
        }
        
        SetLastError(0);
        
    }
    
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if(!stdout || stdout == INVALID_HANDLE_VALUE || GetLastError()){
        os_panic(GetLastError());
    }
    
    
    DWORD old_console_mode;
    if(GetConsoleMode(stdout, &old_console_mode)){
        // we got the console mode
        // @Incomplete: we should reset this in the end maybe?
        DWORD new_console_mode = 0; //old_console_mode | ENABLE_PROCESSED_OUTPUT | ENABLE_WRAP_AT_EOL_OUTPUT;
        if(SetConsoleMode(stdout, new_console_mode) == 0){
            os_panic(GetLastError());
        }
    }else{
        SetLastError(0);
    }
}
#endif

// @hmm maybe we should have a unified os_memory_range_return, that has error information.
// filename needs to be zero_terminated
// you can pass in a zero buffer to find out the size
static struct os_file os_load_file(char *file_name, void *buffer, umem buffer_size){
    struct os_file result = zero_struct;
    
    // @cleanup this should do the W-thing
    HANDLE file_handle = CreateFileA(file_name, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if(file_handle == INVALID_HANDLE_VALUE){
        result.file_does_not_exist = true;
        return result;
    }
    
    LARGE_INTEGER file_size_return;
    if(!GetFileSizeEx(file_handle, &file_size_return)){
        CloseHandle(file_handle);
        // @reserche, when can this happen?
        return result;
    }
    s64 file_size = file_size_return.QuadPart;
    result.size   = cast(u64) file_size;
    
    if((u64)file_size > buffer_size){
        CloseHandle(file_handle);
        return result; // the result.size is set
    }
    
    DWORD bytes_read;
    if(!ReadFile(file_handle, buffer, cast(DWORD) file_size, &bytes_read, 0) || file_size != bytes_read){
        CloseHandle(file_handle);
        return result;
    }
    
    CloseHandle(file_handle);
    
    result.size   = (u64)file_size;
    result.memory = buffer;
    
    return result;
}

static struct os_file os_load_stdin(void *buffer, umem buffer_size){
    struct os_file result = zero_struct;
    
    HANDLE file_handle = GetStdHandle(STD_INPUT_HANDLE);
    // @cleanup this should do the W-thing
    
    LARGE_INTEGER file_size_return;
    if(!GetFileSizeEx(file_handle, &file_size_return)){
        return result;
    }
    s64 file_size = file_size_return.QuadPart;
    result.size   = cast(u64) file_size;
    
    if((u64)file_size > buffer_size){
        return result; // the result.size is set
    }
    
    DWORD bytes_read;
    if(!ReadFile(file_handle, buffer, cast(DWORD) file_size, &bytes_read, 0) || file_size != bytes_read){
        return result;
    }
    
    result.size   = (u64)file_size;
    result.memory = buffer;
    
    return result;
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


static b32 os_append_to_file(char *file_name, void *buffer, smm buffer_size){
    u32 FILE_APPEND_DATA = 4;
    HANDLE file_handle = CreateFileA(file_name, FILE_APPEND_DATA, 0, 0, OPEN_EXISTING, 0, NULL);
    if (file_handle == INVALID_HANDLE_VALUE) return false;
    
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


// @note: credit goes to http://alter.org.ua/docs/win/args/
#if 0
PWCHAR* CommandLineToArgvW(PWCHAR CmdLine, int* _argc)
{
    PWCHAR* argv;
    PWCHAR  _argv;
    ULONG   len;
    ULONG   argc;
    WCHAR   a;
    ULONG   i, j;
    
    BOOLEAN  in_QM;
    BOOLEAN  in_TEXT;
    BOOLEAN  in_SPACE;
    
    len = wcslen(CmdLine);
    i = ((len+2)/2)*sizeof(PVOID) + sizeof(PVOID);
    
    argv = (PWCHAR*)GlobalAlloc(GMEM_FIXED,
                                i + (len+2)*sizeof(WCHAR));
    
    _argv = (PWCHAR)(((PUCHAR)argv)+i);
    
    argc = 0;
    argv[argc] = _argv;
    in_QM = FALSE;
    in_TEXT = FALSE;
    in_SPACE = TRUE;
    i = 0;
    j = 0;
    
    while( a = CmdLine[i] ) {
        if(in_QM) {
            if(a == '\"') {
                in_QM = FALSE;
            } else {
                _argv[j] = a;
                j++;
            }
        } else {
            switch(a) {
                case '\"':
                in_QM = TRUE;
                in_TEXT = TRUE;
                if(in_SPACE) {
                    argv[argc] = _argv+j;
                    argc++;
                }
                in_SPACE = FALSE;
                break;
                case ' ':
                case '\t':
                case '\n':
                case '\r':
                if(in_TEXT) {
                    _argv[j] = '\0';
                    j++;
                }
                in_TEXT = FALSE;
                in_SPACE = TRUE;
                break;
                default:
                in_TEXT = TRUE;
                if(in_SPACE) {
                    argv[argc] = _argv+j;
                    argc++;
                }
                _argv[j] = a;
                j++;
                in_SPACE = FALSE;
                break;
            }
        }
        i++;
    }
    _argv[j] = '\0';
    argv[argc] = NULL;
    
    (*_argc) = argc;
    return argv;
}
#endif

static CHAR** CommandLineToArgvA(CHAR *CmdLine, int* _argc)
{
    CHAR** argv;
    CHAR* _argv;
    ULONG   len;
    ULONG   argc;
    CHAR   a;
    ULONG   i, j;
    
    BOOLEAN  in_QM;
    BOOLEAN  in_TEXT;
    BOOLEAN  in_SPACE;
    
    len = (ULONG)cstring_length(CmdLine);
    i = ((len+2)/2)*sizeof(PVOID) + sizeof(PVOID);
    
    argv = (CHAR **)GlobalAlloc(GMEM_FIXED,
                                i + (len+2)*sizeof(CHAR) + 0x1000);
    
    _argv = (CHAR *)(((UCHAR *) argv)+i);
    
    argc = 0;
    argv[argc] = _argv;
    in_QM = false;
    in_TEXT = false;
    in_SPACE = true;
    i = 0;
    j = 0;
    
    while(true) {
        a = CmdLine[i];
        if(!a) break;
        if(in_QM) {
            if(a == '\"') {
                in_QM = false;
            } else {
                _argv[j] = a;
                j++;
            }
        } else {
            switch(a) {
                case '\"':
                in_QM = true;
                in_TEXT = true;
                if(in_SPACE) {
                    argv[argc] = _argv+j;
                    argc++;
                }
                in_SPACE = false;
                break;
                case ' ':
                case '\t':
                case '\n':
                case '\r':
                if(in_TEXT) {
                    _argv[j] = '\0';
                    j++;
                }
                in_TEXT = false;
                in_SPACE = true;
                break;
                default:
                in_TEXT = true;
                if(in_SPACE) {
                    argv[argc] = _argv+j;
                    argc++;
                }
                _argv[j] = a;
                j++;
                in_SPACE = false;
                break;
            }
        }
        i++;
        
    }
    _argv[j] = '\0';
    argv[argc] = NULL;
    
    (*_argc) = argc;
    return argv;
}

int main(int argument_count, char **argument_values);
__declspec(noreturn) void _start(void) {
    CHAR * command_line = GetCommandLineA();
    int num_args;
    char **args = CommandLineToArgvA(command_line, &num_args);
    
    int exit_code = main(num_args, args);
    ExitProcess((u32)exit_code);
}

