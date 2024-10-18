
#pragma comment(lib, "kernel32")

static typedef void * HANDLE;
static typedef struct HINSTANCE__ *HINSTANCE;
static typedef HINSTANCE HMODULE;
static typedef char *LPSTR;
static typedef unsigned int DWORD;
static typedef unsigned short WORD;
static typedef unsigned char *LPBYTE;

struct _STARTUPINFOA {
    DWORD  cb;
    LPSTR  lpReserved;
    LPSTR  lpDesktop;
    LPSTR  lpTitle;
    DWORD  dwX;
    DWORD  dwY;
    DWORD  dwXSize;
    DWORD  dwYSize;
    DWORD  dwXCountChars;
    DWORD  dwYCountChars;
    DWORD  dwFillAttribute;
    DWORD  dwFlags;
    WORD   wShowWindow;
    WORD   cbReserved2;
    LPBYTE lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
};

__declspec(dllimport) LPSTR GetCommandLineA(void);
__declspec(dllimport) HMODULE GetModuleHandleA(LPSTR lpModuleName);
__declspec(dllimport) __declspec(noreturn) void ExitProcess(unsigned int uExitCode);
__declspec(dllimport) void GetStartupInfoA(struct _STARTUPINFOA *lpStartupInfo);

int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd);

void _start(void){
    struct _STARTUPINFOA StartupInfo;
    GetStartupInfoA(&StartupInfo);
    
    HINSTANCE hInstance = GetModuleHandleA(0); // @cleanup: __ImageBase?
    HINSTANCE hPrevInstance = 0; // "This parameter is always NULL."
    char *lpCmdLine = GetCommandLineA();
    int nShowCmd = (StartupInfo.dwFlags & /*STARTF_USESHOWWINDOW*/1) ? StartupInfo.wShowWindow : /*SW_SHOWDEFAULT*/10;
    
    int ExitCode = WinMain(hInstance, hPrevInstance, lpCmdLine, nShowCmd);
    ExitProcess(ExitCode);
}
