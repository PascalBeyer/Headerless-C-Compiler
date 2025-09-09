
#include "pre_main_common.c"

#pragma comment(lib, "kernel32")

static typedef unsigned short wchar_t;
static typedef void * HANDLE;
static typedef struct HINSTANCE__ *HINSTANCE;
static typedef HINSTANCE HMODULE;
static typedef wchar_t *LPSTR;
static typedef unsigned int DWORD;
static typedef unsigned short WORD;
static typedef unsigned char *LPBYTE;

struct _STARTUPINFOW {
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

__declspec(dllimport) LPSTR GetCommandLineW(void);
__declspec(dllimport) HMODULE GetModuleHandleW(LPSTR lpModuleName);
__declspec(dllimport) __declspec(noreturn) void ExitProcess(unsigned int uExitCode);
__declspec(dllimport) void GetStartupInfoW(struct _STARTUPINFOW *lpStartupInfo);

int wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd);

void _start(void){
    
    pre_main_common();
    
    struct _STARTUPINFOW StartupInfo;
    GetStartupInfoW(&StartupInfo);
    
    HINSTANCE hInstance = GetModuleHandleW(0); // @cleanup: __ImageBase?
    HINSTANCE hPrevInstance = 0; // "This parameter is always NULL."
    wchar_t *lpCmdLine = GetCommandLineW();
    int nShowCmd = (StartupInfo.dwFlags & /*STARTF_USESHOWWINDOW*/1) ? StartupInfo.wShowWindow : /*SW_SHOWDEFAULT*/10;
    
    int ExitCode = wWinMain(hInstance, hPrevInstance, lpCmdLine, nShowCmd);
    ExitProcess(ExitCode);
}
