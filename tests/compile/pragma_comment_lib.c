
#pragma comment(lib, "Shell32.lib")
#pragma comment(lib, "kernel32.lib")

typedef unsigned short wchar_t;

__declspec(dllimport) wchar_t **CommandLineToArgvW(wchar_t *lpCmdLine, int *pNumArgs);
__declspec(dllimport) wchar_t  *GetCommandLineW(void);

int main(){
    int NumArgs = 0;
    CommandLineToArgvW(GetCommandLineW(), &NumArgs);
    return NumArgs;
}
