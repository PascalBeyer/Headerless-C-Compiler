// reject "Warning"
// broken

#pragma comment(lib, "kernel32")

__declspec(dllimport) void ExitProcess(unsigned int uExitCode);
__declspec(dllimport) __declspec(noreturn) void ExitProcess(unsigned int uExitCode);

int _start(){
    ExitProcess(1337);
    // This should NOT warn for not returning a value (but right no (and for now) it does).
}
