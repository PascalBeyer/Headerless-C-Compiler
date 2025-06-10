// run
// check "bug"
// check "main"
// check "tests\run\stacktrace.c:77"
// check "tests\run\stacktrace.c:86"

#include <windows.h>
#include <dbghelp.h>
#include <stdio.h>

#pragma comment(lib, "dbghelp.lib")

// Helper function to print the stack trace
void PrintStackTrace(CONTEXT *Context){
    
    HANDLE hProcess = GetCurrentProcess();
    HANDLE hThread = GetCurrentThread();
    
    // Initialize symbols
    SymInitialize(hProcess, NULL, TRUE);
    
    STACKFRAME64 StackFrame;
    memset(&StackFrame, 0, sizeof(STACKFRAME64));
    
    StackFrame.AddrPC.Offset = Context->Rip;
    StackFrame.AddrPC.Mode = AddrModeFlat;
    StackFrame.AddrFrame.Offset = Context->Rbp;
    StackFrame.AddrFrame.Mode = AddrModeFlat;
    StackFrame.AddrStack.Offset = Context->Rsp;
    StackFrame.AddrStack.Mode = AddrModeFlat;
    
    while (StackWalk64(IMAGE_FILE_MACHINE_AMD64, hProcess, hThread, &StackFrame, Context, NULL, SymFunctionTableAccess64, SymGetModuleBase64, NULL)) {
        if(StackFrame.AddrPC.Offset == 0) break;
        
        printf("%.16llx", StackFrame.AddrPC.Offset);
        
        char Buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)] = {0};
        PSYMBOL_INFO Symbol = (PSYMBOL_INFO)Buffer;
        Symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
        Symbol->MaxNameLen = MAX_SYM_NAME;
        
        char *SymbolName = "unknown";
        
        if(SymFromAddr(hProcess, StackFrame.AddrPC.Offset, NULL, Symbol)) SymbolName = Symbol->Name;
        
        printf("  %-30s", SymbolName);
        
        IMAGEHLP_LINE64 line = { .SizeOfStruct = sizeof(IMAGEHLP_LINE64) };
        
        DWORD displacement;
        if(SymGetLineFromAddr64(hProcess, StackFrame.AddrPC.Offset, &displacement, &line)){
            printf(" %s:%u", line.FileName, line.LineNumber);
        }
        printf("\n");
    }
    
    SymCleanup(hProcess);
}

LONG WINAPI _UnhandledExceptionFilter(EXCEPTION_POINTERS* exceptionInfo) {
    printf("Unhandled exception occurred!\n");
    
    // Print the stack trace.
    PrintStackTrace(exceptionInfo->ContextRecord);
    
    // Exit the process.
    ExitProcess(0);
    
    // This won't actually be reached because ExitProcess will terminate the process.
    return EXCEPTION_EXECUTE_HANDLER;
}

void bug(){
    
    // Cause an exception to test
    int* p = 0;
    *p = 42; // Access violation
    
}

int main() {
    SetUnhandledExceptionFilter(_UnhandledExceptionFilter);
    
    bug();
    
    return 0;
}

