// run
// check "Found variable 'three_deep', value: 0x3333333333333333"
// check "Found variable 'local_scope', value: 0xdeadbeefdeadbabe"
// check "Found variable 'root_scope', value: 0x1337133713371337"
// check "Found variable 'three_deep_2', value: 0x333333333332222"
// check "Found variable 'local_scope_2', value: 0xdeadbeefdead2222"
// check "Found variable 'root_scope', value: 0x1337133713371337"
// reject "Found variable 'should_not_see_this_variable'"
// reject "Found variable 'should_not_see_this_variable_2'"

#include <windows.h>
#include <dbghelp.h>
#include <stdio.h>

#pragma comment(lib, "dbghelp.lib")

BOOL SymbolCheckProc(PSYMBOL_INFO symbol, ULONG size, PVOID UserContext) {
    
    if((symbol->Flags & (SYMFLAG_LOCAL | SYMFLAG_REGREL)) != symbol->Flags) return TRUE;
    
    CONTEXT *StackFrame = UserContext;
    
    ULONG64 register_value = (symbol->Register == /*rbp*/334) ? StackFrame->Rbp : StackFrame->Rsp;
    
    printf("Found variable '%s', value: 0x%llx\n", symbol->Name, *(ULONG64 *)(register_value + symbol->Address));
    
    return TRUE; // Continue enumeration
}

#ifndef __HLC__
#pragma function(memset)
void *memset(void *mem, int val, size_t amount){
    
    if(!amount) return mem;
    __stosb(mem, (unsigned char)val, amount);
    return mem;
}
#endif

void ReadLocalVariable(void) {
    // Initialize DbgHelp
    SymInitialize(GetCurrentProcess(), NULL, TRUE);
    
    // Capture the current context
    CONTEXT context;
    RtlCaptureContext(&context);
    
    // Initialize the stack frame
    STACKFRAME64 stackFrame;
    memset(&stackFrame, 0, sizeof(STACKFRAME64));
    
    // Set up the stack frame for x64 architecture
    stackFrame.AddrPC.Offset = context.Rip;
    stackFrame.AddrPC.Mode = AddrModeFlat;
    stackFrame.AddrFrame.Offset = context.Rsp;
    stackFrame.AddrFrame.Mode = AddrModeFlat;
    stackFrame.AddrStack.Offset = context.Rsp;
    stackFrame.AddrStack.Mode = AddrModeFlat;
    
    HANDLE process = GetCurrentProcess();
    HANDLE thread = GetCurrentThread();
    
    int first_time = 1;
    
    // Unwind the stack
    while (StackWalk64(IMAGE_FILE_MACHINE_AMD64, process, thread, &stackFrame, &context, NULL, SymFunctionTableAccess64, SymGetModuleBase64, NULL)) {
        if(first_time){
            first_time = 0;
            continue;
        }
        
        // Set the context for the current stack frame
        IMAGEHLP_STACK_FRAME frame = {0};
        frame.InstructionOffset = stackFrame.AddrPC.Offset;
        frame.ReturnOffset = stackFrame.AddrReturn.Offset;
        frame.FrameOffset = stackFrame.AddrFrame.Offset;
        frame.StackOffset = stackFrame.AddrStack.Offset;
        frame.BackingStoreOffset = stackFrame.AddrBStore.Offset;
        frame.FuncTableEntry = (ULONG64)stackFrame.FuncTableEntry;
        frame.Params[0] = stackFrame.Params[0];
        frame.Params[1] = stackFrame.Params[1];
        frame.Params[2] = stackFrame.Params[2];
        frame.Params[3] = stackFrame.Params[3];
        frame.Virtual = stackFrame.Virtual;
        
        SymSetContext(process, &frame, NULL); // There is something weird here about the the return value.
        
        // Enumerate local symbols
        SymEnumSymbols(process, /*UseSymSetContext*/0, NULL, SymbolCheckProc, &context);
    }
    
    // Cleanup
    SymCleanup(process);
}

int main() {
    
    __int64 root_scope = 0x1337133713371337;
    
    {
        __int64 local_scope = 0xdeadbeefdeadbabe;
        
        {
            __int64 three_deep = 0x3333333333333333;
            
            ReadLocalVariable();
            
            int k = 1;
            
        }
        {
            __int64 should_not_see_this_variable = 0x12345678abcdef;
        }
        
    }
    
    
    
    {
        __int64 local_scope_2 = 0xdeadbeefdead2222;
        
        {
            __int64 three_deep_2 = 0x333333333332222;
            
            ReadLocalVariable();
            
            int k = 1;
            
        }
        {
            __int64 should_not_see_this_variable_2 = 0x12345678ab2222;
        }
        
    }
    
    return 0;
}

