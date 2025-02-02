// run

#include <windows.h>
#include <dbghelp.h>
#include <string.h>

#pragma comment(lib, "dbghelp.lib")

int arst(){
    
    __int64 *address_of_return_address = _AddressOfReturnAddress();
    
    HANDLE hProcess = GetCurrentProcess();
    
    SymInitialize(hProcess, NULL, TRUE);
    
    char Buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)] = {0};
    PSYMBOL_INFO Symbol = (PSYMBOL_INFO)Buffer;
    Symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
    Symbol->MaxNameLen = MAX_SYM_NAME;
    
    SymFromAddr(hProcess, *address_of_return_address, NULL, Symbol);
    
    int return_value = strcmp(Symbol->Name, "main"); // Return 0 on match!
    
    SymCleanup(hProcess);
    
    return return_value;
}

int main(){
    return arst();
}


