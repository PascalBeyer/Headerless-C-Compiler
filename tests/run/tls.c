// run

__declspec(thread) int thread_local_variable = 1;

typedef void *HANDLE;

__declspec(dllimport) HANDLE __stdcall CreateThread(void * lpThreadAttributes, unsigned long long dwStackSize, void * lpStartAddress, void * lpParameter, void * dwCreationFlags, void * lpThreadId);
__declspec(dllimport) unsigned int  __stdcall WaitForSingleObject(HANDLE hHandle, unsigned int dwMilliseconds);

void *thread_address;

int thread_proc(void *){
    thread_local_variable = 2;
    thread_address = &thread_local_variable;
    return 0;
}

int main(){
    
    thread_local_variable = 3;
    thread_address = &thread_local_variable;
    
    HANDLE ThreadHandle = CreateThread(0, 0, thread_proc, 0, 0, 0);
    WaitForSingleObject(ThreadHandle, /*INFINITE*/0xFFFFFFFF);
    if(thread_address == &thread_local_variable) return 1;
    
    return 0;
}


