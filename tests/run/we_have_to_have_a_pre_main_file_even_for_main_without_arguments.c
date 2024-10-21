// run

#pragma comment(lib, "kernel32")

void *CreateThread(void *, __int64, unsigned int (*)(void *context), void *, int, void *);
void Sleep(int);

unsigned int sleep_and_exit(void *context){
    Sleep(16);
    return 1;
}

int main(){
    CreateThread(0, 0, sleep_and_exit, 0, 0, 0);
    
    // Exiting from main here is supposed to exit the process,
    // but Windows does not treat the main thread differantly
    // from any other thread. 
    // This means, it will only cause the thread to call
    // `ExitThread` and the last thread that exits is `sleep_and_exit`.
    // This will cause the exit code to be `1` incorrectly.
}
