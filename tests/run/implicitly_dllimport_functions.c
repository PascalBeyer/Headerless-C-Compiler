// compile
// check "'GetStdHandle': Function is treated as dllimport, but was not declared '__declspec(dllimport)'."
// check "'WriteFile': Function is treated as dllimport, but was not declared '__declspec(dllimport)'."
// run 
// check "Hello, World!"

void *GetStdHandle(int handle_num);
int WriteFile(void *handle, void *buffer, int bytes_to_write, int *bytes_written, void *overlapped);

int main(){
    
    static char buffer[] = "Hello, World!";
    
    void *handle = GetStdHandle(/*STD_OUTPUT_HANDLE*/-11);
    int bytes_written = 0;
    int success = WriteFile(handle, buffer, sizeof(buffer)-1, &bytes_written, 0);
    if(!success) return 1;
    if(bytes_written != sizeof(buffer)-1) return 1;
    
    return 0;
}
