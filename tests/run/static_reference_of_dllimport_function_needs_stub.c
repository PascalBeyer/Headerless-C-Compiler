// check "causes a stub to be generated"
// run

__declspec(dllimport) void *malloc(unsigned long long size);

void *(*func)(unsigned long long size) = malloc;

int main(){
    char *a = func(1);
    *a = 0;
    return *a;
}

