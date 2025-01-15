// run

#include <strsafe.h>

int a = 0;
int main(){
    
    __noop(a = 1337);
    return a;
}
