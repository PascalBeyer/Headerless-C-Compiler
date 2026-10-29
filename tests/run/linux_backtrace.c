// os linux
// run

#include <execinfo.h>
#include <stdio.h>

int main(void){
    void *frames[16];
    int count = backtrace(frames, 16);
    return 4 - count;
}
