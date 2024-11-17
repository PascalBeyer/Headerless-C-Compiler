// compile /c /MDd
// compile /c /MDd /LD reads_the_file.c
// link reads_the_file.obj /DLL
// link opens_a_file.obj reads_the_file.lib
// run

#include <stdio.h>

__declspec(dllimport) int arst(FILE *file);

int main(){
    FILE *file = fopen("arst", "rb");
    return arst(file);
}
