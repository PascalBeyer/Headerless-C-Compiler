// skip

#include <stdio.h>

__declspec(dllexport) int arst(FILE *file){
    char buffer[8];
    return fread(buffer, 1, sizeof(buffer), file);
}
