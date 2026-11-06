// run

#include <stdbool.h>

int main(){
    bool b = true;

    b -= 1;
    if (b) return 1;

    b -= 1;
    if (!b) return 3;

    b += 1;
    if (!b) return 2;

    return 0;
}
