
#include <limits.h>

int arst[(LLONG_MIN < 0) ? 1 : -1];
int arst[(-LLONG_MAX < 0) ? 1 : -1];

int main(){
    return 0;
}
