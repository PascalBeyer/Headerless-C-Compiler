// run
// check "      arst"
// check "(struct <unnamed-tag>){.a = -1, .b = 1000, .c = 1}"
// check "(int[10]){[0] = 1, [1] = 2, [2] = 3, [3] = 4, [4] = 5, [5] = 6, [6] = 7, [7] = 8, [8] = 9, [9] = 10}"

#include <hlc/print.h>

int main(){
    print("%*s\n", 10, "arst");
    
    struct {
        int a : 1;
        int b : 12;
        int c : 2;
    } bitfield = {
        -1,
        1000,
        1,
    };
    
    print(bitfield);
    
    int array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
    print(array);
    
}
