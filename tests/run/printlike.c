// run
// check "      arst"
// check "(struct <unnamed-tag>){.a = -1, .b = 1000, .c = 1}"
// check "(int[10]){[0] = 1, [1] = 2, [2] = 3, [3] = 4, [4] = 5, [5] = 6, [6] = 7, [7] = 8, [8] = 9, [9] = 10}"
// check "t = 0.200000 : 0.800000"
// check "0x1337[0] = 37"

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
    
    {
        float t = 0.2f;
        print("{t=} : {}\n", (4 * t < 1.0f) ? 4 * t : 1.0f);
    }
    
    {
        int a = 0x1337;
        print("0x1337[0] = %.2x\n", ((char *)&a)[0]);
    }
}
