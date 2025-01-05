// run
// check "array[0] = {1.000000, 2.000000}"
// check "array[1] = {3.000000, 4.000000}"

#include <stdio.h>

int main(){
    
    struct v2{
        float x, y;
    };
    
    
    struct v2 *array = (struct v2 []){1.0f, 2.0f, 3.0f, 4.0f};
    
    printf("array[0] = {%f, %f}\n" , array[0].x, array[0].y);
    printf("array[1] = {%f, %f}\n" , array[1].x, array[1].y);
    return 0;
}


