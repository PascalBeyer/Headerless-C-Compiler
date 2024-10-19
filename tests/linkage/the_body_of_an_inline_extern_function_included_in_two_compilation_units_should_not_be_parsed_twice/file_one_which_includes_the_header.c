// compile file_two_which_includes_the_header.c
// reject "Warning"

#include "header_with_an_inline_function_that_has_a_static_buffer.h"

int main(){
    return function(1) + other();
}

