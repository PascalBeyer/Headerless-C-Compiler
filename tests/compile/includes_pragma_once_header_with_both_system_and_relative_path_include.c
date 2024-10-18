// compile -I .

#ifdef HEADER
#pragma once

static inline int function(){ return 1; };

#else

#define HEADER
#include "includes_pragma_once_header_with_both_system_and_relative_path_include.c"
#include <includes_pragma_once_header_with_both_system_and_relative_path_include.c>

int main(){
    return function();
}
#endif

