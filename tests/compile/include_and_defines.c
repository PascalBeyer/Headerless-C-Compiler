// compile -stdlib

#pragma once

#define stdio asd
// These should probably be handled immediately.
#include <stdio.h>
// #include ""


#define smaller <
#define file stdint.h
#define bigger  >

// This needs to be defered.
#define include_me smaller file bigger

#include include_me

#include __FILE__

int main(){
    printf("hello!\n");
}
