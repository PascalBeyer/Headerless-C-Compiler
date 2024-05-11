// compile other.c
// run

#include "header.h"

static structure *structures[] = {
    &my_structure,
    0
};


int main(){
    for(int i = 0; structures[i]; i++){
        structures[i]->create();
    }
    return 0;
}

