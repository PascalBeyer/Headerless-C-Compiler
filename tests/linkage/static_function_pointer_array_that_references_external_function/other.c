// skip

#include <stdio.h>

typedef void (*function_pointer)(void);

void hi(){
    printf("hi!\n");
}
void bye(void);

static function_pointer pointers[] = {
    hi,
    bye,
};

void exec_pointers(){
    for(int i = 0; i < sizeof(pointers)/sizeof(*pointers); i++){
        (*(pointers[i]))();
    }
}


