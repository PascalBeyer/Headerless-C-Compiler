// run

char array[10];

__int64 a = &array[5] - &array[5]; // @cleanup: truncating this?
int *b = &array[5] + 5;
int *c = &array[5] - 5;
int *d = 5 + &array[5];

int _start(){
    
    if(a != 0) return 1;
    if(b != &array[10]) return 1;
    if(c != &array[0]) return 1;
    if(d != &array[10]) return 1;
    
    return 0;
}
