// run

struct arst{
    int a, b, c;
};

struct arst2{
    struct arst arst;
    struct arst arst2;
} arst2 = {
    (struct arst){1, 2, 3},
    (struct arst){4, 5, 6},
};

int _start(){
    if(arst2.arst.a != 1) return 1;
    if(arst2.arst.b != 2) return 1;
    if(arst2.arst.c != 3) return 1;
    if(arst2.arst2.a != 4) return 1;
    if(arst2.arst2.b != 5) return 1;
    if(arst2.arst2.c != 6) return 1;
    
    return 0;
}
