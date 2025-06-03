// run

struct arst{
    int a;
    int b[];
};

struct arst a1 = (struct arst){1, 2, 3, 4, 5};

int _start(){
    
    struct arst a2 = (struct arst){1, 2, 3, 4, 5};
    
    if(a1.a != 1) return 1;
    if(a1.b[0] != 2) return 1;
    if(a1.b[1] != 3) return 1;
    if(a1.b[2] != 4) return 1;
    if(a1.b[3] != 5) return 1;
    
    if(a2.a != 1) return 1;
    if(a2.b[0] != 2) return 1;
    if(a2.b[1] != 3) return 1;
    if(a2.b[2] != 4) return 1;
    if(a2.b[3] != 5) return 1;
    
    return 0;
}
