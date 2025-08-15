// run

int _start(){
    float f1 = 0.0f;
    float f2 = -0.0f;
    
    unsigned a = *(unsigned *)&f2;
    if(a != 0x80000000) return 1;
    return 0;
}
