// run

int _start(){
    if((void*)((char*)(0) + 8) != (void *)8) return 1;
    if((void*)(0 + (char*)(8)) != (void *)8) return 1;
    
    int *a = (int *)4;
    
    if((a + 1) != (int *)8) return 1;
    if((1 + a) != (int *)8) return 1;
    if((a - 1) != (int *)0) return 1;
    return 0;
}
