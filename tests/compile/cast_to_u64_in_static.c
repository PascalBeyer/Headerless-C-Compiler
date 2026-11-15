
int a;

int *b = (__int64)&a + 1; 
int *c = (char *)&a + 2; 

int _start(){
    
    static int *sb = (__int64)&a + 3; 
    static int *sc = (char *)&a + 4; 
    
}
