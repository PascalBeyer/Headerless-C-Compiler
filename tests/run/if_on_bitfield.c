// run

int _start(){
    
    static struct{
        unsigned int a : 1;
        unsigned int b : 1;
    } arst = {
        .b = 1,
    };
    
    if(arst.a) return 1;
    
    return 0;
}
