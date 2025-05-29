// run

int _start(){
    struct {
        int a : 1;
        int b : 1;
        int c : 1;
    } arst = {
        .a = 1,
        .c = 1,
    };
    
    return (0 || arst.b) + (arst.b || 0) + (arst.b || arst.b) + (1 && arst.b) + (arst.b && 1) + (arst.b && arst.b);
}

