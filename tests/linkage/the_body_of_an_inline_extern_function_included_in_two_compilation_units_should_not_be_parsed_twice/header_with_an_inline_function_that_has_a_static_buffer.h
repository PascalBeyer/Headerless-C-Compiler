// skip

extern inline int function(int a){
    static char buffer[0x100];
    return buffer[a];
}


