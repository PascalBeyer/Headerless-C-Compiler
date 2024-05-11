
void varargs(int _, ...){
    
}

int main(){
    
    struct{
        char  c : 4;
        short s : 4;
        int   i : 4;
        long  l : 4;
        long long ll : 4;
        
        unsigned char  uc : 4;
        unsigned short us : 4;
        unsigned int   ui : 4;
        unsigned long  ul : 4;
        unsigned long long ull : 4;
    } bitfield = {0};
    
    varargs(0, bitfield.c, bitfield.s, bitfield.i, bitfield.l, bitfield.ll, bitfield.uc, bitfield.us, bitfield.ui, bitfield.ul, bitfield.ull);
    
    return 0;
}
