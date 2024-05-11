// run

#define assert(a) if(!(a)) return 1

typedef unsigned __int16 uint16_t;

int main(){
    
    
    int asd(int a, int b, int c){ 
        assert(a == 1237);
        assert(b == 3);
        assert(c == 5);
        return 0;
    }
    
    struct {
        uint16_t a : 13;
        uint16_t b : 3;
        uint16_t c;
    } bitfield = {
        1237,
        3,
        5
    };
    
    return asd(bitfield.a, bitfield.b, bitfield.c);
}

