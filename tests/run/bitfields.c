// run

#define assert(a) if(!(a)) return -1

int main(){
    
    
    {
        struct contains_a_bitfield{
            int a : 1;
            int b : 1;
            int c : 4;
            int d : 6;
            int e : 31;
        };
        
        assert(sizeof(struct contains_a_bitfield) == 8);
        
        struct contains_a_bitfield bitfield = {0};
        
        bitfield.b = 0;
        bitfield.a = 2;
        assert(bitfield.a == 0);
        assert(bitfield.b == 0);
    }
    
    {
        struct{
            unsigned a : 1;
            unsigned b : 1;
        } bitfield;
        bitfield.a = 1;
        bitfield.b = 1;
        assert(bitfield.a == 1);
        assert(bitfield.b == 1);
        bitfield.b = 0;
        
        assert(!bitfield.b);
        
        bitfield.b = 1.0f; // this should theoretically compile.
        assert(bitfield.b == 1);
        
        assert(++bitfield.a == 0);
        assert(bitfield.a   == 0);
        assert(bitfield.a++ == 0);
        assert(bitfield.a   == 1);
        
        assert(bitfield.b-- == 1);
        assert(bitfield.b   == 0);
        assert(--bitfield.b == 1);
        assert(bitfield.b   == 1);
    }
    
    
    struct{
        unsigned short start  : 4;
        unsigned short middle : 4;
        unsigned short skip   : 4;
        unsigned short end    : 4;
    } bitfield = {0};
    
    for(int index = 0; index < 32; index++){
        assert((index % 16) == bitfield.start);
        bitfield.start += 1;
        
        assert((index % 16) == bitfield.middle);
        bitfield.middle += 1;
        
        assert((index % 16) == bitfield.end);
        bitfield.end += 1;
    }
    
    for(int index = 32; index > 0; index--){
        assert((index % 16) == bitfield.start);
        bitfield.start -= 1;
        
        assert((index % 16) == bitfield.middle);
        bitfield.middle -= 1;
        
        assert((index % 16) == bitfield.end);
        bitfield.end -= 1;
    }
    
    for(int index = 0; index < 32; index++){
        assert((index % 16) == bitfield.start);
        bitfield.start++;
        
        assert((index % 16) == bitfield.middle);
        bitfield.middle++;
        
        assert((index % 16) == bitfield.end);
        bitfield.end++;
    }
    
    for(int index = 32; index > 0; index--){
        assert((index % 16) == bitfield.start);
        bitfield.start--;
        
        assert((index % 16) == bitfield.middle);
        bitfield.middle--;
        
        assert((index % 16) == bitfield.end);
        bitfield.end--;
    }
    
    for(int index = 0; index < 32; index++){
        assert((index % 16) == bitfield.start);
        ++bitfield.start;
        
        assert((index % 16) == bitfield.middle);
        ++bitfield.middle;
        
        assert((index % 16) == bitfield.end);
        ++bitfield.end;
    }
    
    for(int index = 32; index > 0; index--){
        assert((index % 16) == bitfield.start);
        --bitfield.start;
        
        assert((index % 16) == bitfield.middle);
        --bitfield.middle;
        
        assert((index % 16) == bitfield.end);
        --bitfield.end;
    }
    
    
    {
        struct{
            int   : 4;
            int a : 4;
            int   : 4;
            int b : 4;
            int   : 6;
            int   : 6;
            int c : 4;
        } bitfield = {1, 1, 1};
        
        assert(bitfield.a == 1 && bitfield.b == 1 && bitfield.c == 1);
    }
    
    
    
    return 0;
}
