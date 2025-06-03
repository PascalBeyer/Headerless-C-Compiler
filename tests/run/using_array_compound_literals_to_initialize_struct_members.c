// run

struct { 
    int m[0x4];
} a = {
    (int [0x4]){1, 2, 3, 4}
};

int main(){
    
    if(a.m[0] != 1) return 1;
    if(a.m[1] != 2) return 1;
    if(a.m[2] != 3) return 1;
    if(a.m[3] != 4) return 1;
    
    struct { 
        int m[0x4];
    } b = {
        (int [0x4]){1, 2, 3, 4}
    };
    
    
    if(b.m[0] != 1) return 1;
    if(b.m[1] != 2) return 1;
    if(b.m[2] != 3) return 1;
    if(b.m[3] != 4) return 1;
    
    return 0;
}
