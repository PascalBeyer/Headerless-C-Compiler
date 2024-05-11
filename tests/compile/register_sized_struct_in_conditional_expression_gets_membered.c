
struct inner{
    unsigned char member;
};

struct outer{
    struct inner inner;
};

int main(){
    
    struct outer *outer = &(struct outer){};
    
    struct inner default_inner;
    
    unsigned char a = (outer ? outer->inner : default_inner).member;
    
    
    return 0;
}
