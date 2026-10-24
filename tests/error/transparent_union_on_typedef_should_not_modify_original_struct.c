// reject "Wanted 'un' (aka union un) given 'char*'."
// fail "Wanted 'union un' given 'char*'."


union un{
    char *a;
    long *b;
};

typedef union un un __attribute__((__transparent_union__));


void func_un(un val){
    
}

void func_union_un(union un val){
    
}

int main(){
    char *a = 0;
    
    func_union_un(a);
    func_un(a);
}
