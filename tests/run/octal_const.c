// check "(8,13): Warning 43 at '0': Octal constant used, use 0o<octal> to squelch this warning."
// run

#define assert(a) if(!(a)) return 1;

int _start(){
    
    int o = 0123;
    assert(0123 == 83 && o == 83);
    
    int o2 = 0o123;
    assert(0o123 == 83 && o2 == 83);
    
    
    return 0;
}
