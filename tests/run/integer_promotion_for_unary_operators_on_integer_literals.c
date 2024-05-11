// run

#define assert(a) if(!(a)) return -1

int main(){
    
    char c;
    unsigned char uc;
    
    assert(sizeof(c) == 1);
    assert(sizeof(++c) == 1);
    assert(sizeof(c++) == 1);
    assert(sizeof(--c) == 1);
    assert(sizeof(c--) == 1);
    
    assert(sizeof(uc) == 1);
    assert(sizeof(++uc) == 1);
    assert(sizeof(uc++) == 1);
    assert(sizeof(--uc) == 1);
    assert(sizeof(uc--) == 1);
    
    short s;
    unsigned short us;
    
    assert(sizeof(s) == 2);
    assert(sizeof(++s) == 2);
    assert(sizeof(s++) == 2);
    assert(sizeof(--s) == 2);
    assert(sizeof(s--) == 2);
    
    assert(sizeof(us) == 2);
    assert(sizeof(++us) == 2);
    assert(sizeof(us++) == 2);
    assert(sizeof(--us) == 2);
    assert(sizeof(us--) == 2);
    
    assert(sizeof(+c) == 4);
    assert(sizeof(+s) == 4);
    assert(sizeof(+uc) == 4);
    assert(sizeof(+us) == 4);
    
    assert(sizeof(-c) == 4);
    assert(sizeof(-s) == 4);
    assert(sizeof(-uc) == 4);
    assert(sizeof(-us) == 4);
    
    assert(sizeof(~c) == 4);
    assert(sizeof(~s) == 4);
    assert(sizeof(~uc) == 4);
    assert(sizeof(~us) == 4);
    
    assert(sizeof(!c) == 4);
    assert(sizeof(!s) == 4);
    assert(sizeof(!uc) == 4);
    assert(sizeof(!us) == 4);
    
    assert(!((void *)0) == 1);
    assert(!((void *)1) == 0);
    
    assert(sizeof(1i8) == 1);
    assert(sizeof(1ui8) == 1);
    
    assert(sizeof(1i16) == 2);
    assert(sizeof(1ui16) == 2);
    
    assert(sizeof(1i32) == 4);
    assert(sizeof(1ui32) == 4);
    
    assert(sizeof(1i64) == 8);
    assert(sizeof(1ui64) == 8);
    
    assert(sizeof(+1i8) == 4);
    assert(sizeof(+1ui8) == 4);
    assert(sizeof(+1i16) == 4);
    assert(sizeof(+1ui16) == 4);
    
    assert(sizeof(-1i8) == 4);
    assert(sizeof(-1ui8) == 4);
    assert(sizeof(-1i16) == 4);
    assert(sizeof(-1ui16) == 4);
    
    assert(sizeof(~1i8) == 4);
    assert(sizeof(~1ui8) == 4);
    assert(sizeof(~1i16) == 4);
    assert(sizeof(~1ui16) == 4);
    
    assert(sizeof(!1i8) == 4);
    assert(sizeof(!1ui8) == 4);
    assert(sizeof(!1i16) == 4);
    assert(sizeof(!1ui16) == 4);
    
    
    assert(!~0xffffffff);
    
    return 0;
}
