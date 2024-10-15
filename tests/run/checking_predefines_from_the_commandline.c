// compile "-Dassert(a)=if(!(a)) return 1;" -DNO_VALUE -DVALUE=1337
// run

int main(){
    assert(NO_VALUE == 1);
    assert(VALUE == 1337);
}

