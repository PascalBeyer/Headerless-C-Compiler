// run

#define assert(a) if(!(a)) return -1

int main(){
    float a = 1.0f;
    float b = 1.0f;
    float c1 = a + b;
    assert(c1 == 2);
    float c2 = a * b;
    assert(c2 == 1);
    float c3 = a - b;
    assert(c3 == 0);
    float c4 = a / b;
    assert(c4 == 1);
    a += 1;
    assert(a == 2);
    a *= 2;
    assert(a == 4);
    a -= 1;
    assert(a == 3);
    a /= 3;
    assert(a == 1);
    //assert(a++ == 1);
    //assert(++a == 3);
    //assert(a-- == 3);
    //assert(--a == 1);
    
    double e = 1.0;
    double f = 1.0;
    double g1 = e + f;
    assert(g1 == 2);
    double g2 = e * f;
    assert(g2 == 1);
    double g3 = e - f;
    assert(g3 == 0);
    double g4 = e / f;
    assert(g4 == 1);
    e += 1;
    assert(e == 2);
    e *= 2;
    assert(e == 4);
    e -= 1;
    assert(e == 3);
    e /= 3;
    assert(e == 1);
    
    if(a){
        assert(a);
    }
    
    assert(f > 0.5f);
    assert(f >= 0.5f);
    assert(f >= 1.0f);
    assert(f <= 1.0f);
    assert(f <= 1.5f);
    assert(f < 1.5f);
    
    return 0;
}
