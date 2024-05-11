

#define h0_1() what##up
#define h0_2() what##1

#define h0_1_no what##no
#define h0_2_no what##0

#define h1l(a) a##1337
#define h1r(a) asd_##a
#define h2(a, b) a##b
#define h3(a, b, c) a##b##c

int main(){
    // basic tests
    int h0_1();
    (void)whatup;
    int h0_2();
    (void)what1;
    
#define what xxx
#define up yyy
    (void)h0_1(); // this means they are not expanded?
    
#define xxxyyy() h0_1
    
    //h2(xxx,yyy)(); // h0_1 undeclared identifier
    
    
    int h0_1_no;
    (void)whatno;
    int h0_2_no;
    (void)what0;
    
    int h1l(asd);
    (void)asd1337;
    int h1r(1337);
    (void)asd_1337;
    
    int h2(asd, das);
    (void)asddas;
    int h2(asd, 6969);
    (void)asd6969;
    
    int h2(asd, [1337]);
    (void) asd;
    
    int h3(asd, _, 6969);
    (void)asd_6969;
    
    (void)(h3(whatno, -, whatup));
    
    int h3(whatno, 1, whatup);
    (void)whatno1whatup;
    
    // non expanded arguments
    #define zero_define
    int h1l(zero_define);
    (void)zero_define1337;
    int h1r(zero_define);
    (void)asd_zero_define;
    
    int h2(zero_define, zero_define);
    (void)zero_definezero_define;
    
    int h3(a, zero_define, b);
    (void)azero_defineb;
    
    // void arguments
    
#define e2(a, b) h2(a, b)
#define e3(a, b, c) h3(a, b, c)
    int one_two_three_four = e2(zero_define, 1234);
    int e2(aaa, zero_define);
    (void)aaa;
	
	aaa = e2(1, ul);
	
    e3(asd, zero_define, 1337) = e2(asd, 6969);
    
    // hard ones
    struct{
        int f;
    } my_1;
    e2(my_, 1.f) = aaa;
    
    int h2(asd, 1blub);
    (void)asd1blub;
}
