
char *asd;

#define __C(a, b) a##b
#define C(a,b) __C(a, b)

#define zero_define

#define __stringfy(a) #a
#define stringfy(a) __stringfy(a)


int main(){
    C(zero_define, asd);
    C(asd, zero_define);
    asd = stringfy(zero_define);
}


