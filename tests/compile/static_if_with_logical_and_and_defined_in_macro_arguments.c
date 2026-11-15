
#define m1
#define m2

#define nothing(a) a

nothing(
#if defined(m1) && !defined(m2)
        a
#endif
        )


int main(){
    
    return 0;
}

