
struct asd{
    int a;
    int array[3];
};

int *asd2 = & ((((struct asd *)0)->array)[1]);
int *asd3 = & (((struct asd *)0)->array[1]);

int main(){
}

