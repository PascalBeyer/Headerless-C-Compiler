
struct structure{
    int *hello;
    int *hello2;
}s = {
    (int *)((char *)&s + ((__int64)&(((struct structure*)0)->hello))),
    (int *)((char *)&s + ((__int64)&(((struct structure*)0)->hello2))),
};

int main(){
    return *s.hello;
}

