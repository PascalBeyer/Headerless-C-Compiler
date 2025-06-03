// fail "Initializer is not a constant."

struct arst{
    int a, b, c;
};

struct arst2{
    int a;
} arst2 = {
    (struct arst){1, 2, 3}.a,
};

int _start(){
    return arst2.a;
}
