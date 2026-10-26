// run

struct inner {
    int a;
    int *p;
};

struct outer {
    char padding[0x30];
    struct inner elements[1];
};

int main(void) {
    int a = (int)(long)&((struct outer *)0)->elements[0].a;
    int p = (int)(long)&((struct outer *)0)->elements[0].p;
    
    return (a == 0x30 && p == 0x38) ? 0 : 1;
}
