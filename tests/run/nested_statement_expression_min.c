// run

#define M(a) ({ typeof(a) _a = (a); _a; })

int main(void)
{
    int x = M(M(1));
    return x != 1;
}
