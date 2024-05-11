
struct s{
    int a : 1;
    struct { int b : 1; };
    int c : 1;
};

typedef int a[sizeof(struct s) == 12 ? 1 : -1];

int main(){
    return 0;
}
