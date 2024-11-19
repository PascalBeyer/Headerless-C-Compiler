

struct s{
    int a;
    int b;
};

int main(){
    struct s s = { 1, 2 };
    (struct s)s;
}
