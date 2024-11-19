// fail "Cast of 'int' to 'struct s' is illegal."

int main(){
    int a;
    struct s {};
    (struct s)a;
}
