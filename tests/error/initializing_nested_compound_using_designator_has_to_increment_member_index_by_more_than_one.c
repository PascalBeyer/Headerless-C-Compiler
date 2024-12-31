// fail "Too many initializers for structure 's'."

struct s{
    union u{
        int arst;
        int arst2;
    };
};

int main(){
    struct s u = {.arst = 1, 2};
}
