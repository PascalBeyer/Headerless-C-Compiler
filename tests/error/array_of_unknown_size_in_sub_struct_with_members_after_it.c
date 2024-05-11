// fail "Array of unknown size has to be the last member of struct."

struct s{
    int a : 1;
    struct { int b[]; };
    int c : 1;
};
