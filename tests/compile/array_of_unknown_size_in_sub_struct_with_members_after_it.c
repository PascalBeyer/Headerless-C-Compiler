// compile -Wall
// check "Array of unknown size has to be the last member of struct. Allowing this is a GNU-extension."

struct s{
    int a : 1;
    struct { int b[]; };
    int c : 1;
};

int _start(){}
