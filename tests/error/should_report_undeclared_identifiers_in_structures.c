// fail "Error at 'type'"

// This should fail because 'type' is undeclared, but currently fails because 'structure' is undeclared.
// This is now fixed, so here is a regression test :)

struct structure{ type a; };
struct structure arst;

int main(){}

