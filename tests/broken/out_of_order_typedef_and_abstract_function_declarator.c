// broken

// This one should be the same as the ones below, 
// as this is how I want to handle out-of-order declarations.
// Currently, this is broken though.
int function1(int (typename));

typedef int typename;

// These two declarations are the same, because of how abstract declarators work.
int function1(int (typename));
int function1(int (*)(typename a));

int main(){}
