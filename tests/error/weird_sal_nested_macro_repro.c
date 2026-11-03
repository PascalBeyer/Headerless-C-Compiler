// fail "Error at 'X': [0] Undeclared identifier."

#define F(a, b, c) a
#define X F(X, (), F(X, (), ))

X

