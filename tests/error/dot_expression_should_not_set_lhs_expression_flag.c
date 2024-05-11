// fail "Operand of '++' must be an L-value."

struct asd{
    int value;
};

struct asd function() {
    return (struct asd){1};
}

int _start(){
    function().value++; // should not compile
    
    return 0;
}
