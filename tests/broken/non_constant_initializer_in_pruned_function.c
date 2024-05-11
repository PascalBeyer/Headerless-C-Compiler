// broken
// fail "Initializer is not a constant."

void foo() {
    static short w = (int)&foo; /* initializer not computable */
}

int main(){
    return 0;
}
