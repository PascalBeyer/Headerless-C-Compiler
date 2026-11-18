// fail "Cannot initialize an zero-sized structure with non-empty initializer-list."


union u {
    int a[0];
};

union u value = {1};
