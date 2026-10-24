// run

int main(void) {
    int x = 3;
    __int64 y = 4;

    int selected = 0;
    int not_selected = 0;

    // The matching association is in the middle. Associations after it must
    // not discard the selected expression's IR.
    int result = _Generic(x,
        __int64: (not_selected = 1, 10),
        int: (selected = 1, 20),
        char: (not_selected = 2, 30)
    );

    if(result != 20) return 1;
    if(selected != 1) return 2;
    if(not_selected != 0) return 3;

    // The default association appears before the matching one. Only the
    // matching expression should be emitted.
    result = _Generic(x,
        default: (not_selected = 3, 40),
        int: (selected = 2, 50)
    );

    if(result != 50) return 4;
    if(selected != 2) return 5;
    if(not_selected != 0) return 6;

    // No matching association: the default is used and the non-matching
    // expressions must not be evaluated.
    result = _Generic(y,
        int: (not_selected = 4, 60),
        default: (selected = 3, 70)
    );

    if(result != 70) return 7;
    if(selected != 3) return 8;
    if(not_selected != 0) return 9;

    return 0;
}
