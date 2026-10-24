// fail "'transparent_union' is currently only supported for unions whose members are all pointer types."

union bad {
    int value;
    int *ptr;
} __attribute__((transparent_union));

int main(void) {
    return 0;
}
