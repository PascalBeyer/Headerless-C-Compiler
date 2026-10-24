// fail "'transparent_union' can only be applied to a union."

struct bad {
    int *ptr;
} __attribute__((transparent_union));

int main(void) {
    return 0;
}
