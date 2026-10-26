typedef union {
    int *integer;
} transparent_union_t __attribute__((transparent_union));

static void use_union(transparent_union_t value) {
    (void)value;
}

int main(void) {
    int integer = 1;
    void *pointer = &integer;
    use_union(pointer);
    return 0;
}
