// run

union pointer_argument {
    int *int_ptr;
    char *char_ptr;
    void *void_ptr;
} __attribute__((transparent_union));

static int use_int(union pointer_argument arg) {
    return *arg.int_ptr;
}

static int use_char(union pointer_argument arg) {
    return *arg.char_ptr;
}

static int use_void(union pointer_argument arg) {
    return arg.void_ptr != 0;
}

// glibc puts the attribute after the typedef name:
//     typedef union { ... } __SOCKADDR_ARG __attribute__ ((__transparent_union__));
typedef union {
    int *int_ptr;
    long *long_ptr;
} typedef_argument __attribute__((__transparent_union__));

static long use_typedef(typedef_argument arg) {
    return *arg.long_ptr;
}

union __attribute__((transparent_union)) leading_argument {
    int *int_ptr;
};

static int use_leading(union leading_argument arg) {
    return *arg.int_ptr;
}

typedef union {
    char *char_ptr;
} __attribute__((transparent_union)) body_typedef_argument;

static int use_body_typedef(body_typedef_argument arg) {
    return *arg.char_ptr;
}

int main(void) {
    int int_value = 42;
    char char_value = 'A';
    long long_value = 123456789;

    if(use_int(&int_value) != 42) return 1;
    if(use_char(&char_value) != 'A') return 2;
    if(!use_void(&int_value)) return 3;
    if(use_typedef(&long_value) != 123456789) return 4;
    if(use_leading(&int_value) != 42) return 5;
    if(use_body_typedef(&char_value) != 'A') return 6;

    return 0;
}
