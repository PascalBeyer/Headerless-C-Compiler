// run

// A 15-byte struct return is passed in two eight-byte pieces under System V.
// The compiler used to call system_v_load_oddly_sized_type_into_register
// with an eight-byte chunk and assert on the unhandled size.

struct caps {
    unsigned char values[15];
};

static struct caps make_caps(void)
{
    struct caps result = {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}};
    return result;
}

int main(void)
{
    struct caps result = make_caps();

    for (int i = 0; i < 15; i++) {
        if (result.values[i] != i + 1) return i + 1;
    }

    return 0;
}
