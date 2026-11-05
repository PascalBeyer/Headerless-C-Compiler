// run

typedef enum __attribute__((__packed__)) {
    first = 0,
    second = 1,
} PackedEnum;

typedef enum __attribute__ ((__packed__)) {
    third = 2,
    fourth = 3,
} SpacedPackedEnum;

int main(void)
{
    PackedEnum a = second;
    SpacedPackedEnum b = fourth;

    if (a != second) return 1;
    if (b != fourth) return 2;
    if (first != 0 || third != 2) return 3;
    return 0;
}
