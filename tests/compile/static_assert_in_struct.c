struct S {
    _Static_assert(sizeof(int) == 4, "int size");
    int x;
};

int main(void)
{
    return 0;
}
