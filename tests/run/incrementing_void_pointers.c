// run

int main(void)
{
    char bytes[4] = {0, 1, 2, 3};
    void *p = bytes;
    void *old;

    old = p++;
    if ((char *)old != bytes) return 1;
    if ((char *)p != bytes + 1) return 2;

    old = ++p;
    if ((char *)old != bytes + 2) return 3;
    if ((char *)p != bytes + 2) return 4;

    old = p--;
    if ((char *)old != bytes + 2) return 5;
    if ((char *)p != bytes + 1) return 6;

    old = --p;
    if ((char *)old != bytes) return 7;
    if ((char *)p != bytes) return 8;

    return 0;
}
