// run

int main(void) {
    int x = 7;
    long y = 3;

    if ((__typeof__(x))y != 3) return 1;
    if ((typeof(y))x != 7) return 2;
    if (sizeof(__typeof__(x)) != sizeof(int)) return 3;

    return 0;
}
