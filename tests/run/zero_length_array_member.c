// run

struct WithZeroArray {
    int before;
    char empty[0];
    int after;
};

struct WithZeroArray global_value;

int main(void)
{
    struct WithZeroArray value;
    value.before = 1;
    value.after = 2;
    global_value.before = 3;
    global_value.after = 4;

    if (value.before != 1 || value.after != 2) return 1;
    if (global_value.before != 3 || global_value.after != 4) return 2;
    return 0;
}
