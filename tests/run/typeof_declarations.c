// run

struct S {
    int a;
    long b;
};

int source_int = 3;
long source_long = 4;
int source_array[3] = { 1, 2, 3 };
int side_effect_count = 0;

typeof(source_long) global_long = 42;

int bump(void) {
    side_effect_count++;
    return 7;
}

int function(int x) {
    return x + 1;
}

void take_int(typeof(source_int) value) {
    (void)value;
}

struct Holder {
    typeof(source_int) a;
    typeof(source_long) b;
};

int main(void) {
    int local_int = 7;
    long local_long = 3;

    typeof(local_int) a = 5;
    __typeof__(local_long) b = 6;
    typeof(&local_int) pointer = &a;
    const typeof(local_int) c = 8;
    typeof(source_array) array = { 4, 5, 6 };
    typeof(source_int + source_long) sum = a + b;
    typeof(function) *function_pointer = function;
    typeof(local_int++) no_side_effect = 9;
    typeof(bump()) no_call = 10;

    typedef typeof(local_int) T;
    T t = 11;

    struct S s = { 12, 13 };
    typeof(s) s2 = s;
    struct Holder holder = { 14, 15 };

    for (typeof(local_int) i = 0; i < 2; i++) {
        a += i;
    }

    take_int(a);

    if (side_effect_count != 0) return 1;
    if (sizeof(global_long) != sizeof(long)) return 2;
    if (sizeof(typeof(local_int)) != sizeof(int)) return 3;
    if (sizeof(typeof(s)) != sizeof(struct S)) return 4;
    if (sizeof(typeof(source_array)) != sizeof(source_array)) return 5;
    if (sizeof(sum) != sizeof(long)) return 6;

    if (a != 6 || b != 6 || *pointer != 6 || c != 8) return 7;
    if (array[0] != 4 || array[2] != 6) return 8;
    if (function_pointer(5) != 6) return 9;
    if (no_side_effect != 9 || no_call != 10) return 10;
    if (t != 11) return 11;
    if (s2.a != 12 || s2.b != 13) return 12;
    if (holder.a != 14 || holder.b != 15) return 13;

    return 0;
}
