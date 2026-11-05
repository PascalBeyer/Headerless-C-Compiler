// run

// A non-constant GNU ?: operand can be a loaded pointer value, not just an
// lvalue. This used to assert while emitting IR_duplicate.

static const char *fallback = "fallback";

static const char *get_value(int which)
{
    if (which == 0) return 0;
    return "value";
}

int main(void)
{
    const char *value;

    value = get_value(0) ?: fallback;
    if (value != fallback) return 1;

    value = get_value(1) ?: fallback;
    if (value[0] != 'v') return 2;

    return 0;
}
