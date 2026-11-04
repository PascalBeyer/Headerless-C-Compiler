// run

static int order[32];
static int order_count;

static void reset_order(void)
{
    order_count = 0;
}

static void record(int value)
{
    if (order_count < 32) order[order_count++] = value;
}

static void cleanup_int(int *value)
{
    record(*value);
}

static int test_scope_exit(void)
{
    reset_order();
    {
        int a __attribute__((cleanup(cleanup_int))) = 1;
        int b __attribute__((cleanup(cleanup_int))) = 2;
        if (a != 1 || b != 2) return 1;
    }
    if (order_count != 2) return 2;
    if (order[0] != 2 || order[1] != 1) return 3;
    return 0;
}

static int test_nested_scopes(void)
{
    reset_order();
    {
        int a __attribute__((cleanup(cleanup_int))) = 1;
        {
            int b __attribute__((cleanup(cleanup_int))) = 2;
        }
        int c __attribute__((cleanup(cleanup_int))) = 3;
    }
    if (order_count != 3) return 4;
    if (order[0] != 2 || order[1] != 3 || order[2] != 1) return 5;
    return 0;
}

static int test_return(void)
{
    int a __attribute__((cleanup(cleanup_int))) = 4;

    reset_order();
    {
        int b __attribute__((cleanup(cleanup_int))) = 5;
        return 0;
    }
}

static int test_break(void)
{
    int i;
    reset_order();
    for (i = 0; i < 1; i++) {
        int a __attribute__((cleanup(cleanup_int))) = 6;
        {
            int b __attribute__((cleanup(cleanup_int))) = 7;
            break;
        }
    }
    if (order_count != 2) return 6;
    if (order[0] != 7 || order[1] != 6) return 7;
    return 0;
}

static int test_continue(void)
{
    int i;
    reset_order();
    for (i = 0; i < 3; i++) {
        int a __attribute__((cleanup(cleanup_int))) = 8 + i;
        if (i == 1) continue;
        if (i == 2) break;
    }
    if (order_count != 3) return 8;
    if (order[0] != 8 || order[1] != 9 || order[2] != 10) return 9;
    return 0;
}

static int test_goto(void)
{
    reset_order();
    {
        int a __attribute__((cleanup(cleanup_int))) = 11;
        goto out;
    }
out:
    if (order_count != 1) return 10;
    if (order[0] != 11) return 11;
    return 0;
}

static int test_declaration_order(void)
{
    reset_order();
    {
        int a __attribute__((cleanup(cleanup_int))) = 12;
        int b __attribute__((cleanup(cleanup_int))) = 13;
        int c __attribute__((cleanup(cleanup_int))) = 14;
        if (a != 12 || b != 13 || c != 14) return 12;
    }
    if (order_count != 3) return 13;
    if (order[0] != 14 || order[1] != 13 || order[2] != 12) return 14;
    return 0;
}

int main(void)
{
    int result;

    result = test_scope_exit();
    if (result) return 20 + result;

    result = test_nested_scopes();
    if (result) return 30 + result;

    result = test_return();
    if (result) return 40 + result;
    if (order_count != 2) return 41;
    if (order[0] != 5 || order[1] != 4) return 42;

    result = test_break();
    if (result) return 50 + result;

    result = test_continue();
    if (result) return 60 + result;

    // result = test_goto();
    // if (result) return 70 + result;

    result = test_declaration_order();
    if (result) return 80 + result;

    return 0;
}
