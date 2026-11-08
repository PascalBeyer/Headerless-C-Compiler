// run

// Regression test for the IR arena rollback bug: a function call inside a
// dead block followed by a goto to a label after live code used to corrupt
// the type field of a later IR_pointer_subscript, crashing codegen in
// emit_code_for_pointer_subscript.

static void g(int x) { (void)x; }

int f(unsigned char *cmd)
{
    if (0) {
        g(1);
        goto out;
    }

    g(cmd[0] | 4);

out:
    return 0;
}

int main(void)
{
    unsigned char c[4] = {0};
    return f(c);
}
