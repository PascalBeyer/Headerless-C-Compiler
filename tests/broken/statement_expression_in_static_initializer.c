// broken

// GCC allows a statement expression in a static local initializer and uses
// its value:
//
//     static int y = ({ 9; });   /* y == 9 */
//
// HLC now compiles this (the old '*scope_offsets[scope_at] == ir_size'
// assert is gone), but the initializer is not evaluated: y ends up 0.
// GCC prints 9, HLC prints 0.

int f(void)
{
    int a = 0;

    static int y = ({
                // if(a) return 0;

                9; });
    return y;
}

int main(void)
{
    return f() == 9 ? 0 : 1;
}
