_Noreturn void die(void);

struct S {
    struct S *next;
};

struct S *g;

int f(void)
{
    typeof(
            __builtin_choose_expr(
                0 || __builtin_types_compatible_p(
                    typeof(
                        ({ 
                              if(g) die();
                              g; 
                        })
                    ), 
                int), 
            (_Bool) 1, 
            (_Bool) 0
            )
        ) w;
    w = 0;
    return w != 0;
}

int main(void)
{
    return f();
}
