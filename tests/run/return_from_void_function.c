// run

static int state;

static void early_return(void)
{
    if (state == 1) {
        return;
    }
    state = 2;
}

int main(void)
{
    state = 1;
    early_return();
    if (state != 1) return 1;

    state = 0;
    early_return();
    if (state != 2) return 2;

    return 0;
}
