// check "(14,23): Error at '+': Cannot use pointer arithmetic on function-pointer."
// check "(15,23): Error at '+=': Cannot use pointer arithmetic on function-pointer."
// fail

void *asd = 0;

void *asd2 = asd + 3;
void *asd3 = asd += 3;

void func(){}

void (*func_ptr)(void) = func;

void *asd4 = func_ptr + 1;
void *asd5 = func_ptr += 1;
