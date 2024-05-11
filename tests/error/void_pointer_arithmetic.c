// check "(9,18): Error at '+': Cannot use pointer arithmetic on pointer to 'void'."
// check "(10,18): Error at '+=': Cannot use pointer arithmetic on pointer to 'void'."
// check "(16,23): Error at '+': Cannot use pointer arithmetic on function-pointer."
// check "(17,23): Error at '+=': Cannot use pointer arithmetic on function-pointer."
// fail

void *asd = 0;

void *asd2 = asd + 3;
void *asd3 = asd += 3;

void func(){}

void (*func_ptr)(void) = func;

void *asd4 = func_ptr + 1;
void *asd5 = func_ptr += 1;
