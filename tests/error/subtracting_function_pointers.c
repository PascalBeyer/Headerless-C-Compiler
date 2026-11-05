// fail "(9,28): Error at '-': Cannot subtract function pointers."

int function(void){}

int main(void)
{
    int (*left)(void) = function;
    int (*right)(void) = function;
    long difference = left - right;
    return (int)difference;
}
