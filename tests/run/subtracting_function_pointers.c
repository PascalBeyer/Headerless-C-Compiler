// compile -Wall
// check "(11,33): Warning 26 at '-': Subtracting function-pointers is a GNU extension."
// run

int function(void){ return 0; }

int main(void)
{
    int (*left)(void) = function;
    int (*right)(void) = function;
    long long difference = left - right;
    return (int)difference;
}
