// fail "(9,28): Error at '-': Cannot subtract pointers to incomplete types."

struct incomplete;

int main(void)
{
    struct incomplete *left = 0;
    struct incomplete *right = 0;
    long difference = left - right;
    return (int)difference;
}
