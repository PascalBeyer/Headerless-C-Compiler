// fail "(3,9): Error at '(': Cannot use ({<statement>}) expressions at global scope."

int y = ({ 9; });

int main(void)
{
    return y;
}
