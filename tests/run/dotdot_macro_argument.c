// run

#define STRINGIFY(x) #x
#define FIRST_ARGUMENT(a, b) a

const char *dots = STRINGIFY(..);

int main(void) {
    int value = FIRST_ARGUMENT(7, ..);
    
    if(dots[0] != '.' || dots[1] != '.' || dots[2] != 0) return 1;
    return value == 7 ? 0 : 2;
}
