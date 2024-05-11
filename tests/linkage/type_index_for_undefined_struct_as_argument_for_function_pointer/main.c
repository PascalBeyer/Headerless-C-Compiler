// compile other.c

typedef void (*function)(struct s *s);

typedef struct s{
    function functions[13];
} struc;

function f = (void *)0;

int main(){
    struct s s;
    f(&s);
    return 0;
}


