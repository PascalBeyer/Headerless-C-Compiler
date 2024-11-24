
extern struct unresolved a;

struct unresolved{
    int arst;
};

int *b = &a.arst;

struct unresolved a = {1};


extern struct unresolved2 c;

int *d = &c.arst;

struct unresolved2{
    int arst;
};

struct unresolved2 c = {1};

int main(){
    a.arst;
    int *b = (void *)&a;
    return *b;
}
