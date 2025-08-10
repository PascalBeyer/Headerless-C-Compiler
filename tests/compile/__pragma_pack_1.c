
__pragma(pack(push, 1))

struct {
    int a;
    void *b;
    int c;
} s;

__pragma(pack(pop))

_Static_assert(sizeof(s) == 16);

int main(){
    
}
