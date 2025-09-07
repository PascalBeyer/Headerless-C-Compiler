// compile /thread-count=8

// Used to fail if thread.
struct a{};
struct b{
    struct a;
};

typedef int t;

struct c{
    t m;
};

int arr[] = {
    1, 2, 3, 4, 5, 6,
};

int size = sizeof(arr);

int main(){
    
}
