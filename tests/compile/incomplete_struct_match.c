
struct{
   struct type *a;
} struct1;

struct type {
    int a;
};

struct{
   struct type *a;
} struct2;

int main(){
   struct1 = struct2;
}
