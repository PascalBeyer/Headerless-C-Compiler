
struct{
   unresolved *a;
   struct unresolved_struct *s;
}s;

struct unresolved_struct{
   char *asd;
};

typedef int unresolved;

int main(){
   *s.a;
   s.a[0];
   *(&s)->a;
   
   s.s->asd;
   s.s[0];
   (*s.s).asd[0];
}
