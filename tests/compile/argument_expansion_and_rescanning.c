
struct {
    int member;
} a;

#define member a.member

#define def(m) m

int main(){
    int b = def(member);
    return b;
}
