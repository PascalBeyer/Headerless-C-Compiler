
struct {
    int member;
} a;

#define member a.member

#define def(m) m
#define def2(m) m + m

int main(){
    int b = def(member);
    int c = def2(member);
    return b + c;
}
