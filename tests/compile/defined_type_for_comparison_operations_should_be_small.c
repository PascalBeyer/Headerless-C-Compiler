// reject "Warning"

int main(){
    unsigned __int64 a;
    
    int b = 1, c = 2;
    // These should not warn for int to u64 as they have "defined type" u8.
    a = (b==c);
    a = (b!=c);
    a = (b<c);
    a = (b>c);
    a = (b<=c);
    a = (b>=c);
    a = (b&&c);
    a = (b||c);
    a = (1 == 2);
    a = (1 != 2);
    a = (1 <= 2);
    a = (1 >= 2);
    a = (1 < 2);
    a = (1 > 2);
    a = (1 && 2);
    a = (1 || 2);
    return (int)a;
}
