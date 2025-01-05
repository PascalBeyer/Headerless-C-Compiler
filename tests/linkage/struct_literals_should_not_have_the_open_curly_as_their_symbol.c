// compile /c
// link

struct arst{
    struct arst2{
        int a;
    } *m, *m2;
} arst = {
    .m = (struct arst2 []){1, 2, 3, 4},   // These symbols used to be called '{'.
    .m2 = (struct arst2 []){15, 2, 3, 4}, // These symbols used to be called '{'.
};

int main(){
    
    
}
