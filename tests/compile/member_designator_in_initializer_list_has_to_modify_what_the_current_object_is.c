// check "Wanted 'void*' given 'int'."


struct{
    int a;
    int b;
    void *c;
} arst = {
    .b = 1,
    2 // This should initialize the 'void *c' and thus report a warning. This used to initialize the 'int b;' as I forgot to set 'member_at'.
};

int main(){
}
