// run

#define ARRAY_SIZE 
void unknown_size(){
    char c[ARRAY_SIZE] = "asd";
    unsigned char uc[ARRAY_SIZE] = "asd";
    short s[ARRAY_SIZE] = L"asd";
    unsigned short us[ARRAY_SIZE] = L"asd";
    int i[ARRAY_SIZE] = U"asd";
    unsigned int ui[ARRAY_SIZE] = U"asd";
}
#undef ARRAY_SIZE

#define ARRAY_SIZE 3
void no_zero(){
    char c[ARRAY_SIZE] = "asd";
    unsigned char uc[ARRAY_SIZE] = "asd";
    short s[ARRAY_SIZE] = L"asd";
    unsigned short us[ARRAY_SIZE] = L"asd";
    int i[ARRAY_SIZE] = U"asd";
    unsigned int ui[ARRAY_SIZE] = U"asd";
}
#undef ARRAY_SIZE

#define ARRAY_SIZE 4
void right_size(){
    char c[ARRAY_SIZE] = "asd";
    unsigned char uc[ARRAY_SIZE] = "asd";
    short s[ARRAY_SIZE] = L"asd";
    unsigned short us[ARRAY_SIZE] = L"asd";
    int i[ARRAY_SIZE] = U"asd";
    unsigned int ui[ARRAY_SIZE] = U"asd";
}
#undef ARRAY_SIZE

#define ARRAY_SIZE 100
void big_size(){
    char c[ARRAY_SIZE] = "asd";
    unsigned char uc[ARRAY_SIZE] = "asd";
    short s[ARRAY_SIZE] = L"asd";
    unsigned short us[ARRAY_SIZE] = L"asd";
    int i[ARRAY_SIZE] = U"asd";
    unsigned int ui[ARRAY_SIZE] = U"asd";
}
#undef ARRAY_SIZE

int main(){
    unknown_size();
    no_zero();
    right_size();
    big_size();
    return 0;
}

