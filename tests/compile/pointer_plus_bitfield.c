
int main(){
    struct arst{
        int a : 10;
    } arst = {1337};
    
    int *pointer = (void *)1337;
    pointer + arst.a;
    pointer - arst.a;
    pointer += arst.a;
    pointer -= arst.a;
}
