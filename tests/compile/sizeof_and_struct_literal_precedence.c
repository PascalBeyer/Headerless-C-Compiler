

struct arst{
    int a;
};


int main(){
    return sizeof (struct arst){0}.a;
}
