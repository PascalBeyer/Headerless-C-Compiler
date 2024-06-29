// run

struct arst{ int a[3]; } arst = {1, 2, -3};

int main(){
    extern struct arst arst;
    
    return arst.a[0] + arst.a[1] + arst.a[2];
}


