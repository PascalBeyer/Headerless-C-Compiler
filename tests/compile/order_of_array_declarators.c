
typedef int A[3];
typedef int B[2][3];
typedef int C[1][2][3];

int array[1][2][3];
int (array[1])[2][3];
int ((array[1])[2])[3];
int (array[1][2])[3];

C array;
B array[1];
A array[1][2];

int main(){
    return 0;
}
