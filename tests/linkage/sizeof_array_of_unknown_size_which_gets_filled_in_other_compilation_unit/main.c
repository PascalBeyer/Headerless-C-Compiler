// compile other.c
// run

int array_of_unknown_size[];

int main(){
    int sum = 0;
    for(int i = 0; i < sizeof(array_of_unknown_size)/sizeof(*array_of_unknown_size); i++){
        sum += array_of_unknown_size[i];
    }
    return !(sum == 55);
}
