// run

struct structure_that_ends_in_flexible_array{
    int hello;
    void *flexible_array[];
} arst = {
    6,
    {
        &arst.flexible_array[0],
        &arst.flexible_array[1],
        &arst.flexible_array[2],
        &arst.flexible_array[3],
        &arst.flexible_array[4],
        &arst.flexible_array[5],
    }
};


int main(){
    for(int i = 0; i < arst.hello; i++){
        if(arst.flexible_array[i] != &arst.flexible_array[i]) return 1;
    }
    return 0;
}

