// run

unsigned char incomplete_array[];

int main(){
    unsigned char (*asd)[] = &incomplete_array;
    
    incomplete_array[0];
    incomplete_array + 1;
    *incomplete_array;
    // sizeof(incomplete_array); - error: invalid application of 'sizeof' to an incomplete type 'unsigned char []'
    
    // asd[0];  - error: subscript of pointer to incomplete type 'unsigned char []'
    // asd + 1; - error: arithmetic on a pointer to an incomplete type 'unsigned char []'
    *asd;
    **asd;
    
    // unsigned char *deref = *asd; // this is fine apperantly.
    // 
    // return **asd;
    return 0;
}
