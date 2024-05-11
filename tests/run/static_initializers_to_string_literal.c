// run

#define assert(a) if(!(a)) return -1

int main(){
    static char array[] = "0123456789";
    assert(array[0] == '0' && array[9] == '9' && array[10] == 0);
    
    static char array2[10] = "0123456789";
    assert(array2[0] == '0' && array2[9] == '9' && array2[10] == 0);
    
    static char *pointer = "0123456789";
    assert(pointer[0] == '0' && pointer[9] == '9' && pointer[10] == 0);
    
    return 0;
}
