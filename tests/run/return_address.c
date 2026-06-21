// run

int main(){
    
    void *a = _ReturnAddress();
    void *b = *(void **)_AddressOfReturnAddress();
    
    return (a != b);
}
