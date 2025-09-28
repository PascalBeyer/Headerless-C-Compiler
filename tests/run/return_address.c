// run

int _start(){
    
    void *a = _ReturnAddress();
    void *b = *(void **)_AddressOfReturnAddress();
    
    return (a != b);
}
