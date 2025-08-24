// run

int _start(){
    
    int a = "123"[1]; // This used to produce the incorrect index, because the backend for array[literal] did not work.
    if(a != '2') return 1;
    
    int b = (char [3]){1, 2, 3}[1];
    if(a != '2') return 1;
    
    return 0;
}


