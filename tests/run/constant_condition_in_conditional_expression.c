// run

int _start(){
    
    {
        int a = 0 ? 1 : 2;
        if(a != 2) return 1;
        
        int arr[0 ? 1 : 2];
        if(sizeof(arr) != 8) return 1;
        
        int arr2[0 ? a : 2]; // Should I make this not compile?
        if(sizeof(arr2) != 8) return 1;
    }
    
    {
        int a = 1 ? 2 : 1;
        if(a != 2) return 1;
        
        int arr[1 ? 2 : 1];
        if(sizeof(arr) != 8) return 1;
        
        int arr2[1 ? 2 : a]; // Should I make this not compile?
        if(sizeof(arr2) != 8) return 1;
    }
    
    int a = 1, b = 2;
    
    int d = 1 ? a : b;
    if(d != a) return 1;
    
    int e = 0 ? a : b;
    if(e != b) return 1;
    
    void *v = 0;
    
    {
        int *p = 1 ? &a : 0;
        int *p2 = 0 ? &a : 0;
        
        int *p3 = 1 ? &a : (void *)0;
        int *p4 = 0 ? &a : (void *)0;
        
        int *p5 = 1 ? &a : v;
        int *p6 = 0 ? &a : v;
        
        if(p != &a) return 1;
        if(p2 != 0) return 1;
        if(p3 != &a) return 1;
        if(p4 != 0) return 1;
        if(p5 != &a) return 1;
        if(p6 != 0) return 1;
    }
    
    {
        int *p = 1 ? 0 : &a;
        int *p2 = 0 ? 0 : &a;
        
        int *p3 = 1 ? (void *)0 : &a;
        int *p4 = 0 ? (void *)0 : &a;
        
        int *p5 = 1 ? v : &a;
        int *p6 = 0 ? v : &a;
        
        if(p != 0) return 1;
        if(p2 != &a) return 1;
        if(p3 != 0) return 1;
        if(p4 != &a) return 1;
        if(p5 != 0) return 1;
        if(p6 != &a) return 1;
    }
    
    return 0;
}
