
int undefined1(void);
int undefined2(void);
int undefined3(void);
int undefined4(void);

int _start(){
    
    if(0 && undefined1()){
        undefined2();
    }
    
    if(1 || undefined3()){
        
    }else{
        undefined4();
    }
    
    return 0;
}
