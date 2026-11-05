

int _start(){
    int a = 10;
    
    switch(a){
        case 1:{
        }break;
        case 2:{
        } __attribute__((fallthrough));
        case 3:{
        }break;
    }
    
    return 0;
}
