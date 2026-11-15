// run

int counter = 0;

int for_loop(){
    int i = 0;
    goto label;
    
    for(; i < 2; i++){
        counter += 1;
        return 1;
        
        label:;
    }
    return 0;
}

int while_loop(){
    int i = 0;
    goto label;
    
    while(i++ < 2){
        counter += 1;
        return 1;
        
        label:;
    }
    return 0;
}

int do_while_loop(){
    int i = 0;
    goto label;
    
    do{
        counter += 1;
        return 1;
        
        label:;
    }while(i++ < 2);
    return 0;
}

int main(){
    for_loop();
    while_loop();
    do_while_loop();
    
    return counter - 3;
}

