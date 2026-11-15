// run

int main(){
    int count = 0;
    
    do{
        continue;
    }while(({count++ < 9;}));
    
    return count == 10 ? 0 : 1;
}
