// run

int main(){
    int other = 0;
    
    for(int index = 1; index < 10; index++, /*this was discarded because it was marked dead*/({other++;})){
        
        if(index) continue;
        
        return 0;
    }
    
    return (other == 9) ? 0 : 1;
}
