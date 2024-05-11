
int main(){
    
    struct unresolved *unresolved = 0;
    
    struct unresolved{
        int a;
    } definition;
    
    unresolved = &definition;
}

