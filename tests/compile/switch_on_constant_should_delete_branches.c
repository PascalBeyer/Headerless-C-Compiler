
void branch_a();
void branch_b();
void branch_default();

int main(){
    int ret = 1;
    
    switch(2){
        case 1: branch_a(); break;
        case 2: ret = 0; break;
        case 3: branch_b(); break;
        default: branch_default();
    }
    
    return ret;
}

