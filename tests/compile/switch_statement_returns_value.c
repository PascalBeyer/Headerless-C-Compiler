// reject "Warning"

int switch_statement_returning_value(int a){
    switch(a){
        
        case 1: return 1;
        case 2: { return 1; } break;
        default: {
            return 1337;
        }
    }
}

int main(){
    return switch_statement_returning_value(1337);
}
