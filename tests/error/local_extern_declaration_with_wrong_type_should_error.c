// fail "'extern' declaration inside function has different type from original"

int main(){
    extern int declaration_at_global_scope;
    declaration_at_global_scope = 1;
}

float declaration_at_global_scope;
