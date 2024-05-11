// run

static int function_that_is_only_referanced_by_global_declaration(int a){
    return a;
}

static int (*global_declaration)(int) = &function_that_is_only_referanced_by_global_declaration;

int main(){
    return global_declaration(0);
}
