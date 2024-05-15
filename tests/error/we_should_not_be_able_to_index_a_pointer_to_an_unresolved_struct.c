// fail "'unresolved': Undeclared struct."

static void function(struct unresolved *unresolved){
    unresolved[1];
}

// struct unresolved{ int member; };

int main(){
    
}
