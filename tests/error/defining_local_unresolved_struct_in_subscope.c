// fail "Undeclared struct."

int main(){
    
    struct asd *asd;
    {
        struct asd{int a;};
        asd->a;
    }
    
    return 0;
}



