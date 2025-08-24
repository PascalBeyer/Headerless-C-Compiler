

int main(){
    extern char arst[];
    
    if(p_arst != arst) return 1;
    
    return 0;
}

char arst[] = "arst";
char *p_arst = arst;
