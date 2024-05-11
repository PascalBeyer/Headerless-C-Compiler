// run

int main(){    
    char *it = "@ ^@ @  @@  @^@ @ @ ^@@@ @@^@@@ @ ^@";
    
    int count = 0;
    for(; *it; it++){
        if(*it == '@' && it[-1] != '^'){
            count++;
        }
    }
    
    return (count == 0xe) ? 0 : 1;
}
