// run

unsigned char global = 0xff;

int main(){    
    
    unsigned char *local = &global;
    
    unsigned int  a = ++(*local);
    
    return a;
}

