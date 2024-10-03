// run

__declspec(printlike) char *arst(char *format, ...){
    return format;
}

int main(){
    
    char *format = arst("%%");
    
    if(format[0] != '%') return 1;
    if(format[1] != '%') return 1;
    
    return 0;
}
