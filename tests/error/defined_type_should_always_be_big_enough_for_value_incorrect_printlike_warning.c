// reject "Warning"

__declspec(printlike) int print(char *format, ...){
	(void)format;
    return 0;
}

int main(){
    
    __int64 arst = 0;
    print("%lld\n", arst + 1);	// This used to warn because we had the wrong defined type.
}
