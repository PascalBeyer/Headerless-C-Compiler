// compile -stdlib
// check "'a': Local variable is never used."

#include <windows.h>
#include <stdio.h>

int main(){
    
#define print(a) printf(#a " %d\n", a)
	
	print(WINVER);
	print(_WIN32_WINNT);
	print(NTDDI_VERSION);
    
    // There used to be a problem where warnings would not show up, 
    // after a system include header, because we report at most 100 warnings.
    // This is a regression for that.
    int a;
}

