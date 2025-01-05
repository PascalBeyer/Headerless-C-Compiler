
// @note: we try to find this by _short-name_ which is 'stat.h'
//        but we search by 'sys/stat.h' so we cannot find it and thus include it twice...
#include <sys/stat.h> 
#include <sys/stat.h>

int main(){
    
}

