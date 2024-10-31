
#include <stddef.h>

int main(){
    ptrdiff_t ptr;
    size_t size;
    // max_align_t max_align;
    wchar_t wchar;
    void *null = NULL;
    
    struct structure{
        int a;
    };
    
    return offsetof(struct structure, a);
}
