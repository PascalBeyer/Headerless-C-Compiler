
#include <stdio.h>


struct string{
    union{
        char *data;
        char *memory;
    };
    union{
        u64 size;
        u64 length;
        u64 amount;
    };
};

struct string load_file(char *file_name){
    struct string ret = {0};
    
    FILE *handle = fopen(file_name, "rb");
    
    if(!handle) return ret;
    
    fseek(handle, 0, SEEK_END);
    
    size_t size = _ftelli64(handle);
    
    if(size == -1) return ret; // @note: '-1' might be the worst error value....
    
    fseek(handle, 0, SEEK_SET);
    
    char *memory = malloc(size + 1);
    memory[size] = 1;
    
    fread(memory, 1, size, handle);
    
    fclose(handle);
    
    ret.data = memory;
    ret.size = size;
    
    return ret;
}

