

#define CONCAT_INNER(a, b) a ## b
#define CONCAT(a, b) CONCAT_INNER(a, b)

#define CONCAT_LINE(a) CONCAT(a, __LINE__)

int main(){
    
    int CONCAT_LINE(a) = 1;
    
    return a10;
}
