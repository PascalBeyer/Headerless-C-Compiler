

enum{
    size = 10,
};

// this caused an infinite loop, as 'size' was marked 'static' but it is not, 
// and thus we slept on the wrong declaration
static char array[size];

void main(){
    
}
