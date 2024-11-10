// run

const char arr[] = {
    "Hello" ", " "World!\n"
};

struct {
    char arr[];
} arst = {
    "Hello" ", " "World!\n"
};

int main(){
    const char arr3[] = {
        "Hello" ", " "World!\n"
    };
    
    struct {
        char arr[];
    } arst3 = {
        "Hello" ", " "World!\n"
    };
    
    
    static const char arr2[] = {
        "Hello" ", " "World!\n"
    };
    
    static struct {
        char arr[];
    } arst2 = {
        "Hello" ", " "World!\n"
    };
    
    return arr[0] + arr2[0] + arr3[0] - arst.arr[0] - arst2.arr[0] - arst3.arr[0];
}
