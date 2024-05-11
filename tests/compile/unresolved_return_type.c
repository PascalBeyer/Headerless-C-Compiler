
struct asd unresolved_predeclaration(struct asd2);

static struct string load_entire_file(char *file_path){
    struct string ret = {};
    return ret;
}

struct string{
    char *data;
    __int64 size;
};

int main(){
    struct string string = load_entire_file("hello");
    return (int)string.size;
}
