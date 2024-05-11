// run

typedef struct unresolved function();
typedef int end_function();

struct unresolved{
    function *f;
    end_function *g;
};

int global_counter;

int g(){
    return global_counter;
}

struct unresolved f(){
    struct unresolved ret = {f, g};
    global_counter++;
    
    return ret;
}

int main(){
    return f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f()
    .f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f()
    .f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().g() == 0x48 ? 0 : 1;
}

