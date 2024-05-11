
int function(unresolved arg);
int function(unresolved arg){
    return arg;
}

typedef int unresolved;

int function2(struct unresolved);
int function2(struct unresolved arg){
    return arg.member;
}

struct unresolved{
    int member;
};

int main(){
    function(1);
    function2((struct unresolved){1});
    return 0;
}
