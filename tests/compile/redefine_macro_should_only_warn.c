// check "Redefinition of macro 'macro'."

#define macro(x) (x)
#define macro(x)

int main(){
    macro(hello syntax error)
}

