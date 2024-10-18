// check "'function': Function declared 'inline' but not 'extern' is implicitly 'extern'"

int function();
inline int function(){
    return 1;
}


int main(){
    return function();
}
