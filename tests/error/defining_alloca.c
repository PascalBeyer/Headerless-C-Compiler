// fail "Cannot define intrinsic declaration '_alloca'."

void *_alloca(unsigned __int64 size){
    return 0;
}

// @cleanup: what should even happen here?
int main(){
    return (int)(unsigned __int64)_alloca(1);
}
