// check "More than one 'default'-case in switch."
// fail  "... Here was the previous 'default'-case."

int _start(){
    switch(1){
        default:
        default:;
    }
    return 1;
}
