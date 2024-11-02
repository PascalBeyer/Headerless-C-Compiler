// reject "Undeclared identifier."
// fail "'extern' variable was never defined."

int main(){
    extern int undefined;
    undefined = 1;
}
