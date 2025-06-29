// run
// check "hello system :)"

int main(){
    __declspec(dllimport) int system(char *);
    system("echo hello system :)");
}
