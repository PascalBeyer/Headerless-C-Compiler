// check "'a': Initializer is not a constant."
// fail "'b': Initializer is not a constant."

unsigned char a = "arst"[0]; // @incomplete: both clang and gcc allow this.

unsigned char b = (char[3]){1, 2, 3}[0]; // but for whatever reason they don't allow this?

int main(){
     a, b;
}
