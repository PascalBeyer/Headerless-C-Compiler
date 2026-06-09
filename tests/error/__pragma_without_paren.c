// fail "'__pragma': Expected a '(' after builtin macro '__pragma'."
// broken Maybe we want to allow this? why not?

int _start(){
    int __pragma = 1;
    return __pragma;
}




