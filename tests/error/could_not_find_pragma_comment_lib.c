// fail "(3,17): Error at 'lib': Error: Could not find specified library 'this-library-hopefully-does-not-exists-anywhere.lib'."

#pragma comment(lib, "this-library-hopefully-does-not-exists-anywhere.lib")

int _start(){
    
}
