// check "void(*)()"

int main(){
	void (*ident_0)();
	int *asd = &*ident_0; // '&*ident_0' should be of type 'void (*)()'.
}
