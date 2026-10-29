// compile -Wall
// check "(5,9): Warning 48 at 'sizeof': Expression of function type has undefined size, returning 0."

int main(){
	return sizeof(main);
}


