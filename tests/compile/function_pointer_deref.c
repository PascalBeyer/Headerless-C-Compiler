
void (*function_pointer)(void);

void main(){
	function_pointer = main;
	(*function_pointer)();
}
