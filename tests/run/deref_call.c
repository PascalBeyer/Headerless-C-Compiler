// run

typedef int (*xfn)( );
static int func (xfn fn)
{
    return (*fn)();
}


int main(){
	
	
	static int int_func(){
		return 1337;
	}
	return 1337 == func(int_func) - 1;
}

