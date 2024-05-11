// run

#define assert(a) if(!(a)) return -1

float asd = (float)1337;

int main(){
	float asd2 = (float)1337;
	assert(asd == 1337);
	assert((int)asd == 1337);
	assert(asd == asd2);
	return 0;
}