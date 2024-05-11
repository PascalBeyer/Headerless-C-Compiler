
typedef struct unresolved sleeping_type;

struct unresolved{
	int a;
};

sleeping_type usage;

int main(){
	usage.a = 1337;
	return usage.a;
}