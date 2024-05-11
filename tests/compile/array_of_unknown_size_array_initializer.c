enum {
	READ,
	WRITE,
	DISCARD,
	OTHER,
	NUM,
};

static const unsigned int array[] = {
	[READ] = 1,
	[WRITE] = 2,
	[DISCARD] = 3,
	[OTHER] = 4,
};

typedef int static_assert[ sizeof(array) / sizeof(*array) == NUM ? 1 : -1];

int main(){
}
