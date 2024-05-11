
// @note: if you define this to anything it should not compile.
#define BUG 

struct empty_struct{};

struct empty_struct simple_initialized = {BUG};

union{
    struct empty_struct empty_struct;
    int i;
} union_containing_empty_struct = {BUG};

struct empty_struct array_of_empty_structs[10] = {BUG};

struct structure_containing_empty_struct{
    struct empty_struct empty_struct;
    int i;
} structure_containing_empty_struct = {
    // @note: I feel like this should work. gcc doesn't.
    // gcc:   'asd3.c:19:5: warning: excess elements in struct initializer'
    // clang: 'asd3.c:20:5: error: initializer for aggregate with no elements requires explicit braces'
    BUG
};


int main(){
    struct empty_struct a;
    struct structure_containing_empty_struct b = {
        a,
        1
    };
	struct structure_containing_empty_struct c = {
		{},
		1
	};
}

