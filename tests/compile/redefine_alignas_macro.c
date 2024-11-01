// check "Redefinition of macro 'alignas'."

#include <stdalign.h>

#define alignas(x) __declspec(align(x))

int main(){}

