// compile /std:c11
// check "Redefinition of macro 'alignas'."

#include <stdalign.h>

#define alignas(x) __declspec(align(x))

int main(){}

