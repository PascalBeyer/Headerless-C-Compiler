
// @cleanup: Get rid of these once we have implemented slices.
#define string(a) ((struct string){.data = a, .amount = (sizeof(a) - 1)})
#define const_string(a) {.data = a, .amount = (sizeof(a) - 1)}

#pragma compilation_unit("string.c")
