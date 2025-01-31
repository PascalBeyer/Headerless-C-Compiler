
#define _MM_SHUFFLE(z, y, x, w) (((z) << 6) | ((y) << 4) | ((x) << 2) | (w))

#include <mmintrin.h>
#include <malloc.h> // For whatever reason, msvc includes malloc.h in xmmintrin.
