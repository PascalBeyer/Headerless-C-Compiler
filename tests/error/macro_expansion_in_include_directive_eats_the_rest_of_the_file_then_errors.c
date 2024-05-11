// fail "Expected a ')' while collecting arguments for macro expansion."

// This does not matter only has to be here.
#define M1(A1, A2, A3, A4)

// This collects all of its "arguments" until the end of the file. Then reports an error.
#define M2(N) M1(L, N, R, 

// This pushes a postponed directive, but is then never resolves, as the M2 eat the entire file.
#include M2(d)

