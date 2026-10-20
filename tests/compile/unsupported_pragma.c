// check "(4,9): Warning 16 at 'arst': Unsupported pragma ignored."
// check "(5,1): Warning 16 at 'arst': Unsupported __pragma ignored."

#pragma arst()
__pragma(arst())

int main(){}
