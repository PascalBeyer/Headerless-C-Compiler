// check "(4,9): Warning 16 at 'arst': Unsupported pragma ignored."
// check "(5,10): Warning 16 at 'arst': Unsuported __pragma ignored."

#pragma arst()
__pragma(arst())

int main(){}
