// check "(5,1): Warning 16 at 'arst': Unsupported __pragma ignored."

#define pragma_define() __pragma(arst)

pragma_define();

int _start(){}
