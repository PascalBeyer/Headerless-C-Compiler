// check "(5,15): Warning 16 at 'arst': Unsupported __pragma ignored."

#define pragma_define() __pragma(arst)

pragma_define();

int _start(){}
