// check "(10,16): Error at 'align': Alignment must be a power of two."
// check "(14,12): Error at 'align': Alignment must be a power of two."
// check "(19,19): Error at 'align': Alignment must be a power of two."
// check "(25,5): Error at '_Alignas': Alignment must be a power of two."
// check "(29,1): Error at '_Alignas': Alignment must be a power of two."
// fail


struct arst{
    __declspec(align(169)) int a;
};


__declspec(align(169)) struct arst2{
    int a;
};


struct __declspec(align(169)) arst3{
    int a;
};


struct tsra{
    _Alignas(169) int a;
};


_Alignas(169) struct tsra2{
    int a;
};

