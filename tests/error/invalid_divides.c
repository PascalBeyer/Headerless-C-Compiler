// check "(8,49): Error at '/': Invalid divide 'LLONG_MIN/(-1)'."
// check "(13,49): Error at '%': Invalid modulation 'LLONG_MIN % (-1)'."
// check "(18,30): Error at '%': Invalid modulation 'INT_MIN % (-1)'."
// check "(23,30): Error at '/': Invalid divide 'INT_MIN/(-1)'."
// fail

__int64 other(){
    __int64 b = -(__int64)9223372036854775808ull/-1;
    return b;
}

__int64 other2(){
    __int64 b = -(__int64)9223372036854775808ull%-1;
    return b;
}

int arst(){
    int b = -(int)2147483648 % -1;
    return b;
}

int _start(){
    int b = -(int)2147483648 / -1;
    return b;
}
