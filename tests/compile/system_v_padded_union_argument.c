// compile

union __attribute__((aligned(16))) padded_union1 {
     short member;
};

union __attribute__((aligned(32))) padded_union2 {
    __int64 member;
    char member2;
};

union padded_union1 takes_padded_union1(union padded_union1 value){
    return value;
}

union padded_union2 takes_padded_union2(union padded_union2 value){
    return value;
}

int _start(void){
    union padded_union1 pu1 = {1};
    pu1 = takes_padded_union1(pu1);
    
    union padded_union2 pu2 = {1};
    pu2 = takes_padded_union2(pu2);
    
    return 0;
}
