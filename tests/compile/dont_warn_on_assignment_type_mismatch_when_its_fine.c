// reject "Warning"

int main(){
    
    __int32 int32 = 1337;
    
    __int8 int8 = 0;
    int8 = int8 + 37; // Should not warn, as s8 + s8 = s8.
    
    int8 = (int32 % 10);
    int8 = (int32 & 0xf);
    int8 = !int32;
    
    int8 = int8 + sizeof(int32);
    int8 = int8 + sizeof int32;
    int8 = int8 + sizeof(__int32);
    
    int8 = int8 + _Alignof(int32);
    int8 = int8 + _Alignof int32;
    int8 = int8 + _Alignof(__int32);
    
    return int8;
}
