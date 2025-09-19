// check "Current "pragma pack(show)"-value is 16."

__pragma(pack(push, 1))
struct{
    __pragma(pack(pop))
    #pragma pack(show)
    int a;
} a;

int main(){}
