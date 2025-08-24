

#pragma pack(push, 1)
typedef struct { unsigned short v; } 
#pragma pack(pop) 
u16_1;


__pragma(pack(push, 1)) typedef struct { unsigned short v; } __pragma(pack(pop)) u16_2;

static_assert(_Alignof(u16_1) == 1);
static_assert(_Alignof(u16_2) == 1);

int main(){
    
}
