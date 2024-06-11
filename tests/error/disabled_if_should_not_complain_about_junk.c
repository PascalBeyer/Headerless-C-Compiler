// reject "Junk"

#if defined(__clang__) || defined(__HLC__)
#define zero_struct {}
#elif defined(_MSC_VER) || defined(__GNUC__)
#define zero_struct {0}
#endif

int main(){
    
}
