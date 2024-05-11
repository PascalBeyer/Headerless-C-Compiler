
#define make_wide__internal(a) L ## a
#define make_wide(a) make_wide__internal(a)
#define stringify(a) make_wide(#a)

int main(){
    stringify(!";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
}