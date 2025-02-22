// fail "Error at '#': Expected a type."

#define arst(a) a error arst

// Make sure it does not except this as an #error directive
arst(#)

int main(){
}
