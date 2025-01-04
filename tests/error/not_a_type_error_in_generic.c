// fail "Error at 'arst': Expected a type-name or 'default' while parsing a _Generic association list."

int main(){
    int a;
    _Generic(a, arst: 123 ); // This used to infinitely loop.
}
