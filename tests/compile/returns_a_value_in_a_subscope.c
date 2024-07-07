// reject "Warning"

int returns_a_value_in_a_subscope(){
    { return 0; }
}

int main(){
    return returns_a_value_in_a_subscope();
}
