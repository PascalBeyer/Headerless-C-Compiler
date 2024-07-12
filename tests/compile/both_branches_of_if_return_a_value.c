// reject "Warning"

int returns_values_in_if(int a){
    if(a) return 1;
    else { return 0; }
}

int main(){
    return returns_values_in_if(1);
}
