
int main(){
    int *values;
    struct { int code: 8; } *reg = 0;
    values[reg->code];  // also already borked
    values[reg->code = 5];
}
