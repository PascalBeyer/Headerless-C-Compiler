


int main(){
    
    struct arst {
        int arst[5];
    } *type = 0;
    
    type = type + (-1);
    
    return !((__int64)type == -sizeof(struct arst));
}
