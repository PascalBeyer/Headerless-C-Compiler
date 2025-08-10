// run

__declspec(inline_asm) unsigned char test(){
    mov eax, 0x13371337
    return al
}


int main(){
    if(test() != 0x37) return 1;
}
