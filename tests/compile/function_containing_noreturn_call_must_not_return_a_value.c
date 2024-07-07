// reject "Warning"

__declspec(inline_asm) __declspec(noreturn) void __fastfail(unsigned int __exit_code){
    mov ecx, __exit_code
    int 0x29
}

int function(){
    
    __fastfail(0);
}

int main(){
    function();
}
