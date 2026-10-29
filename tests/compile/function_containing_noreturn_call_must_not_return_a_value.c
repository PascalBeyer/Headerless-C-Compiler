// reject "Warning"

__declspec(inline_asm) __declspec(noreturn) void crash_and_burn(unsigned int __exit_code){
    mov ecx, __exit_code
    int 0x29
}

int function(){
    crash_and_burn(0);
}

int main(){
    function();
}
