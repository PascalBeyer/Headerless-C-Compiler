
__declspec(noreturn) __declspec(inline_asm) void noreturn_function(){
    int 0x29
}

int _start(){
    ({noreturn_function();});
    return 0;
}
