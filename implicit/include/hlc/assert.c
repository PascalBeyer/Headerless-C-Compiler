
#pragma compilation_unit("print.c")

__declspec(noreturn) void __do_assert(char *file, __int64 line, char *expression, char *function){
    
    print("%s(%lld): ASSERT FIRED: '%s' in function %s.\n", file, line, expression, function);
    
    __asm__ { int3 }
    
    while(1){} // Squelch noreturn warning.
}

