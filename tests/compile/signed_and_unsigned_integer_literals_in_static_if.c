
typedef _Atomic(char)      atomic_char;

__declspec(inline_asm) _Bool __atomic_compare_exchange_char(atomic_char *object, char *expected, char desired){
    
    mov al, [expected]
    lock cmpxchg [object], desired
    mov [expected], al
    
    sete al
    return al
}


extern inline char __atomic_fetch_or_char(atomic_char *object, char operand){
    char desired, expected = *object;
    do{
        desired = operand | expected;
    }while(!__atomic_compare_exchange_char(object, &expected, desired));
    return expected;
}

atomic_char a_char;

int main(){
    char n_char;
    
    n_char = __atomic_fetch_or_char(&a_char, n_char);
    
}
