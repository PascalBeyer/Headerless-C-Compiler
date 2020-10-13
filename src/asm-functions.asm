
; Was compiled with 'nasm -g -f win64 src/asm-functions.asm -o build/asm-functions.obj'
; This file only exists because for some reason msvc does not let you define the 'mem*' routines
; but requires them to work.

SECTION .text
       global memcpy
       global memset
       global memcmp
       global strcmp
       global strlen

memcpy:
        ; rcx = dest 
        ; rdx = source
        ; r8  = count
        mov     rax, rcx                ; return original destination pointer  
        mov     rcx, r8                 ; move count to rcx

        mov     r8, rsi                 ; save rsi in r8
        mov     r9, rdi                 ; save rdi in r9

        mov     rdi, rax                ; move destination pointer to rdi
        mov     rsi, rdx                ; move source pointer to rsi

        rep     movsb                   ; copy source to destination buffer
        mov     rsi, r8                 ; restore rsi
        mov     rdi, r9                ; restore rdi
        ret
        ; return = dest

memset:
        ; rcx = dest 
        ; rdx = value
        ; r8  = count

        mov     r9, rdi  ; save rdi in r9

        mov     rax, rdx ; load 'value'
        mov     rdi, rcx ; load 'dest'
        xchg    rcx, r8  ; move count to rcx, and the original destiniation to r8

        rep     stosb    ; copy AL to destination buffer

        mov     rax, r8  ; return original destination buffer
        mov     rdi, r9  ; restore rdi
        ret

        ; return = dest        

memcmp:
        ; rcx = ptr1
        ; rdx = ptr2
        ; r8  = count

        mov r9, rdi   ; save rdi
        mov rdi, rcx  ; load 'ptr1'
        mov rcx, r8   ; load 'count'

        mov r8, rsi   ; save rsi
        mov rsi, rdx  ; load 'ptr2'

        xor eax, eax  ; clear eax, later used for the return value
        xor edx, edx  ; clear edx, later used for the return value

        repe cmpsb    ; compare 'ptr1' to 'ptr2' for 'count' bytes

        setl dl       ; set edx if last byte had 'ptr1' < 'ptr2'
        setg al       ; set eax if last byte had 'ptr1' > 'ptr2'
        sub  eax, edx ; exactly -1 if 'ptr1' < 'ptr2' and 1 if 'ptr1' > 'ptr2'

        mov rdi, r9   ; restore rdi
        mov rsi, r8   ; restore rsi

        ret


strcmp:
        ; rcx = ptr1
        ; rdx = ptr2

        mov r9, rdi   ; save rdi

        mov r8, rsi   ; save rsi

        mov rdi, rcx  ; load 'ptr1'
        mov rsi, rdx  ; load 'ptr2'

        xor eax, eax  ; clear eax, later used for the return value
        xor edx, edx  ; clear edx, later used for the return value

        loop:

        cmpsb                 ; compare 'ptr1' to 'ptr2', increment both
        jne unequal           ; break strings unequal

        movzx ecx, byte[rdi]
        test ecx, ecx           ; check if we are at the zero terminator
        je equal              ; break both strings zero

        jmp loop              ; loop

        unequal:

        setl dl       ; set edx if last byte had 'ptr1' < 'ptr2'
        setg al       ; set eax if last byte had 'ptr1' > 'ptr2'
        sub  eax, edx ; exactly -1 if 'ptr1' < 'ptr2' and 1 if 'ptr1' > 'ptr2'

        equal:

        mov rdi, r9   ; restore rdi
        mov rsi, r8   ; restore rsi

        ret
; @cleanup: do these all have to do cld (clear direction flag)
strlen:
     ; rcx = string

     mov r9, rdi ; save rdi

     mov rdi, rcx ; load 'string'
     mov rcx, -1  ; load larges possible string
     xor eax, eax ; zero compare operant
     repne scasb  ; compare until string is zero

     not rcx      ; as this is computed in two compliment and rcx got decremented each comparison, 
                  ; rcx now holds the length

     lea rax, [rcx - 1] ; load the length into rax

     mov rdi, r9 ; restore rdi
    
     ret

