
typedef unsigned char jmp_buf[256];

int setjmp(jmp_buf env);
int longjmp(jmp_buf env, int return_value);


