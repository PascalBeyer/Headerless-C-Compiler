// run
// os linux

// glibc declares the POSIX strerror_r as:
//
//     extern int strerror_r(int, char *, size_t)
//         __asm__("__xpg_strerror_r");
//
// We should therefore calls __xpg_strerror_r and return 0 on success.
// Instead of calling strerror_r which returns something else.
// 

extern int strerror_r(int, char *, size_t) __asm__("__xpg_strerror_r");

int main(void){
    char buffer[256] = { 0 };
    int result = strerror_r(1, buffer, sizeof(buffer));
    return result;
}
