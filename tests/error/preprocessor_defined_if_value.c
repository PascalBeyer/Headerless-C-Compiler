// fail "(8,2): Error at 'error': #error: M_ONE"

#if !defined M
#define M 1
#endif

#if M
#error M_ONE
#else
#error M_ZERO
#endif

int main(void){ return 0; }
