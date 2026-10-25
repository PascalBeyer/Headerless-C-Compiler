// run

int main(void) {
    if(sizeof(1) != sizeof(int)) return 1;
    if(sizeof(1L) != sizeof(long)) return 2;
    if(sizeof(1LL) != sizeof(long long)) return 3;
    if(sizeof(1U) != sizeof(unsigned int)) return 4;
    if(sizeof(1UL) != sizeof(unsigned long)) return 5;
    if(sizeof(1ULL) != sizeof(unsigned long long)) return 6;

    if(sizeof(1 + 2) != sizeof(int)) return 7;
    if(sizeof(1 + 2L) != sizeof(long)) return 8;
    if(sizeof(1L + 2L) != sizeof(long)) return 9;
    if(sizeof(1 + 2LL) != sizeof(long long)) return 10;
    if(sizeof(1U + 2UL) != sizeof(unsigned long)) return 11;

    if(sizeof(typeof(1L)) != sizeof(long)) return 12;
    if(sizeof(typeof(1 + 2L)) != sizeof(long)) return 13;

    if(sizeof(0x1) != sizeof(int)) return 101;
    if(sizeof(0x1L) != sizeof(long)) return 102;
    if(sizeof(0x1LL) != sizeof(long long)) return 103;
    if(sizeof(0x1U) != sizeof(unsigned int)) return 104;
    if(sizeof(0x1UL) != sizeof(unsigned long)) return 105;
    if(sizeof(0x1ULL) != sizeof(unsigned long long)) return 106;
    
    if(sizeof(0x1 + 0x2) != sizeof(int)) return 107;
    if(sizeof(0x1 + 0x2L) != sizeof(long)) return 108;
    if(sizeof(0x1L + 0x2L) != sizeof(long)) return 109;
    if(sizeof(0x1 + 0x2LL) != sizeof(long long)) return 1010;
    if(sizeof(0x1U + 0x2UL) != sizeof(unsigned long)) return 1011;
    
    if(sizeof(typeof(0x1L)) != sizeof(long)) return 1012;
    if(sizeof(typeof(0x1 + 0x2L)) != sizeof(long)) return 1013;
    
    return 0;
}
