// run
// os linux

enum A { A4 = 4 };
enum B { BN = -1 };
enum C { CMAX = 0x7fffffff };
enum D { DMAX = 0x80000000u };
enum E { EBIG = 0x100000000ull };
enum F { FN = -1, FBIG = 0x7fffffffffffffffull };

typedef enum __attribute__((packed)) { PA4 = 4 } PA;
typedef enum __attribute__((packed)) { PBN = -1 } PB;
typedef enum __attribute__((packed)) { PC300 = 300 } PC;
typedef enum __attribute__((packed)) { PDN = -200 } PD;
typedef enum __attribute__((packed)) { PEN = -1, PE255 = 255 } PE;

struct SA { enum A v:3; };
struct SB { enum B v:3; };
struct SC { enum C v:31; };
struct SD { enum D v:32; };
struct SPA { PA v:3; };
struct SPB { PB v:3; };
struct SPC { PC v:9; };
struct SPD { PD v:9; };
struct SPE { PE v:9; };

int main(void)
{
    if (sizeof(enum A) != 4 || sizeof(enum B) != 4) return 1;
    if (sizeof(enum C) != 4 || sizeof(enum D) != 4) return 2;
    if (sizeof(enum E) != 8 || sizeof(enum F) != 8) return 3;

    if (sizeof(PA) != 1 || sizeof(PB) != 1) return 4;
    if (sizeof(PC) != 2 || sizeof(PD) != 2 || sizeof(PE) != 2) return 5;

    struct SA a = {0}; a.v = A4;
    struct SB b = {0}; b.v = BN;
    struct SC c = {0}; c.v = CMAX;
    struct SD d = {0}; d.v = DMAX;
    struct SPA pa = {0}; pa.v = PA4;
    struct SPB pb = {0}; pb.v = PBN;
    struct SPC pc = {0}; pc.v = PC300;
    struct SPD pd = {0}; pd.v = PDN;
    struct SPE pe = {0}; pe.v = PE255;

    if (a.v != 4 || b.v != -1) return 6;
    if (c.v != 2147483647) return 7;
    if (d.v != 2147483648u) return 8;
    if (pa.v != 4 || pb.v != -1) return 9;
    if (pc.v != 300 || pd.v != -200 || pe.v != 255) return 10;

    return 0;
}
