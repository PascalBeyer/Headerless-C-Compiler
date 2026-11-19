
#define arst(a) a

arst(
#if 1
        int z;
#endif
);


#define IS_ENABLED(x) 1
#define MACRO(M...) M

MACRO(
#if IS_ENABLED(1)
        int x;
#endif
)

#define OBJECT IS_ENABLED(1)

MACRO(
#if OBJECT
        int y;
#endif
)

int _start(){ return x + y + z; }
