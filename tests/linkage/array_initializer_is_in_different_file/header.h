
typedef struct structure{
    char *name;
    char *desc;
    int (*create)(void);
} structure;

extern structure my_structure;
