

// @note: This is supposed to compile, because 'return_arst' is not referenced,
//        which means 'arst' is not referenced, but thats not how our reference counts work.

extern int arst[3];

static inline int return_arst(int a){
    return arst[a];
}

int main(){}
