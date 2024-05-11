// check "'b': Part of the cycle."
// fail  "'a': Part of the cycle."

struct a{
    struct b b;
};

struct b{
    struct a a;
};
