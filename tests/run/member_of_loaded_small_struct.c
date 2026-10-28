// run
/*
Assigning one small struct to another produces a loaded aggregate value.
Accessing a member of that assignment result still needs a memory base, so
the compiler has to spill it first. LuaJIT does this in irt_t((ir->t = fins->t)).
*/

struct inner {
    unsigned char value;
};

struct outer {
    int padding;
    struct inner t;
};

int main(void) {
    struct outer source = { 0, { 42 } };
    struct outer destination = { 0, { 0 } };
    
    struct outer *fins = &source;
    struct outer *ir = &destination;
    
    int value = (ir->t = fins->t).value;
    return value == 42 ? 0 : 1;
}
