// run

// '##' right after a macro argument is a token paste even when whitespace
// precedes it, and pasting an empty argument with a non-empty one yields the
// non-empty one. HLC's preprocessor used to assert in expand_define while
// looking for the paste's prefix when the left argument expanded to nothing
// ("src/preprocess.c: 'context->token_stack.size > ...'").

#define CAT(a, b) a ## b

int main(void){
    int pasted_left  = CAT(, 1);
    int pasted_right = CAT(2, );

    if(pasted_left != 1) return 1;
    if(pasted_right != 2) return 2;

    return 0;
}
