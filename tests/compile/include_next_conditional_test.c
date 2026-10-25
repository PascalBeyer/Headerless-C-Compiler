// compile -I include_next_conditional_test/first -I include_next_conditional_test/second

#include <conditional.h>

int main(void) {
    return CONDITIONAL_VALUE == 2 ? 0 : 1;
}
