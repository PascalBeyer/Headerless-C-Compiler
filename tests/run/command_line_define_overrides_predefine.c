// compile -D__GNUC__=11 -D__GNUC_MINOR__=4 -D_MSC_VER=1337
// run

// Command-line -D definitions must override compiler-provided predefines.

int main(void) {
    return (__GNUC__ == 11 && __GNUC_MINOR__ == 4 && _MSC_VER == 1337) ? 0 : 1;
}
