// fail "Expected a formal argument after '#' in macro replacement list for 'FOO'."

#define FOO(a) # < 2
FOO(a)
