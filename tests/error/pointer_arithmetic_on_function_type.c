// fail "Cannot use pointer arithmetic on function-pointer."

extern int bar(void);

int foo(void){
  return (&bar + 4096) ();
}
