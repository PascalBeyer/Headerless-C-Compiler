// fail "Cast to function is illegal."

void takes_a_function_pointer (void (*function_pointer) (void *));
void function (void *) { }

int main (void){
  takes_a_function_pointer ((void (void *)) (function));
  return 0;
}
