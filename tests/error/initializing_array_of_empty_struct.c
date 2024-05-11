// fail "Initializing empty structure or union requires explicit '{}'."

struct empty_struct {};

struct empty_struct array_of_empty_struct[1] = { 1 };
