// fail "Error at 'member_that_is_not_contained_in_S': Identifier is not a member of structure."

struct S { struct S *next; int x; };

struct S v = (struct S) { .member_that_is_not_contained_in_S = 42 };
