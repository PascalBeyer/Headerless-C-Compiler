// os linux
// check "Error at 'A1': Overflow in enumeration value."
// check "Error at 'B1': Overflow in enumeration value."
// check "Error at 'C1': Overflow in enumeration value."
// check "Error at 'D1': Overflow in enumeration value."
// check "Error at 'E1': Overflow in enumeration value."
// check "Error at 'F1': Overflow in enumeration value."
// fail

enum A { A0 = 0x7fffffff, A1 };
enum B { B0 = 0xffffffffu, B1 };
enum C { C0 = 0xffffffffffffffffULL, C1 };
enum D { D0 = 0x7fffffffffffffffLL, D1 };

// The expression type is wider, but the value fits into an int, so the next
// increment is still performed as an int and overflows.
enum E { E0 = 0x7fffffffL, E1 };
enum F { F0 = 0x7fffffffu, F1 };
