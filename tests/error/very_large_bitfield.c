// fail "Bitfield width for type ' 'unsigned char'' must be '<= 8', but is '6353347876179427265'."
struct {
    unsigned char ident_0 : 6353347876179427265;
};