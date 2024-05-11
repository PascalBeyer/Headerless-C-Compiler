// check "Got 'struct i_am_an_enum', but 'i_am_an_enum' is an enum."
// check "Got 'union i_am_an_enum', but 'i_am_an_enum' is an enum."
// check "Got 'enum i_am_a_struct' but 'i_am_a_struct' is a struct."
// check "Got 'union i_am_a_struct', but 'i_am_a_struct' is a struct."
// check "Got 'enum i_am_a_union' but 'i_am_a_union' is a union."
// check "Got 'struct i_am_a_union', but 'i_am_a_union' is a union."
// fail

enum i_am_an_enum{
    enum_member,
};

struct i_am_a_struct{
    int struct_member;
};
    
union i_am_a_union{
    int union_member;
};

struct i_am_an_enum  se;
union  i_am_an_enum  ue;
enum   i_am_a_struct es;
union  i_am_a_struct us;
enum   i_am_a_union  eu;
struct i_am_a_union  su;
