// run

struct simple{
    char a;
    short b;
    int c;
    long long d;
} __attribute__((packed));

struct named_inner{
    char b;
    int c;
};

struct outer_named{
    char a;
    struct named_inner inner;
    char d;
} __attribute__((packed));

struct outer_anon_struct{
    char a;
    struct{
        char b;
        int c;
    };
    char d;
} __attribute__((packed));

struct outer_anon_union{
    char a;
    union{
        char b;
        int c;
    };
    char d;
} __attribute__((packed));

struct outer_anon_bitfields{
    char a;
    struct{
        int b : 4;
        int c : 12;
    };
    char d;
} __attribute__((packed));

struct outer_named_union{
    char a;
    union named_union{
        char b;
        int c;
    } u;
    char d;
} __attribute__((packed));

struct packed_inner{
    char b;
    int c;
} __attribute__((packed));

struct outer_nested_packed{
    char a;
    struct packed_inner inner;
    char d;
} __attribute__((packed));

struct outer_inner_trailing_packed{
    char a;
    struct{
        char b;
        int c;
    } __attribute__((packed)) inner;
    char d;
} __attribute__((packed));

struct outer_inner_aligned{
    char a;
    struct{
        char b;
        int c;
    } __attribute__((aligned(4))) inner;
    char d;
} __attribute__((packed));

struct arrays{
    char a;
    int b[3];
    char c;
} __attribute__((packed));

struct anon_nested{
    char a;
    struct{
        char b;
        struct{
            char c;
            int d;
        };
    };
    char e;
} __attribute__((packed));

struct anon_union_in_struct{
    char a;
    struct{
        char b;
        union{
            char c;
            int d;
        };
    };
    char e;
} __attribute__((packed));

struct multi_attr{
    char a;
    int b;
} __attribute__((packed, aligned(4)));

struct multi_attr_reversed{
    char a;
    int b;
} __attribute__((aligned(4), packed));

struct with_declarator{
    char a;
    int b;
} __attribute__((packed)) with_declarator_instance;

typedef struct{
    char a;
    int b;
} __attribute__((packed)) packed_typedef;

struct{
    char a;
    int b;
} __attribute__((packed)) anonymous_packed_instance;

#ifdef __HLC__
struct declspec_packed{
    char a;
    int b;
} __declspec(packed);

struct lead_bitfields{
    char a;
    int b : 4;
    int c : 12;
    char d;
} __declspec(packed);

struct trail_bitfields{
    char a;
    int b : 4;
    int c : 12;
    char d;
} __attribute__((packed));

struct __declspec(align(8)) lead_align8_trail_align4{
    char a;
} __attribute__((aligned(4)));
#endif

union packed_union{
    char a;
    int b;
} __attribute__((packed));

static struct simple simple_obj;
static struct outer_named outer_named_obj;
static struct outer_anon_struct outer_anon_struct_obj;
static struct outer_anon_union outer_anon_union_obj;
static struct outer_anon_bitfields outer_anon_bitfields_obj;
static struct outer_named_union outer_named_union_obj;
static struct outer_nested_packed outer_nested_packed_obj;
static struct outer_inner_trailing_packed outer_inner_trailing_packed_obj;
static struct outer_inner_aligned outer_inner_aligned_obj;
static struct arrays arrays_obj;
static struct anon_nested anon_nested_obj;
static struct anon_union_in_struct anon_union_in_struct_obj;
static struct multi_attr multi_attr_obj;
static struct multi_attr_reversed multi_attr_reversed_obj;
static packed_typedef packed_typedef_obj;
#ifdef __HLC__
static struct declspec_packed declspec_packed_obj;
static struct lead_bitfields lead_bitfields_obj;
static struct trail_bitfields trail_bitfields_obj;
#endif
static union packed_union packed_union_obj;

#define OFF(obj, member) ((char *)&(obj).member - (char *)&(obj))

int main(){
    // Plain packed struct, including a type larger than the natural alignment.
    if(sizeof(struct simple) != 15) return 1;
    if(OFF(simple_obj, a) != 0) return 2;
    if(OFF(simple_obj, b) != 1) return 3;
    if(OFF(simple_obj, c) != 3) return 4;
    if(OFF(simple_obj, d) != 7) return 5;

    // A named nested struct keeps its own natural layout, but the outer
    // struct places it at an unaligned offset.
    if(sizeof(struct outer_named) != 10) return 6;
    if(OFF(outer_named_obj, a) != 0) return 7;
    if(OFF(outer_named_obj, inner) != 1) return 8;
    if(OFF(outer_named_obj, inner.b) != 1) return 9;
    if(OFF(outer_named_obj, inner.c) != 5) return 10;
    if(OFF(outer_named_obj, d) != 9) return 11;

    // Anonymous nested struct: its members are flattened into the outer
    // member list, but the nested struct itself keeps its natural layout.
    if(sizeof(struct outer_anon_struct) != 10) return 12;
    if(OFF(outer_anon_struct_obj, a) != 0) return 13;
    if(OFF(outer_anon_struct_obj, b) != 1) return 14;
    if(OFF(outer_anon_struct_obj, c) != 5) return 15;
    if(OFF(outer_anon_struct_obj, d) != 9) return 16;

    // Anonymous nested union with a larger non-first member. All union
    // members share the same offset and the union contributes its full size.
    if(sizeof(struct outer_anon_union) != 6) return 17;
    if(OFF(outer_anon_union_obj, a) != 0) return 18;
    if(OFF(outer_anon_union_obj, b) != 1) return 19;
    if(OFF(outer_anon_union_obj, c) != 1) return 20;
    if(OFF(outer_anon_union_obj, d) != 5) return 21;

    // Bitfields inside an anonymous nested struct keep their nested layout,
    // but the whole nested block is shifted to the packed outer offset.
    if(sizeof(struct outer_anon_bitfields) != 6) return 89;
    if(OFF(outer_anon_bitfields_obj, a) != 0) return 90;
    if(OFF(outer_anon_bitfields_obj, d) != 5) return 91;
    outer_anon_bitfields_obj.b = 5;
    outer_anon_bitfields_obj.c = 0x123;
    if(outer_anon_bitfields_obj.b != 5) return 92;
    if(outer_anon_bitfields_obj.c != 0x123) return 93;

    // Named nested union.
    if(sizeof(struct outer_named_union) != 6) return 22;
    if(OFF(outer_named_union_obj, a) != 0) return 23;
    if(OFF(outer_named_union_obj, u) != 1) return 24;
    if(OFF(outer_named_union_obj, u.b) != 1) return 25;
    if(OFF(outer_named_union_obj, u.c) != 1) return 26;
    if(OFF(outer_named_union_obj, d) != 5) return 27;

    // Packed inner struct in a packed outer struct.
    if(sizeof(struct outer_nested_packed) != 7) return 28;
    if(OFF(outer_nested_packed_obj, a) != 0) return 29;
    if(OFF(outer_nested_packed_obj, inner) != 1) return 30;
    if(OFF(outer_nested_packed_obj, inner.b) != 1) return 31;
    if(OFF(outer_nested_packed_obj, inner.c) != 2) return 32;
    if(OFF(outer_nested_packed_obj, d) != 6) return 33;

    // Trailing packed attribute on an inner struct.
    if(sizeof(struct outer_inner_trailing_packed) != 7) return 34;
    if(OFF(outer_inner_trailing_packed_obj, a) != 0) return 35;
    if(OFF(outer_inner_trailing_packed_obj, inner) != 1) return 36;
    if(OFF(outer_inner_trailing_packed_obj, inner.b) != 1) return 37;
    if(OFF(outer_inner_trailing_packed_obj, inner.c) != 2) return 38;
    if(OFF(outer_inner_trailing_packed_obj, d) != 6) return 39;

    // Trailing aligned attribute on an inner struct. The inner struct keeps
    // its natural size, but the packed outer struct still places it unaligned.
    if(sizeof(struct outer_inner_aligned) != 10) return 40;
    if(OFF(outer_inner_aligned_obj, a) != 0) return 41;
    if(OFF(outer_inner_aligned_obj, inner) != 1) return 42;
    if(OFF(outer_inner_aligned_obj, inner.b) != 1) return 43;
    if(OFF(outer_inner_aligned_obj, inner.c) != 5) return 44;
    if(OFF(outer_inner_aligned_obj, d) != 9) return 45;

    // Arrays are placed contiguously in a packed struct.
    if(sizeof(struct arrays) != 14) return 46;
    if(OFF(arrays_obj, a) != 0) return 47;
    if(OFF(arrays_obj, b) != 1) return 48;
    if(OFF(arrays_obj, c) != 13) return 49;

    // Nested anonymous compounds.
    if(sizeof(struct anon_nested) != 14) return 50;
    if(OFF(anon_nested_obj, a) != 0) return 51;
    if(OFF(anon_nested_obj, b) != 1) return 52;
    if(OFF(anon_nested_obj, c) != 5) return 53;
    if(OFF(anon_nested_obj, d) != 9) return 54;
    if(OFF(anon_nested_obj, e) != 13) return 55;

    if(sizeof(struct anon_union_in_struct) != 10) return 56;
    if(OFF(anon_union_in_struct_obj, a) != 0) return 57;
    if(OFF(anon_union_in_struct_obj, b) != 1) return 58;
    if(OFF(anon_union_in_struct_obj, c) != 5) return 59;
    if(OFF(anon_union_in_struct_obj, d) != 5) return 60;
    if(OFF(anon_union_in_struct_obj, e) != 9) return 61;

    // A trailing packed attribute combined with aligned. The packed layout
    // is kept, but the overall size is rounded up to the requested alignment.
    if(sizeof(struct multi_attr) != 8) return 62;
    if(OFF(multi_attr_obj, a) != 0) return 63;
    if(OFF(multi_attr_obj, b) != 1) return 64;

    if(sizeof(struct multi_attr_reversed) != 8) return 65;
    if(OFF(multi_attr_reversed_obj, a) != 0) return 66;
    if(OFF(multi_attr_reversed_obj, b) != 1) return 67;

    // A trailing attribute followed by a declarator.
    if(sizeof(with_declarator_instance) != 5) return 68;
    if(OFF(with_declarator_instance, a) != 0) return 69;
    if(OFF(with_declarator_instance, b) != 1) return 70;

    // A typedef whose struct has a trailing packed attribute.
    if(sizeof(packed_typedef) != 5) return 81;
    if(OFF(packed_typedef_obj, a) != 0) return 82;
    if(OFF(packed_typedef_obj, b) != 1) return 83;

    // An anonymous struct with a trailing packed attribute and a declarator.
    if(sizeof(anonymous_packed_instance) != 5) return 84;
    if(OFF(anonymous_packed_instance, a) != 0) return 85;
    if(OFF(anonymous_packed_instance, b) != 1) return 86;

#ifdef __HLC__
    // MSVC spelling of the same attribute.
    if(sizeof(struct declspec_packed) != 5) return 71;
    if(OFF(declspec_packed_obj, a) != 0) return 72;
    if(OFF(declspec_packed_obj, b) != 1) return 73;
#endif

    // A packed union still has the size of its largest member and alignment 1.
    if(sizeof(union packed_union) != 4) return 74;
    if(_Alignof(union packed_union) != 1) return 75;
    if(OFF(packed_union_obj, a) != 0) return 76;
    if(OFF(packed_union_obj, b) != 0) return 77;

#ifdef __HLC__
    // Packed bitfield layout must be identical for a leading and a trailing
    // packed attribute. HLC's packed bitfield layout differs from GCC's, but
    // the trailing attribute must not diverge from the existing leading path.
    if(sizeof(struct lead_bitfields) != sizeof(struct trail_bitfields)) return 78;
    if(OFF(lead_bitfields_obj, a) != OFF(trail_bitfields_obj, a)) return 79;
    if(OFF(lead_bitfields_obj, d) != OFF(trail_bitfields_obj, d)) return 80;

    // A larger leading alignment must not be reduced by a smaller trailing
    // aligned attribute. The final size is rounded to the larger alignment.
    if(sizeof(struct lead_align8_trail_align4) != 8) return 87;
    if(_Alignof(struct lead_align8_trail_align4) != 8) return 88;
#endif

    return 0;
}
