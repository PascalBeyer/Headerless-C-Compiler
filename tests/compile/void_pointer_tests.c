// reject "Warning"

int main(){
    struct s{int a;} *pointer_to_struct = 0;
    union  u{int a;} *pointer_to_union = 0;
    enum   e{asdasd} *pointer_to_enum = 0;
    (void)asdasd;
    
    int *pointer_to_int = 0;
    unsigned int *pointer_to_uint = 0;
    int **pointer_to_pointer = 0;
    int (*pointer_to_array)[10] = 0;
    int (*pointer_to_array_of_unknown_size)[] = 0;
    struct unresolved *pointer_to_unresolved = 0;
    void *void_pointer = 0;
    
    char array[10] = {0};
    
    
    
    
    // 
    // Equality operators:
    // 
    //  "one operand is a pointer to an object type and the other is a pointer to [...] void."
    //  "one operand is a pointer and the other is a null pointer contant."
    // 
    
    (pointer_to_struct  == void_pointer);
    (pointer_to_union   == void_pointer);
    (pointer_to_enum    == void_pointer);
    (pointer_to_int     == void_pointer);
    (pointer_to_uint    == void_pointer);
    (pointer_to_pointer == void_pointer);
    (pointer_to_array   == void_pointer);
    (pointer_to_array_of_unknown_size == void_pointer);
    (pointer_to_unresolved == void_pointer);
    (array == void_pointer);
    
    (pointer_to_struct  != void_pointer);
    (pointer_to_union   != void_pointer);
    (pointer_to_enum    != void_pointer);
    (pointer_to_int     != void_pointer);
    (pointer_to_uint    != void_pointer);
    (pointer_to_pointer != void_pointer);
    (pointer_to_array   != void_pointer);
    (pointer_to_array_of_unknown_size != void_pointer);
    (pointer_to_unresolved != void_pointer);
    (array != void_pointer);
    
    (void_pointer == pointer_to_struct);
    (void_pointer == pointer_to_union);
    (void_pointer == pointer_to_enum);
    (void_pointer == pointer_to_int);
    (void_pointer == pointer_to_uint);
    (void_pointer == pointer_to_pointer);
    (void_pointer == pointer_to_array);
    (void_pointer == pointer_to_array_of_unknown_size);
    (void_pointer == pointer_to_unresolved);
    (void_pointer == array);
    
    (void_pointer != pointer_to_struct);
    (void_pointer != pointer_to_union);
    (void_pointer != pointer_to_enum);
    (void_pointer != pointer_to_int);
    (void_pointer != pointer_to_uint);
    (void_pointer != pointer_to_pointer);
    (void_pointer != pointer_to_array);
    (void_pointer != pointer_to_array_of_unknown_size);
    (void_pointer != pointer_to_unresolved);
    (void_pointer != array);
    
    (void_pointer == void_pointer);
    (void_pointer != void_pointer);
    
    (pointer_to_struct  == 0);
    (pointer_to_union   == 0);
    (pointer_to_enum    == 0);
    (pointer_to_int     == 0);
    (pointer_to_uint    == 0);
    (pointer_to_pointer == 0);
    (pointer_to_array   == 0);
    (pointer_to_array_of_unknown_size == 0);
    (pointer_to_unresolved == 0);
    (array == 0);
    
    (pointer_to_struct  != 0);
    (pointer_to_union   != 0);
    (pointer_to_enum    != 0);
    (pointer_to_int     != 0);
    (pointer_to_uint    != 0);
    (pointer_to_pointer != 0);
    (pointer_to_array   != 0);
    (pointer_to_array_of_unknown_size != 0);
    (pointer_to_unresolved != 0);
    (array != 0);
    
    (0 == pointer_to_struct);
    (0 == pointer_to_union);
    (0 == pointer_to_enum);
    (0 == pointer_to_int);
    (0 == pointer_to_uint);
    (0 == pointer_to_pointer);
    (0 == pointer_to_array);
    (0 == pointer_to_array_of_unknown_size);
    (0 == pointer_to_unresolved);
    (0 == array);
    
    (0 != pointer_to_struct);
    (0 != pointer_to_union);
    (0 != pointer_to_enum);
    (0 != pointer_to_int);
    (0 != pointer_to_uint);
    (0 != pointer_to_pointer);
    (0 != pointer_to_array);
    (0 != pointer_to_array_of_unknown_size);
    (0 != pointer_to_unresolved);
    (0 != array);
    
    (0 == 0);
    (0 != 0);
    
    (void_pointer == 0);
    (0 == void_pointer);
    (void_pointer != 0);
    (0 != void_pointer);
    
    
    // 
    // Conditional operator:
    // 
    //  "one operand is a pointer to an object type and the other is a pointer to [...] void."
    //  "one operand is a pointer and the other is a null pointer constant."
    // 
    
    int asd;
    
    asd ? void_pointer : pointer_to_struct;
    asd ? void_pointer : pointer_to_union;
    asd ? void_pointer : pointer_to_enum;
    asd ? void_pointer : pointer_to_int;
    asd ? void_pointer : pointer_to_uint;
    asd ? void_pointer : pointer_to_pointer;
    asd ? void_pointer : pointer_to_array;
    asd ? void_pointer : pointer_to_array_of_unknown_size;
    asd ? void_pointer : pointer_to_unresolved;
    
    asd ? pointer_to_struct : void_pointer;
    asd ? pointer_to_union : void_pointer;
    asd ? pointer_to_enum : void_pointer;
    asd ? pointer_to_int : void_pointer;
    asd ? pointer_to_uint : void_pointer;
    asd ? pointer_to_pointer : void_pointer;
    asd ? pointer_to_array : void_pointer;
    asd ? pointer_to_array_of_unknown_size : void_pointer;
    asd ? pointer_to_unresolved : void_pointer;
    
    asd ? 0 : pointer_to_struct;
    asd ? 0 : pointer_to_union;
    asd ? 0 : pointer_to_enum;
    asd ? 0 : pointer_to_int;
    asd ? 0 : pointer_to_uint;
    asd ? 0 : pointer_to_pointer;
    asd ? 0 : pointer_to_array;
    asd ? 0 : pointer_to_array_of_unknown_size;
    asd ? 0 : pointer_to_unresolved;
    
    asd ? pointer_to_struct : 0;
    asd ? pointer_to_union : 0;
    asd ? pointer_to_enum : 0;
    asd ? pointer_to_int : 0;
    asd ? pointer_to_uint : 0;
    asd ? pointer_to_pointer : 0;
    asd ? pointer_to_array : 0;
    asd ? pointer_to_array_of_unknown_size : 0;
    asd ? pointer_to_unresolved : 0;
    
    asd ? void_pointer : 0;
    asd ? 0 : void_pointer;
    
    asd ? void_pointer : void_pointer;
    asd ? 0 : 0;
    
    // 
    // Simple assignment:
    // 
    //  "the left operand has [...] pointer type, and one operand is a pointer to an object type, 
    //   and the other is a pointer to [...] void [...]."  << This is worded very confusingly because of qualifiers.
    //   
    //  "the left operand is a [...] pointer, and the right is a null pointer constant."
    // 
    // 
    
    void_pointer = pointer_to_struct;
    void_pointer = pointer_to_union;
    void_pointer = pointer_to_enum;
    void_pointer = pointer_to_int;
    void_pointer = pointer_to_uint;
    void_pointer = pointer_to_pointer;
    void_pointer = pointer_to_array;
    void_pointer = pointer_to_array_of_unknown_size;
    void_pointer = pointer_to_struct;
    void_pointer = pointer_to_unresolved;
    
    pointer_to_struct = void_pointer;
    pointer_to_union = void_pointer;
    pointer_to_enum = void_pointer;
    pointer_to_int = void_pointer;
    pointer_to_uint = void_pointer;
    pointer_to_pointer = void_pointer;
    pointer_to_array = void_pointer;
    pointer_to_array_of_unknown_size = void_pointer;
    pointer_to_struct = void_pointer;
    pointer_to_unresolved = void_pointer;
    
    pointer_to_struct = 0;
    pointer_to_union = 0;
    pointer_to_enum = 0;
    pointer_to_int = 0;
    pointer_to_uint = 0;
    pointer_to_pointer = 0;
    pointer_to_array = 0;
    pointer_to_array_of_unknown_size = 0;
    pointer_to_struct = 0;
    pointer_to_unresolved = 0;
    void_pointer = 0;
    
    struct{
        struct s *pointer_to_struct;
        union  u *pointer_to_union;
        enum   e *pointer_to_enum;
        int *pointer_to_int;
        unsigned int *pointer_to_uint;
        int **pointer_to_pointer;
        int (*pointer_to_array)[10];
        int (*pointer_to_array_of_unknown_size)[];
        struct unresolved *pointer_to_unresolved;
        void *void_pointer;
    } pointers_in_structure = {
        .pointer_to_struct = void_pointer,
        .pointer_to_union = void_pointer,
        .pointer_to_enum = void_pointer,
        .pointer_to_int = void_pointer,
        .pointer_to_uint = void_pointer,
        .pointer_to_pointer = void_pointer,
        .pointer_to_array = void_pointer,
        .pointer_to_array_of_unknown_size = void_pointer,
        .pointer_to_struct = void_pointer,
        .pointer_to_unresolved = void_pointer,
        .void_pointer = void_pointer,
    }, pointers_in_structure_0 = {
        .pointer_to_struct = 0,
        .pointer_to_union = 0,
        .pointer_to_enum = 0,
        .pointer_to_int = 0,
        .pointer_to_uint = 0,
        .pointer_to_pointer = 0,
        .pointer_to_array = 0,
        .pointer_to_array_of_unknown_size = 0,
        .pointer_to_struct = 0,
        .pointer_to_unresolved = 0,
        .void_pointer = 0,
    };
    
    (void)pointers_in_structure;
    (void)pointers_in_structure_0;
    
}


