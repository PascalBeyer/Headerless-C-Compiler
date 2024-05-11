// run

#define assert(a) if(!(a)) return -1

struct contains_flexible_array_member{
    int member;
    int flexible_array_member[];
} s1 = { 1, { 2, 3 } };

struct contains_flexible_array_member{
    int member;
    int flexible_array_member[];
} s2 = { 1,  2, 3  };

struct contains_flexible_array_member2{
    int member;
    int flexible_array_member[];
} s3 = { .flexible_array_member = { 2, 3 } };


struct contains_flexible_array_member2{
    int member;
    int flexible_array_member[];
} s4 = { .flexible_array_member =  2, 3  };

int main(){
    assert(s1.flexible_array_member[0] == 2 && s1.flexible_array_member[1] == 3);
    assert(s2.flexible_array_member[0] == 2 && s2.flexible_array_member[1] == 3);
    assert(s3.flexible_array_member[0] == 2 && s3.flexible_array_member[1] == 3);
    assert(s4.flexible_array_member[0] == 2 && s3.flexible_array_member[1] == 3);
    
    // @Incomplete: unions nested structures, etc.
}

