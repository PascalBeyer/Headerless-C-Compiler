// run

#define assert(a) if(!(a)) return -1

int main(){
    
    union has_flexible_array_member{
        int flexible_array[];
    } array_of_flexible_array_members[10] = {
        1, 2, 3
    };
    
    assert(sizeof(array_of_flexible_array_members) == 0);
    assert(sizeof(array_of_flexible_array_members[0]) == 0);
    assert(array_of_flexible_array_members[0].flexible_array[0] == 1);
    assert(array_of_flexible_array_members[0].flexible_array[1] == 2);
    assert(array_of_flexible_array_members[0].flexible_array[2] == 3);
}
