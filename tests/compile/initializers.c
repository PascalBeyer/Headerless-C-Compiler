
int int_initializer = 1;
int int_initializer_with_braces = {1};

// simple struct initializer
struct { int a; } one_member = {1};
struct { int a; } one_member_with_braces = {{1}};

// aggregate struct initializer
struct { struct {int a;} member; } one_aggregate_member = {1};
struct { struct {int a;} member; } one_aggregate_member_with_braces = {{1}};
struct { struct {int a;} member; } one_aggregate_member_with_braces_with_braces = {{{1}}};
// too many initializers
//struct {struct {int a;} member; } one_aggregate_member_with_braces_with_braces_with_braces = {{{{1}}}}; 

struct {int a; int b;} two_members = {1, 2};
struct {int a; int b;} two_members_with_braces = { {1} , {2} };

struct { struct {int a; int b; } member; } aggregate_two_members = {1, 2};
struct { struct {int a; int b; } member; } aggregate_two_members_with_braces = {{1, 2}};

// @cleanup: simple union initializer 

// simple array initilizers
int int_one_array[1] = {1};
int int_one_array_with_braces[1] = {{1}};

int int_two_array[2] = {1, 2};
int int_two_array_with_braces[2] = {{1}, {2}};
int int_two_array_one_initializer[2] = {1};
int int_two_array_one_initializer_with_braces[2] = {{1}};

// aggregate array initilizers
struct {int a;} aggregate_one_array[1] = {1};
struct {int a;} aggregate_one_array_with_braces[1] = {{1}};
struct {int a;} aggregate_one_array_with_braces_with_braces[1] = {{{1}}};

struct {int a; int b;} aggregate_two_member_array_flat[2]     = { 1, 2,   3, 4};
struct {int a; int b;} aggregate_two_member_array_agg_flat[2] = {{1, 2},  3, 4};
struct {int a; int b;} aggregate_two_member_array_agg_agg[2]  = {{1, 2}, {3, 4}};
struct {int a; int b;} aggregate_two_member_array_flat_agg[2] = { 1, 2,  {3, 4}};

int main(){
    return 0;
}
