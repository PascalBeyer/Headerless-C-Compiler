// check "(6,1): Warning 13 at 'struct': Anonymous struct declaration does not declare anything."
// check "(7,1): Warning 13 at 'union': Anonymous union declaration does not declare anything."
// 
// check "(9,4): Warning 13 at ';': Declaration does not define anything."

struct{};
union{};
enum{}; // We don't check for this... I guess that is fine.
int;

int main(){}
