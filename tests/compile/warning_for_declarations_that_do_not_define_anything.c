// check "(6,1): Warning 13 at '<unnamed-tag>': Anonymous struct declaration does not declare anything."
// check "(7,1): Warning 13 at '<unnamed-tag>': Anonymous union declaration does not declare anything."
// 
// check "(9,4): Warning 13 at ';': Declaration does not define anything."

struct{};
union{};
enum{}; // We don't check for this... I guess that is fine.
int;

int main(){}
