// fail "must be a lhs expression"

#define atomic_load(type, value) (*&(type){} = value, (type)*((volatile type *)(&value)))

__int64 returns_in_a_register(int lhs_expr){
    return 0;
};

int main(){
    int lhs_expr = 0;
    
    atomic_load(__int64, returns_in_a_register(lhs_expr));
}
