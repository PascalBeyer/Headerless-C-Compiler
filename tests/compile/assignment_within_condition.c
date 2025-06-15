// check "(9,17): Warning 25 at '=': Assignment within condition, did you mean '=='?"
// check "(10,21): Warning 25 at '=': Assignment within condition, did you mean '=='?"
// check "(11,17): Warning 25 at '=': Assignment within condition, did you mean '=='?"
// check "(13,14): Warning 25 at '=': Assignment within condition, did you mean '=='?"

int main(){
    int index = 0;
    
    for(; index = 1; index++){}
    do{}while(index = 1);
    while(index = 1);
    
    if(index = 1);
}
