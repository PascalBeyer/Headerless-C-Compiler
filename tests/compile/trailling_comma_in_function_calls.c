
int function(int my_first_very_long_argument_name, int my_second_very_long_argument_name){
    return my_first_very_long_argument_name + my_second_very_long_argument_name;
}


int main(){
    int my_first_very_long_argument_name = 1;
    int my_second_very_long_argument_name = -1;
    
    int result = function(
            my_first_very_long_argument_name,
            my_second_very_long_argument_name,
        );
    
    
    return result;
}
