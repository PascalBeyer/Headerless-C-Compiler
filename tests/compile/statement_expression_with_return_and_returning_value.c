
struct s{ 
    int a; 
};

int _start(){
    
    int a = 1;
    
    struct s s = ({
                if(a) goto is_true;
                
                goto is_false;
                
                (struct s){a};
            });
    
  is_true:
    return 0;
    
  is_false:
    return 1;
}
