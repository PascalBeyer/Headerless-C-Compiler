// compile

int dead_func(void);
extern int dead_var;

int _start(void)
{
    if (1) {
        return 0;
    }
    
    (void)dead_func();
    
    if(dead_var){}
    
    switch(dead_var){}
    
    int var = dead_var;
    
    return 1;
}
