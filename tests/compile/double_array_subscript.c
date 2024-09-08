
int main(){
    {
        static char double_array[2][2] = {
            {1, 2},
            {3, 4},
        };
        
        int i = 0;
        int j = 0;
        
        if(double_array[i][j] != 1) return 1;
        
        i += 1;
        if(double_array[i][j] != 3) return 1;
        
        j += 1;
        if(double_array[i][j] != 4) return 1;
        
        i -= 1;
        if(double_array[i][j] != 2) return 1;
    }
    
    {
        static short double_array[2][2] = {
            {1, 2},
            {3, 4},
        };
        
        int i = 0;
        int j = 0;
        
        if(double_array[i][j] != 1) return 1;
        
        i += 1;
        if(double_array[i][j] != 3) return 1;
        
        j += 1;
        if(double_array[i][j] != 4) return 1;
        
        i -= 1;
        if(double_array[i][j] != 2) return 1;
    }
    
    {
        static int double_array[2][2] = {
            {1, 2},
            {3, 4},
        };
        
        int i = 0;
        int j = 0;
        
        if(double_array[i][j] != 1) return 1;
        
        i += 1;
        if(double_array[i][j] != 3) return 1;
        
        j += 1;
        if(double_array[i][j] != 4) return 1;
        
        i -= 1;
        if(double_array[i][j] != 2) return 1;
    }
    
    {
        static __int64 double_array[2][2] = {
            {1, 2},
            {3, 4},
        };
        
        int i = 0;
        int j = 0;
        
        if(double_array[i][j] != 1) return 1;
        
        i += 1;
        if(double_array[i][j] != 3) return 1;
        
        j += 1;
        if(double_array[i][j] != 4) return 1;
        
        i -= 1;
        if(double_array[i][j] != 2) return 1;
    }
    
    
    return 0;
}

