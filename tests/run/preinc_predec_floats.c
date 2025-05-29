// run

int _start(){
    
    {
        float f = 0, c = 0;
        c = f++;
        if(c != 0 || f != 1.0f) return 1;
        c = ++f;
        if(c != 2.0f || f != 2.0f) return 1;
        c = f--;
        if(c != 2.0f || f != 1.0f) return 1;
        c = --f;
        if(c != 0 || f != 0) return 1;
    }
    
    {
        double f = 0, c = 0;
        c = f++;
        if(c != 0 || f != 1.0f) return 1;
        c = ++f;
        if(c != 2.0f || f != 2.0f) return 1;
        c = f--;
        if(c != 2.0f || f != 1.0f) return 1;
        c = --f;
        if(c != 0 || f != 0) return 1;
    }
    
    return 0;
}
