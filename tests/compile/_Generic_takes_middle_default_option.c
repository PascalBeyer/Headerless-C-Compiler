
int main(void) { 
    double x = 0.0; 
    return _Generic(((x)) , 
            float : ((x) < 0.0f || (1.0f / (x) < 0.0f)) , 
            default : ((x) < 0.0 || (1.0 / (x) < 0.0)) , 
            int : ((x) < 0.0L || (1.0L / (x) < 0.0L))) 
        ? 1 : 0; 
}
