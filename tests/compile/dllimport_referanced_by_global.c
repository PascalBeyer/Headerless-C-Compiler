// compile -L ucrt.lib

__declspec(dllimport) float floorf(float);

int main(){
    
    float asd = (&floorf)(1.43f);
    
    return (int)asd; 
}
