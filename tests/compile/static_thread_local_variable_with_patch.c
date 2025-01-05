// compile /obj

int arst2;

__declspec(thread) static int *arst = &arst2;

int main(){
    return *arst;
}
