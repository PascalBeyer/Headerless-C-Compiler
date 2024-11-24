// compile /c

__declspec(dllexport) extern struct lv_obj_class_t lv_obj_class;

int *arst = &lv_obj_class;

int main(){
    
    return *arst;
}
