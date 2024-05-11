
int a_b;

#define my_define(id) a_##id

int main(){
    return my_define(
            b);
}
