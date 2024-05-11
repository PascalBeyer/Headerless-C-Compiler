
int a_b;

#define my_define(id) a_##id + #id[0]

int main(){
    return my_define(
            b);
}
