// reject "must return a value"


int has_infinite_for(){
    for(int a = 0; ; a++){ }
}

int has_infinite_do_while(){
    do{}while(1);
}

int has_infinite_while(){
    while(1){}
}

int main(){
    has_infinite_for() + has_infinite_do_while() + has_infinite_while();
}
