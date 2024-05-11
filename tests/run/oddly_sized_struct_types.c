// compile -stdlib
// run

void _exit(int status);
#define assert(a) (!(a) ? _exit(1) : (void)0)

struct seven{
    char a[7];
};

struct six{
    char a[6];
};

struct five{
    char a[5];
};

struct three{
    char a[3];
};

struct three get_three(){
    struct three three = {};
    three.a[0] = 1;
    three.a[1] = 2;
    three.a[2] = 3;
    return three;
}

void take_three(struct three three){
    assert(three.a[0] == 1);
    assert(three.a[1] == 2);
    assert(three.a[2] == 3);
}


struct five get_five(){
    struct five five = {};
    five.a[0] = 1;
    five.a[1] = 2;
    five.a[2] = 3;
    five.a[3] = 4;
    five.a[4] = 5;
    return five;
}

void take_five(struct five five){
    assert(five.a[0] == 1);
    assert(five.a[1] == 2);
    assert(five.a[2] == 3);
    assert(five.a[3] == 4);
    assert(five.a[4] == 5);
}


struct six get_six(){
    struct six six = {};
    six.a[0] = 1;
    six.a[1] = 2;
    six.a[2] = 3;
    six.a[3] = 4;
    six.a[4] = 5;
    six.a[5] = 6;
    return six;
}

void take_six(struct six six){
    assert(six.a[0] == 1);
    assert(six.a[1] == 2);
    assert(six.a[2] == 3);
    assert(six.a[3] == 4);
    assert(six.a[4] == 5);
    assert(six.a[5] == 6);
}

struct seven get_seven(){
    struct seven seven = {};
    seven.a[0] = 1;
    seven.a[1] = 2;
    seven.a[2] = 3;
    seven.a[3] = 4;
    seven.a[4] = 5;
    seven.a[5] = 6;
    seven.a[6] = 7;
    return seven;
}

void take_seven(struct seven seven){
    assert(seven.a[0] == 1);
    assert(seven.a[1] == 2);
    assert(seven.a[2] == 3);
    assert(seven.a[3] == 4);
    assert(seven.a[4] == 5);
    assert(seven.a[5] == 6);
    assert(seven.a[6] == 7);
}

int main(){
    struct three three = get_three();
    take_three(three);
    struct five five = get_five();
    take_five(five);
    struct six six = get_six();
    take_six(six);
    struct seven seven = get_seven();
    take_seven(seven);
    return 0;
}
