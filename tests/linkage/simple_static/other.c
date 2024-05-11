// skip

// if this is not explicitly 'extern' then there will be storage allocated.
int a;

static int _static = 6969;


static int asd(){
    return 0xd;
}

int return_my_static(){
    return _static;
}

int return_my_asd(){
    return asd();
}

void set_a(){
    a = 6969;
}
