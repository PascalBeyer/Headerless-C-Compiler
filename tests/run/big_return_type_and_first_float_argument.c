// run

struct v2{
    float x;
    float y;
    float z;
    float w;
};

struct v2 inclusion(float a){
    struct v2 ret = { a, a };
    return ret;
}

int main(){
    inclusion(1.0f);
    return 0;
}
