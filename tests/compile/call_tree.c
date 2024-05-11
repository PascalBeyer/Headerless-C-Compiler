

// import does not exitst, but gets stripped, because it is only referanced in static function,
// which is never referanced.
__declspec(dllimport) void undefined_dllimport();

static int unreferanced_function(){
    undefined_dllimport();
    return 1;
}


// one layer deeper
__declspec(dllimport) void second_undefined_dllimport();
static void statically_referanced_funcion(){
    second_undefined_dllimport();
}

static void second_unreferanced_function(){
    statically_referanced_funcion();
}

int main(){
    return 0;
}
