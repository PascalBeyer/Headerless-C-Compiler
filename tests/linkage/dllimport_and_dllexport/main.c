// broken
// compile -DTEST -L other.lib
// run

#define TEST1 0x5c823042
#define TEST2 0x658195d9
#define TEST3 0x3202fd59
#define TEST4 0x51761348
#define TEST5 0x2832f030
#define TEST6 0x1696ef10

#define assert(a) if(!(a)) return -1

#ifdef TEST

__declspec(dllexport) int variable_the_test_should_export = TEST1;
__declspec(dllexport) int function_the_test_should_export(){
    return TEST2;
};

__declspec(dllimport) int variable_the_test_should_import_globally;
__declspec(dllimport) int function_the_test_should_import_globally();

int main(){
    __declspec(dllimport) int variable_the_test_should_import_locally;
    __declspec(dllimport) int function_the_test_should_import_locally();
    
    assert(variable_the_test_should_import_globally   == TEST3);
    assert(function_the_test_should_import_globally() == TEST4);
    assert(variable_the_test_should_import_locally    == TEST5);
    assert(function_the_test_should_import_locally()  == TEST6);
}

#elif 0

__declspec(dllexport) int variable_the_test_should_export = TEST1;
__declspec(dllexport) int function_the_test_should_export(){
    return TEST2;
};

#else

// 
// This is the part, we don't compile when running the tests.
// 

__declspec(dllimport) int variable_the_test_should_export;
__declspec(dllimport) int function_the_test_should_export();


__declspec(dllexport) int variable_the_test_should_import_globally = TEST3;
__declspec(dllexport) int function_the_test_should_import_globally(){
    if(variable_the_test_should_export   != TEST1) return 0;
    if(function_the_test_should_export() != TEST2) return 0;
    return TEST4;
};

__declspec(dllexport) int variable_the_test_should_import_locally = TEST5;
__declspec(dllexport) int function_the_test_should_import_locally(){
    return TEST6;
};


#endif
