// run

#define assert(a) if(!(a)) return 1

#define stringify(a) #a
#define stringify2(_, a) #a
#define stringify_va(...) #__VA_ARGS__

int main(){
    //
    // normal tests
    //
    
    assert(stringify(a) == "a");
    assert(stringify(a + b) == "a + b");
    assert(stringify(a
            +
            b) == "a + b");
    assert(stringify(a/*comment*/b) == "a b");
    assert(stringify(a    /* comment*/   
            /*comment*/
            b) == "a b");
    
    assert(stringify("hello") == "\"hello\"");
    
    assert(stringify("hello" "hello") == "\"hello\" \"hello\"");
    
    assert(stringify(      a      ) == "a");
    assert(stringify(      a + b      ) == "a + b");
    assert(stringify(      a+b      ) == "a+b");
    
    //
    // second argument
    //
    assert(stringify2(1, a) == "a");
    assert(stringify2(1, a + b) == "a + b");
    assert(stringify2(1, a
            +
            b) == "a + b");
    assert(stringify2(1, a/*comment*/b) == "a b");
    assert(stringify2(1, a    /* comment*/   
            /*comment*/
            b) == "a b");
    
    assert(stringify2(1, "hello") == "\"hello\"");
    
    assert(stringify2(1, "hello" "hello") == "\"hello\" \"hello\"");
    
    //
    // __VA_ARGS__
    //
    assert(stringify_va(a) == "a");
    assert(stringify_va(a + b) == "a + b");
    assert(stringify_va(a
            +
            b) == "a + b");
    assert(stringify_va(a/*comment*/b) == "a b");
    assert(stringify_va(a    /* comment*/   
            /*comment*/
            b) == "a b");
    
    assert(stringify_va("hello") == "\"hello\"");
    
    assert(stringify_va("hello" "hello") == "\"hello\" \"hello\"");
    

    return 0;
}



