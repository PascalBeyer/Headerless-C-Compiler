// check "Bounds for array of unknown size were never filled in."
// run 


// If a _tentative_ declaration is never filled in, it gets a zero-initializer.
// This means in the end this is equivalent to 
// 
//    int array_of_unknown_size[] = {0};
// 
// Hence, it should compile and run just fine.
int array_of_unknown_size[];

int main(){
    return array_of_unknown_size[0];
}
