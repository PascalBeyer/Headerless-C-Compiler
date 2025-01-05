

#if __has_include("this_is_not_a_valid_include_file.h")
#    error We should not have the include file "this_is_not_a_valid_include_file.h".
#endif 

#if !__has_include(__FILE__)
#    error We should have the current file as an include file.
#endif 


#if __has_include(<this_is_not_a_valid_include_file.h>)
#    error We should not have the include file <this_is_not_a_valid_include_file.h>
#endif 

#if !__has_include(<math.h>)
#    error We should have the include file <math.h>
#endif 

int main(){}

