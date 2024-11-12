
#if 0 ? 1 : 0
#error 
#endif

#if 1 ? 1 : 0
#else
#error 
#endif

#if !(1 ? 1 : 0)
#error 
#endif


#if 1 ? 1 : 0 ? 1 : 0

#else 
#error
#endif

#if 0 ? 1 : 0 ? 1 : 0
#error
#endif

#if 0 ? 1 : 1 ? 1 : 0
#else
#error
#endif

int main(){
}



