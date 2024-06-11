// reject "Junk after '#elif'."

#if 0
    #if 1
    #elif defined(__HLC__)
    #endif
#endif

int main(){
   
}

