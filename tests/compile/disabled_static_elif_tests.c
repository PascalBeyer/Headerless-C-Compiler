
#if 1
#elif 1
#error should not fire.
#elif 1
#error should not fire.
#elif 1
#error should not fire.
#else
#error should not fire.
#endif

#if 0
  #if 0
  #elif 1
  #error should not fire.
  #endif
#endif

#if 0
  #if 0
  #else
  #error should not fire.
  #endif
#endif

#if 0
   #if 0
   #elif 0
   #elif 1
   #error should not fire.
   #else
   #error should not fire.
   #endif
#endif

#if 0
  #if 1
    #if 0
    #elif 1
    #error should not fire
    #endif
  #endif
#endif

int main(){}
