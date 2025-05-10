// run

int _start(){
    unsigned int u32 = 0;
    
    //   u32
    //   1ull
    //   cast-lhs
    //   <
    //   cond-jmp l1
    //   temp         <- This is u64 as 1ull
    //   u32          <- This is u32 
    //   assign       <- This is an assignment of u64 and u32.
    //   jump l2
    // l1:
    //   1ull
    //   assign
    // l2:
    // 
    
    return u32 < 1ull ? u32 : 1ull;
}
