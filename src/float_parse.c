
__declspec(dllimport) double strtod(const char *str, char **endptr);

struct large_int_node{
    struct large_int_node *next;
    struct large_int_node *prev;
    u32 val;
};

struct large_int{
    struct large_int_node *first;
    struct large_int_node *last;
    smm count;
};

func b32 large_int_maybe_push_back_val(struct memory_arena *arena, struct large_int *large_int, u32 val){
    if(val){
        struct large_int_node *val_node = push_struct(arena, struct large_int_node);
        val_node->val = val;
        dll_push_back(*large_int, val_node);
        large_int->count += 1;
        return true;
    }
    return false;
}

func void print_large_int(struct large_int large_int){
    if(!large_int.count) return;
    smm count = 0;
    for(struct large_int_node *node = large_int.last; node != large_int.first->prev; node = node->prev){
        print("%.8x%s", node->val, (node != large_int.first) ? "|" : "");
        count += 1;
        if(node == large_int.first) break;
    }
    assert(count == large_int.count);
}

func struct large_int make_large_int(struct memory_arena *arena, u32 val){    
    struct large_int ret = zero_struct;
    large_int_maybe_push_back_val(arena, &ret, val);
    return ret;
}

func void large_int_add__internal(struct memory_arena *arena, struct large_int *large_int, struct large_int_node *node, u32 to_add){
    u32 carry = to_add;
    for(struct large_int_node *it = node; node; node = node->next){
        u64 val = (u64)it->val + (u64)carry;
        carry   = (u32)(val >> 32);
        it->val = (u32)val;
    }
    
    large_int_maybe_push_back_val(arena, large_int, carry);
}

func void large_int_add(struct memory_arena *arena, struct large_int *large_int, u32 to_add){
    large_int_add__internal(arena, large_int, large_int->first, to_add);
}

func void large_int_mul(struct memory_arena *arena, struct large_int *large_int, u32 mul_by){
    // iterate in revers and multiply by mul_by, then add the carry
    for(struct large_int_node *it = large_int->last; it; it = it->prev){
        u64 result = (u64)it->val * (u64)mul_by;
        u32 high   = (u32)(result >> 32);
        it->val    = (u32) result;
        
        if(high){
            large_int_add__internal(arena, large_int, it->next, high);
        }
    }
   
}

func b32 large_int_lshift_no_grow(struct large_int *to_shift, u32 shift_by){
    assert(shift_by < 32);
    u32 carry = 0;
    for(struct large_int_node *node = to_shift->first; node; node = node->next){
        u32 next_carry = (node->val >> (32 - shift_by));
        node->val = (node->val << shift_by) | carry;
        carry = next_carry;
    }
    return carry;
}

func b32 large_int_lshift(struct memory_arena *arena, struct large_int *to_shift, smm shift_by){
    assert(shift_by > 0);
    
    while(shift_by >= 32){
        struct large_int_node *node = push_struct(arena, struct large_int_node);
        dll_push_front(*to_shift, node);
        to_shift->count += 1;
        
        shift_by -= 32;
    }
    
    if(shift_by){
        u32 carry = large_int_lshift_no_grow(to_shift, (u32)shift_by);
        return large_int_maybe_push_back_val(arena, to_shift, carry);    
    }
    return 0;
}


// checks '>=' for the first 'count' values (u32's) of 'lhs' and 'rhs'
func b32 large_int_bigger_equals(struct large_int lhs, struct large_int rhs){
    if(lhs.count > rhs.count) return true;
    if(lhs.count < rhs.count) return false;
    
    struct large_int_node *lhs_node = lhs.last;
    struct large_int_node *rhs_node = rhs.last;
    for(u32 i = 0; i < lhs.count; i++){
        assert(lhs_node && rhs_node);
        if(lhs_node->val > rhs_node->val) return true;
        if(lhs_node->val < rhs_node->val) return false;
        
        lhs_node = lhs_node->prev;
        rhs_node = rhs_node->prev;
    }
    
    return true;
}

func void large_int_sub(struct large_int *lhs, struct large_int rhs){
    assert(lhs->count >= rhs.count);
    
    b32 borrow = 0;
    struct large_int_node *lhs_node = lhs->first;
    for(struct large_int_node *rhs_node = rhs.first; rhs_node; 
        rhs_node = rhs_node->next, 
        lhs_node = lhs_node->next){
        
        u64 val = (u64)lhs_node->val - (u64)rhs_node->val - (u64)borrow;
        borrow = (u32)(val >> 32);
        lhs_node->val = (u32)val;
    }
    
    for(; lhs_node && borrow; lhs_node = lhs_node->next){
        u64 val = (u64)lhs_node->val - (u64)borrow;
        borrow = (u32)(val >> 32);
        lhs_node->val = (u32)val;
    }
    //assert(!borrow); we can have borrow if 'have_carry_from_lshift'
    
    // @note: we need to trim such that the early outs in large_int_bigger_equals work
    
    struct large_int_node *to_trim = lhs->last;
    smm trimmed = 0;
    while(to_trim != lhs->first->prev){
        if(to_trim->val != 0) break;
        to_trim = to_trim->prev;
        trimmed += 1;
    }
    
    assert(trimmed <= lhs->count);
    lhs->count -= trimmed;
    lhs->last = to_trim;
    if(to_trim){
        to_trim->next = null; // @note: leak the rest of the list, @cleanup: maybe we should have a free list?
    }
}

func smm large_int_top_bit_log2(struct large_int large_int){
    u32 high_bit_position = 32 - count_leading_zeros32(large_int.last->val);
    assert(high_bit_position);
    u32 last_exponent = high_bit_position - 1;
    smm exponent = 32 * (large_int.count - 1) + last_exponent;
    return exponent;
}

func double parse_float(struct memory_arena *arena, struct string s){
    struct tempoary_memory temp = begin_tempoary_memory(arena);
    
    smm denominator_pow_two_exp_because_we_only_multiply_by_five = 0;
    
    struct large_int numerator   = make_large_int(arena, 0);
    struct large_int denominator = make_large_int(arena, 1);
    {
        smm i = 0;
        for(; i < s.amount; i++){
            if('0' <= s.data[i] && s.data[i] <= '9'){
                large_int_mul(arena, &numerator, 10);
                large_int_add(arena, &numerator, s.data[i] - '0');
            }else{
                break;
            }
        }
        
        if(s.data[i] == '.'){
            i++;
            for(; i < s.amount; i++){
                if('0' <= s.data[i] && s.data[i] <= '9'){
                    large_int_mul(arena, &numerator, 10);
                    large_int_add(arena, &numerator, s.data[i] - '0');
                    large_int_mul(arena, &denominator, 5);
                    denominator_pow_two_exp_because_we_only_multiply_by_five += 1;
                }else{
                    break;
                }
            }
        }
    }
    
    
    print("   after parse: ");
    print("n = ");
    print_large_int(numerator);
    print(", d = ");
    print_large_int(denominator);
    print("\n");
    
    
    smm exponent_adjustment_because_of_initial_pow_to_scaling;
    {  // multiply by a power of two, such that the result is in [2^52, 2^53)
        // @cleanup: can it be a problem here, that 10 is divisible by 2?
        smm numerator_exp   = large_int_top_bit_log2(numerator);
        smm denominator_exp = large_int_top_bit_log2(denominator);
        
        smm approximate_exp_after_divide = numerator_exp - denominator_exp;
        smm wanted_bits = 53;
        
        if(approximate_exp_after_divide < wanted_bits){
            large_int_lshift(arena, &numerator, wanted_bits - approximate_exp_after_divide);
            exponent_adjustment_because_of_initial_pow_to_scaling = wanted_bits - approximate_exp_after_divide;
        }else if(approximate_exp_after_divide > wanted_bits){
            large_int_lshift(arena, &numerator, -(wanted_bits - approximate_exp_after_divide));
            exponent_adjustment_because_of_initial_pow_to_scaling = -(wanted_bits - approximate_exp_after_divide);
        }else{
            exponent_adjustment_because_of_initial_pow_to_scaling = 0;
        }
    }
    
    print("scaling bias: %lld\n", exponent_adjustment_because_of_initial_pow_to_scaling);
    print("*5 bias:      %lld\n", denominator_pow_two_exp_because_we_only_multiply_by_five);
    
    
    {   // shift both numbers such that the leading bit of 'denominator' is set
        u32 shift = count_leading_zeros32(denominator.last->val);
        large_int_lshift(arena, &denominator, shift);
        large_int_lshift(arena, &numerator,   shift);
    }
    
    print("   after shift: ");
    print("n = ");
    print_large_int(numerator);
    print(", d = ");
    print_large_int(denominator);
    print("\n");
    
    // ??? scale the numerator or denominator, such that the quotient is in the range [2^52, 2^53)?
    
    // I think here we have to binary search to find the right scaling factor... and maybe there are some early
    // outs.
    
    // division:
    struct large_int quotient = zero_struct;
    smm delta_bits = (numerator.count - denominator.count) * 32;
    
    struct large_int truncated_numerator = numerator;
    {   // build a truncated numerator that we can then use to subtract exactly 'denominator.count' words from 
        // the top of 'numerator', note that as we shift without growing, this is stable.
        
        // @cleanup: what should we do if ('denominator.count' > 'numerator.count'), we can somehow disregard
        //           this case, as then the quotient obviously would not be in the range [2^52, 2^53).
        assert(numerator.count >= denominator.count);
        
        print("numerator count   = %d\n", numerator.count);
        print("denominator count = %d\n", denominator.count);
        
        struct large_int_node *it = truncated_numerator.last;
        for(u32 i = 0; i < denominator.count; i++) it = it->prev;
        truncated_numerator.first = it ? it->next : numerator.first;
        truncated_numerator.count = denominator.count;
        print("truncated numerator: ");
        print_large_int(truncated_numerator);
        print("\n");
    }

    b32 have_carry_from_lshift = 0;
    // less then or equal, because we want to do at least one subtract, even if they are equally as long
    for(smm i = 0; i <= delta_bits; i++){
        
        print("   %d\n", i);
        print("      n = ");
        print_large_int(numerator);
        print("\n");
        print("      d = ");
        print_large_int(denominator);
        print("\n");
        print("      q = ");
        print_large_int(quotient);
        print("\n");
        
        // we still have bits to go, so shift one in
        large_int_lshift(arena, &quotient, 1);
        
        // if we _just_ shifted a '1' into the top part of numerator we are always bigger
        if(have_carry_from_lshift || large_int_bigger_equals(truncated_numerator, denominator)){
            print("         subtracting: ");
            print_large_int(truncated_numerator);
            print(" - ");
            print_large_int(denominator);
            
            large_int_sub(&truncated_numerator, denominator);
            
            print(" = ");
            print_large_int(truncated_numerator);
            print("\n");
            
            large_int_add(arena,    &quotient, 1);
        }
        
        have_carry_from_lshift = large_int_lshift_no_grow(&numerator, 1);
    }
    
    struct large_int remainder = truncated_numerator;
    if(!large_int_bigger_equals(denominator, remainder)){
        print("oof our denominator is smaller then our remainder:\n");
        print("   denominator: ");
        print_large_int(denominator);
        print("\n");
        print("   remainder:   ");
        print_large_int(remainder);
        print("\n");
    }
    
    print("   after divide:");
    print("q = ");
    print_large_int(quotient);
    print(", r = ");
    print_large_int(remainder);
    print("\n");
    
    assert(quotient.count);
    
    print("quotiont.count %d quotient.last->val %u\n", quotient.count, quotient.last->val);
    
    smm exponent = large_int_top_bit_log2(quotient);
    exponent -= exponent_adjustment_because_of_initial_pow_to_scaling;
    exponent -= denominator_pow_two_exp_because_we_only_multiply_by_five;
    
    print("exponent = %lld\n", exponent);
    
    int at = 52;
    u32 last_exponent = 31 - count_leading_zeros32(quotient.last->val);
    u64 mantissa = (u64)quotient.last->val << (at - last_exponent);
    at -= last_exponent;
    
    for(struct large_int_node *it = quotient.last->prev; it && (at > 0); it = it->prev){
        int amount_of_bits_to_add = min_of(32, at);
        u64 val = ((u64)it->val >> (32 - amount_of_bits_to_add));
        
        mantissa |= ((u64)val << (at - amount_of_bits_to_add));
        at -= amount_of_bits_to_add;
    }
    
    print("mantissa %.16llx\n", mantissa);
    
    if(at != 0){
        print("missing bits %d ?\n", at);
    }
    
    union{
        u64 as_int;
        double as_double;
    } ret;
    
    // IEEE_754-double: 
    // 1 -sign bit
    // 11-bits of exponent
    // 52-bits of mantissa + one hidden bit
    //   = 64-bits
    
    // add the bias value to the exponent
    exponent = (exponent + 1023) & 2047;
    // remove the implicit bit
    assert(mantissa & (1ull << 52));
    mantissa = (mantissa & ((1ull << 52) - 1));
    
    ret.as_int = mantissa | (exponent << 52) | (0ull << 63);
    
    end_tempoary_memory(temp);
    return ret.as_double;
}

#if 0
int main(int argc, char **argv){
    if(argc == 1){
        print("usage %s <float-list>", argv[0]);
        return 0;
    }
    
    struct memory_arena *arena = &(struct memory_arena)zero_struct;
    
    for(int i = 1; i < argc; i++){
        struct string string = string_from_cstring(argv[i]);
        char *begin = (char *)string.data;
        char *end = (char *)(string.data + string.size);
        
        double parsed = parse_float(arena, string);
        print("original %s -> parsed %f (libc %f)\n", argv[i], parsed, strtod(begin, &end));
    }
}
#endif