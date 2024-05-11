// run

#define assert(a) if(!(a)) return 1;

struct empty {
};

int main()
{
    struct empty *pointer_to_empty = &(struct empty){};
    struct empty *initial = pointer_to_empty;
    pointer_to_empty++;
    assert(pointer_to_empty == initial);
    pointer_to_empty--;
    assert(pointer_to_empty == initial);
    ++pointer_to_empty;
    assert(pointer_to_empty == initial);
    --pointer_to_empty;
    assert(pointer_to_empty == initial);
    
    pointer_to_empty += 5;
    assert(pointer_to_empty == initial);
    pointer_to_empty -= 5;
    assert(pointer_to_empty == initial);

   return 0;
}
