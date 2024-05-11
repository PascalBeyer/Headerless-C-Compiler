
typedef int array_of_unknown_size_t[];

array_of_unknown_size_t array_of_unknown_size3 = {
    0, 1, 2, 3
};

array_of_unknown_size_t array_of_unknown_size4 = {
    0, 1, 2, 3, 4, 5, 6, 7, 
};

array_of_unknown_size_t list1 = {1, 2, 3}, list2 = {1, 2, 3, 4};

int main(){
    array_of_unknown_size_t array_of_unknown_size1 = {
        0, 1, 2, 3
    };
    
    array_of_unknown_size_t array_of_unknown_size2 = {
        0, 1, 2, 3, 4, 5, 6, 7, 
    };
    
    static array_of_unknown_size_t array_of_unknown_size5 = {
        0, 1, 2, 3
    };
    
    static array_of_unknown_size_t array_of_unknown_size6 = {
        0, 1, 2, 3, 4, 5, 6, 7, 
    };
    
    array_of_unknown_size_t list3 = {1, 2, 3}, list4 = {1, 2, 3, 4};
    static array_of_unknown_size_t list5 = {1, 2, 3}, list6 = {1, 2, 3, 4};
}

