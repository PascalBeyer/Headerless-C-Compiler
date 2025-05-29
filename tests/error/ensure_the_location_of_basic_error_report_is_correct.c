// fail "(6,10)"

int func(char a);

int _start(){    
    func("arst"); // Ensure that the reported bug is in the correct spot.
    return 0;
}

