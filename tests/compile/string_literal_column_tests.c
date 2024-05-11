// check "(17,28): Warning"
// check "(18,30): Warning"
// check "(19,29): Warning"
// check "(20,29): Warning"
// check "(21,29): Warning"
// check "(22,30): Warning"
// check "(26,15): Warning"
// check "(27,17): Warning"
// check "(28,16): Warning"
// check "(29,16): Warning"
// check "(30,16): Warning"

int main(){
    
    int asd;
    
    {"string_literal"; int asd;}
    {"string_literal\n"; int asd;}
    {L"string_literal"; int asd;}
    {U"string_literal"; int asd;}
    {u"string_literal"; int asd;}
    {u8"string_literal"; int asd;}
    // {u16"string_literal"; int asd;}
    // {u32"string_literal"; int asd;}
    
    {'c'; int asd;}
    {'c\n'; int asd;}
    {L'c'; int asd;}
    {U'c'; int asd;}
    {u'c'; int asd;}
    // {u8'c'; int asd;}
    // {u16'c'; int asd;}
    // {u32'c'; int asd;}
}
