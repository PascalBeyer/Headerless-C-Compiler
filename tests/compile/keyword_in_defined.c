
#if !defined(int)
//           ^^^
// This used to error, as 'int' was substituted to TOKEN_int, 
// and then 'defined' expected an identifier.
int main(){}
#endif



