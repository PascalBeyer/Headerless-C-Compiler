


/*

declaration: 
   declaration-specifiers init_declarator_list_opt
   
declaration-specifiers:
   type, storage-class, function-specifier, type specifier, type qualifier
   
inti_declarator_list:
   init_declarator
   init_0declarator, init_declarator_list
   
init_declarator:
   declarator
   declarator = initializer
*/

// struct declaration
struct s;
// struct definition
struct s { int a; };

// variable declaration
int a;
// variable definition
int a = 1;

// variable of struct type
struct s a2;
// struct and variable definition
struct s2 { int a; } a3 = { 0 };

// extern variable and function declaration
extern int e;
extern int e2();

// extern variable definitions
extern int e3 = 1337;
extern int e4(){};

// static variable and function declaration
static int si;
static int si2();

// static variable definitions
extern int si3 = 1337;
extern int si4(){};

// functions with calling convention
int __cdecl   si5();
int __stdcall si6();

// pointers with size specifier
int *__ptr32 ptr32;
int *__ptr64 ptr64;

// functions with declspec
__declspec(dllimport) int import();
int  __declspec(dllexport) _export() {};

// aligned type
struct __declspec(align(16)) {int a;} aligned_int;


// static function declaration that gets filled in as non-static
int static static_function(void);



// simple type specifier simple identifier, function, declaration
int main();
// simple type specifier simple identifier, function, defintion
int main(){
    static_function();
}

int static_function(void){}

int (a);
int (b)(int a);
int ((b)(int a));
