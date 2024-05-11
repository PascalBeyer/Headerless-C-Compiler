// compile -L kernel32.lib

enum plants{
   plant,
   whale = 3,
   thats_all_i_know,
   wubba_lubba = 3 + 5,
   stronkh,
   yo_mamma,
   end_of_all_plants = 3 * 3,
};

void *null = (char *)0;

void do_nothing(){
   //void (*asd)(void *asdasd, void ((*asd)[4])(int mymay)) = "hello";
}

#if 0
#error
#endif


struct type_decl{
   int a;
} and_var_decl;

#define im_a_define "im a define"

#define print_function_like(asd) print_string("function like")

#define print3(a, b, c) print_string(a); print_string(b); print_string(c)

struct {
   char array[13];
   char *string_in_struct;
} struct_around_array = { "I'm an array", "I am a char *"};

u32 u32_array[10] = {
   10, 111, 2, 3, 4, 5, 6, 7, 18, 9
};

#define assert(expr) ((expr) ? 0 : (print_string("wtf"), 0, 1))

static void test_16800()
{
   
	u8 first = 59;
   
	u8 second = 17;
   
	assert((first %= second) == 8);
   
}

s32 local_persist_test(){
   static int a;
   return a++;
}

union my_union{
    __int64 pointer;
    char *actually_a_pointer;
};

s32 main(){
    
    123;
   
   test_16800();
   
   float f = 1.0f;
   
   static int local_function(char *what_to_print){
      print_string(what_to_print);
   }
   
   local_function("local function here!");
   
   if(local_persist_test() == 0) print_string("local persist 1");
   if(local_persist_test() == 1) print_string("local persist 2");
   if(local_persist_test() == 2) print_string("local persist 3");
   
   //int a
   struct{
      u32 a;
      u32 b;
      u32 c;
   } anonymous_type = {1, 2, 3};
   
   if(anonymous_type.a == 1 && anonymous_type.b == 2 && anonymous_type.c == 3) print_string("anonymous_type");
   
   {
      typedef s32 local_type;
      local_type a = 1337;
      if(a == 1337){
         print_string("local type");
      }
   }
   
   u32 local_type = 123;
   
   struct type_decl type_decl;
   type_decl.a = 1;
   and_var_decl = type_decl;
   if(and_var_decl.a == 1){
      print_string("type and var decl");
   }
   
   print3("print1", "print2", "print3");
   
#define identity_macro(what_to_put) what_to_put
   
   u32 leet = identity_macro(1337);
   identity_macro(print_string("identitied1"); print_string("and 2"));
   
   print_string(im_a_define);
   
   print_function_like(asd);
   
   for(u32 i = 0; i < sizeof(u32_array)/sizeof(u32_array[0]); i++){
      print_number(u32_array[i]);
   }
   print_string("");
   
   if(u32_array[0] >= u32_array[3]){
      print_string("half life 3 confirmed!");
   }
   
   print_char(struct_around_array.array[11]);
   
   do_nothing();
   
   print_string(struct_around_array.array);
   print_string(struct_around_array.string_in_struct);
   
   char *hey_there = "hey_there";
   print_string(hey_there);
   
   u32 stdout = 0xFFFFFFF5;
   void **handle = GetStdHandle(stdout);
   
   u32 one = 1;
   u32 two = 2;
   
   if(0 || one){
      print_string("logical or");
   }
   
   u8 a_u8 = 1;
   if(0x1101 == (u16)a_u8){
      print_string("please dont be true");
   }
   
   
   switch(one){
      case 1:{
         print_string("you are right, it is indeed 1");
      }break;
      case 2:{
         
      }break;
      default:break;
   }
   
   
   if(-2 == (char)(0 - two)){
      print_string("negative.... correct!");
   }
   
   switch(one){
      case 2:{
         
      }break;
      case '3':{
         
      }break;
      default:{
         print_string("defaulted to number one!");
      }break;
   }
   
   switch(one){
      case 1:
      case 2:
      print_string("fall through");
   }
  
   int compound_a = 3, compound_b = 5;
   if(compound_a == 3 && compound_b == 5){
      print_string("COMPOUNDED!");
   }
   
   
   /*
      int my_awsome_int_array[3] = {1, 2, 3};
      {
         int sum = 0;
         for(u32 i = 0; i < 3; i++) sum += my_awsome_int_array[i];
         if(sum == 6) print_string("huray array");
      }
      */
   
   construct_v3((s32)one, (s32)two, 0);
   
   u32 i_get_incremented = 0;
   u32 incremented = ++i_get_incremented;
   if(incremented == i_get_incremented && incremented == 1){
      print_string("unary preincrement");
   }
   
   
   u32 i_get_incremented2 = 0;
   incremented = i_get_incremented2++;
   if(incremented == 0 && i_get_incremented2 == 1){
      print_string("unary postincrement");
   }
   
   if(two == one){
      // nothing
   }else{
      print_string("it's two my dude");
   }
   
   
   u32 three = one;
   three += two;
   if(three == 3){
      print_string("compound add");
   }
   
   u32 three_div_two = three / two;
   u32 three_div_two2 = three / 2;
   if(1 == three_div_two && three_div_two == three_div_two2){
      print_string("I have never felt so divided");
   }
   
   u32 nine = 19;
   u32 four = nine;
   four /= 2;
   four /= two;
   if(four == 4){
      print_string("divided and assigned");
   }
   
   if(one == two || one == one){
      print_string("logical or");
   }
   
   if(one == one && one == one){
      print_string("logical and");
   }
   
   if(one == one && 1){
      print_string("logical and 2");
   }
   
   if(one == two || 1){
      print_string("logical or 2");
   }
   
   print_string_no_newline("print some numbers: ");
   for(u32 i = 0; i < 10; i++){
      if(i == 4) continue;
      if(i == 8) break;
      print_number(i);
   }
   //print_string(""); @cleanup: why does this crash? probably we emit a null ptr for ""?
   print_string(" ");
   
   //f32 float1 = 0.0f;
   
   if((u32)end_of_all_plants == 9) print_string("compile time mult");
   
   struct v3 large;
   large.x = 1;
   large.y = 2;
   large.z = -+-+-+-+-+-+-+-+-+3;
   
   s32 sum_of_large = i_take_a_large_argument(large);
   if(sum_of_large == 0) print_string("we got it right");
   
   struct v3 i_copy_a_large_argument = large;
   i_copy_a_large_argument.z = 3;
   s32 sum_of_large_2 = i_take_a_large_argument(i_copy_a_large_argument);
   if(sum_of_large_2 == 6) print_string("we got it right twice");
   
   struct v3 i_copy_a_large_argument2 = i_return_a_large_struct();
   s32 sum_of_large_3 = i_take_a_large_argument(i_copy_a_large_argument2);
   if(sum_of_large_3 == 4) print_string("three times the charm");
   ;
   
   imaglobal = 1;
   
   struct v2 i_was_intialized_to_zero = {};
   
   if(i_was_intialized_to_zero.x == 0 && i_was_intialized_to_zero.y == 0){
      print_string("we can initialize to zero");
   }
   
   struct v2 i_was_intialized_to_one_two = {(int)one, (int)two};
   
   if(i_was_intialized_to_one_two.x == 1){
      if(i_was_intialized_to_one_two.y == 2){
         print_string("we can initialize to values");
      }
   }
   
   {
      u32 decl_test = 1;
   }
   u32 decl_test = 2;
   
   struct v2 i_was_intialized_to_one_two_named = {.x =(int) one, .y = (int)two};
   
   if(i_was_intialized_to_one_two_named.x == 1){
      if(i_was_intialized_to_one_two_named.y == 2){
         print_string("we can initialize to named values");
      }
   }
   
   u32 u32_has_braces = {1};
   
   u32 shift = 1 << 3 << 2; // get interpreted as (1 << 3) << 2
   if(shift == 32) print_string("we can shift");
   
   u32 shift_and_or = 1 | 1 << 3 | 1 << 4 | 2;
   
   if(shift_and_or == 27) print_string("shift or");
   
   u32 mult = shift_and_or * shift * 3;
   if(mult == 2592) print_string("multiply");
   
   u32 paren_expression = 3 * (one + two);
   
   if(paren_expression == 9) print_string("(.)(.), do you like what you see?");
   
   //GetLastError();
   
   struct memory_arena arena;
   arena.current = null;
   arena.end = null;
   arena.base = null;
   arena.reserved_size = 0;
   
   u8 *string = push_struct(&arena, 10);
   //imaglobal = 42;
   //u32 *as = (u32 *)null;
   //*as = 0;
   
   //return (s64)string;
   void_ptr my_awesome_pointer;
   string[1] = 'a';
   
   u64 varargs_sum = varargs_sum_u64(7, 1, 2, 3, 4, 5, 6, 7);
   if(varargs_sum == 28) print_string("varargs sum");
   
   
   struct v2 abc;
   abc.x = 1;
   abc.y = 2;
   
   abc;
   
   u32 factiorial_of_9 = 1;
   for(u32 i = 1; i < 10; i = i + 1){
      factiorial_of_9 = factiorial_of_9 * i;
   }
   if(factiorial_of_9 == 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9){
      print_string("'for'-factorial");
   }
   
   
   struct linked_list *first = null;
   struct linked_list *last  = null;
   
   last = linked_list_push_node(&arena, last, 'a');
   first = last;
   last = linked_list_push_node(&arena, last, 'b');
   last = linked_list_push_node(&arena, last, 'c');
   
   for(struct linked_list *it = first; it; it = it->next){
      print_char((char)it->value);
   }
   
   char *str = "whatsup!\n";
   
   u32 amount_written;
   WriteFile(handle, str, 9, &amount_written, (void *)0);
   
   int a = 0;
   int b = 0;
   
   enum plants my_plant = plant;
   
   my_awesome_plants my_awesome_plant = thats_all_i_know;
   
   struct i_contain_a_v3 a_v3 = {{1, 2, 3}};
   if(a_v3.a.x == 1) if(a_v3.a.y == 2) if(a_v3.a.z == 3) print_string("double braces all the way");
   
   struct i_contain_a_v3 b_v3 = {.a.x = 3,};
   
   if(b_v3.a.x == 3) if(b_v3.a.y == 0) if(b_v3.a.z == 0) print_string("double names all the way");
   
   struct i_contain_a_v3 a_v3_named = {.a = {1, 2, 3}};
   if(a_v3_named.a.x == 1 && a_v3_named.a.y == 2 && a_v3_named.a.z == 3){
      print_string("double named braces all the way");
   }
   
   if(global_initializer.x == 0) 
      if(global_initializer.y == 1) 
      if(global_initializer.z == 2) 
      print_string("globally initilized!");
   
   if(imainitializedglobal == 1) print_string("initized u32");
   
   print_string(global_string_literal);
   
   print_number(123456);
   print_char('\n');
   
   void (*my_awesome_print_function)(char *string) = print_string;
   
   my_awesome_print_function("function pointered");
   
   
   u64 many_args_function_call = varargs_sum_u64(8, 1, 2, 3, 4, 5, 6, 7, varargs_sum_u64(8, 100, 200, 300, 400, 500, 600, 700, 800));
   
   print_number((u32)many_args_function_call);
   print_char('\n');
   
   //print_string(__FUNCTION__);
   
   return (s32)amount_of_strings_printed;
}


/* @cleanup:
struct struct_around_struct{
   struct struct_in_struct{
      u32 a;
   };
};*/


char *global_string_literal = "im a global string literal!";

typedef signed char      s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef float f32;
typedef double f64;
typedef s32 b32;

__declspec(dllimport) u32 GetLastError();

__declspec(dllimport) void **GetStdHandle(u32 nStdHandle);
__declspec(dllimport) s32 WriteFile(void **hFile, char *lpBuffer, u32 nNumberOfBytesToWrite,u32 *lpNumberOfBytesWritten, void *lpOverlapped);
__declspec(dllimport) void *VirtualAlloc(void *lpAddress, u64 dwSize, u32 flAllocationType, u32 flProtect);

typedef void *void_ptr;

typedef enum plants my_awesome_plants;

typedef u32 plants;

struct i_contain_a_v3{
   struct v3 a;
};

struct v3 construct_v3(s32 x, s32 y, s32 z){
   struct v3 ret;
   ret.x = x;
   ret.y = y;
   ret.z = z;
   return ret;
}

void *push_struct(struct memory_arena *arena, u64 size){
   u64 rest_size = (u64)(arena->end - arena->current);
   
   if(rest_size < size){
      u64 current_arena_size = (u64)(arena->end - arena->base);
      u64 grow_to = current_arena_size * 2;
      
      if(grow_to < current_arena_size + size){
         grow_to = current_arena_size + size;
      }
      
      u32 megabyte =  1024 * 1024;
      if(grow_to < (u64)megabyte){
         grow_to = (u64)megabyte;
      }
      u32 MEM_COMMIT  = 0x00001000;
      u32 MEM_RESERVE = 0x00002000;
      u32 flags = MEM_COMMIT | MEM_RESERVE;
      u32 PAGE_EXECUTE_READWRITE = 0x40;
      u32 allocation_flags = PAGE_EXECUTE_READWRITE;
      arena->base = VirtualAlloc((void *)0, grow_to, flags, allocation_flags);
      
      if((u64)arena->base == 0){
         u32 error = GetLastError();
         return (void *)error;
      }
      
      arena->current = arena->base;
      arena->reserved_size = grow_to;
      arena->end = arena->base + arena->reserved_size;
   }
   
   
   void *ret = (void *)arena->current;
   arena->current = arena->current + size;
   
   return ret;
}

struct memory_arena{
   u8 *current;
   u8 *end;
   u8 *base;
   u64 reserved_size;
};

u32 factorial(u32 a){
   if(a == 1){
      //void *null = (void *)0;
      //u32 *as = (u32 *)null;
      //*as = 0;
      return 1;
   }
   
   //b = 0;
   
   return factorial(a - 1) * a;
}

s32 i_take_a_large_argument(struct v3 a){
   return(a.x + a.y + a.z);
}


struct v3 i_return_a_large_struct(){
   struct v3 ret;
   ret.x = 1;
   ret.y = 0;
   ret.z = 3;
   return ret;
}

u64 strlen(char *a){
   u64 ret = 0;
   while(*a){ // @cleanup: post and preincrement
      a = a + 1;
      ret = ret + 1; 
   }
   return ret;
}


void print_string_no_newline(char *a){
   u32 stdout = 0xFFFFFFF5;
   void **handle = GetStdHandle(stdout);
   u32 length = (u32)strlen(a);
   u32 ignored;
   WriteFile(handle, a, length, &ignored, null);
}

s64 amount_of_strings_printed;

void print_string(char *a){
   u32 stdout = 0xFFFFFFF5;
   void **handle = GetStdHandle(stdout);
   u32 length = (u32)strlen(a);
   u32 ignored;
   WriteFile(handle, a, length, &ignored, null);
   char *newline = "\n";
   WriteFile(handle, newline, 1, &ignored, null);
   amount_of_strings_printed++;
}

struct v3 global_initializer = {0, 1, 2};

void print_number(u32 n){
   if(n == 0){
      print_char('0');
      return;
   }
   u32 asd = n;
   u32 log = 1;
   while(asd){
      log = log * 10;
      asd = asd / 10;
   }
   log = log / 10;
   
   while(log){
      u8 c = (u8)((n / log) % 10) + '0';
      print_char((char)c);
      log = log / 10;
   }
}

void print_char(char a){
   u32 stdout = 0xFFFFFFF5;
   void **handle = GetStdHandle(stdout);
   u32 amount_written;
   WriteFile(handle, &a, 1, &amount_written, (void *)0);
}

typedef char* va_list;
u64 sum_u64_va_list(u32 amount, va_list va){
   u64 sum = 0;
   for(u32 i = 0; i < amount; i++){
      sum += *(u64* ) ((va += sizeof(__int64)) - sizeof(__int64));
   }
   return sum;
}

u64 varargs_sum_u64(u32 amount, ...){
   va_list va;
   //((void)(__va_start(&va, amount)));
   va = (char *)&amount + 1;
   u64 sum = sum_u64_va_list(amount, va);
   ((void)(va = (va_list)0));
   return sum;
}

static struct linked_list *linked_list_push_node(struct memory_arena *arena, struct linked_list *add_to, u32 value){
   struct linked_list *new_node = push_struct(arena, 16);// sizeof(linked_list));
   if(add_to) add_to->next = new_node;
   new_node->value = (u64)value;
   new_node->next = (void *)0;
   return new_node;
}

struct linked_list{
   struct linked_list *next;
   u64 value;
};

struct v2{
   s32 x;
   s32 y;
};

struct v3{
   s32 x;
   s32 y;
   s32 z;
};

u32 imaglobal;
u32 imainitializedglobal = 1;
struct v2 imaglobalvector;

void contains_cast_to_stdcall_function_ptr(void){
    (int (__stdcall *)(int))1;
}
