
typedef char s8;
typedef short s16;
typedef int s32;
typedef long long s64;

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

typedef long long d64;

#define false 0


#define null (void *)0
__declspec(dllimport) void **GetStdHandle(u32 nStdHandle);
__declspec(dllimport) s32 WriteFile(void **hFile, char *lpBuffer, u32 nNumberOfBytesToWrite,u32 *lpNumberOfBytesWritten, void *lpOverlapped);

u64 strlen(char *a){
   u64 ret = 0;
   while(*a){ // @cleanup: post and preincrement
      a = a + 1;
      ret = ret + 1; 
   }
   return ret;
}


void print_string(char *a){
   u32 stdout = 0xFFFFFFF5;
   void **handle = GetStdHandle(stdout);
   u32 length = (u32)strlen(a);
   u32 ignored;
   WriteFile(handle, a, length, &ignored, null);
}

void print_char(char a){
   u32 stdout = 0xFFFFFFF5;
   void **handle = GetStdHandle(stdout);
   u32 amount_written;
   WriteFile(handle, &a, 1, &amount_written, (void *)0);
}

void print_number(u64 n){
   if(n == 0){
      print_char('0');
      return;
   }
   u64 asd = n;
   u64 log = 1;
   while(asd){
      log = log * 10;
      asd = asd / 10;
   }
   log = log / 10;
   
   while(log){
      char c = (char)((n / log) % 10) + '0';
      print_char(c);
      log = log / 10;
   }
}

void __do_assert(char *file, int line, char *function){
   print_string(file);
   print_string(":");
   print_number((u64)line);
   print_string(":");
   print_string("ASSERT FIRED: ");
   //print_string(function);
   print_string("\n");
}
#define assert(expr) ((expr) ? 0 : (__do_assert(__FILE__,  __LINE__, __FUNCTION__), errors+=1, 1))
