
# Headerless-C Compiler

The goal of this project is to write a compiler, which is _compliant enough_ with the C11-specification 
to compile all relevant source code, while eliminating the need for header files as much as possible.
This is done by allowing declarations within a compilation unit to come in any order, meaning
```c
void a(){ b(1); }
void b(int v){ }
```
is just as valid of a program as 
```c
void b(int v){ }
void a(){ b(1); }
```
and sharing all types, enums and external declarations between all compilation units.
Importantly, as in the example above, the converse is not true, meaning some "invalid" C-source code compiles.

Currently, the compiler is designed to replace MSVC for my personal needs. This means it only supported platform is x64-Windows.
It can produce executables (.exe), shared libraries (.dll) and associated debug information in the form of .pdb-files,
as well as object files (.obj) containing CodeView debug information.

There are some compiler specific extensions, like a [type-inferring print](#`__declspec(printlike)`), (very incomplete) [inline-assembly](#`__asm__`-/-`__declspec(inline_asm)`), 
local functions, slices, etc, but all of these should be considered in the "design"-stage and unstable.

# Installing and Usage

See the release page for pre-build binaries. To compile from source invoke the `build_msvc.bat` with `cl.exe`,
`link.exe` and `ucrt.lib` in your path (for example from an [x64 Native Tools Command Prompt](https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line?view=msvc-170&viewFallbackFrom=vs-2019).

To use standard library functions or Windows library functions, it is necessary to install a version of MSVC (I usually install the "Build Tools for Visual Studio")
and the Windows SDK's.

To make sure it works you can try to self-host: `hlc.exe src/main.c -no_premain -L kernel32.lib -L Advapi32.lib`
Or even try the test-suite: `hlc.exe test_runner.c && test_runner.exe`.

# Compliance

The compiler was written to be mostly compatible the last [C11-specification draft](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf) document.
Some features I dislike are not implemented like `_Generic`, `_Atomic`, `_Complex` and Variable-Length-Arrays
and currently the `const`-keyword is ignored.
Some of the very weird features of C are eliminated, for example the concept of compatible and composite types:
```c
// This is valid C-code but does not compile using `hlc`.
int function(int (*)[]);
int function(int (*)[3]);
```
The general rule is, if something is used by popular C-code, it should be supported.

Furthermore, allowing for "out-of-order" declarations means some valid C-code does not compile using `hlc`, e.g.:
```c
int main(){
    struct structure *predecl = 0;
    struct structure{ int member; };
    
    return predecl->member;
}

struct structure{ int a, b; }; // Global structure definition, seen by `predecl` when using `hlc`.
```
When using `hlc` to compile more than one source file (e.g.: `hlc main.c other.c`) all types, enums and external declarations
values are shared between the two compilation units, which will cause an error when there is a conflict, e.g.:
```c
// main.c
struct structure{ int a; };
int main(){}

// other.c
struct structure{ int member; };
```
One workaround to these sort of problems is to compile the compilation units individually and then link them.
```
hlc /obj main.c
hlc /obj other.c
link.exe main.obj other.obj
```
In this way, `hlc` can be used more like a traditional C-compiler.

# Compiler Specific Extensions

Some smaller extensions are implemented. For example, `hlc` allows empty structures, 
allows using `.` inplace of `->` if `-allow_dot_as_arrow` is specified, 
allows bounds-checked indexing of any structure containing a `data`- and a `size`-member 
and allows for including structures [like MSVC](https://learn.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2013/z2cx9y4f(v=vs.120)?redirectedfrom=MSDN).

The two bigger extensions are a type-infering print function and support for inline assembly (very incomplete).

## `__declspec(printlike)`

This keyword allows `printf`-style functions to infer the format string from the arguments:
```c
    struct v2 {int x,y;} v = {0};
    print("{}", v); // prints "(struct v2){.x = 0, .y = 0}"
```
The compiler replaces the format string supplied to the `print` function with the inferred format string.
Hence, the compiled code is equivalent to:
```c
struct v2 {int x,y;} v = {0};
struct v2 temp = v;
print("(struct v2){.x = %d, .y = %d}", temp.x, temp.y);
```
A simple implementation of `print` is as follows:

```c
#include <stdio.h>
#include <stdarg.h>

__declspec(printlike) int print(char *format, ...){
    va_list va;
    va_start(va, format);
    int ret = vprintf(format, va);
    va_end(va);
    
    fflush(stdout); // I always want my prints to flush, but you can remove this if you don't :)
    
    return ret;
}
```
The syntax is similar to other "modern" languages like [rust](https://doc.rust-lang.org/rust-by-example/hello/print.html) or [c++23](https://en.cppreference.com/w/cpp/io/print),
allowing format specifiers after `:`, e.g.: `print("{:x}\n", 1337) // 0x539`.
It also allows for mixing in C-style format specifiers and the format specifier `%?` also infers its type.
Furthermore, for a structure, the `#` specifier (e.g.: `print("{:#}")`) makes the output multi-line.
Assuming `int i = 1337;`, `char *h = "Hello, World!";` and `struct v2 {int x,y;} v = {0};`, here are some valid prints:
```c
    print("{:x} %s {}\n", i, h, v); // 539 Hello, World! (struct v2){.x = 0, .y = 0}
    print("{i=:.8} %.3?\n", v);     // i = 00001337 (struct v2){.x = 000, .y = 000}
    print(i);                       // i = 1337
    print(i + 1);                   // 1338
    print("%x {h:.3s} {i}\n", i);   // 539 Hel 1337
```

## `__asm__` / `__declspec(inline_asm)`

The compiler allows for inserting inline assembly blocks as follows:
```c
#include <stdlib.h>
int main(int argc, char *argv[]){
    int leaf = atoi(argv[1]), subleaf = atoi(argv[2]), cpuid[4];
    __asm__ {
        mov eax, leaf
        mov ecx, subleaf
        
        cpuid
        
        mov cpuid[0], eax
        mov cpuid[1], ebx
        mov cpuid[2], ecx
        mov cpuid[3], edx
    }
    print("{:x}", cpuid);
}
```
Currently, only instructions work which have been explicitly implemented. In the future a more general solution utilizing
(tables from?) [intel xed](https://github.com/intelxed/xed) or something similar is needed.
The same mechanism is used to implement intrinsics using the `__declspec(inline_asm)` keyword, for example the following function
is taken from the `implicit/instrinsic.c` file:
```c
// usage: `u64 tsc = __rdtsc();`
__declspec(inline_asm) unsigned __int64 __rdtsc(){
    rdtsc
    shl rdx, 32
    or rax, rdx
    return rax
}
```
`__declspec(inline_asm)` functions will be inlined in the function on each call, this also allows them to take integer literals as arguments,
e.g.:
```c
// usage: `int a1 = _mm_extract_epi32(a, 1);`
__declspec(inline_asm) int _mm_extract_epi32(__m128i a, const int imm8){
    pextrd eax, a, imm8
    return eax
}
```

# Internals

Compilations runs through various stages. First, each compilation unit is preprocessed (`preprocess.c`) 
and then split-up into global scope declarations. To facilitate out-of-order compilation, each of the global declarations
is put in a work-queue and individually passed to the parser (`parse.c`). At this stage, only the declarations are parsed,
function definitions are parsed in a later stage. The parser differs from a "normal" C-parser in that 
it does not immediately reports an error, if it encounters an unknown identifier. 
Instead the parser tells the containing declaration to "sleep" and only wake up, once the unknown identifier is resolved.

After all global declarations are known and parsed, the next stage is to parse function definitions into an abstract syntax tree.
The abstract syntax tree allows us to determine, which functions are unreachable and subsequently only emit machine code reachable functions.
The machine code is produced by linearly walking the abstract syntax tree without really performing any optimizations (`emit_x64.c`).

Finally, we are ready to write (`*_writer.c`) the output file (either .obj or .exe) and if not otherwise specified, also produce debug information
by re-iterating the syntax-tree of all declarations/functions which made it into the executable.

# Current State and Next Steps

At this point most C code which compiles using MSVC also compiles using `hlc`. Notable exceptions include code which uses
[thread local storage](https://learn.microsoft.com/en-us/windows/win32/procthread/thread-local-storage), 
[`#pragma pack(<...>)`](https://learn.microsoft.com/en-us/cpp/preprocessor/pack?view=msvc-170), 
[`__declspec(allocate("<section>"))`](https://learn.microsoft.com/en-us/cpp/cpp/allocate?view=msvc-170)
and [structured exception handling](https://learn.microsoft.com/en-us/windows/win32/debug/structured-exception-handling).

Code-wise, most of the C-components (`preprocess.c`, `parse.c`, `emit_x64.c`) are hardened through test-suites and fuzz-testing.
Some larger refactorings are on the "eventually"-list, like separating out the concept of `tokens` and `preprocessor` tokens or 
switching from the current AST-based implementation to a system which immediately produces an intermediate representation.

Currently, the main focus is on improving the internals of PDB- and CodeView-generation as well as introducing some way of 
linking object files. This will then allow implementing wrapper binaries like `hlc-gcc` or `hlc-cl` allowing the compiler to
be integrated into existing build systems more easily.

Further in the future, the goal is to completely decouple `hlc` from the Microsoft tool chain, by providing 
an hlc-specific standard library (including but not limited to a version of libc).

