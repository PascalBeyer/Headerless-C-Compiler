
# Headerless-C Compiler

The goal of this project is to write a compiler, that is _compliant enough_ with the C11-specification 
to compile all relevant source code, while eliminating the need for header files as much as possible.
This is done by sharing all types, enums and external declarations between all compilation units and 
allowing declarations within a compilation unit to come in any order, meaning
```c
void a(){ b(1); }
void b(int v){ }
```
is just as valid of a program as 
```c
void b(int v){ }
void a(){ b(1); }
```

Currently, the compiler is designed to replace MSVC for my personal needs. This means it only supported platform is x64-Windows.
It can produce executables (.exe), shared libraries (.dll) and associated debug information in the form of .pdb-files,
as well as object files (.obj) containing CodeView debug information.

While the compiler still needs a good amount of work, it is able to compile several real world projects like [curl](examples/curl.bat), [raylib](examples/raylib.bat), [glfw](examples/glfw.bat), and [more](examples)!

There are some compiler-specific extensions, like a [type-inferring print](#`__declspec(printlike)`), (very incomplete) [inline-assembly](#`__asm__`-/-`__declspec(inline_asm)`), 
local functions, slices, etc, but all of these should be considered in the "design"-stage and unstable.

## Compiling and Usage

See the [release page](https://github.com/PascalBeyer/Headerless-C-Compiler/releases) for pre-build binaries. To compile from source invoke the `build_msvc.bat` with `cl.exe`,
`link.exe` and `ucrt.lib` in your path (for example, from an [x64 Native Tools Command Prompt](https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line?view=msvc-170&viewFallbackFrom=vs-2019)).

To use standard library or Windows library functions it is necessary to install a version of the [Windows SDK](https://developer.microsoft.com/en-us/windows/downloads/windows-sdk/).

To make sure it works, you can try to self-host: `hlc.exe src/main.c`, or even try the test-suite: `hlc.exe test_runner.c && test_runner.exe`.

## Compliance

The compiler was written to be mostly compatible with the last [C11-specification draft](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf) document.
Features I dislike are not implemented, like `_Complex` or Variable-Length-Arrays and currently, the `const`-keyword is ignored.
Some of the very weird features of C are eliminated, for example, the concept of compatible and composite types:
```c
// This is valid C-code but does not compile using `hlc`.
int function(int (*)[]);
int function(int (*)[3]);
```
The general rule is, that if something is used by popular C-code, it should be supported.

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
One workaround to these sorts of problems is to compile the compilation units individually and then link them.
```
hlc /obj main.c
hlc /obj other.c
link.exe main.obj other.obj
```
In this way, `hlc` can be used more like a traditional C-compiler. This is also how most build system expect to use a C-Compiler.
As `hlc` does not include a linker at this point, this usually requires installing a version of Visual Studio. I usually install the build-tools for Visual Studio.

## Compiler Specific Extensions

Some smaller extensions are implemented. For example, `hlc` allows empty structures, local functions,
using `.` in place of `->`, bounds-checked indexing of any structure containing a `data`- and a `size`-member 
and allows for including structures [like MSVC](https://learn.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2013/z2cx9y4f(v=vs.120)?redirectedfrom=MSDN).

The two bigger extensions are a type-inferring print function and support for inline assembly (very incomplete).

### `__declspec(printlike)`

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
It also allows for mixing in C-style format specifiers, and the format specifier `%?` also infers its type.
Furthermore, for a structure, the `#` specifier (e.g.: `print("{:#}")`) makes the output multi-line.
Assuming `int i = 1337;`, `char *h = "Hello, World!";` and `struct v2 {int x,y;} v = {0};`, here are some valid prints:
```c
    print("{:x} %s {}\n", i, h, v); // 539 Hello, World! (struct v2){.x = 0, .y = 0}
    print("{i=:.8} %.3?\n", v);     // i = 00001337 (struct v2){.x = 000, .y = 000}
    print(i);                       // i = 1337
    print(i + 1);                   // 1338
    print("%x {h:.3s} {i}\n", i);   // 539 Hel 1337
```

### `__asm__` / `__declspec(inline_asm)`

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
Currently, only instructions work that have been explicitly implemented. In the future, a more general solution utilizing
(tables from?) [intel xed](https://github.com/intelxed/xed) or something similar is needed.
The same mechanism is used to implement intrinsics using the `__declspec(inline_asm)` keyword. For example, the following function
is taken from the `implicit/include/instrinsic.c` file:
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

One of the next steps will be to provide a _full_ inline assembler and use it to implement all intel intrinsics, as well as "common" intrinsics from gcc, MSVC and clang.

## Internals

Compilation runs through various stages. First, each compilation unit is preprocessed (`preprocess.c`) 
and then split up into global scope declarations. To facilitate out-of-order compilation, each of the global declarations
is put in a work queue and individually passed to the parser (`parse.c`). At this stage, only the declarations are parsed,
function definitions are parsed in a later stage. The parser differs from a "normal" C-parser in that 
it does not immediately report an error if it encounters an unknown identifier. 
Instead, the parser tells the containing declaration to "sleep" and only "wake up" once the unknown identifier is resolved.

After all global declarations are known and parsed, the next stage is to parse function definitions into an abstract syntax tree.
The abstract syntax tree allows us to determine, which functions are unreachable and subsequently only emit machine code for reachable functions.
The machine code is produced by linearly walking the abstract syntax tree without performing any optimizations (`emit_x64.c`).

Finally, we are ready to write (`*_writer.c`) the output file (either .obj or .exe) and if not otherwise specified, also produce debug information
by re-iterating the syntax tree of all declarations and functions that made it into the executable.

## Current State and Next Steps

At this point, most C code that compiles using MSVC also compiles using `hlc`. Notable exceptions include code, which uses
[`#pragma pack(<...>)`](https://learn.microsoft.com/en-us/cpp/preprocessor/pack?view=msvc-170), 
[`__declspec(allocate("<section>"))`](https://learn.microsoft.com/en-us/cpp/cpp/allocate?view=msvc-170)
and [structured exception handling](https://learn.microsoft.com/en-us/windows/win32/debug/structured-exception-handling).

Code-wise, most of the C-components (`preprocess.c`, `parse.c`, `emit_x64.c`) are hardened through test suites and fuzz-testing.
Some larger refactors are on the "eventually"-list, like separating out the concept of `tokens` and `preprocessor` tokens or 
switching from the current AST-based implementation to a system that immediately produces an intermediate representation.

Currently, the main focus is on improving the internals of PDB- and CodeView-generation as well as introducing some way of 
linking object files. This will then allow implementing wrapper binaries like `hlc-gcc` or `hlc-cl` allowing the compiler to
be integrated into existing build systems more easily.

