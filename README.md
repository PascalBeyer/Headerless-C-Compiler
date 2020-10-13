
## Summary
PBC aims to be a small, fast, dependency-free Windows x64 C-compiler that supports out of order compilation.
"Dependency-free" in this case also includes the linker, meaning that PBC writes out '.exe' and '.pdb' files without invoking a linker. 

## Install
A prebuild binary is included in the build directory. To compile it yourself invoke 'build_msvc.bat' with 'cl.exe', 'link.exe' and 'ucrt.lib' in your path (for example from a [x64 Native Tools Command Prompt](https://docs.microsoft.com/en-us/cpp/build/building-on-the-command-line?view=vs-2019)). (For referance I am using MSVC Version 19.11.25506).

## Compliance
As PBC supports out-of-order compilation, i.e.
```
int a() { return b(); }
int b() { return 1; }
```
is just as valid as
``` 
int b() { return 1; }
int a() { return b(); }
```
conforming to the C-spec is impossible. Therefore, the goal is *most reasonable C-programs should compile*.
Besides out-of-order compilation, the compiler also supports assignments between "differant" anonymous structs
with the same fields allowing for assignments like
```
#define pair(type_a, type_b) struct { type_a first; type_b second; }

pair(int, int) a = { .first = 1, .second = 3 };

pair(int, int) b = a;
```
Further PBC has support for local functions and local types, though they currently do not interact with each other.


## Examples
Some examples can be found in the 'examples' sub directory. For the SDL2 example one needs to download [SDL2.dll](https://libsdl.org/download-2.0.php) and the header files (sub directory 'include' in the source).

## Current State
* Single compilation unit applications compile for the most part.
* Dynamic linking is implemented using '.dll' files. (No '.lib' support yet.)
* Executable PE files (.exe) are supported.
* Debug information in form of .pdb files is supported. (But needs some work.)
* Static linking and object files are not yet supported.
* Linking to the CRT works except <math.h>.
* Some intrinsic functions are implemented. (But only a few.)
* No optimizations are implemented.
* The compiler only relies on the standard library (ucrt.dll) to parse floats. And [stb_sprinf.h](https://github.com/nothings/stb) for printing format strings.
* Currently all type qualifiers(const, restrict, volatile) are ignored.

# Missing Features
* Bit fields (making it hard to interact with <windows.h>)
* _Bool
* Variable-length arrays
* _Thread_local/Atomics/_Generic/Complex/...
* Wide String literals (L"asd", u"asd", u8"asd", U"asd") used in <assert.h>

## Internals
During compilation a file goes through various stages:
* Tokenize (tokenize.c): Breaks the input a list of 'tokens'.
* Preprocess (tokenize.c): Evaluates preprocessor directives into a new list of preprocessed tokens.
* Parse (parse.c): A recursive decent parser is used to create an "Abstract Syntax Tree" and to typecheck each node.
* Code Generation (asm_emit.c): Emit x64-assembly by recursively walking down the Abstract Syntax Tree.
* Output File Generation (coff_writer.c): Writes out an '.exe' and '.pdb'.

## Notes for playing around with the Compiler
* Currently no 'premain' stub gets emitted, so you have to get and parse the command-line yourself.
* Linking to the CRT still does not work fully. Some functions in <math.h> are not declared '_CRTIMP' for some reason.
* No control flow or data flow analysis is performed, so if your program crashes it is likely that you forgot to return from a procedure.
* Warning and error messages still need a lot of work.
