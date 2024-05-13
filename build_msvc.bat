
@echo off

echo Compiling with msvc
IF NOT EXIST build mkdir build

rem *******************************************WARNINGS*****************************************************
rem set warning_options= /w
set warning_options= -Wall -wd4201 -wd4820  -wd4711 -wd4200 -wd4204 -wd4214 -wd4324 -wd5045 -wd4061 -wd4221
rem ********************************************************************************************************

set compiler_flags= -Od -nologo -fp:fast -GR- -EHa- -Zo -Oi -D_Debug  %warning_options% -FC -Z7 -GS- -Gs9999999 /diagnostics:column /TC

set linker_flags= -HEAP:0,0 -STACK:0x100000,0x100000 -incremental:no -opt:ref Advapi32.lib kernel32.lib ucrt.lib /NODEFAULTLIB /ENTRY:_start /SUBSYSTEM:console /NOLOGO /DEBUG 

cl %compiler_flags% src/main.c /Fo:build/hlc.obj /link %linker_flags% /pdb:build/hlc.pdb /out:hlc.exe
