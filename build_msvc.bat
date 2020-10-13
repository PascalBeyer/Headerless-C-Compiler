@echo off

echo Compiling with msvc

set to_build=src/main.c 
set out_name=out.exe

rem *******************************************WARNINGS*****************************************************
rem set warning_options= /w
set warning_options= -Wall -wd4201 -wd4820  -wd4711 -wd4200 -wd4204 -wd4214 -wd4324
rem ********************************************************************************************************

rem *******************************************OPTIONS******************************************************
set compiler_flags= -Od -nologo -fp:fast -GR- -EHa- -Zo -Oi -D_Debug  %warning_options% -FC -Z7 -GS- -Gs9999999 /Fo:build/

set linker_flags= -HEAP:0,0 -STACK:0x100000,0x100000 -incremental:no -opt:ref kernel32.lib ucrt.lib /NODEFAULTLIB /ENTRY:_start /SUBSYSTEM:console /DYNAMICBASE:NO /NOLOGO /DEBUG /out:build/%out_name%
rem ********************************************************************************************************


cl /TC /c %compiler_flags% %to_build% 

link %linker_flags% build/main.obj build/asm-functions.obj 



