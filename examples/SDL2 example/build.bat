@echo off

set defines= -D_MSC_VER=1911 -DDECLSPEC=__declspec(dllimport)
set dlls= -L kernel32.dll -L SDL2.dll

call "../../build/pbc.exe" graph.c %defines% -o graph %dlls%