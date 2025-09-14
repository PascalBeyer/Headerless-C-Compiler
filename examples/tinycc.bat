
git clone --recursive --depth 1 --branch mob https://github.com/TinyCC/tinycc.git
cd tinycc\win32 || exit /b 1

git clean -xdf

set TCC_C=..\tcc.c
call build-tcc.bat -c hlc.exe -t 64 || exit /b 1

cd ..\..


