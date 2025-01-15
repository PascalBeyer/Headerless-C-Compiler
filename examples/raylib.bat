@echo off

git clone --recursive --depth 1 --branch 5.0 https://github.com/raysan5/raylib.git
cd raylib || exit /b 1

git clean -xdf

mkdir build
cd build

cmake .. -G Ninja -D CMAKE_C_COMPILER="C:\Projects\Headerless-C-Compiler\hlc.exe" || exit /b 1
ninja || exit /b 1

:: cd ..\..

