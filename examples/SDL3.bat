@echo off

git clone --recursive --depth 1 --branch preview-3.1.6 https://github.com/libsdl-org/SDL.git SDL3
cd SDL3 || exit /b 1

git checkout *
git clean -xdf

mkdir build
cd build

cmake .. -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe"  || exit /b 1
ninja  || exit /b 1

cd ..\..
