@echo off

git clone --recursive --depth 1 --branch release-2.30.9 https://github.com/libsdl-org/SDL.git
cd SDL || exit /b 1

git clean -xdf

mkdir build
cd build

cmake .. -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe"  || exit /b 1
ninja  || exit /b 1

cd ..\..
