@echo off

git clone --recursive --depth 1 --branch 3.4 https://github.com/glfw/glfw.git
cd glfw || exit /b 1

git checkout *
git clean -xdf

mkdir build
cd build
cmake .. -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" || exit /b 1
ninja || exit /b 1
cd ..\..
