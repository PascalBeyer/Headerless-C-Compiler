@echo off

git clone --recursive --depth 1 --branch v0.1.0 https://github.com/erincatto/box3d.git
cd box3d || exit /b 1

git clean -xdf

cmake -B build -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -D CMAKE_CXX_COMPILER=cl.exe || exit /b 1
cd build || exit /b 1

ninja || exit /b 1
bin\test.exe || exit /b 1

cd ..\..
