git clone --recursive --depth 1 --branch v4.1.0 https://github.com/zyantific/zydis.git
cd zydis || exit /b 1

git clean -xdf

cmake -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -B build || exit /b 1

cd build || exit /b 1
ninja || exit /b 1
ninja test || exit /b 1
cd ..\..
