git clone --recursive --depth 1 --branch v1.3.1 https://github.com/madler/zlib.git
cd zlib || exit /b 1

git clean -xdf

cmake . -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -B build  || exit /b 1
cmake --build build  || exit /b 1

copy build\zconf.h . || exit /b 1
build\example || exit /b 1

cd ..
