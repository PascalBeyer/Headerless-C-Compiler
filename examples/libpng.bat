
if not exist zlib (
    echo Error: Need to build zlib first
    exit /b 1
)

git clone --recursive --depth 1 --branch libpng16 https://github.com/pnggroup/libpng.git
cd libpng || exit /b 1

git clean -xdf

cmake . -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -B build -D ZLIB_LIBRARY="%~dp0zlib\build\zlibstaticd.lib" -D ZLIB_INCLUDE_DIR="%~dp0zlib" || exit /b 1
cd build
ninja || exit /b 1
ninja test || exit /b 1

cd ..\..
