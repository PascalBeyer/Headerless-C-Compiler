

git clone --recursive --depth 1 --branch VER-2-13-3 https://github.com/freetype/freetype.git
cd freetype || exit /b 1

git clean -xdf
git checkout *

mkdir build
cd build

cmake .. -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" || exit /b 1
ninja || exit /b 1

cd ..\..

