
git clone --recursive --depth 1 --branch 3.0.2 https://github.com/richgel999/miniz.git
cd miniz || exit /b 1

git clean -xdf

cmake . -G Ninja -D CMAKE_C_COMPILER="C:\Projects\Headerless-C-Compiler\hlc.exe" -B build || exit /b 1
cd build || exit /b 1

ninja || exit /b 1

cd ..\bin || exit /b 1

example1.exe || exit /b 1
example2.exe || exit /b 1

cd ..\..

