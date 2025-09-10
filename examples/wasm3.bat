
git clone https://github.com/wasm3/wasm3.git --recursive --depth 1 --branch v0.5.0
cd wasm3 || exit /b 1

git clean -xdf

cmake -B build -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" || exit /b 1
cd build || exit /b 1

ninja || exit /b 1

cd ..\..
