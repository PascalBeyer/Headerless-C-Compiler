git clone --recursive --depth 1 https://github.com/yasm/yasm.git 
cd yasm || exit /b 1 

git clean -xdf

cmake -B build -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -G Ninja || exit /b 1
cd build || exit /b 1

ninja || exit /b 1

cd ..\..
