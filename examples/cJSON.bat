
git clone --recursive --depth 1 --branch v1.7.18 https://github.com/DaveGamble/cJSON.git
cd cJSON || exit /b 1

git clean -xdf

mkdir build
cd build

REM Use -D CMAKE_BUILD_TYPE=Release because one of the tests is stupid an checks that a member is null after the struct has been freed
cmake .. -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -D CMAKE_BUILD_TYPE=Release || exit /b 1
ninja      || exit /b 1
ninja test || exit /b 1

cd ..\..
