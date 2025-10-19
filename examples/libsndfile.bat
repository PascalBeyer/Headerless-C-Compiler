git clone https://github.com/libsndfile/libsndfile.git --recursive --depth 1 --branch 1.2.2 
cd libsndfile || exit /b 1

git clean -xdf

cmake -B build -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" -G Ninja || exit /b 1
cd build || exit /b 1

ninja || exit /b 1
ninja test || exit /b 1

cd ..\..
