
git clone --recursive --depth 1 --branch curl-8_11_0 https://github.com/curl/curl.git
cd curl || exit /b 1

git clean -xdf
git checkout *

echo #include ^<intrin.h^> // for _InterlockedExchange and _InterlockedCompareExchange >> src/tool_doswin.c

mkdir build
cd build

cmake .. -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" || exit /b 1
ninja || exit /b 1

copy src\curl.exe lib || exit /b 1
lib\curl.exe google.com || exit /b 1

cd ..\..

