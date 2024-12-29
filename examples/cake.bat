git clone --recursive --depth 1 https://github.com/thradams/cake.git
cd cake\src || exit /b 1
git clean -xdf
git checkout *

hlc build.c || exit /b 1
build.exe || exit /b 1

cd ..\..

