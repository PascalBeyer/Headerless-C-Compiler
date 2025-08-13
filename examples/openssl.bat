@echo off

git clone --recursive --depth 1 --branch openssl-3.5.2 https://github.com/openssl/openssl.git
cd openssl || exit /b 1

git clean -xdf

set CC=%~dp0..\hlc.exe

echo #include ^<intrin.h^> >> include/internal/refcount.h
echo #include ^<intrin.h^> >> crypto/threads_win.c
mkdir installed

perl Configure VC-WIN64A no-makedepend  --prefix=%CD%\installed --openssldir=%CD%\installed || exit /b 1

nmake || exit /b 1
nmake test || exit /b 1
nmake install || exit /b 1

cd ..


