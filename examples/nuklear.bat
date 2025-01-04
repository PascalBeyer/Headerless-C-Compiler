
git clone --recursive --depth 1 --branch 4.12.0 https://github.com/Immediate-Mode-UI/Nuklear.git

cd nuklear || exit /b 1
git clean -xdf || exit /b 1

cd demo\d3d11 || exit /b 1
echo hlc %%* > cl.bat || exit /b 1
call build.bat || exit /b 1

cd ..\..\..

