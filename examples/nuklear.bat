
git clone --recursive --depth 1 --branch 4.12.0 https://github.com/Immediate-Mode-UI/Nuklear.git

cd nuklear
rem git clean -xdf

cd demo\d3d11
echo hlc %%* > cl.bat
build.bat

cd ..\..\..

