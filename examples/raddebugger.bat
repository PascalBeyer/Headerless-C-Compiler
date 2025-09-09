:: @echo off

git clone https://github.com/EpicGamesExt/raddebugger.git --branch v0.9.21-alpha --recursive --depth 1
cd raddebugger || exit /b 1

git checkout *
git clean -xdf 

mkdir build

:: Trick the build script into using hlc instead of cl
echo hlc %%* > build\cl.bat || exit /b 1

:: Generate a manifest file
echo int main(){} > arst.c || exit /b 1

cl arst.c /link /manifestdependency:"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'" || exit /b 1
move arst.exe.manifest build\raddbg.exe.manifest || exit /b 1

:: We are ready to build, build all of it!
:: radlink does not build yet
call build raddbg || exit /b 1

cd ..

