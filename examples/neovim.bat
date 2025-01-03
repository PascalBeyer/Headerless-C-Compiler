
git clone --recursive --depth 1 --branch v0.10.3 https://github.com/neovim/neovim.git
cd neovim || exit /b 1

git checkout *

IF NOT EXIST .deps (
    :: We currently cannot compile the dependencies.
    :: So we use MSVC instead and only compile them once.
    cmake -S cmake.deps -B .deps -G Ninja || exit /b 1
    cmake --build .deps || exit /b 1
)

IF EXIST build (
   cmake --build build --target clean
) ELSE (
   cmake -B build -G Ninja -D CMAKE_C_COMPILER="%~dp0..\hlc.exe" || exit /b 1
)


echo #include ^<intrin.h^> // Their include is in the wrong spot. >> src/nvim/math.c

cmake --build build || exit /b 1

cd ..
