
git clone --recursive --depth 1 --branch 0.11.21 https://github.com/mackron/miniaudio.git
cd miniaudio || exit /b 1

git clean -xdf

cd examples || exit /b 1
hlc simple_playback_sine.c || exit /b 1

cd ..\..
