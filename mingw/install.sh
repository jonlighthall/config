pacman -Sy --disable-download-timeout --noconfirm
pacman --needed -S bash pacman msys2-runtime --disable-download-timeout --noconfirm
pacman -Su --disable-download-timeout --noconfirm
pacman -Syyu --disable-download-timeout --noconfirm
pacman -S mingw64/mingw-w64-x86_64-gcc-fortran mingw64/mingw-w64-x86_64-gcc --disable-download-timeout --noconfirm
pacman -S git --disable-download-timeout --noconfirm
pacman -S mingw-w64-x86_64-toolchain --disable-download-timeout --noconfirm
