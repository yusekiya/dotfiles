#!/usr/bin/env sh
CARCH="x86_64" MINGW_CHOST=x86_64-w64-mingw32 MINGW_PACKAGE_PREFIX=mingw-w64-x86_64 MINGW_PREFIX=/mingw64 makepkg -s --skippgpcheck
pacman -U *.pkg.tar.xz
find . -type f -name "config.log" | xargs -i cp {} .
rm -rf pkg src emacs* *.pkg.tar.xz
