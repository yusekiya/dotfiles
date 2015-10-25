#!/usr/bin/env sh
makepkg
pacman -U *.pkg.tar.xz
rm -rf pkg src nkf* *.pkg.tar.xz
