# dotfiles
## Setup for windows (MSYS2)
### Copy config files to home directory
- Install msys2 under the C: drive
- Uncomment `rem set MSYS=winsymlinks:nativestrict` in /{msys2,mingw32,mingw64}_shell.bat
  to enable to create symbolic link
- Run the any .bat script as administrator, and execute the following commands
  ```bash
  $ cd ~
  $ pacman -S git
  $ git clone https://github.com/yusekiya/dotfiles.git
  $ ./dotfiles/setup.sh
  ```

### Additional manual setup

- Install pacman packages
  ```bash
  $ pacman -S $(< ~/.pacman_pkglist)
  ```
- Make symbolic link to DROPBOX/HOME/*
- Copy DROPBOX/my_script/* to ~/bin
- Install aspell through graphical installer (aspell in MSYS2 repo is aborted when dealing with TeX file)  
  Install directory must be `C:\opt` to work with emacs
- Build packages under .pkgbuild
  ```bash
  $ cd ${package-name}
  $ makepkg
  $ pacman -U ${package-name}*.pkg.tar.xz
  ```
- Make ~/.gitconfig.local and [user] section in it
- Install anaconda to under ~/opt/anaconda
- Install conda packages
  ```bash
  $ conda install --file ~/.conda_pkglist_win
  ```
- Install the following packages through pip:
    - numdifftools
    - grip
    - epc
- Install emacs and compile elisps in ~/.emacs.d
- Install TeXlive
- Install 'gomi' command (if necessary)
  ```bash
  $ curl -L git.io/gomi | bash
  ```

## TODO

- [x] Check if the above procedure works
- [ ] Automate the manual setting
