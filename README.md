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
- Setup with dropbox (option)
  - Make symbolic link to DROPBOX/HOME/*
  - Copy DROPBOX/my_script/* to ~/bin
- Install aspell through graphical installer (aspell in MSYS2 repo is aborted when dealing with TeX file)  
  Install directory must be `C:\opt` to work with emacs
- Build packages under .pkgbuild by executing makepkg.sh in each package directory
- Setup for git
  - Modify permissions of git hooks under ~/.git_template/hooks if necessary

    ```bash
    $ chmod 755 ~/.git_template/hooks/*
    ```
  - Make ~/.gitconfig.local and describe git settings which is not made public
- Install tmux plugin manager

  ```bash
  $ git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  ```
- Setup [Source-highlight](http://www.gnu.org/software/src-highlite/source-highlight.html)
  on MSYS2 (mingw64)

  - Build and install

    ```bash
    cd ~/src
    git clone git://git.savannah.gnu.org/src-highlite.git
    cd src-highlight
    autoreconf -i
    mkdir build
    cd build
    # make sure that the directory /mingw64/local exists
    ../configure --with-boost-libdir=/mingw64/lib --prefix=/mingw64/local
    make
    make install
    ```

  - Apply solarized theme
    [source-highlight-solarized](https://github.com/jrunning/source-highlight-solarized)

    ```bash
    cd ~/src
    git clone https://github.com/jrunning/source-highlight-solarized.git
    cd source-highlight-solarized
    cp *-solarized.style /mingw64/local/share/source-highlight
    cd /mingw64/local/share/source-highlight
    mv esc.style esc.style.org
    mv esc-solarized.style esc.style
    ```
- Setup python development environment
  - Install [anaconda](https://www.continuum.io/downloads) to under ~/opt/anaconda
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
