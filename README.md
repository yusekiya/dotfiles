# dotfiles
## Setup
### Copy config files to home directory
(Windows only) Install msys2 before running the following script.

```bash
$ cd ~
$ git clone https://github.com/yusekiya/dotfiles.git
$ ./dotfiles/setup.sh
```
### Additional manual settings

- (Windows msys2) Install pacman packages

  ```bash
  $ pacman -S $(< ~/.pacman_pkglist)
  ```
- Make symbolic link to DROPBOX/HOME/*
- Copy DROPBOX/my_script/* to ~/bin
- (Windows msys2) Build packages under .pkgbuild

  ```bash
  $ cd ${package-name}
  $ makepkg
  $ pacman -U ${package-name}*.pkg.tar.xz
  ```
- Make ~/.gitconfig.local and [user] section in it
- Install anaconda to under ~/opt/anaconda
- Install conda packages

  ```bash
  $ conda install --file ~/.conda_pkglist
  ```
- Install the following packages through pip:
    - numdifftools
    - grip
    - epc
- Install emacs and compile elisps in ~/.emacs.d
- Install TeXlive
- Install 'gomi' command (if necessary)

  ```bash
  $ curl -L git.io/gomi | sh
  ```

## TODO

- [x] Check if the above procedure works
- [ ] Automate the manual setting
