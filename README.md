# dotfiles
## Setup

- [Mac][1]
- [Windows (WSL2)][2]
- [Windows (MSYS2)][3]
- [Ubuntu][4]
- [Raspbian][5]

## Setup (dotfiles only)

To install the dotfiles, run the setup script `setup.py` as

```shell
$ ./setup.sh
```

This script creates symbolic links to the dotfiles of this repository
in your home directory.
When a dotfile already exists in your home directory,
the dotfile will be renamed with suffix `.bak`.


[1]: https://github.com/yusekiya/mac_setup#setup-of-mac
[2]: https://github.com/yusekiya/dotfiles/wiki#windows-wsl2
[3]: https://github.com/yusekiya/dotfiles/wiki#windows-msys2
[4]: https://github.com/yusekiya/ubuntu_setup#setup-ubuntu-1604-with-ansible
[5]: https://github.com/yusekiya/raspi3_setup#setup-raspi3-with-ansible
