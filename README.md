# dotfiles
## Setup

- [Windows (MSYS2)][1]
- [Mac][2]
- [Ubuntu][3]
- [Raspbian][4]

## Setup (dotfiles only)

To install the dotfiles, run the setup script `setup.py` as

```shell
$ ./setup.sh
```

This script creates symbolic links to the dotfiles of this repository
in your home directory.
When a dotfile already exists in your home directory,
the dotfile will be renamed with suffix `.bak`.


[1]: https://github.com/yusekiya/dotfiles/wiki#windows-msys2
[2]: https://github.com/yusekiya/mac_setup#setup-of-mac
[3]: https://github.com/yusekiya/ubuntu_setup#setup-ubuntu-1604-with-ansible
[4]: https://github.com/yusekiya/raspi3_setup#setup-raspi3-with-ansible
