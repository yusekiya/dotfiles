# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# ~/.bash_profile: executed by bash(1) for login shells.

# The copy in your home directory (~/.bash_profile) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benifitial to all, please feel free to send
# a patch to the msys2 mailing list.

# User dependent .bash_profile file

export PATH=/mingw64/bin:/usr/bin:${PATH}
# export PATH=/mingw64/bin:/usr/bin

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
  PATH="${HOME}/bin:${PATH}"
fi

# python path (anaconda)
# export PATH=${HOME}/.anaconda:${HOME}/.anaconda/Scripts:${PATH}

# include path
if [ -d "/usr/local/include" ] ; then
    export CPLUS_INCLUDE_PATH=/usr/local/include
fi
# library path
if [ -d "/usr/local/lib" ] ; then
    export LIBRARY_PATH=/usr/local/lib
fi

# environment variable for trash command
export TRASHLIST=~/.trashlist # Where trash list is written
export TRASHBOX=~/.Trash # Where trash will be moved in
                         # (.Trash is Mac's trash box)
export MAXTRASHBOXSIZE=2048 # Max trash box size in MB
                            # Used for clean up
export MAXTRASHSIZE=102 # Trashes larger than MAXTRASHBOXSIZE will be removed by 'rm' directly

# # pyenv
# export PYENV_ROOT="${HOME}/.pyenv"
# export PATH=${PYENV_ROOT}/bin:$PATH

# Python path
export PYTHONPATH=~/.anaconda/Lib/site-packages

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH="${HOME}/man:${MANPATH}"
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH="${HOME}/info:${INFOPATH}"
# fi

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

cd ~
