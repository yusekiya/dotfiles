# User dependent .bash_profile file

if [ -d "/mingw64/local/bin" ]; then
    export PATH="/mingw64/local/bin:${PATH}"
fi

if [ -d "/usr/local/opt/coreutils/libexec/gnubin" ]; then
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
fi
if [ -d "/usr/local/opt/coreutils/libexec/gnuman" ]; then
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

# include path
if [ -d "/usr/local/include" ] ; then
    export CPLUS_INCLUDE_PATH=/usr/local/include:${CPLUS_INCLUDE_PATH}
fi
# library path
if [ -d "/usr/local/lib" ] ; then
    export LIBRARY_PATH=/usr/local/lib:${LIBRARY_PATH}
fi

# Python
## for windows (anaconda)
if [ -d "${HOME}/opt/anaconda" ] && [ "$(uname -o)" = "Msys" ]; then
    export PATH="${HOME}/opt/anaconda:${HOME}/opt/anaconda/Scripts:${PATH}"
    export PATH="${PATH}:${HOME}/opt/anaconda/Library/bin"
fi

## for linux (anaconda)
if [ "$(uname)" = "Linux" -a -d "${HOME}/opt/anaconda3/bin" ]; then
    export PATH="/home/yseki/opt/anaconda3/bin:$PATH"
fi

## for mac (anaconda)
if [ -d "${HOME}/anaconda/bin" ]; then
    export PATH="${HOME}/anaconda/bin:${PATH}"
fi

# Add path for node
if [ -f "${HOME}/.nodebrew/nodebrew" ]; then
    export PATH=${HOME}/.nodebrew/current/bin:${PATH}
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
    export PATH="${HOME}/bin:${PATH}"
fi

# Set MANPATH so it includes users' private man if it exists
if [ -d "${HOME}/man" ]; then
  MANPATH="${HOME}/man:${MANPATH}"
fi

# Set INFOPATH so it includes users' private info if it exists
if [ -d "${HOME}/info" ]; then
  INFOPATH="${HOME}/info:${INFOPATH}"
fi

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

cd ~
