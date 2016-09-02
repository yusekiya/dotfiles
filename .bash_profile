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
if [ "$(uname -o)" = "Msys" ] && [ -d "${HOME}/opt/anaconda" ]; then
    export PATH="${HOME}/opt/anaconda:${HOME}/opt/anaconda/Scripts:${PATH}"
    export PATH="${PATH}:${HOME}/opt/anaconda/Library/bin"
## for linux or mac (anaconda)
elif [ -d "${HOME}/opt/anaconda3/bin" ]; then
    export PATH="${HOME}/opt/anaconda3/bin:$PATH"
elif [ -d "${HOME}/opt/anaconda/bin" ]; then
    export PATH="${HOME}/opt/anaconda/bin:$PATH"
elif [ -d "${HOME}/anaconda3/bin" ]; then
    export PATH="${HOME}/anaconda3/bin:$PATH"
elif [ -d "${HOME}/anaconda/bin" ]; then
    export PATH="${HOME}/anaconda/bin:${PATH}"
## for linux or mac (miniconda)
elif [ -d "${HOME}/opt/miniconda3/bin" ]; then
    export PATH="${HOME}/opt/miniconda3/bin:$PATH"
elif [ -d "${HOME}/opt/miniconda/bin" ]; then
    export PATH="${HOME}/opt/miniconda/bin:$PATH"
elif [ -d "${HOME}/miniconda/bin" ]; then
    export PATH="${HOME}/miniconda/bin:${PATH}"
elif [ -d "${HOME}/miniconda3/bin" ]; then
    export PATH="${HOME}/miniconda3/bin:${PATH}"
fi

# Add path for node
if [ -d "${HOME}/.nodebrew" ]; then
    export PATH=${HOME}/.nodebrew/current/bin:${PATH}
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/usr/bin" ]; then
    export PATH="${HOME}/usr/bin:${PATH}"
fi
if [ -d "${HOME}/bin" ] ; then
    export PATH="${HOME}/bin:${PATH}"
fi

# Set MANPATH so it includes users' private man if it exists
if [ -d "${HOME}/man" ]; then
  export MANPATH="${HOME}/man:${MANPATH}"
fi
if [ -d "${HOME}/usr/share/man" ]; then
   export MANPATH="${HOME}/usr/share/man:${MANPATH}"
fi

# Set INFOPATH so it includes users' private info if it exists
if [ -d "${HOME}/info" ]; then
  INFOPATH="${HOME}/info:${INFOPATH}"
fi

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi
