[[ ":$PATH:" != *":/usr/local/bin:"* ]] && export PATH="/usr/local/bin:${PATH}"
[[ ":$PATH:" != *":/usr/local/sbin:"* ]] && export PATH="/usr/local/sbin:${PATH}"

#Rust
[[ ":$PATH:" != *":${HOME}/.cargo:"* ]] && export PATH="${HOME}/.cargo/bin:${PATH}"

# Linuxbrew
if [ -d "${HOME}/.linuxbrew" ]; then
    export PATH=${HOME}/.linuxbrew/bin:${PATH}
fi

# Add path for node
if [ -d "${HOME}/.nodebrew" ]; then
    export PATH=${HOME}/.nodebrew/current/bin:${PATH}
fi

# latex path
if [ -d "/Library/TeX/texbin" ]; then
    export PATH="/Library/TeX/texbin:${PATH}"
fi

# executable path
if [ -d "${HOME}/.local/bin" ]; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi

# include path
if [ -d "${HOME}/.local/include" ] ; then
    export CPLUS_INCLUDE_PATH=${HOME}/.local/include:${CPLUS_INCLUDE_PATH}
fi
# library path
if [ -d "${HOME}/.local/lib" ] ; then
    export LIBRARY_PATH=${HOME}/.local/lib:${LIBRARY_PATH}
fi

# MANPATH
if [ -d "${HOME}/.local/share/man" ]; then
   export MANPATH="${HOME}/.local/share/man:${MANPATH}"
fi

# Set INFOPATH so it includes users' private info if it exists
if [ -d "${HOME}/.local/info" ]; then
  INFOPATH="${HOME}/.local/info:${INFOPATH}"
fi

typeset -U path
