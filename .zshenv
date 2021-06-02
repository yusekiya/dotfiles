# zmodload zsh/zprof && zprof
ARCH=$(uname -m)

[[ ":$PATH:" != *":/usr/local/bin:"* ]] && export PATH="/usr/local/bin:${PATH}"
[[ ":$PATH:" != *":/usr/local/sbin:"* ]] && export PATH="/usr/local/sbin:${PATH}"

# Rust
if [ -d "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# Homebrew for Mac with Apple Silicon
if [ -d "/opt/homebrew" ] && [ "$ARCH" = arm64 ]; then
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
    export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
elif [ -f "/usr/local/bin/brew" ] && [ "$ARCH" = x86_64 ]; then
    export HOMEBREW_PREFIX="/usr/local";
fi

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

# poetry path
if [ -d "${HOME}/.local/etc/poetry/bin" ]; then
    export PATH="${HOME}/.local/etc/poetry/bin:${PATH}"
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
