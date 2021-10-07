# anaconda
anaconda_base_list=("${HOME}"{,/opt}/{anaconda,Anaconda,miniconda}{,3,2})
for anaconda_base in "${anaconda_base_list[@]}"; do
    if [ -d "${anaconda_base}" ]; then
        export ANACONDA_BASE_DIR="${anaconda_base}"
        break
    fi
done
unset anaconda_base_list
if [ -n "${ANACONDA_BASE_DIR}" ]; then
    __conda_setup="$(${ANACONDA_BASE_DIR}/bin/conda 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    elif [ -f "${ANACONDA_BASE_DIR}/etc/profile.d/conda.sh" ]; then
        . "${ANACONDA_BASE_DIR}/etc/profile.d/conda.sh"
    else
        export PATH="${ANACONDA_BASE_DIR}"/bin:$PATH
    fi
fi
unset __conda_setup

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

if [ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin" ]; then
    export PATH=$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:${PATH}
fi
if [ -d "$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin" ]; then
    export PATH=$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin:${PATH}
fi
if [ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman" ]; then
    export MANPATH=$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman:${MANPATH}
fi

# Rust
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
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
elif [ -d "${HOME}/.poetry/bin" ]; then
    export PATH="${HOME}/.poetry/bin:${PATH}"
fi

# executable path
if [ -d "${HOME}/.local/bin" ]; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi

export ZELLIJ_CONFIG_DIR=~/.zellij

typeset -U PATH path
