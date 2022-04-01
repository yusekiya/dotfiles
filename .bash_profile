# User dependent .bash_profile file

# Load .profile if it exists
if [ -f "${HOME}/.profile" ]; then
  source "${HOME}/.profile"
fi

# Load the users bashrc if it exists and is not loaded yet
if [[ -f "${HOME}/.bashrc" && -z "$__BASHRC_LOADED" ]]; then
  source "${HOME}/.bashrc"
fi

[[ ":$PATH:" != *":/usr/local/bin:"* ]] && export PATH="/usr/local/bin:${PATH}"
[[ ":$PATH:" != *":/usr/local/sbin:"* ]] && export PATH="/usr/local/sbin:${PATH}"

if [ -d "/usr/local/opt/coreutils/libexec/gnubin" ]; then
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
fi
if [ -d "/usr/local/opt/coreutils/libexec/gnuman" ]; then
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

# Linuxbrew
if [ -d "${HOME}/.linuxbrew" ]; then
    eval "$(${HOME}/.linuxbrew/bin/brew shellenv)"
elif [ -d "/home/linuxbrew/.linuxbrew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
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

# Load bash completion
export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

