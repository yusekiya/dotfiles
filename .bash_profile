# User dependent .bash_profile file

[[ ":$PATH:" != *":/usr/local/bin:"* ]] && export PATH="/usr/local/bin:${PATH}"

if [ -d "/mingw64/local/bin" ]; then
    export PATH="/mingw64/local/bin:${PATH}"
fi

if [ -d "/usr/local/opt/coreutils/libexec/gnubin" ]; then
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
fi
if [ -d "/usr/local/opt/coreutils/libexec/gnuman" ]; then
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

# Linuxbrew
if [ -d "${HOME}/.linuxbrew" ]; then
    export PATH=${HOME}/.linuxbrew/bin:${PATH}
fi

# Add path for node
if [ -d "${HOME}/.nodebrew" ]; then
    export PATH=${HOME}/.nodebrew/current/bin:${PATH}
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

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f "${HOME}/opt/google-cloud-sdk/path.bash.inc" ]; then source "${HOME}/opt/google-cloud-sdk/path.bash.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "${HOME}/opt/google-cloud-sdk/completion.bash.inc" ]; then source "${HOME}/opt/google-cloud-sdk/completion.bash.inc"; fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
anaconda_base_list=("${HOME}"{,/opt}/{anaconda,Anaconda,miniconda}{,3,2})
for anaconda_base in "${anaconda_base_list[@]}"; do
    if [ -d "${anaconda_base}" ]; then
        export ANACONDA_BASE_DIR="${anaconda_base}"
        break
    fi
done
unset anaconda_base_list
__conda_setup="$(${ANACONDA_BASE_DIR}/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "${ANACONDA_BASE_DIR}/etc/profile.d/conda.sh" ]; then
        . "${ANACONDA_BASE_DIR}/etc/profile.d/conda.sh"
    else
        export PATH="${ANACONDA_BASE_DIR}/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
