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

[[ ":$PATH:" != *":/usr/local/opt/coreutils/libexec/gnubin:"* ]] \
    && export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"

[[ ":$PATH:" != *":/usr/local/opt/gnu-sed/libexec/gnubin:"* ]] \
    && export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"

[[ ":$MANPATH:" != *":/usr/local/opt/coreutils/libexec/gnuman:"* ]] \
    && export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${PATH}"

