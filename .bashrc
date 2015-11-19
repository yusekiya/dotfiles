# User dependent .bashrc file

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Shell Options
#
# See man bash for more options...
#
# Don't wait for job termination notification
# set -o notify
#
# Don't use ^D to exit
# set -o ignoreeof
#
# Use case-insensitive filename globbing
# shopt -s nocaseglob
#
# Make bash append rather than overwrite the history on disk
# shopt -s histappend
#
# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
# shopt -s cdspell

# Completion options
#
# These completion tuning parameters change the default behavior of bash_completion:
#
# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1
#
# Define to avoid stripping description in --option=description of './configure --help'
# COMP_CONFIGURE_HINTS=1
#
# Define to avoid flattening internal contents of tar files
# COMP_TAR_INTERNAL_PATHS=1
#
# Load bash_completion if it exists
if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi

# History Options
#
# Don't put duplicate lines in the history.
# export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
#
# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit'
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls' # Ignore the ls command as well
#
# Whenever displaying the prompt, write the previous line to disk
# export PROMPT_COMMAND="history -a"

# Aliases
#
# Load different file for aliases
if [ -f "${HOME}/.bash_aliases" ]; then
  source "${HOME}/.bash_aliases"
fi
#
# Interactive operations
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
#
# Default to human readable figures
alias df='df -h'
alias du='du -h'
#
# Misc
alias less='less -X'
# alias whence='type -a'                        # where, of a sort
alias grep='grep -i --color'                     # show differences in colour
# alias egrep='egrep --color=auto'              # show differences in colour
# alias fgrep='fgrep --color=auto'              # show differences in colour
#
# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty --group-directories-first'    # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
# alias l='ls -CF'                              #

# applications
alias tree='tree --dirsfirst -C'
alias emacs='emacsclient'
alias pyman='python -m pydoc'
alias lesst='less_table'
function less_table () {
    column -t "$1" | sed '/^\s*#/ s/ \{1,\}/ /g' | less
}
alias tiga='tig --all'
alias gg='git graph'
alias gu='git remote update'

if [ `type -p colordiff` ]; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

# The alias for tmux doesn't work on windows msys2
if [ "$(uname)" = "Linux" ] && [ `type -p direnv` ]; then
    alias tmux='direnv exec / tmux'
fi

function unlink_files () {
    for f in "$@"
    do
        unlink $f
    done
}

function show_path () {
    echo $PATH | tr ":" "\n"
}

# aliases and functions for windows
if [ "$(uname -o)" = "Msys" ]; then
    function trash () {
        winpty gomi "$@"
    }
    alias inkscape='PYTHONPATH= inkscape'
    alias sumatrapdf='sumatrapdf -reuse-instance'
    # function to build cython
    function cython_build_mingw () {
        python $1 build_ext -i --compiler=mingw32 -DMS_WIN64
    }
    #alias ipconfig='winpty ipconfig'
    function ipconfig () {
        command ipconfig "$@" | nkf -w
    }
    # alias ping='winpty ping'
    function ping () {
        command ping "$@" | nkf -wu
    }
    # alias netstat='winpty netstat'
    function netstat () {
        command netstat "$@" | nkf -w
    }
    # alias netsh='winpty netsh'
    function netsh () {
        command netsh "$@" | nkf -wu
    }
    # alias cscript='winpty cscript'
    function cscript () {
        command cscript "$@" | nkf -wu
    }
    #alias tracert='winpty tracert'
    function tracert () {
        command tracert "$@" | nkf -wu
    }
    alias taskkill='winpty taskkill'
    #function taskkill () {
        #command taskkill "$@" | nkf -wu
    #}
    # alias tasklist='winpty tasklist'
    function tasklist () {
        command tasklist "$@" | nkf -wu
    }
    alias pshell='winpty powershell'
    alias ipython='winpty ipython'
    alias cmd='winpty cmd'
fi

# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
# umask 027
# Paranoid: neither group nor others have any perms:
# umask 077

# Functions
#
# Some people use a different file for functions
# if [ -f "${HOME}/.bash_functions" ]; then
#   source "${HOME}/.bash_functions"
# fi
#
# Some example functions:
#
# a) function settitle
# settitle () 
# { 
#   echo -ne "\e]2;$@\a\e]1;$@\a"; 
# }
# 
# b) function cd_func
# This function defines a 'cd' replacement function capable of keeping, 
# displaying and accessing history of visited directories, up to 10 entries.
# To use it, uncomment it, source this file and try 'cd --'.
# acd_func 1.0.5, 10-nov-2004
# Petar Marinov, http:/geocities.com/h2428, this is public domain
# cd_func ()
# {
#   local x2 the_new_dir adir index
#   local -i cnt
# 
#   if [[ $1 ==  "--" ]]; then
#     dirs -v
#     return 0
#   fi
# 
#   the_new_dir=$1
#   [[ -z $1 ]] && the_new_dir=$HOME
# 
#   if [[ ${the_new_dir:0:1} == '-' ]]; then
#     #
#     # Extract dir N from dirs
#     index=${the_new_dir:1}
#     [[ -z $index ]] && index=1
#     adir=$(dirs +$index)
#     [[ -z $adir ]] && return 1
#     the_new_dir=$adir
#   fi
# 
#   #
#   # '~' has to be substituted by ${HOME}
#   [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"
# 
#   #
#   # Now change to the new dir and add to the top of the stack
#   pushd "${the_new_dir}" > /dev/null
#   [[ $? -ne 0 ]] && return 1
#   the_new_dir=$(pwd)
# 
#   #
#   # Trim down everything beyond 11th entry
#   popd -n +11 2>/dev/null 1>/dev/null
# 
#   #
#   # Remove any other occurence of this dir, skipping the top of the stack
#   for ((cnt=1; cnt <= 10; cnt++)); do
#     x2=$(dirs +${cnt} 2>/dev/null)
#     [[ $? -ne 0 ]] && return 0
#     [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
#     if [[ "${x2}" == "${the_new_dir}" ]]; then
#       popd -n +$cnt 2>/dev/null 1>/dev/null
#       cnt=cnt-1
#     fi
#   done
# 
#   return 0
# }
# 
# alias cd=cd_func

# Environment variables
export EDITOR='vim'

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM=1
export GIT_PS1_SHOWUNTRACKEDFILES=
export GIT_PS1_SHOWSTASHSTATE=1

# Enable syntax highlight in less
if [ ! "$(uname -o)" = "Msys" ]\
    && [ `type -p pygmentize` ]\
    && [ `type -p lessfilter` ]; then
    export LESS='-R'
    export LESSOPEN='| lessfilter %s'
elif [ `type -p src-hilite-lesspipe.sh` ]; then
    export LESS='-R'
    export LESSOPEN='| src-hilite-lesspipe.sh %s'
fi

# PS1 setting
# for windows
if [ "$(uname -o)" = "Msys" ]; then
    source /usr/share/git/completion/git-prompt.sh
    source /usr/share/git/completion/git-completion.bash
    GIT_PS1_SHOWDIRTYSTATE=true
    export PS1='\[\033[33;1m\]\u@\h\[\e[0m\]: \[\033[35;1m\]\w$(__git_ps1)\[\e[0m\]'$'\n\$ '
fi

# for ubuntu
if [ "$(uname)" = "Linux" ]; then
    source /etc/bash_completion.d/git-prompt
    GIT_PS1_SHOWDIRTYSTATE=true
    export PS1='\[\033[33m\]\u@\h\[\e[0m\]: \[\033[35m\]\w$(__git_ps1)\[\e[0m\]'$'\n\$ '
fi

# Dircolors setting
if [ "$(uname)" = "Linux" ] && [ -f "${HOME}/.dircolors.256dark" ]; then
    eval `dircolors ${HOME}/.dircolors.256dark`
fi

# Menu completion
bind "C-j":menu-complete

# key bindings
stty stop undef

# direnv
if [ `type -p direnv` ]; then
    eval "$(direnv hook bash)"
fi
