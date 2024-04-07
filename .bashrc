# User dependent .bashrc file

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Reset variables
export PROMPT_COMMAND=

# Environment variables
export EDITOR='vim'

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM=1
export GIT_PS1_SHOWUNTRACKEDFILES=
export GIT_PS1_SHOWSTASHSTATE=1

export LESS='-iRFX -# 5'
export SYSTEMD_LESS='FRSXMK -# 5'

# Load git completion
## for windows
if [ "$(uname -o)" = "Msys" ]; then
    source /usr/share/git/completion/git-prompt.sh
    source /usr/share/git/completion/git-completion.bash
    GIT_PS1_SHOWDIRTYSTATE=true
## for ubuntu
elif [ "$(uname)" = "Linux" ]; then
    if [ -f /etc/bash_completion.d/git-prompt ]; then
        source /etc/bash_completion.d/git-prompt
    elif [ -f /etc/bash_completion.d/git ]; then
        source /etc/bash_completion.d/git
    fi
    GIT_PS1_SHOWDIRTYSTATE=true
## for mac
elif [ "$(uname)" = "Darwin" ]; then
    source /usr/local/etc/bash_completion.d/git-prompt.sh
    source /usr/local/etc/bash_completion.d/git-completion.bash
    GIT_PS1_SHOWDIRTYSTATE=true
fi

## Shorten dirname
export PROMPT_DIRTRIM=4


# Shell Options
#
# See man bash for more options...
#
# Don't wait for job termination notification
# set -o notify
#
# Don't use ^D to exit
set -o ignoreeof
#
# Use case-insensitive filename globbing
# shopt -s nocaseglob
#
# Make bash append rather than overwrite the history on disk
shopt -s histappend
#
# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
# shopt -s cdspell
#
# Don't stop stty with ^s
stty stop undef

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
elif [ -f /usr/local/etc/bash_completion ]; then
    source /usr/local/etc/bash_completion
elif [ -f /usr/local/share/bash-completion/bash_completion ]; then
    source /usr/local/share/bash-completion/bash_completion
fi

# My completion
if type -p git-manage > /dev/null; then
    _git_manage()
    {
        local word="${COMP_WORDS[COMP_CWORD]}"
        if [ $COMP_CWORD -lt 3 ]; then
            COMPREPLY=($(compgen -W "add remove ls lookover pull" -- "$word"))
        fi
    }
    complete -F _git_manage git-manage
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
export PROMPT_COMMAND="history -a;${PROMPT_COMMAND}"

# Functions
#
if [ "$(uname)" = "Darwin" ]; then
    emacs_gui=$(brew --prefix)/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs
else
    emacs_gui=emacs
fi
function emacs () {
    emacsclient "$@" >& /dev/null || $(${emacs_gui} "$@")
}
function less_table () {
    column -t "$1" | sed '/^\s*#/ s/ \{1,\}/ /g' | less
}
function unlink_files () {
    for f in "$@"
    do
        unlink $f
    done
}
function show_path () {
    echo $PATH | tr ":" "\n"
}

function terminal_device_type() {
    tty | perl -pe 's|/dev/([^/0-9]+)/?.*|\1|'
}
TERM_TYPE=$(terminal_device_type)

function yy() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

# Detect session type
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    SESSION_TYPE=remote/ssh
fi

# PS1
## Red='\[\e[31m\]'
## Green='\[\e[32m\]'
## Yellow='\[\e[33m\]'
## Blue='\[\e[34m\]'
## Purple='\[\e[35m\]'
## Cyan='\[\e[36m\]'
## White='\[\e[37m\]'
## Reset='\[\e[0m\]'
case "$TERM" in
    "dumb")
        PS1="> "
        ;;
    xterm*|rxvt*|eterm*|screen*)
        if type -p __git_ps1; then
            # For remote
            if [[ $SESSION_TYPE = remote/ssh ]]; then
                export PS1='\[\e[32;1m\]\u@\[\e[0m\]\[\e[33;1m\]\h\[\e[0m\]: \[\e[34;1m\]\w\[\e[0m\]\[\e[35m\]$(__git_ps1)\[\e[0m\]'$'\n\$ '
            # For local
            else
                export PS1='\[\e[32;1m\]\u@\h\[\e[0m\]: \[\e[34;1m\]\w\[\e[0m\]\[\e[35m\]$(__git_ps1)\[\e[0m\]'$'\n\$ '
            fi
        else
            # For remote
            if [[ $SESSION_TYPE = remote/ssh ]]; then
                export PS1='\[\e[32;1m\]\u@\[\e[0m\]\[\e[33;1m\]\h\[\e[0m\]: \[\e[34;1m\]\w\[\e[0m\]'$'\n\$ '
            # For local
            else
                export PS1='\[\e[32;1m\]\u@\h\[\e[0m\]: \[\e[34;1m\]\w\[\e[0m\]'$'\n\$ '
            fi
        fi
        ;;
    *)
        PS1="> "
        ;;
esac

# Aliases
#
# Load different file for aliases
if [ -f "${HOME}/.bash_aliases" ]; then
  source "${HOME}/.bash_aliases"
fi
#
# Launch vanilla bash
alias bashq='env -i bash --norc --noprofile'
#
# Interactive operations
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias cdh='cd ~'
#
# Default to human readable figures
alias df='df -h'
alias du='du -h'
#
# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty --group-directories-first'    # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias lly='ls -l --time-style=long-iso'
# alias l='ls -CF'
#
# Applications
alias m='\less +F'
alias grep='grep --color'
alias tree='tree --dirsfirst -C'
alias pyman='python -m pydoc'
alias lesst='less_table'
alias tiga='tig --all'
alias gg='git graph'
alias gu='git remote update'
alias jn='jupyter notebook &> /dev/null &'
alias jl='jupyter lab &> /dev/null &'
alias v='vim -RM'
alias d='docker'
alias dcm='docker-compose'
alias p='poetry'
alias pls='pueue status'
alias pad='pueue add --'

# Enable completion for aliases
complete -F _docker d
complete -F _docker_compose dcm

function nbstrip-jq {
    FLAG_INPLACE=
    for ARG in "$@"; do
        case "$ARG" in
            '-i')
                FLAG_INPLACE=1
                shift
                ;;
            -*)
                echo "Invalid option $(echo $1 | sed 's/^-*//')"
                exit 1
                ;;
            *)
                SRC="$ARG"
                shift
                ;;
        esac
    done
    output=$(cat "$SRC" | jq --indent 1 \
        '(.cells[] | select(has("outputs"))
        | .outputs) = [] | (.cells[] | select(has("execution_count"))
        | .execution_count) = null')
    if [ -n "$FLAG_INPLACE" ]; then
        echo "${output}" > "${SRC}"
    else
        echo "${output}"
    fi
    unset ARG
    unset SRC
    unset FLAG_INPLACE
}

function nbstrip-all-cwd {
    for nbfile in *.ipynb; do
        nbstrip-jq -i $nbfile
    done
    unset nbfile
}

if [ `type -p colordiff` ]; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

# The alias for tmux doesn't work on windows msys2
if [ "$(uname)" = "Linux" ] && [ `type -p direnv` ]; then
    alias tmux='direnv exec / tmux'
fi

# aliases and functions for linux
if [ "$(uname)" = "Linux" ]; then
    alias emacs='XMODIFIERS=@im=none emacs'
fi

if [ "$(uname)" = "Darwin" ]; then
    alias airport=/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport
fi

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
    function tasklist () {
        command tasklist "$@" | nkf -wu
    }
    alias pshell='winpty powershell'
    alias ipython='winpty ipython'
    alias cmd='winpty cmd'
fi

# dircolors
if [ -f "${HOME}/.dircolors" ]; then
    eval `dircolors ${HOME}/.dircolors`
fi

# fzf
if [ -f ~/.fzf.bash ]; then
    source ~/.fzf.bash
fi
if [ `type -p fzf` ]; then
   export FZF_CTRL_R_OPTS="--reverse"
   # cd to selected directory including hidden ones
   function cdd() {
       local dir
       dir=$(find ${1:-.} -type d -maxdepth ${2:-1} 2> /dev/null | sort | fzf +m --height 30% --reverse) && cd "$dir"
   }
   # change directory to a directory in which target file exists
   function cdf() {
       local file
       local dir
       file=$(fzf --height 30% --reverse +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
   }
   function fcookie() {
       local dir
       dir=$(find ~/.cookiecutters ~/.cookiecutter_templates -follow -type d -maxdepth 1 -mindepth 1 2> /dev/null | sort | fzf +m --height 30% --reverse) && cookiecutter "$dir"
   }
   function _search_oneliner() {
       local file
       local cmd
       file=~/.oneliner
       cmd=$( cat $file | fzf --height 30% --reverse | perl -pe 's/^\[[^\]]+\]\s*//g' )
       # Exit if canceled
       [[ -z $cmd ]] && return
       #
       if [ ${cmd:$((${#cmd}-1))} = "!" ]; then
           cmd=$(echo ${cmd} | sed 's/!$//')
           eval ${cmd}
       else
           READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$cmd${READLINE_LINE:$READLINE_POINT}"
           READLINE_POINT=$(( READLINE_POINT + ${#cmd} ))
       fi
   }
   bind -x '"\C-s": "_search_oneliner"'
fi

# enhancd
if [ -f ~/repos/enhancd/init.sh ]; then
    source ~/repos/enhancd/init.sh
    export ENHANCD_FILTER="fzf +m --height 50% --reverse:peco:gof"
fi

# direnv
if [ `type -p direnv` ]; then
    eval "${PROMPT_COMMAND}"
    eval "$(direnv hook bash)"
fi

export PROMPT_COMMAND="printf '\n';$PROMPT_COMMAND"

# zoxide
if [ -x "$(command -v zoxide)" ]; then
    eval "$(zoxide init --cmd c bash)"
fi

# Load site-local config for shell if any
if [[ -f "${HOME}/.bashrc.site" ]]; then
  source "${HOME}/.bashrc.site"
fi

__BASHRC_LOADED=yes

