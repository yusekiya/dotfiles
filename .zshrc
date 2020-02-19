HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt noflowcontrol
unsetopt beep
bindkey -e
zstyle :compinstall filename "${HOME}/.zshrc"
autoload -Uz compinit
# Don't call compinit here because it will be called when loading zplug
# compinit

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
if [[ -f ~/.dircolors && -x `which dircolors` ]]; then
  eval `dircolors ~/.dircolors`
  zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
fi

export LESS='-iRFX -# 5'
export SYSTEMD_LESS='FRSXMK -# 5'

function less_table () {
    column -t "$1" | sed '/^\s*#/ s/ \{1,\}/ /g' | less
}
function unlink_files () {
    for f in "$@"
    do
        unlink $f
    done
}

function terminal_device_type() {
    tty | perl -pe 's|/dev/([^/0-9]+)/?.*|\1|'
}
TERM_TYPE=$(terminal_device_type)

# Detect session type
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    SESSION_TYPE=remote/ssh
fi

function tab-reset() {
    echo -ne "\033]6;1;bg;*;default\a"
}
function change_tab_title() {
    local host_name=$(hostname | sed "s/\.local$//")
    local user_name=$(whoami)
    if [[ $SESSION_TYPE = remote/ssh ]]; then
        echo -ne "\033]1;$(whoami)@${host_name}\007"
    else
        echo -ne "\033]1;$(whoami)@${host_name}\007"
    fi
}

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
# Misc
alias m='\less +F'
alias grep='grep --color'
#
# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty --group-directories-first'    # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias lly='ls -l --time-style=long-iso'
# alias l='ls -CF'

# Applications
alias tree='tree --dirsfirst -C'
alias pyman='python -m pydoc'
alias lesst='less_table'
alias tiga='tig --all'
alias gg='git graph'
alias gu='git remote update'
alias jn='jupyter notebook &> /dev/null &'
alias jl='jupyter lab &> /dev/null &'
alias v='view -M'
alias d='docker'
alias dcm='docker-compose'

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

if [[ -x `which colordiff` ]]; then
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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
if [[ -x `which fzf` ]]; then
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
    function search_oneliner() {
        local file cmd
        setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
        file=~/.oneliner
        cmd=$( cat $file | fzf --height 30% --reverse | perl -pe 's/^\[[^\]]+\]\s*//g' )
        local ret=$?
        # Exit if canceled
        if [[ -z $cmd ]]; then
            zle fzf-redraw-prompt
            return $ret
        fi
        #
        if [ ${cmd:$((${#cmd}-1))} = "!" ]; then
            cmd=$(echo -E ${cmd} | sed 's/!$//')
            echo -E "!${cmd}"
            eval ${cmd}
            zle fzf-redraw-prompt
        else
            BUFFER=${cmd}
            CURSOR+=${#cmd}
        fi
        return $ret
    }
    zle -N search_oneliner
    bindkey "^s" search_oneliner
fi

# direnv
if [[ -x`which direnv` ]]; then
    eval "$(direnv hook zsh)"
fi

source ~/.zplug/init.zsh
zplug "b4b4r07/enhancd", use:init.sh
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"

# if ! zplug check --verbose; then
    # printf "Install? [y/N]: "
    # if read -q; then
        # echo; zplug install
    # fi
# fi

export ENHANCD_FILTER="fzf +m --height 50% --reverse:peco:gof"

zplug load

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# if (which zprof > /dev/null 2>&1) ;then
  # zprof | less
# fi
