###############################################################################
# Functions
###############################################################################
# mkdir & cd
function mkcd() {
    mkdir -p "$1" && cd "$1"
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

function scroll-and-clear-screen() {
    local i=1
    while read; do ((i++)); done <<< $PS1
    printf '\n%.0s' {$i..$LINES}
    zle clear-screen
}
zle -N scroll-and-clear-screen
bindkey '^l' scroll-and-clear-screen


###############################################################################
# fzf
###############################################################################
if (( $+commands[fzf] )); then
    eval "$(fzf --zsh)"
elif [ -f ~/.fzf.zsh ]; then
    source ~/.fzf.zsh
fi

if (( $+commands[fzf] )); then
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
    # cd to selected parent directory
    function cdr() {
        local declare dirs=()
        get_parent_dirs() {
            if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
            if [[ "${1}" == '/' ]]; then
            for _dir in "${dirs[@]}"; do echo $_dir; done
            else
            get_parent_dirs $(dirname "$1")
            fi
        }
        local DIR=$(get_parent_dirs $(realpath "${1:-$PWD/../}") | fzf --reverse --preview "ls -lhF --group-directories-first {}")
        cd "$DIR"
    }
    function fcookie() {
        local dir
        dir=$(find ~/.cookiecutters ~/.cookiecutter_templates -follow -type d -maxdepth 1 -mindepth 1 2> /dev/null | sort | fzf +m --height 30% --reverse) && cookiecutter "$dir"
    }
    function search_oneliner() {
        local file cmd
        setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
        file=~/.oneliner
        cmd=$( cat $file | fzf --height 30% --reverse | perl -pe 's/^\[[^\]]+\]\s*//g' )
        local ret=$?
        # Exit if canceled
        if [[ -z $cmd ]]; then
            zle redisplay
            return $ret
        fi
        #
        if [ ${cmd:$((${#cmd}-1))} = "!" ]; then
            cmd=$(echo -E ${cmd} | sed 's/!$//')
            zle push-line
            BUFFER=${cmd}
            zle accept-line
            ret=$?
        else
            zle push-line
            BUFFER=${cmd}
            CURSOR+=${#cmd}
            ret=$?
        fi
        unset file cmd
        zle reset-prompt
        return $ret
    }
    zle -N search_oneliner
    bindkey "^s" search_oneliner
fi

