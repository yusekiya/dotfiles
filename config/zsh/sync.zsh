HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
if [ "$SSH_TTY" != "" ]; then
        stty stop undef
fi
setopt noflowcontrol
setopt IGNOREEOF
unsetopt beep
bindkey -e
bindkey "^[[Z" reverse-menu-complete
zstyle :compinstall filename "${HOME}/.zshrc"

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
if ([ -f ~/.dircolors ] && (( $+commands[dircolors] ))); then
  eval `dircolors ~/.dircolors`
  zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
fi

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

export LESS='-iRFX -# 5'
export SYSTEMD_LESS='FRSXMK -# 5'
if (( $+commands[vim] )); then
    export EDITOR=vim
elif (( $+commands[emacs] )); then
    export EDITOR=emacs
fi

###############################################################################
# Functions & aliases
###############################################################################
# Interactive operations
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias cdh='cd ~'

# Default to human readable figures
alias df='df -h'
alias du='du -h'

# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty --group-directories-first'    # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias llt='ls -lt --time-style=long-iso'
# alias l='ls -CF'

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
alias k='kubectl'
alias pls='pueue status'
alias pad='pueue add --'

if (( $+commands[colordiff] )); then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

# The alias for tmux doesn't work on windows msys2
if ([ "$(uname)" = "Linux" ] && (( $+commands[direnv] ))); then
    alias tmux='direnv exec / tmux'
fi

# aliases and functions for linux
if [ "$(uname)" = "Linux" ]; then
    alias emacs='XMODIFIERS=@im=none emacs'
fi

if [ "$(uname)" = "Darwin" ]; then
    alias airport=/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport
    alias x86term='/usr/bin/env PATH=$(getconf PATH) /usr/bin/arch -x86_64 /bin/zsh -l'
fi


function scroll-and-clear-screen() {
  local i=1
  while read; do ((i++)); done <<< $PS1
  printf '\n%.0s' {$i..$LINES}
  zle clear-screen
}
zle -N scroll-and-clear-screen
bindkey '^l' scroll-and-clear-screen

###############################################################################
# Packages
###############################################################################

local_zsh_dir=$HOME/.config/zsh.site
mkdir -p $local_zsh_dir
touch $local_zsh_dir/{sync,defer,defer-after-compinit}.zsh
unset local_zsh_dir

export _ZO_FZF_OPTS="+m --height 50% --reverse"

# Configure packages
zstyle ':prompt:pure:prompt:continuation' color 244
zstyle ':prompt:pure:virtualenv'          color 244
zstyle ':prompt:pure:user'                color '#EBCB8B'
zstyle ':prompt:pure:host'                color '#EBCB8B'
zstyle ':prompt:pure:git:branch'          color 244
zstyle ':prompt:pure:git:action'          color 244

ZSH_AUTOSUGGEST_STRATEGY=(completion)
export ZSH_AUTOSUGGEST_USE_ASYNC=true

(( ${+ZSH_HIGHLIGHT_STYLES} )) || typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]='none'
ZSH_HIGHLIGHT_STYLES[path_prefix]='none'

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=243"
