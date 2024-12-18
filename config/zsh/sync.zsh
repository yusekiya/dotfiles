###############################################################################
# General
###############################################################################
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


###############################################################################
# Environment variables
###############################################################################
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
# Aliases
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


###############################################################################
# Prompt
###############################################################################
eval "$(starship init zsh)"


###############################################################################
# Suggestion
###############################################################################
ZSH_AUTOSUGGEST_STRATEGY=(completion)
export ZSH_AUTOSUGGEST_USE_ASYNC=true


###############################################################################
# Syntax highlighting
###############################################################################
(( ${+ZSH_HIGHLIGHT_STYLES} )) || typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]='none'
ZSH_HIGHLIGHT_STYLES[path_prefix]='none'
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=243"

