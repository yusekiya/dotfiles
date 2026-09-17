HOSTNAME=$HOST

emit_osc7() {
    printf '\033]7;file://%s%s\033\\' "$HOSTNAME" "$PWD"
}

emit_osc7

autoload -Uz add-zsh-hook
add-zsh-hook -Uz precmd emit_osc7

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
setopt interactive_comments
unsetopt beep
bindkey -e
bindkey "^[[Z" reverse-menu-complete
zstyle :compinstall filename "${HOME}/.zshrc"

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
if ([ -f ~/.dircolors ] && (( $+commands[dircolors] ))); then
  zcache_source dircolors ~/.dircolors $commands[dircolors] -- dircolors ~/.dircolors
  zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
fi


###############################################################################
# Environment variables
###############################################################################
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

# Completions shipped by Nix packages. Their real directories live in the
# store, owned by root, so compinit accepts them where a Homebrew prefix
# installed by another account gets rejected. $nix_profiles comes from path.zsh.
fpath=(
    "$ZSH_CACHE_DIR/completion"
    "${HOME}/.config/zsh.site/completion"
    ${^nix_profiles}/share/zsh/site-functions(N/)
    "${fpath[@]}"
)
typeset -U FPATH fpath

export LESS='-iRFX -# 5'
export SYSTEMD_LESS='FRSXMK -# 5'
if (( $+commands[nvim] )); then
    export EDITOR=nvim
    export SUDO_EDITOR=$(which nvim)
elif (( $+commands[vim] )); then
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
alias d='docker'
alias dcm='docker-compose'
alias k='kubectl'
alias pls='pueue status'
alias pad='pueue add --'
alias lg='lazygit'
if [ $EDITOR = "vim" ]; then
    alias v='vim -RM'
elif [ $EDITOR = "nvim" ]; then
    alias v='nvim -RM'
fi

if (( $+commands[colordiff] )); then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi


###############################################################################
# Prompt
###############################################################################
if (( $+commands[starship] )); then
    # `starship init zsh` forks twice: once for the init script itself, and
    # once more for the continuation prompt it embeds. Both are cached, with
    # the continuation prompt baked in as a literal.
    function _starship_init_zsh {
        starship init zsh | command grep -v '^PROMPT2='
        printf 'PROMPT2=%q\n' "$(starship prompt --continuation)"
    }
    zcache_source starship $commands[starship] ${STARSHIP_CONFIG:-$HOME/.config/starship.toml} -- _starship_init_zsh
    unfunction _starship_init_zsh
fi


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

