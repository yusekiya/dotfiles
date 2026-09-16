###############################################################################
# Optional tools
###############################################################################
# Activation of third-party tools -- their own init, hook or completion --
# each guarded by a command check so that a machine without the tool skips it.
# These get swapped out often, which is why they sit apart from the config the
# shell actually depends on. A function that merely *uses* a tool is not
# activation and belongs in defer.zsh.
#
# Sourced after compinit: zoxide registers its completion through compdef and
# silently skips it when compinit has not run yet. The cost is that a
# completion generated here only takes effect in the next shell, which happens
# once per tool upgrade.

# Keep installers from writing into the shell config themselves.
export INSTALLER_NO_MODIFY_PATH=1

if (( $+commands[direnv] )); then
    zcache_source direnv $commands[direnv] -- direnv hook zsh
fi

if (( $+commands[mise] )); then
    zcache_completion _mise $commands[mise] -- mise completion zsh
    # Not cached: `mise activate` bakes the current PATH into its output and
    # forks `mise hook-env` while being sourced, so replaying a cached copy
    # would be both stale and no faster.
    eval "$($commands[mise] activate zsh)"
fi

if (( $+commands[uv] )); then
    # uv ships a ~7000 line completion script. Installed as an autoloaded
    # function instead, so no shell ever parses it at startup.
    zcache_completion _uv $commands[uv] -- uv generate-shell-completion zsh
fi

if (( $+commands[wezterm] )); then
    zcache_completion _wezterm $commands[wezterm] -- wezterm shell-completion --shell zsh
fi

if (( $+commands[zoxide] )); then
    export _ZO_FZF_OPTS="+m --height 50% --reverse"
    zcache_source zoxide $commands[zoxide] -- zoxide init --cmd c zsh
    alias cd=c
fi

# Generating a completion above drops the compdump, and this file is sourced
# after compinit, so rebuild it here rather than leaving the new completion
# unusable until the next shell.
[[ -f $ZSH_COMPDUMP ]] || zcache_compinit
