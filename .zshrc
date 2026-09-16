# Prepare directory for local configuration
local_zsh_dir=$HOME/.config/zsh.site
if [ ! -d $local_zsh_dir ] || [ ! -f $local_zsh_dir/sync.zsh ] || [ ! -f $local_zsh_dir/defer.zsh ] || [ ! -f $local_zsh_dir/defer-after-compinit.zsh ]; then
    mkdir -p $local_zsh_dir
    mkdir -p $local_zsh_dir/completion
    touch $local_zsh_dir/{sync,defer,defer-after-compinit}.zsh
fi
unset local_zsh_dir

# Cache helpers. .zprofile loads the same file; whichever shell runs first wins.
(( $+functions[zcache_source] )) || source "${HOME}/.config/zsh/cache.zsh"

# Load zsh configuration with Sheldon
if (( $+commands[sheldon] )); then
    # The following config for sheldon is referencing to https://zenn.dev/fuzmare/articles/zsh-plugin-manager-cache
    # Prepare file names for caching
    sheldon_cache="$ZSH_CACHE_DIR/sheldon.zsh"
    sheldon_toml="$HOME/.config/sheldon/plugins.toml"
    # Create cache if necessary
    if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
        mkdir -p $ZSH_CACHE_DIR
        sheldon --config-file $sheldon_toml source > $sheldon_cache
    fi
    source "$sheldon_cache"
    unset sheldon_cache sheldon_toml
else
    echo "**WARNING** Sheldon command not found"
    echo "Install Sheldon from the following URL to load the zsh config."
    echo "https://github.com/rossmacarthur/sheldon"
fi

# Startup profiling: run `ZSH_PROFILE=1 zsh -i -c exit` to collect it.
if [[ -n ${ZSH_PROFILE-} ]]; then
    zprof > /tmp/zshstart.log
fi
