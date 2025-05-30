# Prepare directory for local configuration
local_zsh_dir=$HOME/.config/zsh.site
if [ ! -d $local_zsh_dir ] || [ ! -f $local_zsh_dir/sync.zsh ] || [ ! -f $local_zsh_dir/defer.zsh ] || [ ! -f $local_zsh_dir/defer-after-compinit.zsh ]; then
    mkdir -p $local_zsh_dir
    mkdir -p $local_zsh_dir/completion
    touch $local_zsh_dir/{sync,defer,defer-after-compinit}.zsh
fi
unset local_zsh_dir

# cf. https://zenn.dev/fuzmare/articles/zsh-source-zcompile-all
function source_zcompile {
    ensure_zcompiled $1
    builtin source $1
}
function ensure_zcompiled {
    local compiled="$1.zwc"
    if [[ ! -r "$compiled" || "$1" -nt "$compiled" ]]; then
        echo "Compiling $1"
        zcompile $1
    fi
}

# Load zsh configuration with Sheldon
if (( $+commands[sheldon] )); then
    # The following config for sheldon is referencing to https://zenn.dev/fuzmare/articles/zsh-plugin-manager-cache
    # Prepare file names for caching
    cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}
    sheldon_cache="$cache_dir/sheldon.zsh"
    sheldon_toml="$HOME/.config/sheldon/plugins.toml"
    # Create cache if necessary
    if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
        mkdir -p $cache_dir
        sheldon --config-file $sheldon_toml source > $sheldon_cache
    fi
    source "$sheldon_cache"
    unset cache_dir sheldon_cache sheldon_toml
else
    echo "**WARNING** Sheldon command not found"
    echo "Install Sheldon from the following URL to load the zsh config."
    echo "https://github.com/rossmacarthur/sheldon"
fi


# if (which zprof > /dev/null 2>&1) ;then
  # zprof | less
# fi
