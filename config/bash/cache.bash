# shellcheck shell=bash
###############################################################################
# Caches for slow shell initializers
###############################################################################
# The bash counterpart of config/zsh/cache.zsh. Several tools are configured by
# eval'ing the output of a subprocess (starship init, fzf --bash, ...), and on
# this machine one fork+exec costs 6-14ms -- a plain fork is 0.85ms, the rest is
# exec plus the PATH search over 25 directories. The output only changes when
# the tool or its own config file changes, so it is generated once and replayed
# from a cache.
#
# Unlike the zsh version, validating the cache must not cost a fork itself, or
# the check would be as expensive as the command it avoids. So no `stat` here:
#
#   -nt  catches an edited config file or a rebuilt binary (mtime moved).
#   -ef  catches a tool installed by Nix, where every file in the store carries
#        the same 1970 timestamp so -nt never fires. What does change is the
#        store path the ~/.nix-profile symlink resolves to, and therefore the
#        inode -- which is what -ef compares. The resolved path is recorded at
#        generation time, where a fork is affordable because it is rare.
#
# Both are bash builtins, so a warm start runs no external command at all.
#
# The cache is per machine and is never shared between hosts: it holds absolute
# paths and the output of host-local binaries.
#
# Besides the <dep> files named at the call site, the file *containing* the call
# and the generator binary itself are always implicit dependencies, so editing a
# generator command invalidates its cache by itself and there is never a cache
# to remember to delete by hand.

: "${BASH_CACHE_DIR:=${XDG_CACHE_HOME:-$HOME/.cache}/bash}"

# Full path of a command in $REPLY, without forking: `hash` is a builtin and it
# publishes what it found in BASH_CMDS (bash 4+). Empty when not found.
_bcache_path() {
    REPLY=
    if [[ -n ${BASH_CMDS+x} ]]; then
        hash -- "$1" 2>/dev/null && REPLY=${BASH_CMDS[$1]}
    else
        REPLY=$(type -P -- "$1" 2>/dev/null)
    fi
    [[ -n $REPLY ]]
}

# Resolve symlinks. Only ever called while (re)generating a cache, so the fork
# is affordable. Falls back to the path itself when readlink cannot do it, which
# degrades the -ef check to a tautology and leaves -nt doing the work.
_bcache_resolve() {
    REPLY=$1
    if [[ -L $1 ]]; then
        local target
        target=$(readlink -f -- "$1" 2>/dev/null) && [[ -n $target ]] && REPLY=$target
    fi
}

# Source $BASH_CACHE_DIR/<name>.bash, regenerating it by running <command>
# whenever the cache is missing, empty, or one of its dependencies has changed
# since it was built. A dep that does not exist is ignored.
#
#     bcache_source <name> [<dep> ...] -- <command> [<arg> ...]
#
# Returns non-zero without sourcing anything when the cache cannot be built.
bcache_source() {
    local name=$1
    shift

    local -a deps=()
    # The call site, so that editing the generator command invalidates it.
    [[ -n ${BASH_SOURCE[1]:-} && -e ${BASH_SOURCE[1]} ]] && deps+=("${BASH_SOURCE[1]}")
    while (($#)) && [[ $1 != -- ]]; do
        deps+=("$1")
        shift
    done
    shift

    (($#)) || return 1
    local REPLY
    # The generator binary. A shell function has no file to watch, so it is the
    # call site above that stands in for it.
    _bcache_path "$1" && deps+=("$REPLY")

    local cache=$BASH_CACHE_DIR/$name.bash
    local stamp=$cache.dep

    if [[ -s $cache && -r $stamp ]]; then
        local -a _bcache_stamp=()
        # A bash file holding assignments, so reading it is `source`, not a fork.
        source "$stamp"
        local i fresh=1
        if ((${#_bcache_stamp[@]} >= 2)); then
            for ((i = 0; i < ${#_bcache_stamp[@]}; i += 2)); do
                # Gone, moved to another store path, or touched since the build.
                if [[ ! -e ${_bcache_stamp[i]} ]] ||
                    [[ ! ${_bcache_stamp[i]} -ef ${_bcache_stamp[i + 1]} ]] ||
                    [[ ${_bcache_stamp[i]} -nt $cache ]]; then
                    fresh=
                    break
                fi
            done
        else
            fresh=
        fi
        if [[ -n $fresh ]]; then
            source "$cache"
            return
        fi
    fi

    [[ -d $BASH_CACHE_DIR ]] || mkdir -p "$BASH_CACHE_DIR" || return 1
    local tmp=$cache.$$
    if ! "$@" >"$tmp" 2>/dev/null || [[ ! -s $tmp ]]; then
        command rm -f "$tmp"
        return 1
    fi
    command mv -f "$tmp" "$cache"

    local dep
    {
        printf '_bcache_stamp=(\n'
        for dep in "${deps[@]}"; do
            [[ -e $dep ]] || continue
            _bcache_resolve "$dep"
            printf '  %q %q\n' "$dep" "$REPLY"
        done
        printf ')\n'
    } >"$stamp"

    source "$cache"
}
