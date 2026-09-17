###############################################################################
# Caches for slow shell initializers
###############################################################################
# Several tools are configured by eval'ing the output of a subprocess
# (brew shellenv, starship init, direnv hook, ...). Each of those is a fork and
# they dominate the startup time. The output only changes when the tool or its
# own config file changes, so it is generated once and replayed from a cache.
#
# The cache is per machine and is never shared between hosts: it holds absolute
# paths and the output of host-local binaries.
#
# Besides the <dep> files named at the call site, the file *containing* the call
# is always an implicit dependency, so editing a generator command invalidates
# its cache by itself and there is never a cache to remember to delete by hand.
#
# This file is sourced from both .zprofile and .zshrc, whichever runs first.

: ${ZSH_CACHE_DIR:=${XDG_CACHE_HOME:-$HOME/.cache}/zsh}
: ${ZSH_COMPDUMP:=$ZSH_CACHE_DIR/zcompdump}

# Source $ZSH_CACHE_DIR/<name>.zsh, regenerating it by running <command>
# whenever the cache is missing, empty, or older than one of the <dep> files or
# the file this was called from. A dep that does not exist is ignored.
#
#     zcache_source <name> [<dep> ...] -- <command> [<arg> ...]
#
# Returns non-zero without sourcing anything when the cache cannot be built.
function zcache_source {
    local name=$1
    # funcfiletrace names the call site as "file:line", or something that is not
    # a path ("zsh") when called from an interactive prompt rather than a file.
    local caller=${funcfiletrace[1]%:*}
    local -a deps
    [[ $caller == /* ]] && deps=( $caller )
    shift
    while (( $# )) && [[ $1 != -- ]]; do
        deps+=( $1 )
        shift
    done
    shift

    local cache=$ZSH_CACHE_DIR/$name.zsh
    local stale=1 dep
    if [[ -s $cache ]]; then
        stale=0
        for dep in $deps; do
            if [[ -e $dep && $dep -nt $cache ]]; then
                stale=1
                break
            fi
        done
    fi

    if (( stale )); then
        [[ -d $ZSH_CACHE_DIR ]] || mkdir -p $ZSH_CACHE_DIR
        local tmp=$cache.$$
        if ! "$@" > $tmp 2>/dev/null || [[ ! -s $tmp ]]; then
            command rm -f $tmp
            return 1
        fi
        command mv -f $tmp $cache
    fi

    builtin source $cache
}

# Write a completion function into $ZSH_CACHE_DIR/completion/<name> so that
# compinit autoloads it on demand, instead of the shell parsing the generator's
# output on every start. Regenerated on the same conditions as zcache_source.
#
#     zcache_completion <name> [<dep> ...] -- <command> [<arg> ...]
#
# Nothing is sourced here; the compdump is dropped so that the next compinit
# picks the new file up.
function zcache_completion {
    local name=$1
    local caller=${funcfiletrace[1]%:*}
    local -a deps
    [[ $caller == /* ]] && deps=( $caller )
    shift
    while (( $# )) && [[ $1 != -- ]]; do
        deps+=( $1 )
        shift
    done
    shift

    local dir=$ZSH_CACHE_DIR/completion
    local target=$dir/$name
    local stale=1 dep
    if [[ -s $target ]]; then
        stale=0
        for dep in $deps; do
            if [[ -e $dep && $dep -nt $target ]]; then
                stale=1
                break
            fi
        done
    fi
    (( stale )) || return 0

    [[ -d $dir ]] || mkdir -p $dir
    local tmp=$target.$$
    if ! "$@" > $tmp 2>/dev/null || [[ ! -s $tmp ]]; then
        command rm -f $tmp
        return 1
    fi
    command mv -f $tmp $target
    command rm -f $ZSH_COMPDUMP $ZSH_COMPDUMP.zwc
}

# compinit is slow mostly because of the security check it runs over every
# directory in $fpath. Run the full check at most once a day and replay the
# dump otherwise, and keep the dump byte-compiled.
#
# -i is not an optimisation, it is a safety catch. Without it a single $fpath
# directory that compaudit dislikes makes compinit stop and ask, and this runs
# as a zsh-defer task: the prompt aborts compinit, no dump is written, and
# every task queued behind it -- optional-tools.zsh among them -- is dropped.
# The next shell then finds no dump, runs the full check again and wedges the
# same way. With -i such a directory is skipped instead; run `compaudit` when a
# completion is unexpectedly missing.
function zcache_compinit {
    [[ -d $ZSH_CACHE_DIR ]] || mkdir -p $ZSH_CACHE_DIR
    autoload -Uz compinit
    # The glob qualifier has to be matched in a globbing context: inside [[ ]]
    # the word is not subject to filename generation, so `[[ -n $dump(N.mh-24) ]]`
    # silently tests a literal string and is always true.
    local -a fresh=( $ZSH_COMPDUMP(N.mh-24) )
    if (( $#fresh )); then
        compinit -C -d $ZSH_COMPDUMP
    else
        compinit -i -d $ZSH_COMPDUMP
        [[ -s $ZSH_COMPDUMP ]] && zcompile $ZSH_COMPDUMP
    fi
}

# cf. https://zenn.dev/fuzmare/articles/zsh-source-zcompile-all
# Only worth it for files large enough for parsing to show up; the .zwc is
# written next to the source, which must therefore not live in the shared
# dotfiles tree.
function source_zcompile {
    local compiled="$1.zwc"
    if [[ ! -r "$compiled" || "$1" -nt "$compiled" ]]; then
        zcompile $1
    fi
    builtin source $1
}
