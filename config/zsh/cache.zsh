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

zmodload -F zsh/stat b:zstat

: ${ZSH_CACHE_DIR:=${XDG_CACHE_HOME:-$HOME/.cache}/zsh}
: ${ZSH_COMPDUMP:=$ZSH_CACHE_DIR/zcompdump}

# Identify the dependencies in $REPLY, one "<resolved path> <mtime>" per line,
# to be stored next to a cache and compared against on the next start.
#
# Comparing mtimes against the cache would be simpler but does not work for a
# tool installed by Nix: every file in the store carries the same 1970
# timestamp, so `-nt` never fires and the cache stays frozen across upgrades.
# What does change is the path, since the store path holds a hash of the build,
# and $dep is a symlink into it. For anything outside the store the path is
# stable and the mtime is what moves, so both are recorded.
function _zcache_stamp {
    local dep target
    local -a st lines
    for dep in "$@"; do
        target=${dep:A}
        [[ -e $target ]] || continue
        zstat -A st +mtime -- $target 2>/dev/null || continue
        lines+=( "$target $st[1]" )
    done
    # Joined without a trailing newline: $(<file) strips one, so a stamp built
    # with one would never compare equal to what was written.
    REPLY=${(F)lines}
}

# Source $ZSH_CACHE_DIR/<name>.zsh, regenerating it by running <command>
# whenever the cache is missing, empty, or one of the <dep> files has changed
# since it was built. A dep that does not exist is ignored.
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
    local stamp=$cache.dep
    local REPLY
    _zcache_stamp $deps
    [[ -s $cache && -r $stamp && $REPLY == "$(<$stamp)" ]] && { builtin source $cache; return }

    [[ -d $ZSH_CACHE_DIR ]] || mkdir -p $ZSH_CACHE_DIR
    local tmp=$cache.$$
    if ! "$@" > $tmp 2>/dev/null || [[ ! -s $tmp ]]; then
        command rm -f $tmp
        return 1
    fi
    command mv -f $tmp $cache
    print -rn -- $REPLY > $stamp

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
    # The stamp is kept out of $dir: that directory is on $fpath and compinit
    # reads every file in it looking for a #compdef tag.
    local stamp=$ZSH_CACHE_DIR/completion.dep/$name
    local REPLY
    _zcache_stamp $deps
    [[ -s $target && -r $stamp && $REPLY == "$(<$stamp)" ]] && return 0

    [[ -d $dir ]] || mkdir -p $dir
    local tmp=$target.$$
    if ! "$@" > $tmp 2>/dev/null || [[ ! -s $tmp ]]; then
        command rm -f $tmp
        return 1
    fi
    command mv -f $tmp $target
    [[ -d ${stamp:h} ]] || mkdir -p ${stamp:h}
    print -rn -- $REPLY > $stamp
    command rm -f $ZSH_COMPDUMP $ZSH_COMPDUMP.zwc
}

# compinit is slow mostly because of the security check it runs over every
# directory in $fpath. Run the full check at most once a day and replay the
# dump otherwise, and keep the dump byte-compiled.
#
# -i answers the question compinit would otherwise stop and ask when compaudit
# rejects a directory in $fpath, so it cannot block while running as a
# zsh-defer task. The directory is then dropped from $fpath, which is fine
# until it is the one holding zsh's own completion functions -- with zsh from
# Homebrew it is -- and compinit dies with "compdump: function definition file
# not found". That error is fatal and would abort the rest of the deferred
# queue, optional-tools.zsh included, so it is contained in an eval: the worst
# case is completion that does not work, not a shell that never finishes
# loading its config. Run `compaudit` and fix the permission when that happens.
function zcache_compinit {
    [[ -d $ZSH_CACHE_DIR ]] || mkdir -p $ZSH_CACHE_DIR
    autoload -Uz compinit
    # The glob qualifier has to be matched in a globbing context: inside [[ ]]
    # the word is not subject to filename generation, so `[[ -n $dump(N.mh-24) ]]`
    # silently tests a literal string and is always true.
    local -a fresh=( $ZSH_COMPDUMP(N.mh-24) )
    if (( $#fresh )); then
        eval 'compinit -C -d $ZSH_COMPDUMP'
    else
        eval 'compinit -i -d $ZSH_COMPDUMP'
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
