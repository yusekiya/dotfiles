###############################################################################
# PATH priority
###############################################################################
# The front of PATH is settled here rather than by prepending at each site.
# Prepending alone is not enough on macOS: /etc/zprofile runs path_helper,
# which rebuilds PATH with the /etc/paths entries first, and /etc/zshrc (the
# stock one, or the one nix-darwin installs) runs after ~/.zprofile. Both can
# push entries back down after they were put in front.
#
# The order below is the intended one, highest priority first:
#   /run/wrappers/bin   NixOS setuid wrappers (sudo etc.); the copies under
#                       /run/current-system/sw/bin lack setuid and must not win
#   ~/.local/bin        hand-installed and per-machine scripts
#   Nix / home-manager  must beat Homebrew and the system tools
# Everything else keeps the relative order it already had. Tools that prepend
# later at runtime (mise shims, direnv) still win, which is what they are for.

# The Nix profiles present on this machine, highest priority first. PATH and
# fpath (see sync.zsh) are both derived from these, so the list lives once.
typeset -ga nix_profiles
nix_profiles=()
for _dir in \
    "$HOME/.nix-profile" \
    "${XDG_STATE_HOME:-$HOME/.local/state}/nix/profile" \
    "${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/home-manager/home-path" \
    "/etc/profiles/per-user/${USER}" \
    "/run/current-system/sw" \
    "/nix/var/nix/profiles/default"
do
    [[ -d $_dir ]] && nix_profiles+=( $_dir )
done
unset _dir

function path_prioritize {
    local -a front
    local dir
    for dir in /run/wrappers/bin "$HOME/.local/bin" ${^nix_profiles}/bin; do
        [[ -d $dir ]] && front+=( $dir )
    done
    (( $#front )) || return 0

    # -U keeps the first occurrence of each entry, so prepending is also how an
    # entry that is already further down gets moved up. -g is required: a plain
    # typeset inside a function would shadow $path with a local one.
    typeset -gU path PATH
    path=( $front $path )
}
