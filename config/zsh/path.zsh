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
#   ~/.local/bin        hand-installed and per-machine scripts
#   Nix / home-manager  must beat Homebrew and the system tools
# Everything else keeps the relative order it already had. Tools that prepend
# later at runtime (mise shims, direnv) still win, which is what they are for.

function path_prioritize {
    local -a front
    local dir
    for dir in \
        "$HOME/.local/bin" \
        "$HOME/.nix-profile/bin" \
        "${XDG_STATE_HOME:-$HOME/.local/state}/nix/profile/bin" \
        "${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/home-manager/home-path/bin" \
        "/etc/profiles/per-user/${USER}/bin" \
        "/run/current-system/sw/bin" \
        "/nix/var/nix/profiles/default/bin"
    do
        [[ -d $dir ]] && front+=( $dir )
    done
    (( $#front )) || return 0

    # -U keeps the first occurrence of each entry, so prepending is also how an
    # entry that is already further down gets moved up. -g is required: a plain
    # typeset inside a function would shadow $path with a local one.
    typeset -gU path PATH
    path=( $front $path )
}
