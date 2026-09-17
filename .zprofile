# Cache helpers. .zshrc loads the same file; whichever shell runs first wins.
(( $+functions[zcache_source] )) || source "${HOME}/.config/zsh/cache.zsh"
(( $+functions[path_prioritize] )) || source "${HOME}/.config/zsh/path.zsh"

# `brew shellenv` is a ~25ms fork on every login shell, and its output only
# changes when brew itself is updated, so it is cached.
if [ -d "/opt/homebrew" ] && [ "$ARCH" = arm64 ]; then
    brew_bin=/opt/homebrew/bin/brew
elif [ -f "/usr/local/bin/brew" ] && [ "$ARCH" = x86_64 ]; then
    brew_bin=/usr/local/bin/brew
elif [ -d "${HOME}/.linuxbrew" ]; then
    # Linuxbrew
    brew_bin="${HOME}/.linuxbrew/bin/brew"
elif [ -d "/home/linuxbrew/.linuxbrew" ]; then
    brew_bin=/home/linuxbrew/.linuxbrew/bin/brew
fi
if [ -n "${brew_bin:-}" ]; then
    zcache_source brew "$brew_bin" -- "$brew_bin" shellenv
fi
unset brew_bin

if [ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin" ]; then
    export PATH=$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:${PATH}
fi
if [ -d "$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin" ]; then
    export PATH=$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin:${PATH}
fi
if [ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman" ]; then
    export MANPATH=$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman:${MANPATH}
fi

# Rust
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# latex path
if [ -d "/Library/TeX/texbin" ]; then
    export PATH="/Library/TeX/texbin:${PATH}"
fi

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
elif [ -e "${HOME}/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
fi

# home-manager keeps its own session variables and `home.sessionPath` here;
# without this they only apply to the shell home-manager generates itself.
if [ -e "${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
    . "${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi

# Config path for tealdeer
export TEALDEER_CONFIG_DIR="${HOME}/.config/tealdeer/"

# Every block above prepends to PATH, so the intended order is restored once
# they have all run.
path_prioritize

if [[ "$TERM_PROGRAM" == "WezTerm" && -f "$HOME"/.config/wezterm/wezterm.sh ]]; then
    source "$HOME"/.config/wezterm/wezterm.sh
fi

if [[ -d "/Applications/Obsidian.app/Contents/MacOS" ]]; then
    export PATH="$PATH:/Applications/Obsidian.app/Contents/MacOS"
fi

typeset -U PATH path
