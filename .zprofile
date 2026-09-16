# Cache helpers. .zshrc loads the same file; whichever shell runs first wins.
(( $+functions[zcache_source] )) || source "${HOME}/.config/zsh/cache.zsh"

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
fi

# Config path for tealdeer
export TEALDEER_CONFIG_DIR="${HOME}/.config/tealdeer/"

# add path to raise priority of ~/.local/bin
if [ -d "${HOME}/.local/bin" ]; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi

if [[ "$TERM_PROGRAM" == "WezTerm" && -f "$HOME"/.config/wezterm/wezterm.sh ]]; then
    source "$HOME"/.config/wezterm/wezterm.sh
fi

if [[ -d "/Applications/Obsidian.app/Contents/MacOS" ]]; then
    export PATH="$PATH:/Applications/Obsidian.app/Contents/MacOS"
fi

typeset -U PATH path
