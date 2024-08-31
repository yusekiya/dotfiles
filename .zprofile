if [ -d "/opt/homebrew" ] && [ "$ARCH" = arm64 ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -f "/usr/local/bin/brew" ] && [ "$ARCH" = x86_64 ]; then
    eval "$(/usr/local/bin/brew shellenv)"
elif [ -d "${HOME}/.linuxbrew" ]; then
    # Linuxbrew
    eval "$(${HOME}/.linuxbrew/bin/brew shellenv)"
elif [ -d "/home/linuxbrew/.linuxbrew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

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

# Config path for tealdeer
export TEALDEER_CONFIG_DIR="${HOME}/.config/tealdeer/"

# add path to raise priority of ~/.local/bin
if [ -d "${HOME}/.local/bin" ]; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi

fpath=( "${HOME}/.config/zsh.site/completion" "${fpath[@]}" )

if [[ "$TERM_PROGRAM" == "WezTerm" && -f "$HOME"/.config/wezterm/wezterm.sh ]]; then
    source "$HOME"/.config/wezterm/wezterm.sh
fi


typeset -U PATH path
