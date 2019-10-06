set -x EDITOR "kak"
set -x VISUAL "$EDITOR"
set -x NIXPKGS_ALLOW_UNFREE 1
if _exists nvim
    set -x MANPAGER "nvim -c 'set ft=man' -"
end
set -x npm_config_prefix "$HOME/.node_modules"
set -x NNN_USE_EDITOR 1
set -x PSVM_HOME "$HOME/.config/psvm"
set -x CARGO_HOME "$HOME/.config/cargo"
set -x RUSTUP_HOME "$HOME/.config/rustup"
if test (uname) = "Linux"
    set -x BROWSER "chromium"
end

# PATH (highest priority is defined last)
set paths "$HOME/.emacs.d/bin" $paths
set paths "$HOME/.node_modules/bin" $paths
set paths "$PSVM_HOME/current/bin" $paths
set paths "$CARGO_HOME/bin" $paths
set paths "$HOME/.ghcup/bin" $paths
set paths "$HOME/.cabal/bin" $paths
set paths "$HOME/.config/git/scripts" $paths
set paths "$HOME/.local/bin" $paths
for i in $paths
    if test -d $i
        set -x PATH "$i" $PATH
    end
end
