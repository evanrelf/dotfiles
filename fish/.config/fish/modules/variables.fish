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

# PATH (highest priority is defined first)
set -l paths ""
set paths "$HOME/.local/bin" $paths
set paths "$HOME/.config/git/scripts" $paths
set paths "$HOME/.cabal/bin" $paths
set paths "$HOME/.ghcup/bin" $paths
set paths "$CARGO_HOME/bin" $paths
set paths "$PSVM_HOME/current/bin" $paths
set paths "$HOME/.node_modules/bin" $paths
set paths "$HOME/.emacs.d/bin" $paths
for i in $paths
    if test -d $i
        set -x PATH $i $PATH
    end
end

# Restore nix-shell PATH priority
if test -n "$IN_NIX_SHELL"
    set -l nix_paths ""
    set -l non_nix_paths ""
    for i in $PATH
        if test "$i" = "."
            # Do nothing
        else if echo "$i" | grep -q "/nix/store"
            set nix_paths $nix_paths $i
        else
            set non_nix_paths $non_nix_paths $i
        end
    end
    set -x PATH ""
    for i in $nix_paths
        set -x PATH $PATH $i
    end
    for i in $non_nix_paths
        set -x PATH $PATH $i
    end
end
