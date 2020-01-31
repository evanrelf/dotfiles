set -x EDITOR "kak"
set -x VISUAL "$EDITOR"
set -x NIXPKGS_ALLOW_UNFREE 1
# if _exists kak
#     set -x MANPAGER "col -b -x | kak -e 'set-option buffer filetype man'"
# else
if _exists nvim
    set -x MANPAGER "nvim -c 'set ft=man' -"
else
    set -x MANPAGER "less"
end
# if _exists kak
#     set -x MANPAGER "col -b -x | kak -e 'set-option buffer filetype man'"
# else if _exists nvim
#     set -x MANPAGER "nvim -c 'set ft=man' -"
# else
#     set -x MANPAGER "less"
# end
set -x npm_config_prefix "$HOME/.node_modules"
set -x NNN_USE_EDITOR 1
set -x PSVM_HOME "$HOME/.config/psvm"
set -x CARGO_HOME "$HOME/.config/cargo"
set -x RUSTUP_HOME "$HOME/.config/rustup"
if test (uname) = "Linux"
    set -x BROWSER "chromium"
end

# Fix issues:
# https://github.com/NixOS/nix/pull/3130
# https://github.com/NixOS/nix/issues/1865
set --append NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
set --append NIX_PATH "$HOME/.nix-defexpr/channels"

# PATH (highest priority is defined first)
set -l paths ""
set --append paths "$HOME/.local/bin"
set --append paths "$HOME/.config/git/scripts"
set --append paths "$HOME/.cabal/bin"
# set --append paths "$HOME/.ghcup/bin"
set --append paths "$CARGO_HOME/bin"
set --append paths "$PSVM_HOME/current/bin"
set --append paths "$HOME/.node_modules/bin"
set --append paths "$HOME/.emacs.d/bin"
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
