if test -z "$IN_NIX_SHELL"
    if _exists luarocks
        set --prepend PATH "$HOME/.luarocks/bin"
    end

    if _exists brew
        set --prepend PATH "/usr/local/sbin"
    end

    if _exists emacs && test -d "$HOME/.emacs.d/bin"
        set --prepend PATH "$HOME/.emacs.d/bin"
    end

    if _exists emacs && test -d "$HOME/.config/emacs/bin"
        set --prepend PATH "$HOME/.config/emacs/bin"
    end

    set --export RUSTUP_HOME "$HOME/.config/rustup"
    set --export CARGO_HOME "$HOME/.config/cargo"
    set --prepend PATH "$CARGO_HOME/bin"
    set --prepend PATH "$HOME/.local/share/cargo/bin"

    if _exists cabal
        set --prepend PATH "$HOME/.cabal/bin"
    end

    if _exists git
        set --prepend PATH "$HOME/.config/git/scripts"
    end

    set --prepend PATH "$HOME/.local/bin"
end
