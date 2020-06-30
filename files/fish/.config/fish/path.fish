if _exists brew
    set --prepend PATH "/usr/local/sbin"
end

if _exists emacs && test -d "$HOME/.emacs.d/bin"
    set --prepend PATH "$HOME/.emacs.d/bin"
end

if _exists npm
    set --export npm_config_prefix "$HOME/.local/share/node_modules"
    set --prepend PATH "$npm_config_prefix/bin"
end

if _exists rustup
    set --export RUSTUP_HOME "$HOME/.config/rustup"
end

if _exists cargo
    set --export CARGO_HOME "$HOME/.config/cargo"
    set --prepend PATH "$CARGO_HOME/bin"
end

if _exists cabal
    set --prepend PATH "$HOME/.cabal/bin"
end

if _exists git
    set --prepend PATH "$HOME/.config/git/scripts"
end

set --prepend PATH "$HOME/.local/bin"
