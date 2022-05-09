if test (uname) = Darwin
    set --global --export --prepend PATH /opt/homebrew/bin
end
set --global --export --prepend PATH /nix/var/nix/profiles/default/bin
set --global --export --prepend PATH "$HOME/.nix-profile/bin"
if test -d "$HOME/.nix-profile/channels/"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-profile/channels"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-profile/channels/nixpkgs"
else
    echo "Using imperative Nix channels"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
end
set --global --export XDG_CONFIG_HOME "$HOME/.config"
set --global --export COLORTERM "$TERM"
set --global --export EDITOR nvim
set --global --export RUSTUP_HOME "$XDG_CONFIG_HOME/rustup"
set --global --export CARGO_HOME "$XDG_CONFIG_HOME/cargo"
set --global --export --prepend PATH "$CARGO_HOME/bin"
set --global --export --prepend PATH (dirname (rustup which rustc))
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
set --universal FZF_CD_COMMAND "fd --type directory --follow --exclude '.git'          . \$dir | sed -e 's_^\./__' -e 's_\$_/_'"
set --universal FZF_CD_WITH_HIDDEN_COMMAND "fd --type directory --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__' -e 's_\$_/_'"
set --universal FZF_FIND_FILE_COMMAND "fd --type file      --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_OPEN_COMMAND "fd --type file      --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
alias ls "ls --color=auto"
set --global fish_greeting ""
abbr --add --global g git
abbr --add --global k kak
abbr --add --global n nvim
