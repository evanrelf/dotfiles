set --global --export XDG_CONFIG_HOME "$HOME/.config"
set --global --export COLORTERM "$TERM"
set --global --export EDITOR nvim
set --global --export RUSTUP_HOME "$XDG_CONFIG_HOME/rustup"
set --global --export CARGO_HOME "$XDG_CONFIG_HOME/cargo"
set --global --export --prepend PATH "$HOME/.config/git/scripts"
if test -z "$IN_NIX_SHELL"
    if test (uname) = Darwin
        set --global --export --prepend PATH /opt/homebrew/bin
    end
    set --global --export --prepend PATH /nix/var/nix/profiles/default/bin
    set --global --export --prepend PATH "$HOME/.nix-profile/bin"
    set --global --export --prepend PATH "$CARGO_HOME/bin"
    if command -v rustup >/dev/null
        set --global --export --prepend PATH (dirname (rustup which rustc))
    end
end
if test -d "$HOME/.nix-profile/channels/"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-profile/channels"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-profile/channels/nixpkgs"
else
    echo "Using imperative Nix channels"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels"
    set --global --export --prepend NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
end
if command -v direnv >/dev/null
    set --global --export DIRENV_LOG_FORMAT ""
    direnv hook fish | source
end
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
set --universal FZF_CD_COMMAND "fd --type directory --follow --exclude '.git' . \$dir | sed -e 's_^\./__'"
set --universal FZF_CD_WITH_HIDDEN_COMMAND "fd --type directory --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_FIND_FILE_COMMAND "fd --type file --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_OPEN_COMMAND "fd --type file --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
alias ls "ls --color=auto"
set --global fish_greeting ""
abbr --add --global g git
abbr --add --global k kak
abbr --add --global n nvim
function hstype
    rg \
        --type haskell \
        "^ *\b(?:type|type\s+family|newtype|data|class)\b\s+\b(?:$argv[1])\b" \
        $argv[2..-1]
end
function hsfunc
    rg \
        --type haskell \
        --multiline \
        "^ *\b(?:$argv[1])\b\s+::" \
        $argv[2..-1]
end
