source "$__fish_config_dir/home-manager.fish" 2>/dev/null || true
set --global --export XDG_CONFIG_HOME "$HOME/.config"
set --global --export XDG_DATA_HOME "$HOME/.local/share"
set --global --export GHCUP_USE_XDG_DIRS 1
set --global --export COLORTERM "$TERM"
set --global --export EDITOR nvim
set --global --export RUSTUP_HOME "$XDG_CONFIG_HOME/rustup"
set --global --export CARGO_HOME "$XDG_CONFIG_HOME/cargo"
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
set --universal FZF_CD_COMMAND "fd --type directory --follow --exclude '.git' . \$dir | sed -e 's_^\./__'"
set --universal FZF_CD_WITH_HIDDEN_COMMAND "fd --type directory --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_FIND_FILE_COMMAND "fd --type file --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_OPEN_COMMAND "fd --type file --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
if test -z "$IN_NIX_SHELL"
    if test (uname) = Darwin
        set --global --export --prepend PATH /opt/homebrew/bin
    end
    set --global --export --prepend PATH /nix/var/nix/profiles/default/bin
    set --global --export --prepend PATH "$HOME/.nix-profile/bin"
    set --global --export --prepend PATH "$CARGO_HOME/bin"
    set --global --export --prepend PATH "$HOME/.config/git/scripts"
    set --global --export --prepend PATH "$HOME/.local/bin"
    if command -q rustup
        set --global --export --prepend PATH (dirname (rustup which rustc))
    end
end
if command -q direnv
    set --global --export DIRENV_LOG_FORMAT ""
    direnv hook fish | source
end
if command -q cached-nix-shell
    alias nix-shell "cached-nix-shell --run fish"
end
if command -q zoxide
    set --global --export _ZO_DATA_DIR "$XDG_DATA_HOME/zoxide"
    set --global --export _ZO_FZF_OPTS "$FZF_DEFAULT_OPTS"
    zoxide init fish | source
end
if command -q starship
    starship init fish | source
end
alias ls "ls --color=auto"
alias less "less -RMK"
set --global fish_greeting ""
abbr --add --global g git
abbr --add --global k kak
abbr --add --global n nvim
function rg
    if isatty stdout
        command rg --pretty $argv | command less -RMFXK
    else
        command rg $argv
    end
end
function hstype
    rg \
        --type haskell \
        --multiline \
        "^ *\b(?:type|type\s+family|newtype|data|class)\b.*\s+\b(?:$argv[1])\b" \
        $argv[2..-1]
end
function hsterm
    rg \
        --type haskell \
        --multiline \
        "^ *\b(?:$argv[1])\b\s+::" \
        $argv[2..-1]
end
source "$__fish_config_dir/local.fish" 2>/dev/null || true
