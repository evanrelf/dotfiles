source "$__fish_config_dir/home-manager.fish" 2>/dev/null || true
set --universal fish_features qmark-noglob
set --global --export XDG_CONFIG_HOME "$HOME/.config"
set --global --export XDG_DATA_HOME "$HOME/.local/share"
set --global --export GHCUP_USE_XDG_DIRS 1
set --global --export EDITOR kak
set --global --export RUSTUP_HOME "$XDG_CONFIG_HOME/rustup"
set --global --export CARGO_HOME "$XDG_CONFIG_HOME/cargo"
set --global --export RIPGREP_CONFIG_PATH "$XDG_CONFIG_HOME/ripgrep/config"
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
set --universal FZF_CD_COMMAND "fd --type directory --follow --exclude '.git' --exclude '.jj' . \$dir | sed -e 's_^\./__'"
set --universal FZF_CD_WITH_HIDDEN_COMMAND "fd --type directory --follow --exclude '.git' --exclude '.jj' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_FIND_FILE_COMMAND "fd --type file --follow --exclude '.git' --exclude '.jj' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_OPEN_COMMAND "fd --type file --follow --exclude '.git' --exclude '.jj' --hidden . \$dir | sed -e 's_^\./__'"
if test -z "$IN_NIX_SHELL"
    set --global --export NIX_SSL_CERT_FILE /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt
    set --global --export NIX_PROFILES "/nix/var/nix/profiles/default $HOME/.nix-profile"
    if test (uname) = Darwin
        set --global --export --prepend PATH /opt/homebrew/bin
    end
    set --global --export --prepend PATH /nix/var/nix/profiles/default/bin
    set --global --export --prepend PATH "$HOME/.nix-profile/bin"
    set --global --export --prepend PATH "$CARGO_HOME/bin"
    set --global --export --prepend PATH "$HOME/.config/git/scripts"
    set --global --export --prepend PATH "$HOME/.local/bin"
    if test -d "$HOME/.config/emacs/bin"
        set --global --export --prepend PATH "$HOME/.config/emacs/bin"
    end
end
if command -q direnv
    set --global --export DIRENV_LOG_FORMAT ""
    direnv hook fish | source
end
if command -q nix-your-shell
    nix-your-shell fish | source
end
if command -q zoxide
    set --global --export _ZO_DATA_DIR "$XDG_DATA_HOME/zoxide"
    set --global --export _ZO_FZF_OPTS "$FZF_DEFAULT_OPTS"
    zoxide init fish | source
end
if command -q starship
    starship init fish | source
end
if command -q jj
    set --global --export JJ_CONFIG "$XDG_CONFIG_HOME/jj/config.toml"
    jj util completion --fish | source
end
function fish_user_key_bindings
    bind \cz 'fg 2>/dev/null; commandline -f repaint'
end
alias ls "ls --color=auto"
alias less "less -RMK"
set --global fish_greeting ""
abbr --add --global g git
abbr --add --global j jj
abbr --add --global k kak
abbr --add --global n nvim
function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.{2,}$' --function multicd
function last_history_item
    echo $history[1]
end
abbr --add !! --position anywhere --function last_history_item
source "$__fish_config_dir/local.fish" 2>/dev/null || true
