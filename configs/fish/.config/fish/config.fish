source "$__fish_config_dir/home-manager.fish" 2>/dev/null || true
set --global --export XDG_CONFIG_HOME "$HOME/.config"
set --global --export XDG_DATA_HOME "$HOME/.local/share"
set --global --export GHCUP_USE_XDG_DIRS 1
set --global --export COLORTERM "$TERM"
set --global --export EDITOR kak
set --global --export RUSTUP_HOME "$XDG_CONFIG_HOME/rustup"
set --global --export CARGO_HOME "$XDG_CONFIG_HOME/cargo"
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
set --universal FZF_CD_COMMAND "fd --type directory --follow --exclude '.git' --exclude '.jj' . \$dir | sed -e 's_^\./__'"
set --universal FZF_CD_WITH_HIDDEN_COMMAND "fd --type directory --follow --exclude '.git' --exclude '.jj' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_FIND_FILE_COMMAND "fd --type file --follow --exclude '.git' --exclude '.jj' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_OPEN_COMMAND "fd --type file --follow --exclude '.git' --exclude '.jj' --hidden . \$dir | sed -e 's_^\./__'"
if test -z "$IN_NIX_SHELL"
    set --global --export NIX_SSL_CERT_FILE /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt
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
    if test -d "$HOME/.config/emacs/bin"
        set --global --export --prepend PATH "$HOME/.config/emacs/bin"
    end
end
if command -q direnv
    set --global --export DIRENV_LOG_FORMAT ""
    direnv hook fish | source
end
# TODO: Combine `nix-your-shell` and `cached-nix-shell`, or drop
# `cached-nix-shell`
if command -q nix-your-shell
    nix-your-shell fish | source
else if command -q cached-nix-shell
    alias nix-shell "cached-nix-shell --run fish"
else
    alias nix-shell "command nix-shell --run fish"
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
    jj debug completion --fish | source
end
if test -e "$__fish_config_dir/conf.d/plugin-fish-colored-man.fish"
    set --global man_blink --reverse blue
    set --global man_bold --dim --bold blue
    set --global man_standout --background brwhite black
    set --global man_underline --underline brblack
end
function fish_user_key_bindings
    bind \cz 'fg 2>/dev/null; commandline -f repaint'
end
alias ls "ls --color=auto"
alias less "less -RMK"
alias cargod "cargo watch --exec 'lclippy --all-targets' --clear --quiet"
set --global fish_greeting ""
abbr --add --global g git
abbr --add --global j jj
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
function hsimport
    rg \
        --type haskell \
        "^import +(?:\"[\w-]+\" +)?(?:qualified +)?\b$argv[1]\b(?: .*)?\$" \
        $argv[2..-1]
end
source "$__fish_config_dir/local.fish" 2>/dev/null || true
