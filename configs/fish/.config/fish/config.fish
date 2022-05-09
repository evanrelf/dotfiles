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
if command -v rustup >/dev/null
    set --global --export --prepend PATH (dirname (rustup which rustc))
end
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
set --universal FZF_CD_COMMAND "fd --type directory --follow --exclude '.git'          . \$dir | sed -e 's_^\./__' -e 's_\$_/_'"
set --universal FZF_CD_WITH_HIDDEN_COMMAND "fd --type directory --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__' -e 's_\$_/_'"
set --universal FZF_FIND_FILE_COMMAND "fd --type file      --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
set --universal FZF_OPEN_COMMAND "fd --type file      --follow --exclude '.git' --hidden . \$dir | sed -e 's_^\./__'"
alias ls "ls --color=auto"
set --global fish_greeting ""
abbr --add --global k kak
abbr --add --global n nvim
abbr --add --global g git
abbr --add --global ga "git add"
abbr --add --global gap "git add --patch"
abbr --add --global gb "git branch"
abbr --add --global gbd "git branch --delete --force"
abbr --add --global gbm "git branch --move"
abbr --add --global gc "git commit"
abbr --add --global gca "git commit --amend"
abbr --add --global gcan "git commit --amend --no-edit"
abbr --add --global gd "git diff"
abbr --add --global gds "git diff --staged"
abbr --add --global gp "git push"
abbr --add --global gpf "git push --force-with-lease"
abbr --add --global gpu "git push --set-upstream origin HEAD"
abbr --add --global grb "git rebase"
abbr --add --global grbi "git rebase --interactive"
abbr --add --global gre "git restore"
abbr --add --global gres "git restore --staged"
abbr --add --global gs "git status"
abbr --add --global gsw "git switch"
abbr --add --global gswc "git switch --create"
