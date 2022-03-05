set --global --export --prepend PATH "/nix/var/nix/profiles/default/bin"
set --global --export --prepend PATH "$HOME/.nix-profile/bin"
set --global --export COLORTERM "$TERM"
set --universal FZF_LEGACY_KEYBINDINGS 0
set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
alias ls "command ls --color=auto"
