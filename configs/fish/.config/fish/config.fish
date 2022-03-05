set --global --export --prepend PATH "/nix/var/nix/profiles/default/bin"
set --global --export --prepend PATH "$HOME/.nix-profile/bin"
set --global --export COLORTERM "$TERM"
alias ls "command ls --color=auto"
