set --global --export --prepend PATH "/nix/var/nix/profiles/default/bin"
set --global --export --prepend PATH "$HOME/.nix-profile/bin"
set --global --export COLORTERM "$TERM"
set --global --export EDITOR "nvim"
begin
    set --universal FZF_LEGACY_KEYBINDINGS 0
    set --universal FZF_DEFAULT_OPTS "--color=light --height=40% --layout=reverse --exact"
    set --local fzf_default_command "fd --type file --follow --exclude '.git'"
    set --universal FZF_CD_COMMAND "$fzf_default_command"
    set --universal FZF_CD_WITH_HIDDEN_COMMAND "$fzf_default_command --hidden"
    set --universal FZF_FIND_FILE_COMMAND "$fzf_default_command --hidden"
    set --universal FZF_OPEN_COMMAND "$fzf_default_command --hidden"
end
alias ls "command ls --color=auto"
