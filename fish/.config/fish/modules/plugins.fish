# Auto-install fisher and plugins
if not functions -q fisher
    echo "Installing fisher..."
    curl https://git.io/fisher --create-dirs -sLo $HOME/.config/fish/functions/fisher.fish
    source $HOME/.config/fish/functions/fisher.fish
    fisher
end

set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_DEFAULT_OPTS "--exact --height $FZF_TMUX_HEIGHT --reverse --ansi"
set -U FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git' --color=always"
set -U FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
set -U FZF_CD_COMMAND "fd --type directory --follow"
set -U FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND --hidden --exclude '.git'"
set -U FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"
