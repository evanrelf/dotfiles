if _exists fzf
    set --export FZF_LEGACY_KEYBINDINGS 0
    set --export FZF_DEFAULT_OPTS "--color=light --exact --height $FZF_TMUX_HEIGHT --reverse --ansi --no-multi"
    set --export FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git' --color=always"
    set --export FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
    set --export FZF_CD_COMMAND "fd --type directory --follow --hidden --exclude '.git'"
    set --export FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND"
    set --export FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"
end
