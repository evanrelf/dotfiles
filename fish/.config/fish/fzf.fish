if _exists fzf
    set FZF_LEGACY_KEYBINDINGS 0
    set FZF_DEFAULT_OPTS "--exact --height $FZF_TMUX_HEIGHT --reverse --ansi"
    set FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git' --color=always"
    set FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
    set FZF_CD_COMMAND "fd --type directory --follow"
    set FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND --hidden --exclude '.git'"
    set FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"
end
