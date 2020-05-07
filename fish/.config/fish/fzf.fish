if _exists fzf
    set --global FZF_LEGACY_KEYBINDINGS 0
    set --global FZF_DEFAULT_OPTS "--exact --height $FZF_TMUX_HEIGHT --reverse --ansi"
    set --global FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git' --color=always"
    set --global FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
    set --global FZF_CD_COMMAND "fd --type directory --follow"
    set --global FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND --hidden --exclude '.git'"
    set --global FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"
end
