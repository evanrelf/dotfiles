if _exists zoxide
    set --export _ZO_DATA_DIR "$HOME/.local/share/"
    zoxide init fish | source
end
