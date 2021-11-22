if _exists zoxide
    set --export _ZO_DATA "$HOME/.cache/zoxide"
    if test -f "$HOME/.zo"
        rm "$HOME/.zo"
    end
    zoxide init fish | source
end
