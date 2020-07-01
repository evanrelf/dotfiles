if _exists kak
    set --export EDITOR "kak"
else if _exists nvim
    set --export EDITOR "nvim"
else
    set --export EDITOR "vi"
end
