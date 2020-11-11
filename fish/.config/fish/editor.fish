if _exists kak
    set --export EDITOR "kak"
else if _exists evil
    set --export EDITOR "evil"
else if _exists nvim
    set --export EDITOR "nvim"
else
    set --export EDITOR "vi"
end

# Magit as a standalone command, for use alongside other editors
if _exists evil
    function magit
        evil --eval "(magit-status)" $argv
    end
end

# Better support for Emacs' vterm package
function vterm_printf
    if test -n "$TMUX"
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end
