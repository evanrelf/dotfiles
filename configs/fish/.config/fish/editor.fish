for editor in kak nvim evil vim vi nano
    if _exists "$editor"
        set --export EDITOR "$editor"
        break
    end
end

if _exists emacs
    if _exists evil
        abbr --add e "evil"
    end

    function emacs
        TERM=xterm-24bit command emacs --no-window-system $argv
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
end

if _exists kak
    abbr --add k "kak"
end

if _exists nvim
    abbr --add v "nvim"

    function nvim
        if test -n "$TMUX"
            env TERM=screen-256color nvim $argv
        else
            command nvim $argv
        end
    end
end

